/* Copyright (C) Advanced RISC Machines Limited, 1995. All rights reserved. */

/*
 * RCS $Revision: 1.3 $
 * Checkin $Date: 1995/06/26 09:52:39 $
 * Revising $Author: mwilliam $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>             /* opendir() etc. */

#include "host.h"

#ifndef NLS
#  define NLS
#endif
#include "msg.h"

/* based on msgfile.c, this file takes two message files, reads one into
 * memory, and then reports any new, changed or deleted messages from one
 * file to the other.
 */

/*
 * Program to read a message file and then perform lookups into it.
 * Uses a closed hash table for string lookups.
 */

#ifndef HASH
#define HASH 2048               /* Based on 640 messages in the compiler */
#endif
#ifndef STRIDE
#define STRIDE 8
#endif
#ifndef HASH_VALUE
#define HASH_VALUE 29
#endif
#ifndef MESSAGEFILE
#define MESSAGEFILE "messages"
#endif
#if STRIDE == stride            /* debugging */
extern int stride;
#endif

#define USAGE "Usage: %s [-n] oldfile [newfile]\n"

#ifdef DBCS                     /* DOS code page scheme */
static char next_off[256];      /* Table of offset values */
#  define NextChar(sp) (next_off[*((unsigned char *)(sp))])
#  define CopyChar(to,from) \
  if (next_off[(unsigned char)(*(to)=*(from))]==2) (to)[1]=(from)[1];
#  include <windows.h>
static void init_char(void)
{
  /* Initialise the next-char offset code */
  CPINFO lpCPInfo;
  BYTE *vbLBRange;
  int i;

  GetCPInfo(CP_ACP, &lpCPInfo);
  vbLBRange=lpCPInfo.LeadByte;

  memset(next_off,'\001',256);

  while (*vbLBRange) {
    for (i=vbLBRange[0];i<=vbLBRange[1];i++) next_off[i]=2;
    vbLBRange+=2;
  }
}
#  define InitChar() init_char()

/* Define a msg_strchr for external consumption */
char *msg_strchr(char *s,char c)
{
  InitChar();
  while (*s)
    if (*s==c) return s;
    else s+=NextChar(s);
  if (c) return s;
  else return NULL;
}
#else                           /* Standard ASCII */
#  define NextChar(sp) 1
#  define InitChar()
#  define CopyChar(to,from) (*(to)=*(from))
#  define msg_strchr strchr
#endif

#define LIMIT 20

static int check_fmt(const char *format)
{
  int i;
  const char *t;
  
  i=0;
  t=msg_strchr(format,'%');
  while (t) {
    if (t[1]!='\0' && t[1]!='%') {
      int arg;
      
      if (t[2]=='$' && t[1]!='0' && isdigit(t[1])) {
        arg=t[1]-'1';
        
        t=&t[3];
      } else {
        arg=-1;
        t=&t[1];
      }
      
      while (*t=='-' || *t=='+' || *t==' ' || *t=='#') t++;
#ifndef DONT_SUPPORT_STARS
      if (*t=='*') {
        if (t[2]=='$' && t[1]!='0' && isdigit(t[1])) {
          t+=3;
        } else if (i>=LIMIT) return 1;
      } else
#endif
        while (isdigit(*t)) t++;
      if (*t=='.') {
        t++;
#ifndef DONT_SUPPORT_STARS
        if (*t=='*') {
          if (t[2]=='$' && t[1]!='0' && isdigit(t[1])) {
            t+=3;
          } else if (i>=LIMIT) return 1;
        } else
#endif
          while (isdigit(*t)) t++;
      }
      /* t now points at the format argument, or a preceeding 'l' */
      if (arg<0) arg=i++;
      if (arg>=LIMIT) return 1;
    } else if (t[1]=='%') {
      t++;
    }
    
    t=msg_strchr(t+1,'%');          /* Go to next percent */
  }
  
  return 0;
}

static char *hash[HASH];
static int flg[HASH];

static int nodiffs=0;
static int justtags=0;

static unsigned int hash_string(const char *string)
{
  unsigned int hv=0;

  while (*string) {
    hv=HASH_VALUE*hv+*string;
    string+=NextChar(string);
  }

  return (hv*HASH_VALUE) % HASH;
}

static char *hash_in(char *string,int *hvp)
{
  char *p;
  unsigned int hv=0;
  unsigned int hv_base;

  for (p=string;*p!=':' && *p!='\n' && *p;p+=NextChar(p))
    hv=HASH_VALUE*hv+*p;

  if (*p==':') *p++='\0';
  else {
    *hvp=0;                     /* Indicate processing error */
    return NULL;                /* error - no ':' on line */
  }

  hv_base=hv=(hv*HASH_VALUE) % HASH;

  do {
    if (hash[hv]==NULL) {
      *hvp=hv;
      return p;
    }
    if (strcmp(string,hash[hv])==0) {
      *hvp=hv;
      return p;
    }
    hv=(hv+STRIDE) % HASH;
  } while (hv!=hv_base);

  return NULL;
}

static int hash_out(const char *string)
{
  int hv=hash_string(string);
  int hv_base=hv;

  do {
    if (hash[hv]==NULL) return -1; /* Not found */
    if (strcmp(string,hash[hv])==0) return hv;
    hv=(hv+STRIDE) % HASH;
  } while (hv!=hv_base);

  return -1;                    /* Indicate not found */
}

static char *load_file(const char *filename,size_t *size)
{
  FILE *f;
  size_t sz;
  char *blk;

  f=fopen(filename,"rb");
  if (f==NULL) {
    perror("message file did not open");
    return NULL;
  }

  if (fseek(f,0,SEEK_END)) {
    perror("could not read message file");
    return NULL;
  }

  sz=ftell(f); *size=sz;

  if ((blk=(char *)malloc(sz+1))!=NULL) {
    blk[sz]='\0';
    fseek(f,0,SEEK_SET);
    if (fread(blk,1,sz,f)<sz) {
      perror("error loading message file");
      free(blk);
      blk=NULL;
    }
  }

  fclose(f);
  return blk;
}

char *msg_lookup(msg_t msg_tag)
{
  int hv;
  const char *tag=(const char *)msg_tag;

  hv=hash_out(tag);

  if (hv>=0) return hash[hv]+strlen(hash[hv])+1;
  else return NULL;
}

static char *process_string(char *s,int *lineno)
{
  char *to,*from;

  from=s;

  while (*from!='\\')
    switch (*from) {
    case '\r':
      *from++='\0';
      if (*from=='\n') from++;
      (*lineno)++;
      return from;
    case '\n':
      *from++='\0';
      if (*from=='\r') from++;
      (*lineno)++;
      return from;
    case '\0':
      (*lineno)++;
      return ++from;
    default: from+=NextChar(from); break;
    }

  to=from;

  while (1) {
    /* could use a lookup table for this
     * e.g.
     * ch=lookup[from[1]];
     * if (ch) *to++=ch;
     * else switch (from[1]) {
     *  case '\n': ...
     */     
    switch (from[1]) {
    case '\\': *to++='\\'; break;
    case '\"': *to++='\"'; break;
    case '\?': *to++='\?'; break;
    case '\'': *to++='\''; break;
    case 'a': *to++='\a'; break;
    case 'b': *to++='\b'; break;
    case 'f': *to++='\f'; break;
    case 'n': *to++='\n'; break;
    case 'r': *to++='\r'; break;
    case 't': *to++='\t'; break;
    case 'v': *to++='\v'; break;
    case '\r':          /* \<nl> - remove completely. */
      if (from[2]=='\n') from++;
      (*lineno)++;
      break;
    case '\n':
      if (from[2]=='\r') from++;
      (*lineno)++;
      break;
    default:
      *to++='\\';
    {
      size_t skip=NextChar(&from[1]);
      CopyChar(to,&from[1]);
      to+=skip;
    }
      break;
    }
    from+=2;
    while (*from!='\\')
      switch (*from) {
      case '\r':
        *to++='\0';
        if (*++from=='\n') from++;
        (*lineno)++;
        return from;
      case '\n':
        *to++='\0';
        if (*++from=='\r') from++;
        (*lineno)++;
        return from;
      case '\0':
        return ++from;
      default: {
        size_t skip=NextChar(from);
        CopyChar(to,from);
        to+=skip; from+=skip;
      }
        break;
      }
  }
}

static char *msg_open2(const char *filename,int diff)
{
  char *file,*line;
  int n=1;
  size_t size;
  int i;

#ifdef DEBUG
  fprintf(stderr,"msgopen: Attempting to open \"%s\"\n",filename);
#endif

  if (diff)
    for (i=0;i<HASH;i++) flg[i]=0;
  else
    for (i=0;i<HASH;i++) hash[i]=NULL;

#ifdef MSGS_LINKED
{
  extern char *msg_file;
  line=file=msg_file;
}
#else
  line=file=load_file(filename,&size);
  if (file==NULL) return NULL;
#endif

  while (line && line<file+size) {
    switch (*line) {
    case '#': case ';':         /* comment line */
      line=strchr(line,'\n');
      /* now points to end of line - fixed on next pass of loop */
      break;
    case '\0':                  /* End of file */
      line=NULL;                /* Force exit */
      break;
    case '\r':
      if (*++line=='\n') line++;
      n++;
      break;
    case '\n':
      if (*++line=='\r') line++;
      n++;
      break;
    default: {
      int hv;
      char *colon=hash_in(line,&hv);
      if (colon) {
        char *p=process_string(colon,&n);

        /* hash[hv]!=NULL -> string already hashed in. We just overwrite
         * the old value with the new.
         */
        if (diff) {
          if (hash[hv]) {
            if (hash[hv]>file & hash[hv]<file+size) {
              printf("** Duplicate tag \"%s\" in new file\n",hash[hv]);
            } else {
              char *q=hash[hv]+strlen(hash[hv])+1;
              if (nodiffs==0 && strcmp(q,colon)!=0) {
                printf("** Changed tag \"%s\"\n",hash[hv]);
                if (!justtags)
                  printf("<<<<<\n%s\n=====\n%s\n>>>>>\n\n",q,colon);
              }
              flg[hv]=1;
            }
          } else {
            printf("** New tag \"%s\"\n",line);
            if (!justtags) printf("%s\n\n",colon);
          }
        } else if (hash[hv]) {
          printf("** Duplicate tag \"%s\" in old file\n",hash[hv]);
        }
        if (check_fmt(colon)) {
          printf("** Bad format for tag \"%s\"\n",line);
          if (!justtags) printf("%s\n\n",colon);
        }
        hash[hv]=line;
        line=p;
      } else {
        if (hv==HASH) {
          fprintf(stderr,"hash table full\n");
          free(file);
          return NULL;
        }

        /* Badly formed string. Report the error, and then use
         * process_string to move on to the next line.
         */
        fprintf(stderr,"\
\"%s\", line %d: Badly formed line in message file.\n",
                filename,n);
        line=process_string(line,&n);
      }
    }
      break;
    }
  }
  
  return file;
}

static void compare(const char *f1,const char *f2)
{
  int hv;
  char *handle1=msg_open2(f1,0);
  char *handle2;

  if (f2) {
    handle2=msg_open2(f2,1);

    for (hv=0;hv<HASH;hv++) {
      if (hash[hv] && !flg[hv]) {
        char *q=hash[hv]+strlen(hash[hv])+1;
        printf("** Deleted tag \"%s\"\n",hash[hv]);
        if (!justtags) printf("%s\n\n",q);
        hash[hv]=NULL;
      }
    }
    if (handle2) free(handle2);
  }
  if (handle1) free(handle1);
}

static void directory(const char *dir1, const char *dir2)
{
  DIR *direct;
  struct dirent *dent;
  struct stat buf;
  char fpath1[MAXPATHLEN],fpath2[MAXPATHLEN];
  char *end1,*end2;
  size_t n;

  direct=opendir(dir1);

  sprintf(fpath1,"%s/%n",dir1,&n); end1=fpath1+n;
  if (dir2) {
    sprintf(fpath2,"%s/%n",dir2,&n); end2=fpath2+n;
  }
  
  if (direct==NULL) {
    fprintf(stderr,"** No files found.\n");
    return;
  }

  while ((dent=readdir(direct))!=NULL && ((int)dent)!=EOF) {
    strcpy(end1,dent->d_name);
    if (dir2) strcpy(end2,dent->d_name);
    if (stat(fpath1,&buf)==0 && !S_ISDIR(buf.st_mode)) {
      if (dir2 && stat(fpath2,&buf)==-1) {
        printf("** File \"%s\" not found in new directory.\n",dent->d_name);
      } else {
        printf("== %s\n",dent->d_name);
        compare(fpath1,dir2 ? fpath2 : NULL);
      }
    }
  }

  closedir(direct);
}

int main(int argc, char *argv[])
{
  int hv;
  const char *commandname=argv[0];
  struct stat buf;

  InitChar();

  while (argv[1] && argv[1][0]=='-') {
    char *p;
    p=&argv[1][1];

    while (*p) {
      switch (*p++) {
      case '\0': break;
      case 'n': case 'N':       /* No diffs */
        nodiffs=1;
        break;
      case 'z': case 'Z':       /* Just tag names */
        justtags=1;
        break;
      case '?': case 'h': case 'H': /* Help */
        fprintf(stderr,USAGE,commandname);
        fputs("Options:\n\
 -n  -  Don't show differences, only new/old messages\n\
 -z  -  Just give the tag names, not the messages.\n",
              stderr);
        return 0;
      default: error:
        fprintf(stderr,USAGE,commandname);
        return 1;
      }
    }

    argv++; argc--;
  }    

  if (argc==3 || argc==2) {
    if (stat(argv[1],&buf)!=0) {
      perror("could not stat() directory");
      return 1;
    } else if (S_ISDIR(buf.st_mode)) {
      if (argc==2) {
        directory(argv[1],NULL);
      } else if (stat(argv[2],&buf)!=0) {
        perror("could not stat() directory");
        return 1;
      } else if (S_ISDIR(buf.st_mode)) {
        directory(argv[1],argv[2]);
      } else {
        fprintf(stderr,
                "Both arguments must be directories, or must be files.");
        return 1;
      }
    } else {
      compare(argv[1],argv[2]/*==NULL if argc==2*/);
    }
  } else {
    fprintf(stderr,USAGE,commandname);
    exit(1);
  }


  return 0;
}
