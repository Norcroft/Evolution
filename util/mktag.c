/* mktag.c */
/* Copyright (C) Advanced RISC Machines Limited, 1995. All rights reserved. */

/*
 * RCS $Revision: 1.10 $
 * Checkin $Date: 1995/04/04 13:35:43 $
 * Revising $Author: mwilliam $
 */

/* Convert a header file to a tagged header file. */

#include <stdio.h>
#include <string.h>

/* #define X "Y" -> #define X "X"
 * #define X "Y\<nl>Z" -> #define X "X"  etc.
 * #define X(...) "Y", ... -> #define X(...) "X", ...
 * #define X -> #define X
 */

static int process_file(const char *);
static int small=0,pcodes=0;

int main(int argc,char *argv[])
{
  int append=0;
  char *outfile=NULL;  
  const char *commandname=argv[0];

  while (argv[1] && argv[1][0]=='-') {
    char *p;
    p=&argv[1][1];

    while (*p) {
      switch (*p++) {
      case '\0': break;
      case 'a':                 /* Append */
        append=1;
        break;
      case 'p':                 /* CC percent codes */
        pcodes=1;
        break;
      case 'o':                 /* Output */
        if (outfile) goto error;
        outfile=argv[2];
        argv++; argc--;
        break;
      case 'z':                 /* Small */
        small=1;
        break;
      case '?': case 'h': case 'H': /* Help */
        fprintf(stderr,"Usage: %s [-ahopz [output]] [file ... [output]]\n",
                commandname);
        fputs("Options:\n\
 -a  -  Append to file\n\
 -h  -  Produce this help\n\
 -o  -  Specifies output file (otherwise is last parameter, or stdout)\n\
 -p  -  Interpret genhdrs % directives\n\
 -z  -  Output only the tags, and nothing else from the headers.\n",
              stderr);
        return 0;
      default: error:
        fprintf(stderr,"Usage: %s [-ahopz [output]] [file ... [output]]\n",
                commandname);
        return 1;
      }
    }

    argv++; argc--;
  }    

  if (argc>2 && outfile==NULL) {
    outfile=argv[argc-1]; argc--;
  }

  if (outfile) {
    if (freopen(outfile,append ? "a" : "w",stdout)==NULL) {
      fprintf(stderr,"Could not open %s as stdout\n",outfile);
      return 1;
    }
  }

  if (small) {                  /* protect with a guard define */
    char define[17];
    if (outfile) {
      char *t;
#if defined(__unix) || defined(unix) || defined(__helios) /* Unix version */
      t=strrchr(outfile,'/');
      if (t) t++; else t=outfile;
#elif defined(__ZTC__) || defined(__WATCOMC__) || defined(_MSDOS) || defined(_WINDOWS) || defined(__dos) || defined(_CONSOLE) /* DOS version */
      t=strrchr(outfile,'\\');
      if (t) t++; else t=outfile;
#else
      t=outfile;
#endif
      sprintf(define,"TAG_%.13s",t);
      for (t=define;*t;t++)
        if (*t=='.' || *t=='\\' || *t=='/')
          *t='_';
    } else {
      sprintf(define,"TAG_%8X",time(NULL));
    }
    printf("#ifndef %s\n#define %s\n",define,define);
  }

  if (argc==1) {
    if (process_file(argv[1])) return 1;
  } else {
    int i;
    for (i=1;i<argc;i++) if (process_file(argv[i])) return 1;
  }

  if (small) puts("#endif");

  return 0;
}

static int process_file(const char *filename)
{
  char *macro;
  char buffer[1024],*def,*defptr;
  char outbuffer[1024],*out;
  int line=0;
  int tags=1;

  if (filename && freopen(filename,"r",stdin)==NULL) {
    fprintf(stderr,"Could not open %s as stdin\n",filename);
    exit(2);
  }

  while (gets(def=buffer)!=NULL) {
    line++;
    if (def[0]=='/' && def[1]=='*' && def[2]=='-') {
      /* Special control message */
      if (strcmp(def,"/*-NOT TAGS-*/")==0) tags=0;
      else if (strcmp(def,"/*-TAGS-*/")==0) tags=1;
    } else if (def[0]=='%' && pcodes) {
      switch (def[1]) {
      case 'O': case 'o':
      case 'Z': case 'z': tags=1; break;
      case 'S': case 's': tags=0; break;
      }
      if (!small) puts(buffer+2);
    } else if (def[0]!='#' || !tags) {
      if (!small) puts(buffer);
    } else {
      defptr=def+1;
      while (*defptr && isspace(*defptr)) defptr++;
      if (strncmp(defptr,"define",6)) {
        if (!small) puts(buffer); 
      } else {
        char *p,dropped;
        defptr+=6;
        while (isspace(*defptr)) defptr++;

        /* defptr points to start of token */
        macro=p=defptr;
        while (*p && *p!='(' && *p!='\\' && !isspace(*p)) p++;
        dropped=*p;
        if (dropped) {
          *p++=0;
          sprintf(outbuffer,"#define %s%c",macro,dropped);
          out=outbuffer+strlen(outbuffer);
          if (dropped=='(') {
            while (*p && *p!=')') *out++=dropped=*p++;
            if (!*p) goto error;
          }

          def=p;
          
          /* p points to end of macroname */
          while (*p!='\"')
            if (!*p) {
              if (dropped=='\\') {
                if (gets(p=def)==NULL) goto error;
                else line++;
                *out++='\n';
                dropped=0;
              } else break;           
            } else *out++=dropped=*p++;

          *out='\0';

          if (!*p) {
            if (!small) puts(outbuffer);
          } else {
            printf("%s(msg_t)\"%s\"",outbuffer,macro);
            
            p++;
            while (1) {           /* Skip old def'n */
              for (;*p && *p!='\"';p++) if (*p=='\\') {
                if (p[1]) p++; else break;
              }
              if (*p=='\"') break;
              /* Haven't found end of string */
              if (*p) {
                if (gets(p=def)==NULL) goto error;
                line++;
              } else goto error;
            }
            printf("%s\n",p+1);
          }
        } else {
          if (!small) printf("#define %s\n",macro);
        }
      }
    }
  }
  return 0;

 error:
  fprintf(stderr,"Error on input line %d\n",line);
  return 1;
}

