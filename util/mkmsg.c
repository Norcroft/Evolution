/* mkmessage.c */
/* Copyright (C) Advanced RISC Machines Limited, 1995. All rights reserved. */

/*
 * RCS $Revision: 1.6 $
 * Checkin $Date: 1995/04/12 15:58:36 $
 * Revising $Author: mwilliam $
 */

/* Convert a header file to a messagefile. */

#include <stdio.h>
#include <string.h>

/* #define X "Y" -> X:Y
 * #define X "Y\<nl>Z" -> X:Y\<nl>Z  etc.
 * #define X(...) "Y", ... -> X:Y (+emit diagnostic???)
 */

static int quiet=0,small=0;
static int pcodes=0;

static int process_file(const char *);

int main(int argc,char *argv[])
{
  int append=0;
  const char *outfile=NULL;  
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
      case 'o':                 /* Output */
        if (outfile) goto error;
        outfile=argv[2];
        argv++; argc--;
        break;
      case 'p':                 /* CC percent codes */
        pcodes=1;
        break;
      case 'q':                 /* Quiet */
        quiet=1;
        break;
      case 'z':                 /* Small */
        small=1;
        break;
      case '?': case 'h': case 'H': /* Help */
        fprintf(stderr,"Usage: %s [-ahoqz [output]] [file ... [output]]\n",
                commandname);
        fputs("Options:\n\
 -a  -  Append to file\n\
 -h  -  Produce this help\n\
 -o  -  Specifies output file (otherwise is last parameter, or stdout)\n\
 -p  -  Interpret genhdrs % directives\n\
 -q  -  Don't complain about macros taking parameters\n\
 -z  -  Output only the tags, and nothing else from the headers.\n",
              stderr);

        return 0;
      default: error:
        fprintf(stderr,"Usage: %s [-ahoqz [output]] [file ... [output]]\n",
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

  if (argc==1 || argc==2) return process_file(argv[1]);
  else {
    int i;
    for (i=1;i<argc;i++) if (process_file(argv[i])) return 1;
  }

  return 0;
}

static int process_file(const char *filename)
{
  char *macro;
  char buffer[1024],*def,*defptr;
  int line=0,tags=1;

  if (filename && freopen(filename,"r",stdin)==NULL) {
    fprintf(stderr,"Could not open %s as stdin\n",filename);
    return 1;
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
      if (!small) printf(";%s\n",buffer);
    } else if (def[0]!='#' || !tags) {
      if (!small) {
        if (def[0]) printf(";%s\n",def);
        else putchar('\n');
      }
    } else {
      defptr=def+1;
      while (*defptr && isspace(*defptr)) defptr++;
      if (strncmp(defptr,"define",6)) {
        if (!small) { putchar(';'); puts(def); }
      } else {
        char *p,*end;
        defptr+=6;
        while (isspace(*defptr)) defptr++;

        /* defptr points to start of token */
        macro=p=defptr;
        while (*p && *p!='(' && *p!='\\' && !isspace(*p)) p++;
        if (*p=='(') {
          *p++='\0';
          while (*p && *p!=')') p++;
          if (!*p) goto error;
          if (!quiet)
            fprintf(stderr,"+++ Macro '%s' on line %d takes arguments\n",
                    macro,line);
        }

        end=p; if (*p) p++;

        /* p points to end of macroname */
        while (*p!='\"')
          if (!*p) {
            if (p[-1]=='\\') {
              if (gets(p)==NULL) goto error;
              else line++;
            } else break;
          } else p++;

        *end='\0';

        if (!*p) {
          /* Not a macro def'n */
          if (!quiet) printf(";#define %s\n",macro);
        } else {
          def=++p;
          while (1) {
            for (;*p && *p!='\"';p++) if (*p=='\\') {
              if (p[1]) p++; else break;
            }
            if (*p=='\"') break;
            /* Haven't found end of string */
            if (*p) {
              p[1]='\n';
              if (gets(p+2)==NULL) goto error;
              line++;
            } else goto error;
          }
          *p='\0';
          printf("%s:%s\n",macro,def);
        }
      }
    }
  }
  return 0;

 error:
  fprintf(stderr,"Error on input line %d\n",line);
  return 1;
}
