#ifndef ERSATZ_STDIO_H
#define ERSATZ_STDIO_H

#include <stdarg.h>

#ifndef NULL
#define NULL ((void *)0)
#endif

#define EOF (-1)

#define FILENAME_MAX 1024

typedef unsigned long fpos_t;

typedef struct _iobuf FILE;

extern int printf (const char *, ...);
extern int fprintf (FILE *, const char *, ...);
extern int sprintf (char *, const char *, ...);

extern int vprintf (const char *, va_list);
extern int vfprintf (FILE *, const char *, va_list);
extern int vsprintf (char *, const char *, va_list);

extern int remove (const char *);

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

extern int fseek (FILE *, long, int);
extern long ftell(FILE *);
extern void rewind(FILE *);
extern int fgetpos(FILE *, fpos_t *);
extern int fsetpos(FILE *, fpos_t *);

extern int fgetc(FILE *);
extern char *fgets(char *, int, FILE *);

extern FILE *fopen(const char *, const char *);
extern int fclose(FILE *);
extern FILE *freopen(const char *, const char *, FILE *);

#define getc(p) (fgetc(p))

extern FILE *stdin, *stdout, *stderr;

#endif /* ERSATZ_STDIO_H */
