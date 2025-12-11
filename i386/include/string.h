#ifndef ERSATZ_STRING_H
#define ERSATZ_STRING_H

#include <stddef.h>

extern void *memset(void *, int, size_t);
extern void *memcpy(void *, const void*, size_t);
extern int strcmp(const char *, const char *);
extern int strncmp(const char *, const char *, size_t);
extern size_t strlen(const char *);
extern char *strcpy(char *, const char *);
extern char *strncpy(char *, const char *, size_t);
extern char *strcat(char *, const char *);
extern char *strncat(char *, const char *, size_t);
extern char *strchr(const char *, int);
extern char *strrchr(const char *, int);

#endif /* ERSATZ_STRING_H */
