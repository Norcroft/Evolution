#ifndef ERSATZ_STDLIB_H
#define ERSATZ_STDLIB_H

#include <stddef.h>

typedef struct {
    int quot;
    int rem;
} div_t;

typedef struct {
    long quot;
    long rem;
} ldiv_t;

extern void *malloc(size_t);
extern void free(void *);
extern void *realloc(void *, size_t);
extern char *getenv(const char *);
extern int system(const char *);
extern void exit(int);

extern div_t div(int, int);
extern ldiv_t ldiv(long, long);

#endif /* ERSATZ_STDLIB_H */
