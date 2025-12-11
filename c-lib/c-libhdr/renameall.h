/*
 * Rename everything that the Codemist library defines so that symbol
 * names will clash less with any other C library present on the
 * computer concerned.
 */


#ifndef __renameall_h
#define __renameall_h

#define _Codemist 1

#define main(a,b)     _Codemist_main(a,b)

#define __assert(x,y,z) _Codemist___assert(x,y,z)

#define __ctype     _Codemist___ctype
/*
 * #define isalnum    _Codemist_isalnum
 * #define isalpha    _Codemist_isalpha
 * #define iscntrl    _Codemist_iscntrl
 * #define isdigit    _Codemist_isdigit
 * #define isgraph    _Codemist_isgraph
 * #define islower    _Codemist_islower
 * #define isprint    _Codemist_isprint
 * #define ispunct    _Codemist_ispunct
 * #define isspace    _Codemist_isspace
 * #define isupper    _Codemist_isupper
 * #define isxdigit   _Codemist_isxdigit
 * #define tolower(x) _Codemist_tolower(x)
 * #define toupper(x) _Codemist_toupper(x)
 */

#define errno      _Codemist_errno
extern volatile int _Codemist_errno;

#define setlocale(a, b) _Codemist_setlocale(a,b)
#define lconv      _Codemist_lconv
#define localeconv _Codemist_localeconv

#define __huge_val    _Codemist___huge_val
#define acos(x)       _Codemist_acos(x)
#define asin(x)       _Codemist_asin(x)
#define atan(x)       _Codemist_atan(x)
#define atan2(x,y)    _Codemist_atan2(x,y)
#define cos(x)        _Codemist_cos(x)
#define sin(x)        _Codemist_sin(x)
#define tan(x)        _Codemist_tan(x)
#define cosh(x)       _Codemist_cosh(x)
#define sinh(x)       _Codemist_sinh(x)
#define tanh(x)       _Codemist_tanh(x)
#define exp(x)        _Codemist_exp(x)
#define frexp(x,y)    _Codemist_frexp(x,y)
#define ldexp(x,y)    _Codemist_ldexp(x,y)
#define log(x)        _Codemist_log(x)
#define log10(x)      _Codemist_log10(x)
#define modf(x,y)     _Codemist_modf(x,y)
#define pow(x,y)      _Codemist_pow(x,y)
#define sqrt(x)       _Codemist_sqrt(x)
#define ceil(x)       _Codemist_ceil(x)
#define fabs(x)       _Codemist_fabs(x)
#define floor(x)      _Codemist_floor(x)
#define fmod(x,y)     _Codemist_fmod(x,y)
#define acosf(x)      _Codemist_acosf(x)
#define asinf(x)      _Codemist_asinf(x)
#define atanf(x)      _Codemist_atanf(x)
#define atan2f(x,y)   _Codemist_atan2f(x,y)
#define cosf(x)       _Codemist_cosf(x)
#define sinf(x)       _Codemist_sinf(x)
#define tanf(x)       _Codemist_tanf(x)
#define coshf(x)      _Codemist_coshf(x)
#define sinhf(x)      _Codemist_sinhf(x)
#define tanhf(x)      _Codemist_tanhf(x)
#define expf(x)       _Codemist_expf(x)
#define frexpf(x,y)   _Codemist_frexpf(x,y)
#define ldexpf(x,y)   _Codemist_ldexpf(x,y)
#define logf(x)       _Codemist_logf(x)
#define log10f(x)     _Codemist_log10f(x)
#define modff(x,y)    _Codemist_modff(x,y)
#define powf(x,y)     _Codemist_powf(x,y)
#define sqrtf(x)      _Codemist_sqrtf(x)
#define ceilf(x)      _Codemist_ceilf(x)
#define fabsf(x)      _Codemist_fabsf(x)
#define floorf(x)     _Codemist_floorf(x)
#define fmodf(x,y)    _Codemist_fmodf(x,y)

/* setjmp nasty! */
#define longjmp(a, b) _Codemist_longjmp(a,b)

#define signal(a, b)  _Codemist_signal(a,b)
#define raise(a)      _Codemist_raise(a)

#define __iob                  _Codemist___iob
#define remove(a)              _Codemist_remove(a)
#define rename(a,b)            _Codemist_rename(a,b)
#define tmpfile                _Codemist_tmpfile  
#define tmpname(a)             _Codemist_tmpname(a)
#define fclose(a)              _Codemist_fclose(a)
#define fflush(a)              _Codemist_fflush(a)
#define fopen(a,b)             _Codemist_fopen(a,b)
#define freopen(a,b,c)         _Codemist_freopen(a,b,c)
#define setbuf(a,b)            _Codemist_setbuf(a,b)
#define setvbuf(a,b,c,d)       _Codemist_setvbuf(a,b,c,d)
#define fprintf                _Codemist_fprintf
#define printf                 _Codemist_printf
#define sprintf                _Codemist_sprintf
#define fscanf                 _Codemist_fscanf
#define scanf                  _Codemist_scanf
#define sscanf                 _Codemist_sscanf
#define vprintf                _Codemist_vprintf
#define vfprintf               _Codemist_vfprintf
#define vsprintf               _Codemist_vsprintf
#define fgetc(a)               _Codemist_fgetc(a)
#define fgets(a,b,c)           _Codemist_fgets(a,b,c)
#define fputc(a,b)             _Codemist_fputc(a,b)
#define fputs(a,b)             _Codemist_fputs(a,b)
#define __fillbuf(a)           _Codemist___fillbuf(a)
/* getc */             /* getc */
/* getchar */          /* getchar */
#define gets(a)                _Codemist_gets(a)
#define __flsbuf(a,b)          _Codemist___flsbuf(a,b)
/* putc */             /* putc */
#define puts(a)                _Codemist_puts(a)
#define ungetc(a,b)            _Codemist_ungetc(a,b)
#define fread(a,b,c,d)         _Codemist_fread(a,b,c,d)
#define fwrite(a,b,c,d)        _Codemist_fwrite(a,b,c,d)
#define fgetpos(a,b)           _Codemist_fgetpos(a,b)
#define fseek(a,b,c)           _Codemist_fseek(a,b,c)
#define fsetpos(a,b)           _Codemist_fsetpos(a,b)
#define ftell(a)               _Codemist_ftell(a)
#define rewind(a)              _Codemist_rewind(a)
#define clearerr(a)            _Codemist_clearerr(a)
/* feof */             /* feof */
/* ferror */           /* ferror */
#define perror(a)              _Codemist_perror(a)

#define atof(a)             _Codemist_atof(a)
#define atoi(a)             _Codemist_atoi(a)
#define atol(a)             _Codemist_atol(a)
#define strtod(a,b)         _Codemist_strtod(a,b)
#define strtol(a,b,c)       _Codemist_strtol(a,b,c)
#define strtolul(a,b,c)     _Codemist_strtolul(a,b,c)
#define rand                _Codemist_rand
#define srand(a)            _Codemist_srand(a)
#define __rand              _Codemist___rand  
#define __srand(a)          _Codemist___srand(a)
#define calloc(a,b)         _Codemist_calloc(a,b)
#define free(a)             _Codemist_free(a)
#define malloc(a)           _Codemist_malloc(a)
#define realloc(a,b)        _Codemist_realloc(a,b)
#define abort               _Codemist_abort  
#define atexit(a)           _Codemist_atexit(a)
#define exit(a)             _Codemist_exit(a)
#define getenv(a)           _Codemist_getenv(a)
#define system(a)           _Codemist_system(a)
#define bsearch(a,b,c,d,e)  _Codemist_bsearch(a,b,c,d,e)
#define qsort(a,b,c,d)      _Codemist_qsort(a,b,c,d)
#define abs(a)              _Codemist_abs(a)
#define div(a,b)            _Codemist_div(a,b)
#define labs(a)             _Codemist_labs(a)
#define ldiv(a,b)           _Codemist_ldiv(a,b)
#define mblen(a,b)          _Codemist_mblen(a,b)
#define mbtowc(a,b,c)       _Codemist_mbtowc(a,b,c)
#define wctomb(a,b)         _Codemist_wctomb(a,b)
#define mbstowcs(a,b,c)     _Codemist_mbstowcs(a,b,c)
#define wcstombs(a,b,c)     _Codemist_wcstombs(a,b,c)
#define memcpy(a,b,c)       _Codemist_memcpy(a,b,c)
#define memmove(a,b,c)      _Codemist_memmove(a,b,c)
#define strcpy(a,b)         _Codemist_strcpy(a,b)
#define strncpy(a,b,c)      _Codemist_strncpy(a,b,c)
#define strcat(a,b)         _Codemist_strcat(a,b)
#define strncat(a,b,c)      _Codemist_strncat(a,b,c)
#define memcmp(a,b,c)       _Codemist_memcmp(a,b,c)
#define strcmp(a,b)         _Codemist_strcmp(a,b)
#define strncmp(a,b,c)      _Codemist_strncmp(a,b,c)
#define strcoll(a,b)        _Codemist_strcoll(a,b)
#define strxfrm(a,b,c)      _Codemist_strxfrm(a,b,c)
#define memchr(a,b,c)       _Codemist_memchr(a,b,c)
#define strchr(a,b)         _Codemist_strchr(a,b)
#define strcspn(a,b)        _Codemist_strcspn(a,b)
#define strbrkp(a,b)        _Codemist_strbrkp(a,b)
#define strrchr(a,b)        _Codemist_strrchr(a,b)
#define strspn(a,b)         _Codemist_strspn(a,b)
#define strstr(a,b)         _Codemist_strstr(a,b)
#define strtok(a,b)         _Codemist_strtok(a,b)
#define memset(a,b,c)       _Codemist_memset(a,b,c)
#define strerror(a)         _Codemist_strerror(a)
#define strlen(a)           _Codemist_strlen(a)

#define clock               _Codemist_clock  
#define mktime(a)           _Codemist_mktime(a)
#define time(a)             _Codemist_time(a)
#define asctime(a)          _Codemist_asctime(a)
#define ctime(a)            _Codemist_ctime(a)
#define gmtime(a)           _Codemist_gmtime(a)
#define localtime(a)        _Codemist_localtime(a)
#define strftime(a,b,c,d)   _Codemist_strftime(a,b,c,d)



#endif /* _renameall_h */

/* end of renameall.h */
