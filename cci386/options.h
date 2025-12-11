/*
 * i386/options.h -- compiler configuration options set at compile time
 */

#ifndef _options_LOADED
#define _options_LOADED

#define NEW_J_ALIGN_CODE 1              /* develop alignment code */

#define TARGET_SYSTEM   "Unix"
#define TARGET_IS_UNIX  1
#define HOSTS_WANTS_NO_BANNER 1

/*
 * The following defines are vestigial and probably wrong. But for
 * simple test programs we don't need them so I'll leave them for
 * now.
 */
#define DRIVER_OPTIONS { "-D__i386", "-D__unix", "-D__LANGUAGE_C__", \
                         "-D__unix__", "-D__linux__",                \
                         "-D_SYSTYPE_BSD",                           \
			 "-D_EXTERN_INLINE=static",		     \
			 "-D__signed__=signed",			     \
			 "-I/usr/home/sgt20/ncc-new/ersatz",	     \
                         NULL}

#ifndef RELEASE_VSN
#  define ENABLE_ALL          1 /* -- to enable all debugging options */
#endif

/*
 * The following structure MUST be kept in step with the EnvTable structure
 * used in mip/driver.c.  The values in here at present are in some cases
 * just placeholders until I know what libraries, link options etc are
 * really wanted - which gets important when ncc can generate coff files
 * directly.
 */

#ifdef COMPILING_ON_UNIX
#define DRIVER_ENV \
    { 0,                     /* unused */                    \
     (KEY_UNIX | KEY_LINK),  /* initial_flags */             \
      0,                     /* initial_pragmax */           \
      "/usr/include", /* include_ansi_path */         \
      "/usr/include",        /* include_pcc_path */          \
      "",    /* include_pas_path */          \
      "/",                   /* lib_dir */                   \
      "/usr/include",        /* lib_root */                  \
      "/",                   /* pas_lib_root */              \
      "lst",                 /* list */                      \
      "nasm -felf",          /* assembler_cmd */             \
      "ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2", /* link_cmd */ \
      "a.out",               /* output_file */               \
      NULL,                  /* link_ext */                  \
      "/usr/lib/crt1.o",			       /* link_startup */ \
      "",                    /* profile_startup */           \
      "",                    /* profile_g_startup */         \
      "-lc -lm",     /* default_lib */               \
      "-lc -lm",     /* host_lib */                  \
      "-lc -lm",    /* profile.lib */               \
      "",                    /* fort_lib */                  \
      "",                    /* fort_profile_lib */          \
      ""                     /* pas_lib */                   \
    }
#endif

#define DEBUG_LIBRARY ""  	       /* no -lg on this system */

#endif

/* end of i386/options.h */
