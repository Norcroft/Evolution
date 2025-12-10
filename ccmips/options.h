/*
 * options.h -- compiler configuration options set at compile time
 * Copyright (C) Codemist Ltd. 1988-2001
 */

#ifndef _options_LOADED
#define _options_LOADED

#define NON_RELEASE_VSN "1.08 (Codemist Ltd)"
/* #define VERSION_DATE "August 29 2001" */
#define REL193_HACK 1

#define TARGET_SYSTEM   "Unix"
#define TARGET_HAS_ELF 1
#define TARGET_HAS_DEBUGGER 1
#define TARGET_HAS_DBX 1

#define EXTENSION_SYSV 1   /* #ident in SGI headers */

#define TARGET_MACHINE "MIPS-1/MIPS16"

#define DRIVER_OPTIONS { "-zpn0", "-fa", "-D__unix", \
                         NULL }     /* __mips in target.h  */

#ifndef RELEASE_VSN
#  define ENABLE_ALL          1 /* -- to enable all debugging options */
#endif

#define DRIVER_ENV \
    { 0,                     	/* unused */                    \
      (KEY_UNIX | KEY_LINK),	/* initial_flags */             \
      0,                     	/* initial_pragmax */           \
      ".",			/* include_ansi_path */         \
      ".",			/* include_pcc_path */          \
      ".",			/* include_pas_path */          \
      "/",                   	/* lib_dir */                   \
      "/usr/local/ncc.lib",    	/* lib_root */                  \
      "/",                   	/* pas_lib_root */              \
      "lst",                 	/* list */                      \
      "mips.as",                	/* assembler_cmd */             \
      "mips.ld",		 	/* link_cmd */			\
      "mips.a.out",            	/* output_file */               \
      NULL,                  	/* link_ext */                  \
      "Ncrt0.o",             	/* link_startup */              \
      "Ncrt0.o",                /* profile_startup */           \
      "/usr/lib/gcrt0.o",       /* profile_g_startup */         \
      "lib.a",     		/* default_lib */               \
      "",     			/* host_lib */                  \
      "lib.a -lc_p",    	/* profile.lib */               \
      "",                    	/* fort_lib */                  \
      "",                    	/* fort_profile_lib */          \
      ""                     	/* pas_lib */                   \
    }

#define    msg_driver_help \
"Usage:         %s [options] file1.c file2.c ... filen.c",		\
"",									\
"Main options:",							\
"",									\
"-mips16        Compile for MIPS16",					\
"-littleend     Compile for little-endian MIPS",			\
"-bigend        Compile for big-endian MIPS",				\
"-ansi          Compile ANSI-style C source code (default)",		\
"-pcc           Compile pre-ANSI C (produces MIPS16 code bloat)",	\
"-list          Generate a compilation listing",			\
"",									\
"-D<symbol>     Define <symbol> on entry to the compiler",		\
"-E             Preprocess the C source code only - do not compile it",	\
"-F<options>    Enable various compiler features",			\
"   -Fa         Report dataflow anomalies (used before set)",		\
"   -Fh         Report extern definitions without header declaration",	\
"   -Fw         Do not place strings in .text segment",			\
"-I<directory>  Include <directory> on the #include search path",	\
"-J<directory>  Replace the default #include path with <directory>",	\
"-o<file>       Place object/assember output in <file>",		\
"-S             Output assembly code instead of object code",		\
"-U<symbol>     Undefine <symbol> on entry to the compiler",		\
"-W<options>    Disable all or selected warning and error messages",	\
"-zc            Make plain 'char' signed",				\
"-zpe1          Keep output files after (non-serious) errors",	        \
"",									\
"Temporary options:",							\
"-zh3           Don't use MIPS16 SAVE/RESTORE",				\
"-zh4           [-S only] use $a0,$v0 etc instead of $4,$2 etc.",	\
"-zh9           Use $v0 only for function return value",		\
"-zhc1          Calling standard uses $4 for return (not $2)",		\
"-zhc4            Offset SP by 0 (not 16) at procedure entry",		\
"-zhc5            Keep SP offset at 16, but reuse the 16 bytes",	\
0

#endif

/* end of ccmips/options.h */
