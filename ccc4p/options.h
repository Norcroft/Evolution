/*
 * options.h -- compiler configuration options set at compile time
 * Copyright (C) Codemist Ltd. 1988-2005
 */

#ifndef _options_LOADED
#define _options_LOADED

#define NON_RELEASE_VSN "1.06 (Codemist Ltd)"
#define VERSION_DATE "19 Jun 2006"

#define TARGET_SYSTEM   "Unix"
#define TARGET_HAS_ELF 1
#define TARGET_HAS_DEBUGGER 1
#define TARGET_HAS_DBX 1
#define TARGET_PASS_FLOAT_AS_FLOAT 1
#define TARGET_HAS_BSS 1
#define BSS_THRESHOLD 0
/* #define TARGET_ASM_NAMES_LITERALS 1 */

/* #define DRIVER_OPTIONS { "-zpz0", "-zpq256", "-g", "-fa", "-D__unix", */
#define DRIVER_OPTIONS { "-zpn0", "-fa", "-D__unix", \
                         NULL }     /* __c4p in target.h  */

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
      "c4pas",                	/* assembler_cmd */             \
      "c4pld",		 	/* link_cmd */			\
      "c4p.a.out",            	/* output_file */               \
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
"-ansi          Compile ANSI-style C source code (default)",		\
"-pcc           Compile UNIX PCC style C source code",			\
"-list          Generate a compilation listing",			\
"-quiet         Suppress start-up banner",				\
"",									\
"-D<symbol>     Define <symbol> on entry to the compiler",		\
"-E             Preprocess the C source code only - do not compile it",	\
"-I<directory>  Include <directory> on the #include search path",	\
"-J<directory>  Replace the default #include path with <directory>",	\
"-o<file>       Place assembler output in <file>",			\
"-U<symbol>     Undefine <symbol> on entry to the compiler",		\
"-W<options>    Disable all or selected warning and error messages",	\
"-zc            Make plain 'char' signed",              		\
"-zpb0          Disallow use of new Xin byte-shift instructions",	\
"-zpe1          Keep output files after (non-serious) errors",	        \
"-zpk0          Disallow use of store.d in external memory",		\
"-zpk1          Disallow use of load.d to const data (ROM)",		\
"-zpk2          Both the previous options",				\
"-zpm1          **OBSOLETE** -- uses of this option are faulted",	\
"-zpn<num>      Place variables upto size <num> bytes in page zero",	\
"",									\
"-zpu1          Only use internal stack transiently (v1.04 code)",	\
"-zpu2          Use internal stack (not C stack) for calls",		\
"-zpu3          [default] as -zpu2 but assume C stack internal too",	\
"-zpu4          as -zpu3 but also fault all uses of r7",		\
"",									\
"Pragmas:",								\
"#pragma pagezero_off    Do not try to use page zero",			\
"#pragma pagezero_on     Use page zero for all static objects",		\
"#pragma pagezero_bysize Use page zero up to -zpn value (default 0)",	\
"#pragma internalmemory_off   [default]",				\
"#pragma internalmemory_on    Subsequent decls access internal memory",	\
"#pragma code_section <name>  Use .<name> instead of .text for code",	\
"#pragma data_section <name> <rep>  Use .<rep> instead of .<name> data", \
"#pragma -u<num>         As option -zpu<num>",				\
0

#endif

/* end of ccc4p/options.h */
