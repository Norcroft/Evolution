/*
 * options.h -- compiler configuration options set at compile time
 * Copyright (C) Acorn Computers Ltd. 1988
 * Copyright (C) Codemist Ltd. 1988
 */

#ifndef _options_LOADED
#define _options_LOADED

#define TARGET_SYSTEM   "Unix"
#define TARGET_IS_UNIX  1
#define HOSTS_WANTS_NO_BANNER 1

#define DRIVER_OPTIONS { "-D__unix", NULL }     /* __sparc in target.h  */

#ifndef RELEASE_VSN
#  define ENABLE_ALL          1 /* -- to enable all debugging options */
#endif

#ifdef FORTRAN
#define DRIVER_ENV { 0, ( KEY_UNIX | KEY_LINK ), 		           \
		     (EXT_DOUBLECOMPLEX | EXT_HINTEGER | EXT_CASEFOLD |    \
		      EXT_LCKEYWORDS | EXT_FREEFORMAT | EXT_IMPUNDEFINED | \
		      EXT_RECURSION | EXT_AUTO | EXT_HOLLERITH |           \
		      EXT_TOPEXPRESS | EXT_F66 | EXT_MIXEDCOMM |	   \
		      EXT_VMSCHARS | EXT_VMSCASTS | EXT_VMSIO | 	   \
		      EXT_VMSTYPES | OPT_STATICLOCALS | OPT_NOARGALIAS),   \
		       "/users.xenakis/jpff/ncc.include",		   \
		       "/users.xenakis/jpff/ncc.include",		   \
		       "/", "", "", "lst",		 		   \
		       "as",						   \
		       "ld -X -Bstatic -e __start", "a.out", "",	   \
		       "/lib/Ncrt0.o", "/lib/Ncrt0.o", "/usr/lib/gcrt0.o", \
		       "-lAnsi -lc", "", "-lAnsi -lc_p", "", "-lpc"	   \
    }
#else
#define DRIVER_ENV \
    { 0,                     	/* unused */                    \
      (KEY_UNIX | KEY_LINK),  	/* initial_flags */             \
      0,                     	/* initial_pragmax */           \
      "/usr/local/ncc.include",	/* include_ansi_path */         \
      "/usr/local/ncc.include",	/* include_pcc_path */          \
      "/usr/local/ncc.include",	/* include_pas_path */          \
      "/",                   	/* lib_dir */                   \
      "/usr/local/ncc.lib",    	/* lib_root */                  \
      "/",                   	/* pas_lib_root */              \
      "lst",                 	/* list */                      \
      "as",                  	/* assembler_cmd */             \
      "ld -X -Bstatic -e __start", 	/* link_cmd */		\
      "a.out",               	/* output_file */               \
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
#endif /* FORTRAN */

#endif

/* end of ccsparc/options.h */
