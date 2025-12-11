
/* C compiler file elfobj.c :  Copyright (C) Codemist Ltd, 1993-97.    */
/* 'ELF' (system V unix) output routines */
/* version 2 */

/* target.h shall specify: TARGET_HAS_ELF: why?  */
/* Note that -zpq65536 can be used to suppress ELF admininstrative (Shdr,  */
/* Ehdr, Sym, Rela) resexing.  This is useful for unix 'dump'.             */

#ifndef __STDC__
#  include <strings.h>
#  define  SEEK_SET 0
#else
#  include <string.h>
#endif
#include <time.h>   /* not currently  used, but adding to .comment possible */

#include "globals.h"    /* loads host.h,options.h,target.h,defaults.h   */
#include "mcdep.h"
/* #include "mcdpriv.h" */
#include "store.h"
#include "codebuf.h"
#include "builtin.h"
#include "xrefs.h"
#include "errors.h"
/* #include "sem.h"		/* experiment: for sizeoftype */
#include "version.h"		/* for CC_BANNER */

#ifndef __STDC__
#  include <sys/types.h>
#  include <elf.h>            /* sysV elf definitions */
#  ifdef TARGET_IS_SPARC    /* sysV architecture-specific elf definitions */
#    include <elf_SPARC.h>
#  endif
#else
/*
 * "target.h" (via globals.h) must be included before "elf.h", since our
 * private version of a header defining the ELF format is parameterised
 * wrt the type of target machine involved.  If we are not cross compiling
 * and a host specific <elf_xxx.h> is available it might be better to use
 * that rather than "elf.h".
 */
#  include "elf.h"            /* Codemist private version */
#endif

#ifdef TARGET_IS_SPARC
# define ELF_TARGETID EM_SPARC
# define OBSOLETE_BSS 1
#endif
#ifdef TARGET_IS_C4P
# define ELF_TARGETID EM_C4P
# define NEC_OR_C4P 1         /* A bit of a hack */
# undef TARGET_IS_NEC         /* @@@ see c4p/target.h hack defining both */
#endif
#ifdef TARGET_IS_NEC
# define ELF_TARGETID EM_NEC850
# define NEC_OR_C4P 1         /* A bit of a hack */
#endif
#ifndef ELF_TARGETID
# define ELF_TARGETID 999
# define OBSOLETE_BSS 1
#endif

#ifndef target_elf_prefix
#  define target_elf_prefix ""
#endif

/* Hacks to get NEC-modified ELF to work... */
#ifdef TARGET_IS_NEC    /* BUGFIX */
#define XSHF_ALLOC 0
#define EHDR_FLAGS 0x84
/* #define ELF_NO_COMMENT_SCN 1 */
extern int32 nec_opt;
#define elf_mapto_segnames (nec_opt & 512)
#else
#define XSHF_ALLOC SHF_ALLOC
#define EHDR_FLAGS 0
#define elf_mapto_segnames 1
#endif

#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
#define SEG_is_BSS(n) (datasects[n].xrarea & xr_bss)
#define SEG_is_ROM(n) (datasects[n].xrarea & xr_constdata)
#define SEG_is_GP(n)  (datasects[n].xrarea & xr_zeropage)
#else
#define SEG_is_BSS(n) ((n) >= 1)
#define SEG_is_ROM(n) 0
#define SEG_is_GP(n)  0
#endif

#define SCN_MAX (2*TARGET_NUM_DATASECTS+9)  /* allows 2 for DBX */
#define SCN_SYM (TARGET_NUM_DATASECTS+3)

static struct myscn {int32 scn; Elf32_Shdr shdr;} scns[SCN_MAX];
#define scn_null     scns[0]
#define scn_shstrtab scns[1]
#define scn_text     scns[2]
#define scn_data     scns[3]
#define scn_seg(i)   scns[(i)+3]
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
#define scn_const    scns[4]
#define scn_sdata    scns[5]
#define scn_sconst   scns[6]
#define scn_sbss     scns[7]
#define scn_bss      scns[8]
#ifdef TARGET_HAS_NEC_SECTS
#define scn_tidata   scns[9]
#define scn_sidata   scns[10]
#define scn_sedata   scns[11]
#define scn_sebss    scns[12]
#endif
#ifdef TARGET_HAS_C4P_SECTS
#define scn_idata    scns[9]
#define scn_iconst   scns[10]
#define scn_ibss     scns[11]
#endif
#else
#define scn_bss      scns[4]
#endif
#define scn_symtab   scns[SCN_SYM]
#define scn_strtab   scns[SCN_SYM+1]
#define scn_textrel  scns[SCN_SYM+2]
#define scn_datarel  scns[SCN_SYM+3]
#define scn_segrel(i) scns[(i)+SCN_SYM+3]
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
/* #define scn_constrel  scns[SCN_SYM+4] */
/* #define scn_sdatarel  scns[SCN_SYM+5] */
/* #define scn_sconstrel scns[SCN_SYM+6] */
#endif
#ifdef TARGET_HAS_DBX
#define scn_dbxtab      scns[SCN_MAX-3]
#define scn_dbxstrtab   scns[SCN_MAX-2]
#endif
#ifndef ELF_NO_COMMENT_SCN
#define scn_comment  scns[SCN_MAX-1]
#endif

static int32 elf_nsections;

#ifdef TARGET_HAS_DBX
typedef struct StabList {
    struct StabList *next;
    struct nlist nlist;
} StabList;

static StabList *obj_stablist;
static int32 obj_stabcount;
#endif

/* ELF format, at least on the DRS6000, seems to put the static bss into one
   section, separate from the data segment, and each of the non-static bss
   items into its own 'common' segment. */
static int32 obj_fwrite_cnt;

static void obj_fwrite(const void *buff, int32 n, int32 m, FILE *f)
{
   if (debugging(DEBUG_OBJ))
    {   int32 i;
        fprintf(f, "%.6lx:", (long)obj_fwrite_cnt);
        for (i=0; i<n*m; i++)
          fprintf(f, " %.2x", (int)((unsigned8 *)buff)[i]);
        fprintf(f, "\n");
    }
    else fwrite(buff,(size_t)n,(size_t)m,f);
    obj_fwrite_cnt += n*m;
}

#define obj_puts(buff,f) obj_fwrite(buff, 1, (size_t)(strlen(buff)+1), f)
#define obj_putsc(buff,f) obj_fwrite(buff, 1, sizeof(buff), f)

/* Code for writing ELF files in the reverse sex from that of the host. */
static void rev_Half(Elf32_Half *p)
{   Elf32_Half v = *p;
    ((char *)p)[0] = ((char *)&v)[1];
    ((char *)p)[1] = ((char *)&v)[0];
}

static void rev_Word(Elf32_Word *p)
{   Elf32_Word v = *p;
    ((char *)p)[0] = ((char *)&v)[3];
    ((char *)p)[1] = ((char *)&v)[2];
    ((char *)p)[2] = ((char *)&v)[1];
    ((char *)p)[3] = ((char *)&v)[0];
}
#define rev_Addr rev_Word
#define rev_Off  rev_Word
#define rev_Sword(p)  rev_Word((Elf32_Word *)(p))

static void obj_fwrite_Ehdr(Elf32_Ehdr *p, FILE *f)
{   Elf32_Ehdr q;
    if (target_lsbytefirst!=host_lsbytefirst && !(var_cc_private_flags&65536))
    {   memcpy(&q, p, sizeof(q));
        /* unsigned char e_ident[EI_NIDENT]; */
        rev_Half(&q.e_type);
        rev_Half(&q.e_machine);
        rev_Word(&q.e_version);
        rev_Addr(&q.e_entry);
        rev_Off(&q.e_phoff);
        rev_Off(&q.e_shoff);
        rev_Word(&q.e_flags);
        rev_Half(&q.e_ehsize);
        rev_Half(&q.e_phentsize);
        rev_Half(&q.e_phnum);
        rev_Half(&q.e_shentsize);
        rev_Half(&q.e_shnum);
        rev_Half(&q.e_shstrndx);
        p = &q;
    }
    obj_fwrite(p, sizeof(*p), 1, f);
}

static void obj_fwrite_Shdr(Elf32_Shdr *p, FILE *f)
{   Elf32_Shdr q;
    if (target_lsbytefirst!=host_lsbytefirst && !(var_cc_private_flags&65536))
    {   memcpy(&q, p, sizeof(q));
        rev_Word(&q.sh_name);
        rev_Word(&q.sh_type);
        rev_Word(&q.sh_flags);
        rev_Addr(&q.sh_addr);
        rev_Off(&q.sh_offset);
        rev_Word(&q.sh_size);
        rev_Word(&q.sh_link);
        rev_Word(&q.sh_info);
        rev_Word(&q.sh_addralign);
        rev_Word(&q.sh_entsize);
        p = &q;
    }
    obj_fwrite(p, sizeof(*p), 1, f);
}

#ifdef TARGET_HAS_DBX
static void obj_fwrite_nlist(struct nlist *p, FILE *f)
{   struct nlist q;
    if (target_lsbytefirst!=host_lsbytefirst && !(var_cc_private_flags&65536))
    {   memcpy(&q, p, sizeof(q));
        rev_Sword(&q.n_un.n_strx);
  /* unsigned char   n_type;  */
  /*         char    n_other; */
        rev_Half((unsigned short *)&q.n_desc);
        rev_Word(&q.n_value);
        p = &q;
    }
    obj_fwrite(p, sizeof(*p), 1, f);
}
#endif

static void obj_fwrite_Sym(Elf32_Sym *p, FILE *f)
{   Elf32_Sym q;
    if (target_lsbytefirst!=host_lsbytefirst && !(var_cc_private_flags&65536))
    {   memcpy(&q, p, sizeof(q));
        rev_Word(&q.st_name);
        rev_Addr(&q.st_value);
        rev_Word(&q.st_size);
        /* unsigned char st_info; */
        /* unsigned char st_other; */
        rev_Half(&q.st_shndx);
        p = &q;
    }
    obj_fwrite(p, sizeof(*p), 1, f);
}

#ifdef ELF_TRACE_RELOCS
static char *reloc_name[100] = {0};
#endif

static void obj_fwrite_Rela(Elf32_Rela *p, FILE *f)
{   Elf32_Rela q;
#ifdef ELF_TRACE_RELOCS
/*
 * This code is by ACN to record the selection of relocation modes
 * actually used.
 */
    static int mentioned[100] = {0};
    int mode = ELF32_R_TYPE(p->r_info);
    if (0 <= mode && mode < 100)
    {   if (!mentioned[mode])
        {   mentioned[mode] = 1;
            if (reloc_name[0] == NULL)
            {   int i;
                for (i=0; i<100; i++) reloc_name[i] = "Unknown";
#ifdef TARGET_IS_SPARC
                reloc_name[R_SPARC_32] =          "R_SPARC_32";
                reloc_name[R_SPARC_WDISP30] =     "R_SPARC_WDISP30";
                reloc_name[R_SPARC_HI22] =        "R_SPARC_HI22";
                reloc_name[R_SPARC_LO10] =        "R_SPARC_LO10";
#endif
#ifdef TARGET_IS_C4P
                reloc_name[R_C4P_32] =            "R_C4P_32";
                reloc_name[R_C4P_PC24] =          "R_C4P_PC24";
                reloc_name[R_C4P_ABS24] =         "R_C4P_ABS24";
                reloc_name[R_C4P_ZPDISP7B] =      "R_C4P_ZPDISP7B";
                reloc_name[R_C4P_ZPDISP7H] =      "R_C4P_ZPDISP7H";
                reloc_name[R_C4P_ZPDISP7L] =      "R_C4P_ZPDISP7L";
#endif
#ifdef TARGET_IS_NEC
                reloc_name[R_NEC850_32] =         "R_NEC850_32";
                reloc_name[R_NEC850_HI16S] =      "R_NEC850_HI16S";
                reloc_name[R_NEC850_GPDISP16bu] =  "R_NEC850_GPDISP16bu";
                reloc_name[R_NEC850_GPDISP16b] =  "R_NEC850_GPDISP16b";
                reloc_name[R_NEC850_GPDISP16hu] =  "R_NEC850_GPDISP16hu";
                reloc_name[R_NEC850_GPDISP16h] =  "R_NEC850_GPDISP16h";
                reloc_name[R_NEC850_GPDISP16w] =  "R_NEC850_GPDISP16w";

                reloc_name[R_NEC850_ZPDISP16bu] =  "R_NEC850_ZPDISP16bu";
                reloc_name[R_NEC850_ZPDISP16b] =  "R_NEC850_ZPDISP16b";
                reloc_name[R_NEC850_ZPDISP16hu] =  "R_NEC850_ZPDISP16hu";
                reloc_name[R_NEC850_ZPDISP16h] =  "R_NEC850_ZPDISP16h";
                reloc_name[R_NEC850_ZPDISP16w] =  "R_NEC850_ZPDISP16w";

                reloc_name[R_NEC850_EPDISP16bu] =  "R_NEC850_EPDISP16bu";
                reloc_name[R_NEC850_EPDISP16b] =  "R_NEC850_EPDISP16b";
                reloc_name[R_NEC850_EPDISP16hu] =  "R_NEC850_EPDISP16hu";
                reloc_name[R_NEC850_EPDISP16h] =  "R_NEC850_EPDISP16h";
                reloc_name[R_NEC850_EPDISP16w] =  "R_NEC850_EPDISP16w";

                reloc_name[R_NEC850_EP7b] =       "R_NEC850_EP7b";
                reloc_name[R_NEC850_EP7h] =       "R_NEC850_EP7h";
                reloc_name[R_NEC850_EP4bu] =      "R_NEC850_EP4bu";
                reloc_name[R_NEC850_EP4hu] =      "R_NEC850_EP4hu";
                reloc_name[R_NEC850_EP7w] =       "R_NEC850_EP7w";

                reloc_name[R_NEC850_DISP22] =     "R_NEC850_DISP22";
                reloc_name[R_NEC850_LO16bu] =     "R_NEC850_LO16bu";
                reloc_name[R_NEC850_LO16hu] =     "R_NEC850_LO16hu";
/* Do the next 3 last because they may equal R_NEC850_EPDISP16[bhw].    */
                reloc_name[R_NEC850_LO16b] =      "R_NEC850_LO16b";
                reloc_name[R_NEC850_LO16h] =      "R_NEC850_LO16h";
                reloc_name[R_NEC850_LO16w] =      "R_NEC850_LO16w";
#endif
            }
            cc_msg("Used ELF mode %d (%s)\n", mode, reloc_name[mode]);
        }
    }
    else cc_msg("Odd ELF relocation mode %d\n", mode);
#endif
    if (target_lsbytefirst!=host_lsbytefirst && !(var_cc_private_flags&65536))
    {   memcpy(&q, p, sizeof(q));
        rev_Addr(&q.r_offset);
        rev_Word(&q.r_info);
        rev_Sword(&q.r_addend);
        p = &q;
    }
    obj_fwrite(p, sizeof(*p), 1, f);
}

static void obj_fwrite4(int32 *v, FILE *f, int32 len)
{   int32 w;
    if (target_lsbytefirst != host_lsbytefirst)
    {   char *p = (char *)&w, *q = (char *)v;
        p[0] = q[3]; p[1] = q[2]; p[2] = q[1]; p[3] = q[0];
        v = &w;
    }
    obj_fwrite(v, (int)len, 1, f);
}
/* See also half-written code in buffer_code for resexing */

FILE *objstream;

/* imports: codebase, dataloc */
static obj_symcount;
ExtRef *obj_symlist;
CodeXref *codexrefs;
/* DataXref *dataxrefs; -- now exported from codebuf.c as data.xrefs.   */

#ifdef COMPILING_ON_SMALL_MEMORY
/* Buffer code in a temporary file */

FILE *obj_tmpfile;
____notyetfinished;

#else /* !COMPILING_ON_SMALL_MEMORY */
/* Buffer code in memory */

/* In general, COFF requires references to symbols which are defined    */
/* in codeseg, dataseg or bssseg to be replaced by a reference to       */
/* segment+offset.  Hence the code for a C routine with an 'extern'     */
/* reference is not complete until we know whether it was truly extern  */
/* or merely 'forward'.  So buffer the code (in codevec) from each fn   */
/* into 'xcodevec' and then call relocate_code_refs_to_locals() before  */
/* writing xcodevec into the COFF .o file.                              */

/* For a.out ...                                                          */
/* All the code from all functions is buffered until the end of           */
/* compilation so that local references can be resolved to avoid the      */
/* linker bombing.  AM vaguely remembers weasel words about a.out which   */
/* might fix this.                                                        */

/* @@@ What do we do for ELF ??                                            */

#define MAXCODESEGS 256
static int32 (*(xcodevec[MAXCODESEGS]))[CODEVECSEGSIZE], codesize;

#ifdef TARGET_HAS_BYTE_INSTRUCTIONS
#define xcode_byte_(q) ((unsigned8 *)(*xcodevec[(q)>>(CODEVECSEGBITS+2)])) \
                                  [(q)&(CODEVECSEGSIZE*4-1)]
#else
#define xcode_inst_(q) (*xcodevec[(q)>>(CODEVECSEGBITS+2)]) \
                                  [((q)>>2)&(CODEVECSEGSIZE-1)]
#endif

#ifdef TARGET_HAS_HALFWORD_INSTRUCTIONS
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
/* The next line makes different files on different hosts, this needs   */
/* to be fixed, but is currently compatible with n850sim (q.v.).        */
#define xcode_byte_(q) ((unsigned8 *)(*xcodevec[(q)>>(CODEVECSEGBITS+2)])) \
                [(q)&(CODEVECSEGSIZE*4-1)]
static int32 get_code(int32 q)
{   return (int32)xcode_byte_(q+1)<<24 | (int32)xcode_byte_(q+0)<<16 |
           (int32)xcode_byte_(q+3)<<8  | (int32)xcode_byte_(q+2)<<0;
}
static void set_code(int32 q, int32 w)
{   xcode_byte_(q+1) = (int)(w >> 24); xcode_byte_(q+0) = (int)(w >> 16);
    xcode_byte_(q+3) = (int)(w >> 8);  xcode_byte_(q+2) = (int)(w >> 0);
}
#else /* TARGET_IS_NEC */
/* A fix up for machines (like Clipper) which may have unaligned relocated */
/* words in opcodes.  NB: needs improving for 386 (use memcpy to align?).  */
/* (Or make xcode_inst_ optionally a halfword/byte vector as code_inst_).  */
/* Also worry about cross compilation and byte sex here one day.           */
#  ifdef TARGET_IS_LITTLE_ENDIAN
static void set_code(int32 q, int32 n)
{   if ((q&3)==0) xcode_inst_(q) = n;
    else
    {   xcode_inst_(q)   = ((xcode_inst_(q))&0xffff)+(n<<16);
        xcode_inst_(q+2) = ((xcode_inst_(q+2))&0xffff0000)+((unsigned32)n>>16);
    }
}
static int32 get_code(int32 q)
{   return ((q&3)==0 ? xcode_inst_(q) :
             ((xcode_inst_(q+2))<<16) + (((unsigned32)xcode_inst_(q))>>16));
}
#  endif
#  ifdef TARGET_IS_BIG_ENDIAN
static void set_code(int32 q, int32 n)
{   if ((q&3)==0) xcode_inst_(q) = n;
    else
    {   xcode_inst_(q)   = ((xcode_inst_(q))&0xffff0000)+((unsigned32)n>>16);
        xcode_inst_(q+2) = ((xcode_inst_(q+2))&0xffff)+(n<<16);
    }
}
static int32 get_code(int32 q)
{   return ((q&3)==0 ? xcode_inst_(q) :
             ((xcode_inst_(q))<<16) + (((unsigned32)xcode_inst_(q+2))>>16));
}
#  endif
#endif /* TARGET_IS_NEC */
#else
#  define get_code(q)   xcode_inst_(q)
#  define set_code(q,n) xcode_inst_(q)=n
#endif

/* The following code is in flux and allows for the generic compiler    */
/* interfaces to use byte addresses which are corrected here.  It now   */
/* occurs to AM that we could have used word addresses more, but this   */
/* could also be a can of worms.  Let's try this way FIRST.             */
#define machine_address_(v,f) (v)

static void buffer_code(int32 *src, int32 nwords)
{
  int32 *p;
  for (p = src; nwords > 0; --nwords) {
    int32 hi = codesize >> (CODEVECSEGBITS+2);
    int32 lo = (codesize >> 2) & (CODEVECSEGSIZE-1);
    if (lo == 0) { /* need another segment */
      if (hi >= MAXCODESEGS) cc_fatalerr("elf_fatalerr_toobig");
      xcodevec[hi] = (int32(*)[CODEVECSEGSIZE]) GlobAlloc(
                                        SU_Other, sizeof(*xcodevec[0]));
    }
/* The following line starts the resexing code for non-32 bit instructions */
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
    if (!host_lsbytefirst)
    {   ((unsigned8 *)(*xcodevec[hi]))[lo*4+0] = ((unsigned8 *)p)[1];
        ((unsigned8 *)(*xcodevec[hi]))[lo*4+1] = ((unsigned8 *)p)[0];
        ((unsigned8 *)(*xcodevec[hi]))[lo*4+2] = ((unsigned8 *)p)[3];
        ((unsigned8 *)(*xcodevec[hi]))[lo*4+3] = ((unsigned8 *)p)[2];
        p++;
    }
    else
#endif
        (*xcodevec[hi])[lo] = *p++;
    codesize += 4;
  }
}


static int32 obj_checksym(Symstr *s);

static void obj_align (void)
{
  while (obj_fwrite_cnt & 3) obj_fwrite("", 1, 1, objstream);
}

static void obj_writeshstrtab(void)
{
  int32 start = obj_fwrite_cnt;
  scn_shstrtab.scn = ++elf_nsections;

  /* scn_shstrtab.shdr.sh_name   = ... see below; section name */
  scn_shstrtab.shdr.sh_type      = SHT_STRTAB;
  /* scn_shstrtab.shdr.sh_flags  = ??; */
  /* scn_shstrtab.shdr.sh_addr   = ??;	 		/* virtual address */
  scn_shstrtab.shdr.sh_offset    = obj_fwrite_cnt;	/* file offset */
  /* scn_shstrtab.shdr.sh_size   = ... see below;	/* section size */
  /* scn_shstrtab.shdr.sh_link   = ??;			/* misc info */
  /* scn_shstrtab.shdr.sh_info   = ??;			/* misc info */
  scn_shstrtab.shdr.sh_addralign = 1;			/* memory alignment */
  /* scn_shstrtab.shdr.sh_entsize= ??;			/* entry size if table */

  obj_putsc("",           objstream);

  scn_shstrtab.shdr.sh_name = obj_fwrite_cnt - start;
  obj_putsc(".shstrtab",  objstream);

#ifdef TARGET_IS_C4P
  if (asm_codesegname[0] && codesize > 0) {
    scn_text.shdr.sh_name = obj_fwrite_cnt - start;
    obj_fwrite(".", 1, 1, objstream);
    obj_puts(asm_codesegname, objstream);
  }
  else
#endif
  if (codesize > 0) {
    scn_text.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".text",      objstream);
  }

#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
  { int i;
    for (i=0; i<TARGET_NUM_DATASECTS; i++)
    {   if (datasects[i].size > 0)
        {    scn_seg(i).shdr.sh_name = obj_fwrite_cnt - start;
             obj_fwrite(".", 1, 1, objstream);
             obj_puts(datasects[i].elfsegname, objstream);
        }
    }
  }
#else
  if (dataloc > 0) {
    scn_data.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".data",      objstream);
  }
  if (bss_defloc_size != 0) {
    scn_bss.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".bss",      objstream);
  }
#endif

  scn_symtab.shdr.sh_name = obj_fwrite_cnt - start;
  obj_putsc(".symtab",    objstream);

  scn_strtab.shdr.sh_name = obj_fwrite_cnt - start;
  obj_putsc(".strtab",    objstream);

#ifdef TARGET_HAS_DBX
  if (obj_stabcount != 0) {
    scn_dbxtab.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".dbx",       objstream);
    scn_dbxstrtab.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".dbxstr",    objstream);
  }
#endif

#ifdef TARGET_IS_C4P
  if (asm_codesegname[0] && codexrefs != NULL) {
    scn_textrel.shdr.sh_name = obj_fwrite_cnt - start;
    obj_fwrite(".rela.", 1, 6, objstream);
    obj_puts(asm_codesegname, objstream);
  }
  else
#endif
  if (codexrefs != NULL) {
    scn_textrel.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".rela.text", objstream);
  }

#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
  { int i;
    for (i=0; i<TARGET_NUM_DATASECTS; i++) if (!SEG_is_BSS(i))
    {   if (datasects[i].xrefs != NULL)
        {    scn_segrel(i).shdr.sh_name = obj_fwrite_cnt - start;
             obj_fwrite(".rela.", 1, 6, objstream);
             obj_puts(datasects[i].elfsegname, objstream);
        }
    }
  }
#else
  if (dataxrefs != NULL) {
    scn_datarel.shdr.sh_name = obj_fwrite_cnt - start;
    obj_putsc(".rela.data", objstream);
  }
#endif

#ifndef ELF_NO_COMMENT_SCN
  scn_comment.shdr.sh_name = obj_fwrite_cnt - start;
  obj_putsc(".comment",   objstream);
#endif

  scn_shstrtab.shdr.sh_size = obj_fwrite_cnt - start;	/* section size */
  obj_align();
}

static void relocate_code_refs_to_locals(void)
{   /* This proc. should soon callback to a routine in gen.c:          */
  CodeXref *cxr;
  for (cxr = codexrefs;  cxr != NULL;  cxr = cxr->codexrcdr)
  { Symstr *s = cxr->codexrsym;
    ExtRef *x = symext_(s);
        if (debugging(DEBUG_OBJ))
            cc_msg("ELF: codexref $r symext %lx\n", s, x);
  {
    int32 codeoff = cxr->codexroff & 0xffffff;
    int32 w = get_code(codeoff);
    switch (cxr->codexroff & 0xff000000)
    {
    /* @@@ beware no default here... */
    case X_absreloc:
        if (x->extflags & (xr_defloc | xr_defext))
        {   /*
             * code ref to local code or data via address literal...
             * (or clipper/vax absolute 32 bit branch).
             * Unix linker cannot cope with filling in the address literal
             * with the value of a locally defined symbol: we have to convert
             * it into a reference relative to v$codeseg or v$dataseg.
             * AM: note that we do take notice of the old value here.
             */
	    set_code(codeoff, w + x->extoffset);
        }
        break;
#ifdef TARGET_IS_C4P
    case X_PCreloc:
        if (x->extflags & (xr_defloc | xr_defext))
        {   /* defined in this compilation unit so relocate... */
            set_code(codeoff, (w & 0xff000000) |
		              ((x->extoffset-(codeoff+4))/2 & 0x00ffffff));
/* @@@ C4P is get_code() right? */
        }
        if (debugging(DEBUG_OBJ))
            cc_msg("PCFixup %.8lx extoff=%.8lx, codeoff=%.8lx, make %.8lx\n",
                (long)w, (long)x->extoffset, (long)codeoff,
                (long)get_code(codeoff));
        break;
#endif
#ifdef TARGET_IS_NEC
    case X_PCreloc:
        if (x->extflags & (xr_defloc | xr_defext))
        {   /* defined in this compilation unit so relocate... */
            set_code(codeoff, (w & 0xffc00000) |
		              (x->extoffset-codeoff & 0x003fffff));
        }
        if (debugging(DEBUG_OBJ))
            cc_msg("PCFixup %.8lx extoff=%.8lx, codeoff=%.8lx, make %.8lx\n",
                (long)w, (long)x->extoffset, (long)codeoff,
                (long)get_code(codeoff));
        break;
#endif
#ifdef TARGET_IS_SPARC
    case X_PCw30reloc:
        if (x->extflags & (xr_defloc | xr_defext))
        {   /* defined in this compilation unit so relocate... */
            set_code(codeoff, (w & 0xc0000000) |
		              (((x->extoffset-codeoff) >> 2) & 0x3fffffff));
        }
        if (debugging(DEBUG_OBJ))
            cc_msg("PCFixup %.8lx extoff=%.8lx, codeoff=%.8lx, make %.8lx\n",
                (long)w, (long)x->extoffset, (long)codeoff,
                (long)get_code(codeoff));
        break;
#endif
    case X_DataAddr:
#ifdef TARGET_IS_SPARC
	set_code(codeoff, w & 0xffc00000); /* Remove the immediate field */
#endif
	break;
    case X_DataAddr1:
#ifdef TARGET_IS_SPARC
	set_code(codeoff, w & 0xffffe000); /* Remove the immediate field */
#endif
	break;

#ifdef TARGET_IS_NEC
/* Probably the following is not necessary as gen.c doesn't insert...   */
    case X_HI16S:
	set_code(codeoff, w & 0xffff0000); /* Remove the immediate field */
	break;
    case X_LO16b:
    case X_GPOFFb:      /* Currently GP relative (gpoff(sym) in .sdata)  */
    case X_ZPOFFb:      /* Currently R0 relative (zpoff(sym) in .sconst) */
    case X_EPOFFb:      /* Currently EP relative (epoff(sym) in .sidata) */
	set_code(codeoff, w & 0xffff0000); /* Remove the immediate field */
	break;
    case X_LO16h:
    case X_GPOFFh:      /* Currently GP relative (gpoff(sym) in .sdata)  */
    case X_ZPOFFh:      /* Currently R0 relative (zpoff(sym) in .sconst) */
    case X_EPOFFh:      /* Currently EP relative (epoff(sym) in .sidata) */
	set_code(codeoff, w & 0xffff0001); /* Remove the immediate field */
	break;
    case X_LO16w:
    case X_GPOFFw:      /* Currently GP relative (gpoff(sym) in .sdata)  */
    case X_ZPOFFw:      /* Currently R0 relative (zpoff(sym) in .sconst) */
    case X_EPOFFw:      /* Currently EP relative (epoff(sym) in .sidata) */
	set_code(codeoff, w & 0xffff0001); /* Remove the immediate field */
	break;
    case X_EPOFF7b:     /* Currently EP relative (epoff(sym) in .tidata) */
    case X_EPOFF7h:     /* Currently EP relative (epoff(sym) in .tidata) */
	set_code(codeoff, w & 0xff80ffff); /* Remove the immediate field */
	break;
    case X_EPOFF7w:     /* Currently EP relative (epoff(sym) in .tidata) */
	set_code(codeoff, w & 0xff81ffff); /* Remove the immediate field */
	break;
#endif
    }
  }
  }
}

static void obj_writecode(void)
{
  int i = 0;
  if (codesize <= 0) return;

  scn_text.scn = ++elf_nsections;

  /* scn_text.shdr.sh_name   = ... done in obj_writeshstrtab */
  scn_text.shdr.sh_type      = SHT_PROGBITS;
  scn_text.shdr.sh_flags     = SHF_ALLOC | SHF_EXECINSTR;
  /* scn_text.shdr.sh_addr   = ??; */
  scn_text.shdr.sh_offset    = obj_fwrite_cnt;
  scn_text.shdr.sh_size      = codesize;
  /* scn_text.shdr.sh_link   = ??; */
  /* scn_text.shdr.sh_info   = ??; */
  scn_text.shdr.sh_addralign = 8;
  /* scn_text.shdr.sh_entsize= ??; */

#if (alignof_double > 4)      /* TARGET_ALIGNS_DOUBLES */
  if (codesize & 7)
    {   static int32 pad[] = {0,0,0};
        buffer_code(pad, 1); /* Double word aligned */
    }
#endif
  if (debugging(DEBUG_OBJ)) cc_msg("start relocate_locals()\n");
  relocate_code_refs_to_locals();
  if (debugging(DEBUG_OBJ)) cc_msg("end relocate_locals()\n");

  while ((codesize>>2) - CODEVECSEGSIZE*i > CODEVECSEGSIZE)
    obj_fwrite(xcodevec[i++], 4, CODEVECSEGSIZE, objstream);
  obj_fwrite(xcodevec[i], 4,(codesize>>2)-CODEVECSEGSIZE*i, objstream);
  if (debugging(DEBUG_OBJ)) cc_msg("test ferror()\n");
  if (ferror(objstream)) cc_fatalerr("driver_fatalerr_io_object");
}

#endif /* COMPILING_ON_SMALL_MEMORY */


void obj_codewrite(Symstr *name)
{   /* Called after each routine is compiled -- code_instvec_ (doubly    */
    /* indexed) has codep (multiple of 4) bytes of code.                 */
    /* In BSD a.out, this has to be buffered to the end of compilation   */
    /* so that the BSD linker can be cow-towed to.                       */
    /* #define COMPILING_ON_SMALL_MEMORY can be used to buffer on disc.  */
    int32 i, nwords;
    IGNORE(name);
    for (i = 0, nwords = codep>>2; nwords > 0; ++i)
    { int32 seg = nwords > CODEVECSEGSIZE ? CODEVECSEGSIZE : nwords;
/* @@@ When cross compiling we really ought to swap byte sex here        */
/* (by consulting code_flagvec_) because we are just about to throw      */
/* away the information of how to swap bytes.                            */
      buffer_code(code_instvec_(i), seg); nwords -= seg;
    }
}

/* the remaining fns are intended to be internal only */


/* In ELF, there appear to be no restrictions on segment names */
#define elfname_(s) (symname_(s))

static void obj_writedata(int thisseg, DataInit *thisinitp, int32 thissize)
{ DataInit *p;

  if (thissize > 0) {
    struct myscn *thisscn = &scn_seg(thisseg);

    thisscn->scn = ++elf_nsections;

    /* thisscn->shdr.sh_name   = ... done in obj_writeshstrtab; section name */
    thisscn->shdr.sh_type      = SEG_is_BSS(thisseg) ? SHT_NOBITS :
                                   SHT_PROGBITS;
    thisscn->shdr.sh_flags     = SEG_is_ROM(thisseg) ? SHF_ALLOC :
                                 SEG_is_GP(thisseg) ?
                                   SHF_ALLOC | SHF_WRITE | SHF_GPDATA :
                                   SHF_ALLOC | SHF_WRITE;
    /* thisscn->shdr.sh_addr   = ??; */
    thisscn->shdr.sh_offset    = obj_fwrite_cnt;
    thisscn->shdr.sh_size      = thissize;
    /* thisscn->shdr.sh_link   = ??; */
    /* thisscn->shdr.sh_info   = ??; */
    thisscn->shdr.sh_addralign = 8;
    /* thisscn->shdr.sh_entsize= ??; */

    if (SEG_is_BSS(thisseg)) return;

    for (p = thisinitp; p != 0; p = p->datacdr) {
      int32 rpt = p->rpt, sort = p->sort, len = p->len, val = p->val;
      switch (sort) {
      case LIT_LABEL:   /* name only present for xxx/asm.c */
	break;
      default:  syserr("syserr_elf_gendata %ld", (long)sort);
      case LIT_BBBB:
      case LIT_HH:
      case LIT_BBH:
      case LIT_HBB:
        val = totargetsex(val, (int)sort);
      case LIT_NUMBER:
	if (len != 4) syserr("syserr_elf_datalen %ld", (long)len);
      case_any_number:
	while (rpt-- != 0) obj_fwrite4(&val, objstream, len);
	break;
#if (alignof_toplevel <= 2)
      case LIT_BBX:
      case LIT_HX:
        val = totargetsex(val, (int)sort);
        if (len != 2) syserr("syserr_elf_datalen %ld", (long)len);
        goto case_any_number;
#endif
#if (alignof_toplevel <= 1)
      case LIT_BX:
        val = totargetsex(val, (int)sort);
        if (len != 1) syserr("syserr_elf_datalen %ld", (long)len);
        goto case_any_number;
      case LIT_BBBX:
      case LIT_HBX:
        val = totargetsex(val, (int)sort);
        if (len != 3) syserr("syserr_elf_datalen %ld", (long)len);
        goto case_any_number;
#endif
      case LIT_ADCON:              /* (possibly external) name + offset */
	{   Symstr *sv = (Symstr *)len;  /* this reloc also in dataxrefs */
	    ExtRef *xr= symext_(sv);
	    (void)obj_checksym(sv);
#ifdef never
/* @@@ AM: see comment about "effectively double" at datarelocADCON     */
@@	    cc_msg("@@@writedataADCON %s extoffset=0x%lx val=%lx",
@@		    symname_(sv), (symext_(sv))->extoffset, val);
@@	    if (xr->extflags & (xr_defloc|xr_defext) &&
@@		!(xr->extflags & xr_bss)) val += xr->extoffset;
@@	    /* Can't do this for xr_bss, as we don't yet know the true
@@	       xr->extoffset */
@@	    cc_msg("->%lx\n",val);
#endif
#ifdef TARGET_IS_C4P
/* See matching special purpose code in obj_datarelocation.             */
            p->val = val;
            val = 0;
#endif
	    while (rpt-- != 0) obj_fwrite4(&val, objstream, 4);
	  }
	break;
      case LIT_FPNUM:
	{   FloatCon *fc = (FloatCon *)val;
	    /* do we need 'len' when the length is in fc->floatlen?? */
	    if (len == 4 || len == 8);
	    else syserr("syserr_elf_data %ld %ld %s",
			(long)rpt, (long)len, fc->floatstr);
	    while (rpt-- != 0)
            {   obj_fwrite4(&fc->floatbin.irep[0], objstream, 4);
                if (len == 8) obj_fwrite4(&fc->floatbin.irep[1], objstream, 4);
            }
	  }
	break;
      }
    }
  }
}

static int32 shn_from_datasect(int32 flags, DataDesc *seg)
{   int k;
    if (!(flags & xr_defloc+xr_defext)) return SHN_UNDEF;
    if (flags & xr_code) return scn_text.scn;
    if (seg == 0) return SHN_UNDEF;
    k = seg - datasects;
    if (k < (unsigned)TARGET_NUM_DATASECTS) return scn_seg(k).scn;
    syserr("shn_from_datasect");
    return SHN_UNDEF;

/* the SHN_COMMON below was inherited from SPARC...                     */
                        
#ifdef never
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
#ifdef TARGET_HAS_NEC_SECTS
                        flags & xr_zeropage2 ?
			  flags & xr_zeropage ? scn_tidata.scn:
			  flags & xr_bss  ?  scn_sebss.scn:
/* what about sedata?             flags & xr_data ? scn_sedata.scn:     */
				             scn_sidata.scn :
#endif
                        flags & xr_zeropage ?
			  flags & xr_constdata ? scn_sconst.scn:
			  flags & xr_bss  ?  scn_sbss.scn:
			  flags & xr_data ? scn_sdata.scn:
				             SHN_UNDEF :
#ifdef TARGET_HAS_C4P_SECTS
                        flags & xr_immpage2 ?
			  flags & xr_constdata ? scn_iconst.scn:
			  flags & xr_bss  ?  scn_ibss.scn:
			  flags & xr_data ? scn_idata.scn:
				             SHN_UNDEF :
#endif
			flags & xr_constdata ? scn_const.scn:
			flags & xr_bss  ?  scn_bss.scn:
#endif
			flags & xr_data ? scn_data.scn:
/* beware xr_data set with xr_bss in section (always?  tentative? */
			flags & xr_bss  ? (flags & xr_defloc? scn_bss.scn:
					                      SHN_COMMON):
				          SHN_UNDEF);
#endif /* never */
}


static bool sym_is_segmentlabel(Symstr *s)
{
    int i;
    if (s == bindsym_(codesegment)) return 1;
    for (i=0; i<TARGET_NUM_DATASECTS; i++)
      if (s == datasects[i].segsym) return 1;
    return 0;
}

static void obj_outsymtab()
{
  ExtRef *x;
  unsigned32 obj_stringpos = sizeof("");
  Elf32_Sym v;
  int32 start = obj_fwrite_cnt;
  int32 pass, pass_flags;

  scn_symtab.scn = ++elf_nsections;
  scn_strtab.scn = ++elf_nsections;

  /* scn_symtab.shdr.sh_name   = ... done in obj_writeshstrtab */
  scn_symtab.shdr.sh_type      = SHT_SYMTAB;
  scn_symtab.shdr.sh_flags     = XSHF_ALLOC;
  /* scn_symtab.shdr.sh_addr   = ??; */
  scn_symtab.shdr.sh_offset    = obj_fwrite_cnt;
  /* scn_symtab.shdr.sh_size   = ... see below */
  scn_symtab.shdr.sh_link      = scn_strtab.scn;
#ifndef TARGET_IS_NECxxxxNO  /* BUGFIX */
  scn_symtab.shdr.sh_info      = 1; /* for dummy entry ... and see below */
#else
  scn_symtab.shdr.sh_info      = scn_strtab.scn;
#endif
  scn_symtab.shdr.sh_addralign = 4;
  scn_symtab.shdr.sh_entsize   = sizeof(Elf32_Sym);

  /* write a dummy entry */
  memclr(&v, sizeof(v));
  obj_fwrite_Sym(&v, objstream);

  obj_symcount = 0;		/* renumber the symbols to match the order
				   generated here */

#ifdef TARGET_IS_NEC /* DEVFILE */
  {
      v.st_name = obj_stringpos;
      v.st_value = 0;
      v.st_size = 0;
      v.st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE);
      v.st_other = 0;
      v.st_shndx = SHN_ABS;
      obj_fwrite_Sym(&v, objstream);
      obj_stringpos += strlen(sourcefile)+1;

      v.st_name = obj_stringpos;
      v.st_value = 0;
      v.st_size = 0;
      v.st_info = ELF32_ST_INFO(STB_LOCAL, STT_DEVFILE);
      v.st_other = 0;
      v.st_shndx = SHN_ABS;
      obj_fwrite_Sym(&v, objstream);
      obj_stringpos += sizeof("d3000.800");

      obj_symcount += 2;
  }
#endif

  for (pass = 1, pass_flags = xr_defloc;
       pass <= 2;
       pass++, pass_flags = 0) {
    /* seem to need all local symbols first, then all globals etc */
    for (x = obj_symlist; x != NULL; x = x->extcdr) {
      Symstr *s = x->extsym;
      char *name;
      size_t n, k;
      int stinfof, stinfob;
      int32 flags = x->extflags;
        if (debugging(DEBUG_OBJ))
            cc_msg("ELF1: Sym $r index %ld\n", x->extsym, x->extindex);
      if ((flags & xr_defloc) != pass_flags) continue;

      name = elfname_(s);
      n = strlen(name);
      k = name[0]=='.' ? 0 : sizeof(target_elf_prefix)-1;

      memclr(&v, sizeof(v));
      if (debugging(DEBUG_OBJ)) cc_msg("sym $r%lx ", s, (long)flags);
      v.st_name = obj_stringpos;
/* @@@ AM: beware extoffset */
      v.st_value = x->extoffset; /* ... but see bss below */
      v.st_size = 0; /* @@@ NEED TO KNOW !!!!! ????? !!!!! */
      stinfof = !(flags & (xr_defloc+xr_defext)) ? STT_NOTYPE :
                sym_is_segmentlabel(s) ? STT_SECTION :
		flags & xr_code            ? STT_FUNC :
				  	     STT_OBJECT;
      stinfob = flags & xr_defloc ? STB_LOCAL : STB_GLOBAL;
      v.st_info = ELF32_ST_INFO(stinfob, stinfof);
      v.st_other = 0;
      v.st_shndx = (Elf32_Half) shn_from_datasect(flags, x->extseg);
      if (v.st_shndx == (Elf32_Half)-999) continue;
#ifndef TARGET_IS_NECxxxxNO  /* BUGFIX */
      /* section is empty, but may still have labels (eg dataseg) */
      /* /* what if next scn is empty too? */
      if (flags & xr_defloc) scn_symtab.shdr.sh_info++; /* @@@ empirically! */
#endif

      obj_stringpos += (size_t)(n+k+1);

      obj_fwrite_Sym(&v, objstream);
      x->extindex = ++obj_symcount;

    }
  }

  scn_symtab.shdr.sh_size = obj_fwrite_cnt - start;
}

static void obj_outstrtab()
{ ExtRef *x;
  int32 start = obj_fwrite_cnt;
  int32 pass, pass_flags;

  /* scn_strtab.scn = ++elf_nsections; ... done in obj_outsymtab() */

  /* scn_strtab.shdr.sh_name   = ... done in obj_writeshstrtab */
  scn_strtab.shdr.sh_type      = SHT_STRTAB;
  scn_strtab.shdr.sh_flags     = XSHF_ALLOC;
  /* scn_strtab.shdr.sh_addr   = ??; */
  scn_strtab.shdr.sh_offset    = obj_fwrite_cnt;
  /* scn_strtab.shdr.sh_size   = ... see below */
  scn_strtab.shdr.sh_link      = 0;
  /* scn_strtab.shdr.sh_info   = ??; */
  scn_strtab.shdr.sh_addralign = 1;
  scn_strtab.shdr.sh_entsize   = 0;

  /* now write the string table, starting with a null string */
  obj_putsc("", objstream);

#ifdef TARGET_IS_NEC /* DEVFILE */
  obj_puts(sourcefile, objstream);
  obj_putsc("d3000.800", objstream);
#endif

  for (pass = 1, pass_flags = xr_defloc;
       pass <= 2;
       pass++, pass_flags = 0) {
    /* seem to need all local symbols first, then all globals etc - need not do
       it that way here, but needs to match obj_outsymtab expectations */
    for (x = obj_symlist; x != 0; x = x->extcdr) {
      Symstr *s = x->extsym;
      char *name;
      size_t n, k;
      int shndx;
      int32 flags = x->extflags;
        if (debugging(DEBUG_OBJ))
            cc_msg("ELF2: Sym $r index %ld\n", x->extsym, x->extindex);
      if ((flags & xr_defloc) != pass_flags) continue;
#ifdef OLD_SPARC_CODE
      if ( /* v.st_shndx */
	  (Elf32_Half) (!(flags & xr_defloc+xr_defext) ? SHN_UNDEF :
			flags & xr_code ? scn_text.scn:
			flags & xr_data ? scn_data.scn:
			flags & xr_bss  ? SHN_COMMON:
			                  SHN_UNDEF) == (Elf32_Half)-999)
	continue;
#else
      shndx = (Elf32_Half) shn_from_datasect(flags, x->extseg);
      if (shndx == (Elf32_Half)-999) continue;
#endif

      name = elfname_(s);
      n = strlen(name);
      k = name[0]=='.' ? 0 : sizeof(target_elf_prefix)-1;
      if (k > 0) obj_fwrite(target_elf_prefix, 1, k, objstream);
      obj_fwrite(name, 1, (size_t)(n+1), objstream);
    }
  }
  scn_strtab.shdr.sh_size = obj_fwrite_cnt - start;
  obj_align();
}

static int32 obj_checksym(Symstr *s)
{   ExtRef *x = symext_(s);
    if (x != 0) {
      if (!elf_mapto_segnames)
	return x->extindex;
      if (!(x->extflags & xr_defloc+xr_defext)
            || sym_is_segmentlabel(s)
#ifndef NEC_OR_C4P /* TARGET_IS_NEC */
            /* this has to do with the new bss code               */
	    || (x->extflags & (xr_bss+xr_defext)) == (xr_bss+xr_defext)
#endif
         )
          /* honest external or segment defining symbol */
	return x->extindex;
      if (x->extflags & xr_code)
        return obj_checksym(bindsym_(codesegment));
      if (x->extseg && x->extseg->segsym)
        return obj_checksym(x->extseg->segsym);
    }
    syserr("syserr_elf_checksym %s", symname_(s));
    return 0;
}

static void obj_coderelocation(void)
{   CodeXref *x;
    Elf32_Rela r;
    unsigned32 ncoderelocs = 0;
    if (codexrefs == NULL) return;

    scn_textrel.scn = ++elf_nsections;

    /* scn_textrel.shdr.sh_name   = ... done in obj_writeshstrtab;
       					section name */
    scn_textrel.shdr.sh_type      = SHT_RELA;
    scn_textrel.shdr.sh_flags     = XSHF_ALLOC;
    /* scn_textrel.shdr.sh_addr   = ??; */
    scn_textrel.shdr.sh_offset    = obj_fwrite_cnt;
    /* scn_textrel.shdr.sh_size   = ... see below */
    scn_textrel.shdr.sh_link      = scn_symtab.scn;
    scn_textrel.shdr.sh_info      = scn_text.scn;
    scn_textrel.shdr.sh_addralign = 4;
    scn_textrel.shdr.sh_entsize   = sizeof(Elf32_Rela);

#ifdef TARGET_IS_SPARC
    /* @@@ codexrefs = (CodeXref*)dreverse((List*)codexrefs); */
#endif
    memclr(&r, sizeof(r));
    for (x = codexrefs; x != NULL; x = x->codexrcdr)
    {   Symstr *s    = x->codexrsym;
        int32  sno   = obj_checksym(s);
        r.r_offset  = (int32)x->codexroff & 0xffffff;
        r.r_addend = elf_mapto_segnames ?
            x->codexrlitoff + symext_(s)->extoffset :
            x->codexrlitoff;
        /* Note r_addend may be updated below (check still needed).     */

        switch (x->codexroff & 0xff000000)
        {
            case X_absreloc:
                r.r_info = ELF32_R_INFO(sno, R_SPARC_32);
                if (debugging(DEBUG_OBJ))
                  cc_msg("addreloc '%s' ",symname_(s));
                break;
#ifndef TARGET_IS_NEC         /* really just SPARC and the like */
#ifndef TARGET_IS_C4P         /* really just SPARC and the like */
	    case X_DataAddr:
                r.r_info = ELF32_R_INFO(sno, R_SPARC_HI22);
                if (debugging(DEBUG_OBJ))
		  cc_msg("addreloc-hi '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_DataAddr1:
                r.r_info = ELF32_R_INFO(sno, R_SPARC_LO10);
                if (debugging(DEBUG_OBJ))
		  cc_msg("addreloc-lo '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
#endif
#endif
#ifdef TARGET_IS_C4P
	    case X_DataAddr:
                r.r_info = ELF32_R_INFO(sno, R_C4P_ABS24);
                if (debugging(DEBUG_OBJ))
		  cc_msg("abs24 '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFb:
                r.r_info = ELF32_R_INFO(sno, R_C4P_ZPDISP7B);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFFb '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFh:
                r.r_info = ELF32_R_INFO(sno, R_C4P_ZPDISP7H);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFFh '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFw:
                r.r_info = ELF32_R_INFO(sno, R_C4P_ZPDISP7L);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFFw '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
#endif
#ifdef TARGET_IS_NEC
	    case X_HI16S:
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_HI16S);
                if (debugging(DEBUG_OBJ))
		  cc_msg("addreloc-hi '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_LO16b:
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_LO16b);
                if (debugging(DEBUG_OBJ))
		  cc_msg("LO16b '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_LO16h:
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_LO16h);
                if (debugging(DEBUG_OBJ))
		  cc_msg("LO16h '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_LO16bu:        /* 850e only */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_LO16bu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("LO16bu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_LO16hu:        /* 850e only */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_LO16hu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("LO16hu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_LO16w:
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_LO16w);
                if (debugging(DEBUG_OBJ))
		  cc_msg("LO16w '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_GPOFFb:   /* actually just GP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_GPDISP16b);
                if (debugging(DEBUG_OBJ))
		  cc_msg("GPOFF16b '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_GPOFFh:   /* actually just GP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_GPDISP16h);
                if (debugging(DEBUG_OBJ))
		  cc_msg("GPOFF16h '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_GPOFFbu:   /* (850e only) GP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_GPDISP16bu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("GPOFF16bu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_GPOFFhu:   /* (850e only) GP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_GPDISP16hu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("GPOFF16hu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_GPOFFw:   /* actually just GP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_GPDISP16w);
                if (debugging(DEBUG_OBJ))
		  cc_msg("GPOFF16w '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFb:   /* actually just R0 relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_ZPDISP16b);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFF16b '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFh:   /* actually just R0 relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_ZPDISP16h);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFF16h '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFbu:   /* (850e only) R0 relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_ZPDISP16bu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFF16bu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFhu:   /* (850e only) R0 relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_ZPDISP16hu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFF16hu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_ZPOFFw:   /* actually just R0 relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_ZPDISP16w);
                if (debugging(DEBUG_OBJ))
		  cc_msg("ZPOFF16w '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFFb:   /* actually just EP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EPDISP16b);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EPOFF16b '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFFh:   /* actually just EP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EPDISP16h);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EPOFF16h '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFFbu:   /* (850e only) EP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EPDISP16bu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EPOFF16bu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFFhu:   /* (850e only) EP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EPDISP16hu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EPOFF16hu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFFw:   /* actually just EP relative for NEC-850 */
                r.r_offset += 2;  /* ACN */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EPDISP16w);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EPOFF16w '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFF7b:  /* variant of X_EPOFF */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EP7b);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EP7b '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFF7h:  /* variant of X_EPOFF */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EP7h);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EP7h '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFF4bu:  /* (850e only) variant of X_EPOFF */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EP4bu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EP4bu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFF4hu:  /* (850e only) variant of X_EPOFF */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EP4hu);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EP4hu '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
            case X_EPOFF7w:  /* variant of X_EPOFF */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_EP7w);
                if (debugging(DEBUG_OBJ))
		  cc_msg("EP7w '%s'%lx\n",symname_(s), x->codexrlitoff);
                break;
#endif
#ifdef TARGET_IS_C4P
            case X_PCreloc:
                /* @@@ beware jump to array of code? */
		if (symext_(s)->extflags & (xr_defloc | xr_defext)) continue;
		/* defined in this compilation unit so relocated already */
                r.r_info = ELF32_R_INFO(sno, R_C4P_PC24);
                r.r_addend = 0;         /* @@@ BEWARE */
                if (debugging(DEBUG_OBJ))
                    cc_msg("pcreloc24 '%s'\n",symname_(s));
                break;
#endif
#ifdef TARGET_IS_NEC
            case X_PCreloc:
                /* @@@ beware jump to array of code? */
		if (symext_(s)->extflags & (xr_defloc | xr_defext)) continue;
		/* defined in this compilation unit so relocated already */
                r.r_info = ELF32_R_INFO(sno, R_NEC850_DISP22);
                r.r_addend = 0;         /* @@@ BEWARE */
                if (debugging(DEBUG_OBJ))
                    cc_msg("disp22reloc '%s'\n",symname_(s));
                break;
#endif
#ifdef TARGET_IS_SPARC
            case X_PCw30reloc:
		if (symext_(s)->extflags & (xr_defloc | xr_defext)) continue;
		/* defined in this compilation unit so relocated already */
                r.r_info = ELF32_R_INFO(sno, R_SPARC_WDISP30);
                r.r_addend = 0;         /* @@@ BEWARE */
                if (debugging(DEBUG_OBJ)) cc_msg("W30reloc '%s'\n",symname_(s));
                break;
#endif
            default:
                syserr("syserr_elf_reloc1 %.8lx", (long)x->codexroff);
                continue;
        }
        obj_fwrite_Rela(&r, objstream);
        ncoderelocs++;
        if (debugging(DEBUG_OBJ))
            cc_msg("\
r_offset=%lx, r_symbolnum=%lx, r_type=%d, r_addend=%lx, sno=%ld\n",
		   r.r_offset, ELF32_R_SYM(r.r_info),
		   ELF32_R_TYPE(r.r_info), r.r_addend, sno);
    }
    scn_textrel.shdr.sh_size = ncoderelocs * sizeof(Elf32_Rela);
}

static void obj_datarelocation(int thisseg, DataXref *thisxrefs,
                               DataInit *thisinitp)
{   DataXref *x;
    Elf32_Rela r;
    struct myscn *thisscnrel = &scn_segrel(thisseg);
    unsigned32 nrelocs = 0;

    if (thisxrefs == NULL) return;

    thisscnrel->scn = ++elf_nsections;

    /* thisscnrel->shdr.sh_name   = ... done in obj_writeshstrtab;
       					section name */
    thisscnrel->shdr.sh_type      = SHT_RELA;
    thisscnrel->shdr.sh_flags     = XSHF_ALLOC;
    /* thisscnrel->shdr.sh_addr   = ??; */
    thisscnrel->shdr.sh_offset    = obj_fwrite_cnt;
    /* thisscnrel->shdr.sh_size   = ... see below */
    thisscnrel->shdr.sh_link      = scn_symtab.scn;
    thisscnrel->shdr.sh_info      = scn_seg(thisseg).scn;
    thisscnrel->shdr.sh_addralign = 4;
    thisscnrel->shdr.sh_entsize   = sizeof(Elf32_Rela);

    memset(&r, 0, sizeof(r));
    for (x = thisxrefs; x != NULL; x = x->dataxrcdr)
    {   Symstr *s  = x->dataxrsym;
        int32  sno = obj_checksym(s);
        /* all data relocs are X_backaddrlit (abs ref) so far ? */
/* @@@	fprintf(stderr,"datarelocADCON %s extoffset=0x%lx\n",
		symname_(s), (symext_(s))->extoffset); */
        r.r_offset = (int)x->dataxroff;

	r.r_info = ELF32_R_INFO (sno, R_SPARC_32);
        r.r_addend = elf_mapto_segnames ? symext_(s)->extoffset : 0;
/* We normally don't add any C offset (e.g. static int *x = &a + 4) to  */
/* r_addend, since we have already put any offset (20=5*4 above) in the */
/* data segment and setting it would effectively double the offset.     */

#ifdef TARGET_IS_C4P
/* However, on the C4P, we the relocation *overwrites* the data value   */
/* and hence we must add the offset into r_addend.                      */
	{
	  DataInit *p;
	  int32 i;
/* 	  fprintf(stderr,"Looking for %x\n", r.r_offset); */
	  for (i = 0, p = thisinitp;
	       i!=r.r_offset;
	       i += (p->rpt) * (p->sort == LIT_ADCON ? 4 : p->len),
	          p = p->datacdr);
/* 	  fprintf(stderr,"Found: rpt, sort, len, val = %ld, %ld, %ld, %lx\n",
		    p->rpt, p->sort, p->len, p->val);  */
	  if (p->sort != LIT_ADCON)
              syserr("Not ADCON in Data Reloc %ld\n", p->sort);
          if (debugging(DEBUG_OBJ))
              cc_msg("<augment by %d>", p->val);
	  r.r_addend += p->val; /* obj_writedata treats p->val as 0 ... */
	}
#endif

        /*
         * Unix linker can't handle relocations relative to things
         * defined locally... checksym converts to text/data relative.
         * Correction: ELF linkers seem in general happy with this,
         * and indeed it seems the prefered way, at least on SPARC/NEC.
         */
        obj_fwrite_Rela(&r, objstream);
        if (debugging(DEBUG_OBJ))
            cc_msg("data reloc '%s'", symname_(s)),
            cc_msg(" \
r_offset=%lx, r_symbolnum=%lx, r_type=%d, r_addend=%lx, sno=%ld\n",
                   r.r_offset, ELF32_R_SYM(r.r_info),
                   ELF32_R_TYPE(r.r_info), r.r_addend, sno);
        nrelocs++;
    }
    thisscnrel->shdr.sh_size = nrelocs * sizeof(Elf32_Rela);
}

/* exported functions... */

int32 obj_symref4(Symstr *s, int flags, int32 loc, DataDesc *seg)
{   ExtRef *x;
    if ((x = symext_(s)) == 0)    /* saves a quadratic loop */
    {   if (obj_symcount > 0x7fffffff)
            cc_fatalerr("elf_fatalerr_toomany");
        x = (ExtRef *)GlobAlloc(SU_Xref, sizeof(ExtRef));
        x->extcdr = obj_symlist,
          x->extsym = s,
          x->extindex = ++obj_symcount,
          x->extflags = 0,
          x->extoffset = 0;
        obj_symlist = symext_(s) = x;
        if (debugging(DEBUG_OBJ))
            cc_msg("ELF: Sym $r index %ld\n", x->extsym, x->extindex);
    }
/* The next two lines cope with further ramifications of the abolition of */
/* xr_refcode/refdata in favour of xr_code/data without xr_defloc/defext  */
/* qualification.  This reduces the number of bits, but needs more        */
/* checking in that a symbol defined as data, and then called via         */
/* casting to a code pointer may acquire defloc+data and then get         */
/* xr_code or'ed in.  Suffice it to say this causes confusion.            */
/* AM wonders if gen.c ought to be more careful instead.                  */
    if (flags & (xr_defloc+xr_defext))
        x->extflags &= ~(xr_code+xr_data+xr_constdata+xr_bss);
    if (x->extflags & (xr_defloc+xr_defext))
        flags &= ~(xr_code+xr_data+xr_constdata+xr_bss);
/* end of fix, but perhaps we should be more careful about mult. defs.?   */
    x->extflags |= flags;
    if (flags & xr_defloc+xr_defext)
    {            /* private or public data or code */
        x->extoffset = machine_address_(loc,flags);
        if (seg) x->extseg = seg;
    }
    else if ((loc > 0) && !(flags & xr_code) &&
               !(x->extflags & xr_defloc+xr_defext))
    {
                            /* common data, not already defined */
                            /* -- put length in x->extoffset    */
        if (loc > x->extoffset) x->extoffset = machine_address_(loc,flags);
    }
    /* The next line returns the offset of a function in the codesegment */
    /* if it has been previously defined -- this saves store on the arm  */
    /* and allows short branches on other machines.  Otherwise it        */
    /* returns -1 for undefined objects or data objects.                 */
    return ((x->extflags & (xr_defloc+xr_defext)) && (x->extflags & xr_code) ?
            x->extoffset : -1);
}

#ifndef NEC_OR_C4P /* TARGET_IS_NEC */
/* For fortran... */
void obj_common_start(Symstr *name)
{   /* There is no real support in COFF for common definitions (BLOCK   */
    /* DATA).  What needs to be done is to turn the block name into     */
    /* an exported symbol in the normal data area (.data).              */
    labeldata(name);
    obj_symref_xxx(name, xr_data+xr_defext, dataloc);
}

void obj_common_end(void) {}
#endif

#ifndef ELF_NO_COMMENT_SCN
static void obj_outcomment(void)
{
  int32 start = obj_fwrite_cnt;
  char *banner = CC_BANNER;
  scn_comment.scn = ++elf_nsections;

  /* scn_comment.shdr.sh_name   = ... done in obj_writeshstrtab; section name*/
  scn_comment.shdr.sh_type      = SHT_PROGBITS;
  /* scn_comment.shdr.sh_flags  = 0; */
  /* scn_comment.shdr.sh_addr   = ??; */
  scn_comment.shdr.sh_offset    = obj_fwrite_cnt;
  /* scn_comment.shdr.sh_size   = ... see below */
  /* scn_comment.shdr.sh_link   = ??; */
  /* scn_comment.shdr.sh_info   = ??; */
  scn_comment.shdr.sh_addralign = 1;
  /* scn_comment.shdr.sh_entsize= ??; */

  obj_fwrite(banner, 1, (int32) 1+strlen(banner), objstream);
  scn_comment.shdr.sh_size = obj_fwrite_cnt - start;
  obj_align();
}
#endif

#ifdef TARGET_HAS_DBX
/* Rearrangement of function between aoutobj and dbx here - dbx data are now
   buffered (in global store) in dbx, and only handed to aoutobj at compilation
   unit end (as distinct from handed over at the end of each unit, and buffered
   in aoutobj).
 */
void obj_stabentry(struct nlist *p)
{
    StabList *stab;
    stab = (StabList *)SynAlloc(sizeof(StabList));
    stab->next  = obj_stablist; obj_stablist = stab;
    stab->nlist = *p;
    ++obj_stabcount;
    if (debugging(DEBUG_OBJ)) cc_msg("stabentry '%s'\n", p->n_un.n_name);
}

static void obj_outdbxtab()
{
  unsigned32 obj_stringpos = sizeof("");
  struct nlist v;
  int32 start = obj_fwrite_cnt;

  if (obj_stabcount == 0) return;

  scn_dbxtab.scn = ++elf_nsections;
  scn_dbxstrtab.scn = ++elf_nsections;

  /* scn_dbxtab.shdr.sh_name   = ... done in obj_writeshstrtab */
  scn_dbxtab.shdr.sh_type      = SHT_PROGBITS;
  scn_dbxtab.shdr.sh_flags     = 0;
  /* scn_dbxtab.shdr.sh_addr   = ??; */
  scn_dbxtab.shdr.sh_offset    = obj_fwrite_cnt;
  /* scn_dbxtab.shdr.sh_size   = ... see below */
  scn_dbxtab.shdr.sh_link      = scn_dbxstrtab.scn;
  /* scn_dbxtab.shdr.sh_info      = ??; dummy entry? */
  scn_dbxtab.shdr.sh_addralign = 4;
  scn_dbxtab.shdr.sh_entsize   = sizeof(struct nlist);

  /* write a dummy entry */
  memclr(&v, sizeof(v));
  obj_fwrite_nlist(&v, objstream);

    obj_stablist = (StabList *)dreverse((List *)obj_stablist);

  { StabList *p;
    for (p = obj_stablist; p != 0; p = p->next)
    {
        char *name = p->nlist.n_un.n_name;
        if (name != 0) {
            p->nlist.n_un.n_strx = obj_stringpos;
            obj_stringpos += strlen(name) + 1;
        }
/* The following must be more NEC-850 ELF-like.                         */
#define N_DATA  0x6             /* data */
#define N_BSS   0x8             /* bss */
#define N_TYPE  0x1e            /* mask for all the type bits */
        if ((p->nlist.n_type & N_TYPE) == N_DATA) {
            p->nlist.n_value += codesize;
        } else if ((p->nlist.n_type & N_TYPE) == N_BSS)
            p->nlist.n_value += codesize+vardata.size;
        obj_fwrite_nlist(&p->nlist, objstream);
        p->nlist.n_un.n_name = name;     /* restore 'string' form.      */
    }
  }

  scn_dbxtab.shdr.sh_size = obj_fwrite_cnt - start;
}

static void obj_outdbxstrtab()
{
  int32 start = obj_fwrite_cnt;

  if (obj_stabcount == 0) return;

  /* scn_dbxstrtab.scn = ++elf_nsections; ... done in obj_outdbxtab() */

  /* scn_dbxstrtab.shdr.sh_name   = ... done in obj_writeshstrtab */
  scn_dbxstrtab.shdr.sh_type      = SHT_STRTAB;
  scn_dbxstrtab.shdr.sh_flags     = XSHF_ALLOC;
  /* scn_dbxstrtab.shdr.sh_addr   = ??; */
  scn_dbxstrtab.shdr.sh_offset    = obj_fwrite_cnt;
  /* scn_dbxstrtab.shdr.sh_size   = ... see below */
  scn_dbxstrtab.shdr.sh_link      = 0;
  /* scn_dbxstrtab.shdr.sh_info   = ??; */
  scn_dbxstrtab.shdr.sh_addralign = 1;
  scn_dbxstrtab.shdr.sh_entsize   = 0;

  /* now write the string table, starting with a null string */
  obj_putsc("", objstream);

  { StabList *p;
    for (p = obj_stablist; p != 0; p = p->next) {
        char *name = p->nlist.n_un.n_name;
        if (name != 0) {
            obj_fwrite(name, 1, (int32)strlen(name)+1, objstream);
        }
    }
  }
  scn_dbxstrtab.shdr.sh_size = obj_fwrite_cnt - start;
  obj_align();
}
#endif

void obj_init()
{   obj_symcount = 0;
    obj_symlist = 0;
#ifndef NEC_OR_C4P /* TARGET_IS_NEC */
    dataxrefs = 0;
#endif
    codexrefs = 0;
    codesize = 0;     /* remove */
#ifdef TARGET_HAS_DBX
    obj_stabcount = 0;
    obj_stablist = 0;
#endif
    elf_nsections = -1;
    { int32 i;
      for (i=0; i<SCN_MAX; i++) {
	scns[i].scn = -999;
	memclr(&scns[i].shdr, sizeof(scns[i].shdr));
      }
    }
    scn_null.scn = ++elf_nsections;	/* to ensure it is written out ... */
    scn_null.shdr.sh_type = SHT_NULL;

    obj_fwrite_cnt = 0;
}

static void obj_outshtable(void)
{
  int32 i;
  for (i=0; i<SCN_MAX; i++)
    if (scns[i].scn != -999)
      obj_fwrite_Shdr(&scns[i].shdr, objstream);
  obj_align();
}

void obj_header()
{   Elf32_Ehdr h;
    memclr (&h, sizeof(h));
    h.e_ident[EI_MAG0] = ELFMAG0;
    h.e_ident[EI_MAG1] = ELFMAG1;
    h.e_ident[EI_MAG2] = ELFMAG2;
    h.e_ident[EI_MAG3] = ELFMAG3;
    h.e_ident[EI_CLASS] = ELFCLASS32;
    h.e_ident[EI_DATA] = (var_cc_private_flags&65536) ?
                            (host_lsbytefirst ? ELFDATA2LSB : ELFDATA2MSB) :
                            (target_lsbytefirst ? ELFDATA2LSB : ELFDATA2MSB);
    h.e_ident[EI_VERSION] = 1;
    h.e_type = ET_REL;
    h.e_machine = ELF_TARGETID;
    h.e_version = EV_CURRENT;
    h.e_shoff = obj_fwrite_cnt - ((1+elf_nsections)*sizeof(Elf32_Shdr));
    h.e_flags = EHDR_FLAGS;
    h.e_ehsize = sizeof(h);
    h.e_shentsize = sizeof(Elf32_Shdr);
    h.e_shnum = (Elf32_Half) (1+elf_nsections);
    h.e_shstrndx = (Elf32_Half) scn_shstrtab.scn;
    obj_fwrite_cnt = 0;
    obj_fwrite_Ehdr(&h, objstream);
    obj_align();
}

void obj_trailer()
{
  codexrefs = (CodeXref *)dreverse((List *)codexrefs);
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
  { int i;
    for (i=0; i<TARGET_NUM_DATASECTS; i++) if (!SEG_is_BSS(i))
        datasects[i].xrefs = (DataXref *)dreverse((List *) datasects[i].xrefs);
  }
#else
  dataxrefs = (DataXref *)dreverse((List *)dataxrefs);
#endif
  obj_symlist = (ExtRef *)dreverse((List *)obj_symlist);
  /* oldest first */

#ifdef TARGET_HAS_DBX
  if (debugging(DEBUG_OBJ)) cc_msg("dbg_writedebug\n");
  dbg_writedebug();
#endif

  if (debugging(DEBUG_OBJ)) cc_msg("writeshstrtab\n");
  obj_writeshstrtab();
  if (debugging(DEBUG_OBJ)) cc_msg("writecode\n");
  obj_writecode();
  if (debugging(DEBUG_OBJ)) cc_msg("writedata\n");
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
  { int i;
    for (i=0; i<TARGET_NUM_DATASECTS; i++)
        obj_writedata(i, datasects[i].head, datasects[i].size);
  }
#else
  obj_writedata(0, datainitp, dataloc);
  obj_writebss();
#endif
  if (debugging(DEBUG_OBJ)) cc_msg("symtab\n");
  obj_outsymtab();
  if (debugging(DEBUG_OBJ)) cc_msg("strtab\n");
  obj_outstrtab();
#ifdef TARGET_HAS_DBX
  if (debugging(DEBUG_OBJ)) cc_msg("dbxtab\n");
  obj_outdbxtab();
  if (debugging(DEBUG_OBJ)) cc_msg("dbxstrtab\n");
  obj_outdbxstrtab();
#endif
  if (debugging(DEBUG_OBJ)) cc_msg("coderelocation\n");
  obj_coderelocation();
  if (debugging(DEBUG_OBJ)) cc_msg("datarelocation\n");
#ifdef NEC_OR_C4P /* TARGET_IS_NEC */
  { int i;
    for (i=0; i<TARGET_NUM_DATASECTS; i++) if (!SEG_is_BSS(i))
        obj_datarelocation(i, datasects[i].xrefs, datasects[i].head);
  }
#else
  obj_datarelocation(0, dataxrefs, datainitp);
#endif
#ifndef ELF_NO_COMMENT_SCN
  if (debugging(DEBUG_OBJ)) cc_msg("outcomment\n");
  obj_outcomment();
#endif
  if (debugging(DEBUG_OBJ)) cc_msg("outshtable\n");
  obj_outshtable();
  if (debugging(DEBUG_OBJ)) cc_msg("rewind\n");
  rewind(objstream);   /* works for hex format too */
  if (debugging(DEBUG_OBJ)) cc_msg("rewriting header\n");
  obj_header();        /* re-write header at top of file */
  /* file now opened and closed in main(). */
}

#ifdef TARGET_IS_C4P
/* This hack is gross, but leave it for now (helps catncc.c work)         */
# define TARGET_IS_NEC 1       /* @@@ see c4p/target.h hack defining both */
#endif

/* end of elfobj.c */
