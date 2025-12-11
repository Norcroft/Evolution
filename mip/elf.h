
/* mip/elf.h: data structures and constants for ELF,                    */
/* Copyright (C) Codemist Ltd, 1993.                                    */

/* The following is NOT a full definition of ELF -- it includes         */
/* only those parts required by the Norcroft compiler suite.            */

typedef unsigned long	Elf32_Addr;
typedef unsigned short	Elf32_Half;
typedef unsigned long	Elf32_Off;
typedef long		Elf32_Sword;
typedef unsigned long	Elf32_Word;

/*	ELF header
 */

/*	Section header
 */

#define EI_NIDENT	16

typedef struct {
	unsigned char	e_ident[EI_NIDENT];	/* ident bytes */
	Elf32_Half	e_type;			/* file type */
	Elf32_Half	e_machine;		/* target machine */
	Elf32_Word	e_version;		/* file version */
	Elf32_Addr	e_entry;		/* start address */
	Elf32_Off	e_phoff;		/* phdr file offset */
	Elf32_Off	e_shoff;		/* shdr file offset */
	Elf32_Word	e_flags;		/* file flags */
	Elf32_Half	e_ehsize;		/* sizeof ehdr */
	Elf32_Half	e_phentsize;		/* sizeof phdr */
	Elf32_Half	e_phnum;		/* number phdrs */
	Elf32_Half	e_shentsize;		/* sizeof shdr */
	Elf32_Half	e_shnum;		/* number shdrs */
	Elf32_Half	e_shstrndx;		/* shdr string index */
} Elf32_Ehdr;

#define EI_MAG0		0		/* e_ident[] indexes */
#define EI_MAG1		1
#define EI_MAG2		2
#define EI_MAG3		3
#define EI_CLASS	4
#define EI_DATA		5
#define EI_VERSION	6

#define ELFMAG0		0x7f		/* EI_MAG */
#define ELFMAG1		'E'
#define ELFMAG2		'L'
#define ELFMAG3		'F'

					/* EI_CLASS */
#define ELFCLASS32	1

					/* EI_DATA */
#define ELFDATA2LSB	1
#define ELFDATA2MSB	2

					/* e_type */
#define ET_REL		1

					/* e_machine */
#define EM_SPARC	2		/* Sun SPARC */
#define EM_NEC850	28867		/* NEC-850 */
#define EM_C4P        	29869		/* C4P processor */

					/* e_version, EI_VERSION */
#define EV_CURRENT	1

/*	Section header
 */

typedef struct {
	Elf32_Word	sh_name;	/* section name */
	Elf32_Word	sh_type;	/* SHT_... */
	Elf32_Word	sh_flags;	/* SHF_... */
	Elf32_Addr	sh_addr;	/* virtual address */
	Elf32_Off	sh_offset;	/* file offset */
	Elf32_Word	sh_size;	/* section size */
	Elf32_Word	sh_link;	/* misc info */
	Elf32_Word	sh_info;	/* misc info */
	Elf32_Word	sh_addralign;	/* memory alignment */
	Elf32_Word	sh_entsize;	/* entry size if table */
} Elf32_Shdr;

#define SHT_NULL	0		/* sh_type */
#define SHT_PROGBITS	1
#define SHT_SYMTAB	2
#define SHT_STRTAB	3
#define SHT_RELA	4
#define SHT_NOBITS	8

#define SHF_WRITE	0x1		/* sh_flags */
#define SHF_ALLOC	0x2
#define SHF_EXECINSTR	0x4
#define SHF_GPDATA	0x10000000      /* NEC-850 GP-relative */

#define SHN_UNDEF	0		/* special section numbers */
#define SHN_ABS		0xfff1
#define SHN_COMMON	0xfff2

/*	Symbol table
 */

typedef struct {
	Elf32_Word	st_name;
	Elf32_Addr	st_value;
	Elf32_Word	st_size;
	unsigned char	st_info;	/* bind, type: ELF_32_ST_... */
	unsigned char	st_other;
	Elf32_Half	st_shndx;	/* SHN_... */
} Elf32_Sym;

#define ELF32_ST_INFO(bind,type)	(((bind)<<4)+((type)&0xf))

#define STB_LOCAL	0		/* BIND */
#define STB_GLOBAL	1

#define STT_NOTYPE	0		/* TYPE */
#define STT_OBJECT	1
#define STT_FUNC	2
#define STT_SECTION	3
#define STT_FILE	4
#define STT_DEVFILE	5

/*	Relocation
 */

typedef struct {
	Elf32_Addr	r_offset;
	Elf32_Word	r_info;		/* sym, type: ELF32_R_... */
	Elf32_Sword	r_addend;
} Elf32_Rela;

#define ELF32_R_SYM(info)	((info)>>8)
#define ELF32_R_TYPE(info)	((unsigned char)(info))
#define ELF32_R_INFO(sym,type)	(((sym)<<8)+(unsigned char)(type))

                                        /* relocation types     */
#define R_SPARC_32              3
#define R_SPARC_WDISP30         7
#define R_SPARC_HI22            9
#define R_SPARC_LO10           12

#define R_C4P_32                3       /* for data (and movi.d?)       */
#define R_C4P_PC24            128       /* for call.l jump.l            */
#define R_C4P_ABS24           129       /* for movi.l                   */
#define R_C4P_ZPDISP7B        130       /* for load.b (direct)          */
#define R_C4P_ZPDISP7H        131       /* for load.w (direct)          */
#define R_C4P_ZPDISP7L        132       /* for load.l (direct)          */

#define R_NEC850_32             3       /* R_V810_WORD          */
#define R_NEC850_HI16S          6       /* R_V810_WHI1       */
#ifdef NEC850_UNIQUE_RELOC_CODES
/* NEC as850 uses the same relocation codes for lo(sym) as for %sym.    */
/* The former logically gives the low order 16 bits of an addres (for   */
/* combining with hi1(sym).  The latter is really only valid if 'sym'   */
/* is allocated within 64K of EP.  The effect of using the same reloc   */
/* code is twofold:                                                     */
/* 1. it requires EP to be given a value on 64K boundary.               */
/* 2. it loses error checking if 'sym' is not within +/- 0x8000 of EP.  */
/* Set #define NEC850_UNIQUE_RELOC_CODES 1 for a test vsn.              */
#define R_NEC850_LO16b         50       /* beware! */
#define R_NEC850_LO16h         51       /* beware! */
#define R_NEC850_LO16w         52       /* beware! */
#else
#define R_NEC850_LO16b          4       /* R_V810_WLO        */
#define R_NEC850_LO16h         23       /* R_V850_HWLO       */
#define R_NEC850_LO16bu        38       /* R_V850_BLO       (850e only) */
#define R_NEC850_LO16hu        R_NEC850_LO16h      /*       (850e only) */
#define R_NEC850_LO16w         24       /* R_V850_WLO        */
#endif
#define R_NEC850_GPDISP16b     16       /* R_V810_GPWLO         */
#define R_NEC850_GPDISP16h     33       /* R_V850_GPHWLO        */
#define R_NEC850_GPDISP16bu    44       /* R_V850_GPBLO     (850e only) */
#define R_NEC850_GPDISP16hu    R_NEC850_GPDISP16h  /*       (850e only) */
#define R_NEC850_GPDISP16w     34       /* R_V850_GPWLO         */

#define R_NEC850_ZPDISP16b      4       /* R_V810_WLO         */
#define R_NEC850_ZPDISP16h     23       /* R_V850_HWLO          */
#define R_NEC850_ZPDISP16bu    38       /* R_V850_BLO       (850e only) */
#define R_NEC850_ZPDISP16hu    R_NEC850_ZPDISP16h  /*       (850e only) */
#define R_NEC850_ZPDISP16w     24       /* R_V850_WLO           */

#define R_NEC850_EPDISP16b     10       /* R_V810_REGWLO        */
#define R_NEC850_EPDISP16h     28       /* R_V850_REGHWLO       */
#define R_NEC850_EPDISP16bu    41       /* R_V850_REGBLO    (850e only) */
#define R_NEC850_EPDISP16hu    R_NEC850_EPDISP16h  /*       (850e only) */
#define R_NEC850_EPDISP16w     29       /* R_V850_REGWLO        */

#define R_NEC850_EP7b          25       /* R_V850_REG7BIT       */
#define R_NEC850_EP7h          26       /* R_V850_REGHBYTE      */
#define R_NEC850_EP7w          27       /* R_V850_REGWBYTE      */
#define R_NEC850_EP4bu         39       /* R_V850_REG4BIT   (850e only) */
#define R_NEC850_EP4hu         40       /* R_V850_REG5BIT   (850e only) */
#define R_NEC850_DISP22        35       /* R_V850_PC22          */
/* currently unused, but could be when using disp32[GP]...      */
/* (see below).                                                 */
#define R_NEC850_GPHI16Sxx     99       /* R_V810_GPWHI1        */
#define R_NEC850_GPLO16xx      98       /* R_V810_GPWLO         */

/* There is need for technical discussion about the exact relocation    */
/* types used.                                                          */
/*   GPDISP16 = 16-bit signed offset from GP (fault if out-of-range)    */
/*   GPLO16 =   low half of 32-bit offset from GP                       */
/*   GPHI16S =  top half of 32-bit offset from GP (corrected for sign)  */
/* GPLO16 and GPDISP16 are almost same if the offset is -8000..7fff.    */

/* end of elf.h */
