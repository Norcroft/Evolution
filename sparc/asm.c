/* sparc/asm.c: Copyright (C) Codemist Ltd., 1991.                       */

/* version 1 */
/* Assembler output is routed to asmstream, annotated if FEATURE_ANNOTATE.  */
/* See aoutobj.c for more details on datastructures.                        */

/* exports: asmstream,
            display_assembly_code, asm_header, asm_trailer */

#include <stdarg.h>
#include <errno.h>

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <ctype.h>

#include "globals.h"
#include "mcdep.h"
#include "xrefs.h"
#include "store.h"
#include "codebuf.h"
#include "ops.h"
#include "mcdpriv.h"
#include "builtin.h"
#include "version.h"
#include "errors.h"

FILE *asmstream;

#ifndef NO_ASSEMBLER_OUTPUT

#define annotations (feature & FEATURE_ANNOTATE)

#define toevensex(x,y) (x)    /* for cross compilation one day */

static int32 fncount;         /* maybe should be more global */
static bool asm_error;

static void maybe_export(Symstr *);
static void maybe_import(Symstr *, bool);

static void asm_blank(int32 n)
{   while (n-- > 0) fprintf(asmstream, "!\n");
}

static int32 asm_padcol8(int32 n)
{   if (!annotations) n = 7;      /* compact the asm file */
    while (n<8) fputc(annotations ? ' ':'\t', asmstream), n++;
    return n;
}

static void pr_chars(int32 w)   /* works on both sex machines */
{
  int i, c;
  fputc('\'', asmstream);
  for (i=0; i<sizeof(int32); i++)
  { switch(c = ((unsigned char *)&w)[i])
    {
case '\\':
case '\'':
case '\"':
        break;
case '\a':
        c = 'a';
        break;
case '\b':
        c = 'b';
        break;
case CHAR_FF:
        c = 'f';
        break;
case '\n':
        c = 'n';
        break;
case CHAR_CR:
        c = 'r';
        break;
case '\t':
        c = 't';
        break;
case CHAR_VT:
        c = 'v';
        break;
default:
        if (c < ' ' || c >= 127) fprintf(asmstream, "\\%o", (int)c);
        else fputc(c, asmstream);
        continue;
    }
    fputc('\\', asmstream);
    fputc(c, asmstream);
  }
  fputc('\'', asmstream);
}

static int32 pr_asmname(Symstr *sym, FILE *stream)
{   char *s = sym == 0 ? (asm_error = 1, "?") : symname_(sym);
#ifdef TARGET_IS_DRS6000
    return fprintf(stream, "%s", s);
#else
    return fprintf(stream, "_%s", s);
#endif
}

static unsigned32 xroffset = 0;

/* decode_external checks if (and by what) a location is to be relocated.  */
static Symstr *decode_external(int32 p)
/* p is now a byte address */
{
  CodeXref *x;
/*  fprintf(stderr,"External at %lx\n", p); */
  for (x = codexrefs; x!=NULL; x = x->codexrcdr)    /* SLOW loop !!! */
    if (p == (x->codexroff & 0x00ffffff)) {
      switch (x->codexroff & 0xff000000) {
      default:
	xroffset = 0;
	break;
      case X_DataAddr:
      case X_DataAddr1:
      case X_absreloc:
	xroffset = x->codexrlitoff;
      }
/*      fprintf(stderr,"found %s + %lx\n",symname_(x->codexrsym),xroffset); */
      return x->codexrsym;
    }
/*    syserr(syserr_decode_external);     /* may exploit zero return one day */
  return 0;			/* not an external reference */
}

static int32 printlabelname(int32 offset, int32 loc)
{
/* For data references, ignore the function name: it will be wrong for backward
 * references to the literal pool of another function.  (Labels which are the
 * subject of data references have already been removed from asm_lablist).
 * For code references, generate a label of the form L<label>F<proc>.
 * Such label names seem a bit long and ugly.
 */
  LabList *p;
  for ( p = asm_lablist ; p != NULL ; p = p->labcdr) {
    LabelNumber *lab = p->labcar;
    if ((lab->u.defn & 0x00ffffff) == offset)
#ifdef TARGET_IS_DRS6000
      return fprintf(asmstream, ".L%ldF%ld",
		     (long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
#else
      return fprintf(asmstream, "L%ldF%ld",
		     (long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
#endif
  }
#ifdef TARGET_IS_DRS6000
  return fprintf(asmstream, ".L%06lx???  /* %lx -> %lx */",
		 offset+codebase, loc, offset);
#else
  return fprintf(asmstream, "L%06lx???  /* %lx -> %lx */",
		 offset+codebase, loc, offset);
#endif
}

static void decode_addr(int32 addr, int32 off)
/* Decodes the address addr (referenced at code offset off in current function)
 * Works for external addresses
 */
{
  /* First, is it an external name ? */

  Symstr *name = decode_external(codebase+off);

  if (name == bindsym_(codesegment)) {
    /* although we have found it in codexrefs, we may still be able to
       produce a local label name for it                                */
    LabList *p;

    for ( p = asm_lablist ; p != NULL ; p = p->labcdr) {
      LabelNumber *lab = p->labcar;
      if (codebase+(lab->u.defn & 0x00ffffff)+addr == xroffset) {
#ifdef TARGET_IS_DRS6000
	fprintf(asmstream, ".L%ldF%ld",
		(long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
#else
	fprintf(asmstream, "L%ldF%ld",
		(long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
#endif
	if (addr != 0) fprintf(asmstream, "+0x%lx", addr);
	return;
      }
    }
  }

  if (name != 0) {
    pr_asmname(name,asmstream);
    if (xroffset != 0) fprintf(asmstream, "+0x%lx", xroffset);
    return;
  }

  /* Otherwise, assume it is a local label */
  printlabelname (addr, off);	/* /* not right ??? */
}

static void decode_branch_address(int32 w, int32 p, Symstr *name, int32 type)
{
  int32 n;
  switch (type) {
  default: syserr("decode_branch_address %.8lx", type);
  case LABREF_DISP22:
    n = p + ((w << 10) >> 8);
    printlabelname(n, p);
  }
}


/* Disassembler routines                                                 */

static char *regstr = "%g0\0%g1\0%g2\0%g3\0%g4\0%g5\0%g6\0%g7\0"
                      "%o0\0%o1\0%o2\0%o3\0%o4\0%o5\0%sp\0%o7\0"
                      "%l0\0%l1\0%l2\0%l3\0%l4\0%l5\0%l6\0%l7\0"
                      "%i0\0%i1\0%i2\0%i3\0%i4\0%i5\0%fp\0%i7\0"
                      "%f0\0%f1\0%f2\0%f3\0%f4\0%f5\0%f6\0%f7\0"
                      "%f8\0%f9\0%f10%f11%f12%f13%f14%f15"
                      "%f16%f17%f18%f19%f20%f21%f22%f23"
                      "%f24%f25%f26%f27%f28%f29%f30%f31";

#define _regname(r) ((((r)&0x3f)*4)+regstr)

static void decode_reg_or_imm(int32 w, int32 p, int32 f,
			      Symstr *name, char *s)
{
  if ((w&A_IMM)==0)
    fprintf(asmstream,"%s\t%.4s,%.4s,%.4s",s,_regname(D_RS1(w)),
	    _regname(D_RS2(w)),_regname(D_RD(w,0)));
  else if (f==LIT_OPCODE) {
    int32 d = D_SIMM13(w);
    if (d<0) fprintf(asmstream,"%s\t%.4s,-0x%lx,%.4s",
		     s,_regname(D_RS1(w)),-d,_regname(D_RD(w,0)));
    else fprintf(asmstream,"%s\t%.4s,0x%lx,%.4s",
		 s,_regname(D_RS1(w)),d,_regname(D_RD(w,0)));
  }
  else /* (f==LIT_RELADDR) */ {
    fprintf(asmstream,"%s\t%.4s,%%lo(",s,_regname(D_RS1(w)));
    decode_addr(w&0x1fff,p);
    fprintf(asmstream,"),%.4s",_regname(D_RD(w,0)));
  }
}

static void decode_full_address(int32 w, int32 p, int32 f,
				Symstr *name, char *s, int32 RD_is_flt)
{
  char *lb, *rb;
  if (strcmp(s,"jmpl")!=0) { lb="["; rb="]"; }
  else { lb=""; rb=""; }
  if ((w&A_IMM)==0) {
    if (D_RS2(w)==0) fprintf(asmstream,"%s\t%s%.4s%s,%.4s",
			     s,lb,_regname(D_RS1(w)),rb,
			     _regname(D_RD(w,RD_is_flt)));
    else fprintf(asmstream,"%s\t%s%.4s+%.4s%s,%.4s",s,
		 lb,_regname(D_RS1(w)),_regname(D_RS2(w)),rb,
		 _regname(D_RD(w,RD_is_flt)));
  }
  else if (f==LIT_OPCODE) {
    int32 d = D_SIMM13(w);
    if (d==0) fprintf(asmstream,"%s\t%s%.4s%s,%.4s",
		      s,lb,_regname(D_RS1(w)),rb,
		      _regname(D_RD(w,RD_is_flt)));
    else if (d<0) fprintf(asmstream,"%s\t%s%.4s-0x%lx%s,%.4s",
			  s,lb,_regname(D_RS1(w)),-d,rb,
			  _regname(D_RD(w,RD_is_flt)));
    else fprintf(asmstream,"%s\t%s%.4s+0x%lx%s,%.4s",
		 s,lb,_regname(D_RS1(w)),d,rb,
		 _regname(D_RD(w,RD_is_flt)));
  }
  else /* (f==LIT_RELADDR) */ {
    fprintf(asmstream,"%s\t%s%.4s+%%lo(",s,lb,_regname(D_RS1(w)));
    decode_addr(w&0x1fff,p);
    fprintf(asmstream,")%s,%.4s",rb,_regname(D_RD(w,RD_is_flt)));
  }
}

static void decode_full_address_r(int32 w, int32 p, int32 f,
				  Symstr *name, char *s, int32 RD_is_flt)
{
  if ((w&A_IMM)==0) {
    if (D_RS2(w)==0) fprintf(asmstream,"%s\t%.4s,[%.4s]",
			     s,_regname(D_RD(w,RD_is_flt)),_regname(D_RS1(w)));
    else fprintf(asmstream,"%s\t%.4s,[%.4s+%.4s]",s,
		 _regname(D_RD(w,RD_is_flt)),
		 _regname(D_RS1(w)),_regname(D_RS2(w)));
  }
  else if (f==LIT_OPCODE) {
    int32 d = D_SIMM13(w);
    if (d==0) fprintf(asmstream,"%s\t%.4s,[%.4s]",
		      s,_regname(D_RD(w,RD_is_flt)),_regname(D_RS1(w)));
    else if (d<0) fprintf(asmstream,"%s\t%.4s,[%.4s-0x%lx]",
			  s,_regname(D_RD(w,RD_is_flt)),_regname(D_RS1(w)),-d);
    else fprintf(asmstream,"%s\t%.4s,[%.4s+0x%lx]",
		 s,_regname(D_RD(w,RD_is_flt)),_regname(D_RS1(w)),d);
  }
  else /* (f==LIT_RELADDR) */ {
    fprintf(asmstream,"%s\t%.4s,[%.4s+%%lo(",
	    s, _regname(D_RD(w,RD_is_flt)), _regname(D_RS1(w)));
    decode_addr(w&0x1fff, p);
    fprintf(asmstream,")]");
  }
}


static void decode_branch(int32 branch)
{
  char *inst;
  switch (branch&0x1e000000) {
  case C_REQ:
    inst = "be"; break;
  case C_RLE:
    inst = "ble"; break;
  case C_RLT:
    inst = "bl"; break;
  case C_RLEU:
    inst = "bleu"; break;
  case C_RLTU:
    inst = "blu"; break;
  case C_N:
    inst = "bneg"; break;
  case C_V:
    inst = "bvs"; break;
  case C_AL:
    inst = "ba"; break;
  case C_RNE:
    inst = "bne"; break;
  case C_RGT:
    inst = "bg"; break;
  case C_RGE:
    inst = "bge"; break;
  case C_RGTU:
    inst = "bgu"; break;
  case C_RGEU:
    inst = "bgeu"; break;
  case C_P:
    inst = "bpos"; break;
  case C_NV:
    inst = "bvc"; break;
  }
  fprintf(asmstream,"%s%s\t",inst,(branch&A_ANNUL ? ",a" : "") );
}

static void decode_fbranch(int32 branch)
{
  char *inst;
  switch (branch&0x1e000000) {
  case C_FREQ:
    inst = "fbe"; break;
  case C_FRLE:
    inst = "fble"; break;
  case C_FRLT:
    inst = "fbl"; break;
  case C_FAL:
    inst = "fba"; break;
  case C_FRNE:
    inst = "fbne"; break;
  case C_FRGT:
    inst = "fbg"; break;
  case C_FRGE:
    inst = "fbge"; break;
  }
  fprintf(asmstream,"%s%s\t",inst,(branch&A_ANNUL ? ",a" : "") );
}

void decode_trap(int32 w)
{
  char *inst;
  switch (w&0x1e000000) {
  case C_REQ:
    inst = "te"; break;
  case C_RLE:
    inst = "tle"; break;
  case C_RLT:
    inst = "tl"; break;
  case C_RLEU:
    inst = "tleu"; break;
  case C_RLTU:
    inst = "tlu"; break;
  case C_N:
    inst = "tneg"; break;
  case C_V:
    inst = "tvs"; break;
  case C_AL:
    inst = "ta"; break;
  case C_RNE:
    inst = "tne"; break;
  case C_RGT:
    inst = "tg"; break;
  case C_RGE:
    inst = "tge"; break;
  case C_RGTU:
    inst = "tgu"; break;
  case C_RGEU:
    inst = "tgeu"; break;
  case C_P:
    inst = "tpos"; break;
  case C_NV:
    inst = "tvc"; break;
  }

  if ((w&A_IMM)==0) {
    if (D_RS2(w)==0) fprintf(asmstream,"%s\t%.4s", inst,_regname(D_RS1(w)));
    else fprintf(asmstream,"%s\t%.4s+%.4s",
		 inst,_regname(D_RS1(w)),_regname(D_RS2(w)));
  }
  else {
    int32 d = D_SIMM13(w);
    if (d==0) fprintf(asmstream,"%s\t%.4s", inst,_regname(D_RS1(w)));
    else if (d<0) fprintf(asmstream,"%s\t%.4s-0x%lx",
			  inst,_regname(D_RS1(w)),-d);
    else fprintf(asmstream,"%s\t%.4s+0x%lx",
		 inst,_regname(D_RS1(w)),d);
  }
}

static void decode_0(int32 w, int32 p, int32 f, Symstr *name)
{
  switch (w & 0xc1c00000) {
  case OP_UNIMP:
    fprintf(asmstream,"unimp\t0x%lx",w&0x3fffff);
    return;
  case OP_B:
    decode_branch(w);
    decode_branch_address(w, p, name, LABREF_DISP22);
    return;
  case OP_SETHI:
    if (f==LIT_OPCODE) {
      if (w == OP_NOOP) fprintf(asmstream,"nop");
      else fprintf(asmstream,"sethi\t0x%lx,%.4s",
		   w&0x3fffff, _regname(D_RD(w,0)));
    }
    else /* (f==LIT_RELADDR) */ {
      fprintf(asmstream,"sethi\t%%hi(");
      decode_addr(w&0x3fffff, p);
      fprintf(asmstream,"),%.4s", _regname(D_RD(w,0)));
    }
    return;
  case OP_BF:
#ifdef TARGET_IS_DRS6000
    fprintf(asmstream,".word\t0x%lx\t!! ", w);
#endif
    decode_fbranch(w);
    decode_branch_address(w, p, name, LABREF_DISP22);
    return;
  default:
    fprintf(asmstream,".word\t0x%lx\n", w);
  }
}

static void decode_1(int32 w, int32 p, int32 f, Symstr *name)
{
  fprintf(asmstream,"call\t");
  pr_asmname((Symstr *)code_aux_(p),asmstream);
}

#define decode_full_i_address(w,p,f,name,s) \
  decode_full_address ((w), (p), (f), (name), (s), 0)
#define decode_full_f_address(w,p,f,name,s) \
  decode_full_address ((w), (p), (f), (name), (s), 1)
#define decode_full_i_address_r(w,p,f,name,s) \
  decode_full_address_r ((w), (p), (f), (name), (s), 0)
#define decode_full_f_address_r(w,p,f,name,s) \
  decode_full_address_r ((w), (p), (f), (name), (s), 1)
#define decode_fpop_12(w, s) \
  fprintf(asmstream,"%s\t%.4s,%.4s", (s), \
	  _regname(D_RS1(w)+R_F0), _regname(D_RS2(w)+R_F0))
#define decode_fpop_12d(w, s) \
  fprintf(asmstream,"%s\t%.4s,%.4s,%.4s", \
	  (s), _regname(D_RS1(w)+R_F0), \
	  _regname(D_RS2(w)+R_F0), _regname(D_RD(w,1)))
#define decode_fpop_2d(w, s) \
  fprintf(asmstream,"%s\t%.4s,%.4s", (s), \
	  _regname(D_RS2(w)+R_F0), _regname(D_RD(w,1)))

static void decode_2(int32 w, int32 p, int32 f, Symstr *name)
{
  switch (w&0xc1f80000) {	/* ignoring A_IMM */
  case OP_ADD:
    decode_reg_or_imm(w, p, f, name, "add"); return;
  case OP_ADDCC:
    decode_reg_or_imm(w, p, f, name, "addcc"); return;
  case OP_ADDX:
    decode_reg_or_imm(w, p, f, name, "addx"); return;
  case OP_ADDXCC:
    decode_reg_or_imm(w, p, f, name, "addxcc"); return;
  case OP_SUB:
    decode_reg_or_imm(w, p, f, name, "sub"); return;
  case OP_SUBCC:
    decode_reg_or_imm(w, p, f, name, "subcc"); return;
  case OP_SUBX:
    decode_reg_or_imm(w, p, f, name, "subx"); return;
  case OP_SUBXCC:
    decode_reg_or_imm(w, p, f, name, "subxcc"); return;
  case OP_MULSCC:
    decode_reg_or_imm(w, p, f, name, "mulscc"); return;
  case OP_AND:
    decode_reg_or_imm(w, p, f, name, "and"); return;
  case OP_ANDCC:
    decode_reg_or_imm(w, p, f, name, "andcc"); return;
  case OP_ANDN:
    decode_reg_or_imm(w, p, f, name, "andn"); return;
  case OP_ANDNCC:
    decode_reg_or_imm(w, p, f, name, "andncc"); return;
  case OP_OR:
    decode_reg_or_imm(w, p, f, name, "or"); return;
  case OP_ORCC:
    decode_reg_or_imm(w, p, f, name, "orcc"); return;
  case OP_ORN:
    decode_reg_or_imm(w, p, f, name, "orn"); return;
  case OP_ORNCC:
    decode_reg_or_imm(w, p, f, name, "orncc"); return;
  case OP_XOR:
    decode_reg_or_imm(w, p, f, name, "xor"); return;
  case OP_XORCC:
    decode_reg_or_imm(w, p, f, name, "xorcc"); return;
  case OP_XNOR:
    decode_reg_or_imm(w, p, f, name, "xnor"); return;
  case OP_XNORCC:
    decode_reg_or_imm(w, p, f, name, "xnorcc"); return;
  case OP_SLL:
    decode_reg_or_imm(w, p, f, name, "sll"); return;
  case OP_SRL:
    decode_reg_or_imm(w, p, f, name, "srl"); return;
  case OP_SRA:
    decode_reg_or_imm(w, p, f, name, "sra"); return;
  case OP_SAVE:
    decode_reg_or_imm(w, p, f, name, "save"); return;
  case OP_RESTORE:
    if (w==OP_RESTORE) fprintf(asmstream,"restore");
    else decode_reg_or_imm(w, p, f, name, "restore");
    return;
  case OP_JMPL:
    if (w==OP_RET) fprintf(asmstream,"ret");
    else if (w==OP_RETL) fprintf(asmstream,"retl");
    else decode_full_i_address(w, p, f, name, "jmpl");
    return;
  case OP_TICC:
    decode_trap(w); return;
  }
  switch (w&0xc1f83fe0) {
  case OP_FITOS:
    decode_fpop_2d(w, "fitos"); return;
  case OP_FITOD:
    decode_fpop_2d(w, "fitod"); return;
  case OP_FSTOI:
    decode_fpop_2d(w, "fstoi"); return;
  case OP_FDTOI:
    decode_fpop_2d(w, "fdtoi"); return;
  case OP_FSTOD:
    decode_fpop_2d(w, "fstod"); return;
  case OP_FDTOS:
    decode_fpop_2d(w, "fdtos"); return;
  case OP_FMOVS:
    decode_fpop_2d(w, "fmovs"); return;
  case OP_FNEGS:
    decode_fpop_2d(w, "fnegs"); return;
  case OP_FABSS:
    decode_fpop_2d(w, "fabss"); return;
  case OP_FADDS:
    decode_fpop_12d(w, "fadds"); return;
  case OP_FADDD:
    decode_fpop_12d(w, "faddd"); return;
  case OP_FSUBS:
    decode_fpop_12d(w, "fsubs"); return;
  case OP_FSUBD:
    decode_fpop_12d(w, "fsubd"); return;
  case OP_FMULS:
    decode_fpop_12d(w, "fmuls"); return;
  case OP_FMULD:
    decode_fpop_12d(w, "fmuld"); return;
  case OP_FDIVS:
#ifndef TARGET_IS_DRS6000
    fprintf(asmstream,".word\t0x%lx\t!! ",w);
#endif
    decode_fpop_12d(w, "fdivs"); return;
  case OP_FDIVD:
#ifndef TARGET_IS_DRS6000
    fprintf(asmstream,".word\t0x%lx\t!! ",w);
#endif
    decode_fpop_12d(w, "fdivd"); return;
  case OP_FCMPS:
    decode_fpop_12(w, "fcmps"); return;
  case OP_FCMPD:
    decode_fpop_12(w, "fcmpd"); return;
  default:
    fprintf(asmstream,".word\t0x%lx\n", w); return;
  }
}

static void decode_3(int32 w, int32 p, int32 f, Symstr *name)
{
  switch (w&0xc1f80000) {	/* ignoring A_IMM */
  case OP_LDSB:
    decode_full_i_address(w, p, f, name, "ldsb"); return;
  case OP_LDSH:
    decode_full_i_address(w, p, f, name, "ldsh"); return;
  case OP_LDUB:
    decode_full_i_address(w, p, f, name, "ldub"); return;
  case OP_LDUH:
    decode_full_i_address(w, p, f, name, "lduh"); return;
  case OP_LD:
    decode_full_i_address(w, p, f, name, "ld"); return;
  case OP_LDD:
    decode_full_i_address(w, p, f, name, "ldd"); return;
  case OP_LDF:
    decode_full_f_address(w, p, f, name, "ld"); return;
  case OP_LDDF:
    decode_full_f_address(w, p, f, name, "ldd"); return;
  case OP_STB:
    decode_full_i_address_r(w, p, f, name, "stb"); return;
  case OP_STH:
    decode_full_i_address_r(w, p, f, name, "sth"); return;
  case OP_ST:
    decode_full_i_address_r(w, p, f, name, "st"); return;
  case OP_STD:
    decode_full_i_address_r(w, p, f, name, "std"); return;
  case OP_STF:
    decode_full_f_address_r(w, p, f, name, "st"); return;
  case OP_STDF:
    decode_full_f_address_r(w, p, f, name, "std"); return;
  }
  fprintf(asmstream,".word\t0x%lx\n", w);
}

#undef decode_fpop_12
#undef decode_fpop_2d
#undef decode_fpop_12d
#undef decode_full_i_address
#undef decode_full_f_address
#undef decode_full_i_address_r
#undef decode_full_f_address_r

static void decode_instruction(int32 w, int32 p, int32 f, Symstr *name)
{
  /* return number of chars displayed on the line */
  switch ((unsigned)w >> 30)
    {
    case 0: decode_0(w, p, f, name); return;	/* format 2 instructions !! */
    case 1: decode_1(w, p, f, name); return;	/* format 1 instructions    */
    case 2: decode_2(w, p, f, name); return;	/* format 3 instructions !! */
    default:
    case 3: decode_3(w, p, f, name); return;	/* format 3 instructions    */
    }
}

static void decode_DC(int32 w)
{
  fprintf(asmstream, ".word\t%#.8lx", (long)w);
}

static void decode_DCAx(int32 w, int32 p)
{
/*  fprintf(stderr,"DCAx w=%lx p=%lx\n", w, p); */
  fprintf(asmstream, ".word\t");
  decode_addr(w, p);
}

static void decode_DCA(Symstr *s, int32 w)
{
  fprintf(asmstream, ".word\t");
/*     decode_addr(w, 0); */
  pr_asmname(s,asmstream);
  if (w!=0) fprintf(asmstream, "%+ld", (long)w);
}

static Symstr *find_extsym(int32 p)
{   CodeXref  *x;
    for (x = codexrefs; x != NULL; x = x->codexrcdr) {
        if (p == (x->codexroff & 0x00ffffffL)) return(x->codexrsym);
    }
    syserr("syserr_find_extsym %lx", p);
    return(NULL);
}

static void pr_common_defs(void)
{
  ExtRef *x;
  bool seen = 0;
  /* AM: I hope the next line does not upset anyone	*/
  obj_symlist = (ExtRef *) dreverse((List *)obj_symlist);
				/* oldest = smallest numbered first */
  for (x = obj_symlist; x!=0; x = x->extcdr) {
    int32 sz = x->extoffset;
    if (sz>0 && !(x->extflags & (xr_defloc|xr_defext))) {
      fprintf(asmstream, "\t.common\t");
      pr_asmname(x->extsym, asmstream);
#ifdef TARGET_IS_DRS6000
      fprintf(asmstream, ", %lu, %d\n", (long)sz, alignof_max);
#else
      fprintf(asmstream, ", %lu\n", (long)sz);
#endif
      seen = 1;
    }
  }
  if (seen) asm_blank(1);
}

/* exported functions ...*/

void display_assembly_code(Symstr *name)
{   int32 q;
    LabList *asm_lablist2 = 0;
    asm_blank(2);
    if (name != 0)   /* may be 0 for string literals from static inits   */
    {   asm_lablist2 = asm_lablist;
        fncount++;
        if (annotations)
            fprintf(asmstream, "%.6lx  %20s", (long)codebase, "");
        maybe_export(name);
        pr_asmname(name,asmstream);
        fprintf(asmstream, ":\n");
    }
    for (q=0; q < codep; q+=4)    /* q is now a BYTE offset */
    {   const int32 w = code_inst_(q),
                    f = code_flag_(q);
        VoidStar aux = code_aux_(q);
        {
          int32 labq;
          LabelNumber *t;
          while (asm_lablist2 &&
                 (t = asm_lablist2->labcar,
                  labq = t->u.defn & 0x00ffffff) <= q)
            { if (annotations)
                fprintf(asmstream, "%18s", "");
#ifdef TARGET_IS_DRS6000
              fprintf(asmstream, ".L%ldF%ld:\n",
                      (long)(lab_name_(t) & 0x7fffffff), (long)fncount);
#else
              fprintf(asmstream, "L%ldF%ld:\n",
                      (long)(lab_name_(t) & 0x7fffffff), (long)fncount);
#endif
              if (labq != q)
                syserr(syserr_asmlab, (long)labq);
              asm_lablist2 = asm_lablist2->labcdr;
            }
        }
        if (annotations)
        {   int32 i;
            fprintf(asmstream, "%.6lx  ", (long)(q + codebase));
            switch (f)
            { case LIT_OPCODE:
                for (i = 0; i < 8; i += 4)
                    if (i < 4)
                        fprintf(asmstream, "%.8lx ", (long)code_inst_(q+i));
                    else
                        fprintf(asmstream, "     ");
                break;
              case LIT_STRING:
                fprintf(asmstream, "      %.8lx      ", (long)totargetsex(w,LIT_BBBB));
                break;
              default:
                fprintf(asmstream, "      %.8lx      ", (long)w);
                break;
            }
        }
        fputc('\t', asmstream);
        switch (f) {
	  case LIT_RELADDR:
	    maybe_import((Symstr *)aux, NO);
/*	    break; */
          case LIT_OPCODE:
	    /* all instructions are four bytes on sparc */
	    /* so a lot of grotty code has been removed */
	    /* concerned with making branch table       */
	    /* entries always four bytes                */
            decode_instruction(w, q, f, name);
            break;
	  case LIT_STRING:
            decode_DC(totargetsex(w, LIT_BBBB));
            if (annotations) fprintf(asmstream, "         ! "), pr_chars(w);
            break;
	  case LIT_NUMBER:
            decode_DC(w);
            break;
	  case LIT_ADCON:
            maybe_import(find_extsym(codebase+q), YES);
            decode_DCAx(w, q);	/* /* ??? */
            break;
	  case LIT_FPNUM:
            decode_DC(w);
            if (annotations)
                fprintf(asmstream, " ! E'%s'", (char *)aux);
            break;
	  case LIT_FPNUM1:
            decode_DC(w);
            if (annotations)
                fprintf(asmstream, " ! D'%s'",(char *)aux);
            break;
	  case LIT_FPNUM2:    /* all printed by the FPNUM1 */
            decode_DC(w);
            break;
	  default:
            syserr(syserr_display_asm, (long)f);
            fprintf(asmstream, "?");
        }
        fprintf(asmstream, "\n");
    }
    if (asm_lablist2) syserr(syserr_asmlab, 0L);
    /* idly ensure code segment stays aligned if no terminating literal pool */
    asm_lablist = 0;    /* stop confusion when called from vargen.c  */
}

typedef struct Import {
    struct Import *next;
    Symstr  *sym;
    int32   patch;
} Import;

static Import *asm_imported;

void asm_header()
{
    asm_imported = NULL;
    asm_error = 0;
    fncount = 0;
    if (annotations) return;   /* do not bore interactive user */
    fprintf(asmstream, "! generated by %s\n", CC_BANNER);
    asm_blank(1);
#ifdef TARGET_IS_DRS6000
    fprintf(asmstream, "\t.section \".text\"\n\t.align 8\n");
#else
    fprintf(asmstream, "\t.text\n\t.align 8\n");
#endif
}

/* (not exported) */

static void asm_outextern() {}
/*
 *{   ExtRef *x;
 *    for (x = obj_symlist; x != 0; x = x->extcdr)
 *    {   int32 flags = x->extflags;
 *        if (!(flags & xr_defloc) && (flags & xr_defext))
 *        {   fprintf(asmstream, "\t.global \t");
 *            pr_asmname(x->extsym,asmstream);
 *            fprintf(asmstream, "\n");
 *        }
 *    }
 *    asm_blank(1);
 *    for (x = obj_symlist; x != 0; x = x->extcdr)
 *    {   int32 flags = x->extflags;
 *        if (!(flags & xr_defloc) && !(flags & xr_defext))
 *        {   fprintf(asmstream, "!\t.extern\t");
 *            pr_asmname(x->extsym,asmstream);
 *            fprintf(asmstream, "\n");
 *        }
 *    }
 *}
 */

typedef struct ExtRefList {
        struct ExtRefList *cdr;
        ExtRef *car;
} ExtRefList;


void asm_trailer()
{ DataInit *p;
  int32 i = 0;
  asm_blank(1);
#ifdef TARGET_IS_DRS6000
  if (dataloc > 0)
    fprintf(asmstream, "\t.section \".data\"\n\t.align 8\n");
#else
  fprintf(asmstream, "\t.data\n\t.align 8\n");
#endif
  asm_blank(1);
  pr_common_defs();		/* Any Common stuff output now */
#ifdef TARGET_IS_DRS6000
  if (dataloc > 0)
#endif
  for (p = datainitp; p != 0; p = p->datacdr)
  { int32 rpt = p->rpt, sort = p->sort, len = p->len, val = p->val;
    if (sort != LIT_LABEL) asm_padcol8(0);
    switch (sort)
    {   case LIT_LABEL:
            maybe_export((Symstr *)rpt);
            pr_asmname((Symstr *)rpt,asmstream);
            fprintf(asmstream, ":\t! 0x%lx", i);
            break;
        default:  syserr(syserr_asm_trailer, (long)sort);
        case LIT_BBBB:
        case LIT_HH:
        case LIT_BBH:
        case LIT_HBB:
            val = totargetsex(val, (int)sort);
        case LIT_NUMBER:
            if (len != 4) syserr(syserr_datalen, (long)len);
            if (rpt == 1)
                decode_DC(val);
            else if (val == 0)
            {   while (rpt-- != 0)      /* yukky -- what is the unixism? */
                {   decode_DC(0);
                    if (rpt != 0) fprintf(asmstream, "\n"), asm_padcol8(0);
                }
            }
            else syserr(syserr_asm_trailer1, (long)rpt, (long)val);
	    i += len*(p->rpt);
            break;
        case LIT_FPNUM:
        {   int32 *p = ((FloatCon *)val) -> floatbin.irep;
            decode_DC(p[0]);
            if (annotations)
                fprintf(asmstream, " ! %s", ((FloatCon *)val) -> floatstr);
            if (len == 8) fprintf(asmstream, "\n"),
                          asm_padcol8(0), decode_DC(p[1]);
	    i += len;
            break;
        }
        case LIT_ADCON:              /* (possibly external) name + offset */
	    maybe_import((Symstr *)len, YES);
            if (rpt != 1) syserr(syserr_asm_trailer2);
            decode_DCA((Symstr *)len, val);
	    i += 4;
            break;
    }
    fprintf(asmstream, "\n");
  }
  asm_blank(1);

  if (bss_size != 0) {
    int32 n = 0;
    ExtRef *x;
    ExtRefList *zisyms = NULL;
#ifdef TARGET_IS_DRS6000
    fprintf(asmstream, "\t.section \".bss\"\n\t.align 8\n");
#else
    fprintf(asmstream, "!!! Start of BSS\n");
#endif
    for (x = obj_symlist; x != NULL; x = x->extcdr)
      if (x->extflags & xr_bss) {
	ExtRefList **prev = &zisyms;
	ExtRefList *p;
	for (; (p = *prev) != 0; prev = &cdr_(p))
	  if (x->extoffset < car_(p)->extoffset) break;
	*prev = (ExtRefList *)syn_cons2(*prev, x);
      }
    for (; zisyms != NULL; zisyms = cdr_(zisyms)) {
      x = car_(zisyms);
      if (x->extoffset != n)
#ifdef TARGET_IS_DRS6000
	fprintf(asmstream, ",%ld,%d\n", x->extoffset-n, alignof_max);
#else
        fprintf(asmstream, ",%ld,%cbss%c\n", x->extoffset-n, '"', '"');
#endif
      n = x->extoffset;
#ifdef TARGET_IS_DRS6000
      if (x->extflags & xr_defloc) {
	fprintf(asmstream,"\t.local\t");
	pr_asmname(x->extsym,asmstream);
	fputc('\n', asmstream);
      } else
#endif
      maybe_export(x->extsym);
      fprintf(asmstream,"\t.common\t");
      pr_asmname(x->extsym,asmstream);
    }
    if (n != bss_size)
#ifdef TARGET_IS_DRS6000
      fprintf(asmstream, ",%ld,%d\n", bss_size-n, alignof_max);
#else
      fprintf(asmstream, ",%ld,%cbss%c\n", bss_size-n, '"', '"');
#endif
  }
  asm_blank(1);
  asm_outextern();
  asm_blank(1);
  fprintf(asmstream, "! END\n");
  if (asm_error) syserr(syserr_asm_confused);
}

#endif

static void maybe_import(Symstr *sym, bool atcol8)
{   ExtRef  *x;
    Import  *p;

/* Unless external there is nothing to do here. */
    if ((x = symext_(sym)) != 0 &&
        (x->extflags & xr_defloc) != 0) return;

/* Else put out an IMPORT the first time and possibly later patch.  Yuk.   */
    for (p = asm_imported; p != NULL; p = p->next) if (p->sym == sym) return;

/* /* @@@HCM what is this loop about - isn't symext_(sym) guaranteed to be what
         this is looking for ? */

    for (x = obj_symlist; x != NULL; x = x->extcdr) {
        if (x->extsym == sym) {
            if ((x->extflags & (xr_defloc|xr_defext)) == 0 ||
                (x->extflags & xr_cblock)) {
                /*
                 * Horribly, we remember where we are in the output stream
                 * so that we can later change "IM" to "EX" if we discover
                 * that the thing is declared locally.
                 */
                asm_imported = p = (Import *)global_list3(SU_Other,
                                                          asm_imported, sym,0);
                p->patch = ftell(asmstream);
                if (!(x->extflags & xr_code) && x->extoffset != 0)
                  /*
                   * Common reference
                   */
                    fprintf(asmstream, "\t.global \t");
                else
                    fprintf(asmstream, "\t!.extern\t");
                pr_asmname(x->extsym, asmstream);
                fputc('\n', asmstream);
                if (annotations) fprintf(asmstream, "%22s", "");
                fputc('\t', asmstream);
            }
            break;
        }
    }
}

static void maybe_export(Symstr *sym)
{   char   *s = symname_(sym);
    char   c;
    FILE   *as = asmstream;
    Import *p;
    ExtRef *x;

/*@@@ AM does not see how the following can ever now happen as x$dataseg etc. */
/* are very local statics.  Is this if error recovery inserted gensyms?    */
    while ((c = *s++) != 0) { /* look for odd characters in x$dataseg etc */
        if (!(isalnum(c) || (c == '_'))) return;
    }

    for (p = asm_imported; p != NULL; p = p->next)
        if ((p->sym == sym) && (p->patch != 0)) { /* have IMported it! */
            int32 curpos = ftell(as);
/* Pray tell, how does this work if output is to terminal? Answer: badly.  */
/*	    fprintf(stderr,"Seeking to %lx (%d)\n", p->patch,
		    ((x = symext_(sym)) != 0 && (x->extflags & xr_defloc))); */
            fseek(as, p->patch, 0);
            if ((x = symext_(sym)) != 0 && (x->extflags & xr_defloc))
                fprintf(as, "\t!.extern");
            else
                fputc('!', as);
            fseek(as, curpos,   0);
            break /*return*/;
        }
    /* If static, don't export */
    if ((x = symext_(sym)) != 0 && (x->extflags & xr_defext) == 0) return;
    fprintf(as, "\t.global \t"); pr_asmname(sym, as); fputc('\n', as);
}


/* end of sparc/asm.c */
