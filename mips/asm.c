
/* mips/asm.c: Copyright (C) Codemist Ltd., 1990-2001.                      */

/* version 3 */
/* Assembler output is routed to asmstream, annotated if FEATURE_ANNOTATE.  */
/* See coffobj.c for more details on datastructures.                        */

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

static int32 fncount;         /* maybe should be more global */
static bool asm_error;
static int32 dotwordcnt;
static int32 jugglecnt;

static List3 *litlabels;
static int32 litlabno, litlabdef;
static void notelitadcon()
{   if (litlabno)
    {   char v[80], *p;
        sprintf(v, "L%ldF%ld", (long)litlabno, (long)fncount);
        p = (char *)GlobAlloc(SU_Other, (int32)strlen(v)+1);
        strcpy(p, v);
        litlabels = (List3 *)global_list3(SU_Other, litlabels, litlabdef, p);
#ifdef never
        if (annotations)
            fprintf(asmstream, " # noting %s @ 0x%.6x", p, litlabdef);
#endif
        litlabno = 0;
    }
}

static int32 asm_padcol8(int32 n)
{
    while (n<8) fputc(' ',asmstream), n++;
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
        else putc(c, asmstream);
        continue;
    }
    putc('\\', asmstream);
    putc(c, asmstream);
  }
  fputc('\'', asmstream);
}

/* Disassembler routines                                                 */

static void decode_regname(int32 r)
{
  if (mips_opt & 16)
  {
    char *name = ((r & 0x1f)*2) +
         "0\0atv0v1a0a1a2a3t0t1t2t3t4t5t6t7s0s1s2s3s4s5s6s7t8t9k0k1gpspfpra";
    fprintf(asmstream, "$%.2s", name);
  }
  else
    fprintf(asmstream, "$%ld", (long)(r&0x1f));
}

static void pr_asmname(Symstr *sym)
{   char *s = sym == 0 ? (asm_error = 1, "?") : symname_(sym);
    fprintf(asmstream, "%s", s);
}

static void printlabelname(int32 offset, int32 loc, bool nooffset)
{
/* For data references, ignore the function name: it will be wrong for backward
 * references to the literal pool of another function.  (Labels which are the
 * subject of data references have already been removed from asm_lablist).
 * For code references, generate a label of the form L<label>F<proc>.
 * Such label names seem a bit long and ugly.
 */
    LabList *p, *q;
    List3 *lp;
    Symstr *nearestsym=NULL; int32 nearestoff = 0x7fffffff, newoff;
    for ( p = asm_lablist, q = 0; p != NULL ; q = p, p = p->labcdr)
    {   LabelNumber *lab = p->labcar;
        if ((lab->u.defn & 0x00ffffff) > offset) break;
    }
    if (q)
    {   LabelNumber *lab = q->labcar;
        fprintf(asmstream, "L%ldF%ld",
                (long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
        /* we need offset != 0 for literal pool entries.            */
        offset -= lab->u.defn & 0x00ffffff;
        if (offset != 0)
        {   fprintf(asmstream, "+%ld", offset);
            if (nooffset) asm_error = 1, fprintf(asmstream, "??? ");
        }
        return;
    }
/* now try previous functions -- for tailcall via OP16_BR           */
    if (mips_opt & 2)
    {   ExtRef *x;
        for (x = obj_symlist; x != 0; x = x->extcdr)
          if ((x->extflags & (xr_defloc+xr_defext)) && (x->extflags & xr_code))
          { newoff = offset+codebase - x->extoffset;
            if (newoff == 0)
            { pr_asmname(x->extsym);
              return;
            }
            if (newoff >= 0 && newoff < nearestoff)
            {
              nearestoff = newoff, nearestsym = x->extsym;
            }
          }
    }
    for (lp = litlabels; lp != NULL; lp = (List3 *)lp->cdr)
    { newoff = offset+codebase - lp->car;
      if (newoff >= 0 && newoff < nearestoff)
      { fprintf(asmstream, "%s", (char *)lp->csr);
        if (newoff != 0) fprintf(asmstream, "+%ld", newoff);
        return;
      }
    }
    if (nearestsym)
    { pr_asmname(nearestsym);
      fprintf(asmstream, "+%ld", nearestoff);
      return;
    }
    asm_error = 1, fprintf(asmstream, "__codeseg+%06lx???  #* %lx -> %lx */",
                                      offset+codebase, loc, offset);
}

static unsigned32 xroffset = 0;  /* extra result for decode_external */

/* decode_external checks if (and by what) a location is to be relocated.  */
static Symstr *decode_external(int32 p)
/* p is now a byte address */
{
    CodeXref *x;
/*    fprintf(stderr,"External at %lx\n", p); */
    for (x = codexrefs; x!=NULL; x = x->codexrcdr)    /* SLOW loop !!! */
        if (p == (x->codexroff & 0x00ffffff))
        {   switch (x->codexroff & 0xff000000)
            {
        default:
                xroffset = 0;
                break;
        case X_DataAddr:
        case X_DataAddr1:
        case X_DataVal:
                xroffset = x->codexrlitoff;
            }
/*     fprintf(stderr,"found %s + %lx\n",symname_(x->codexrsym),xroffset);
 */
            return x->codexrsym;
        }
/*    syserr(syserr_decode_external);    / * may exploit zero return one day */
    return 0;   /* not an external reference */
}

static void decode_addr(int32 op, int32 off)
/* Decodes the address in op.  Works for external addresses.           */
{
  Symstr *name = decode_external(codebase+off);
  if (name == 0) {
    fprintf(asmstream, "0x%lx", (long)op<<2);
  }
  else if (name == bindsym_(codesegment) && xroffset >= codebase)
    {   printlabelname(xroffset-codebase, -1, 0);
    }
  else
    {   pr_asmname(name);
        if (xroffset != 0) fprintf(asmstream, "+0x%lx", xroffset);
    }
}


#define D_RD(x) (((x)>>11)&0x1f)
#define D_RS(x) (((x)>>21)&0x1f)
#define D_RT(x) (((x)>>16)&0x1f)
#define D_SHAMT(x) (((x)>>6)&0x1f)

#define D_FT(x) (((x)>>16)&0x1f)
#define D_FS(x) (((x)>>11)&0x1f)
#define D_FD(x) (((x)>>6)&0x1f)

static void op_dts(char *op, int32 w)
{
  int32 rd = D_RD(w);
  int32 rs = D_RS(w);
  int32 rt = D_RT(w);
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rd);
  fprintf(asmstream, ",");
  decode_regname(rt);
  fprintf(asmstream, ",");
  decode_regname(rs);
}

static void op_dst(char *op, int32 w)
{
  int32 rd = D_RD(w);
  int32 rs = D_RS(w);
  int32 rt = D_RT(w);
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rd);
  fprintf(asmstream, ",");
  decode_regname(rs);
  fprintf(asmstream, ",");
  decode_regname(rt);
}

static void op_rri(char *op, int32 w, int issigned)
{
  int32 rt = D_RT(w);
  int32 rs = D_RS(w);
  int32 n = w&0xffff;
  if (issigned && (n&0x8000) != 0) n |= 0xffff0000;
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rt);
  fprintf(asmstream, ",");
  decode_regname(rs);
  fprintf(asmstream, ",0x%lx", n);
}

static void op_rro(char *op, int32 w, int32 p, int flags)
{
  int32 rt = D_RT(w);
  int32 rs = D_RS(w);
  int32 n = p+4+(s16_(w) << 2);
  fprintf(asmstream, "\t%s\t",op);
  if (flags & 2) { decode_regname(rs); fprintf(asmstream, ","); }
  if (flags & 1) { decode_regname(rt); fprintf(asmstream, ","); }
  printlabelname(n, p, 0);
}

static void op_rrs(char *op, int32 w)
{
  int32 rd = D_RD(w);
  int32 rt = D_RT(w);
  int32 n = D_SHAMT(w);
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rd);
  fprintf(asmstream, ",");
  decode_regname(rt);
  fprintf(asmstream, ",%ld", n);
}

static void op_rd(char *op, int32 w)
{
  int32 rd = D_RD(w);
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rd);
}

static void op_rs(char *op, int32 w)
{
  int32 rs = D_RS(w);
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(rs);
}

static void op_rbn(char *op, int32 w)
{
  int32 rt = D_RT(w);
  int32 rs = D_RS(w);
  int32 n = w&0xffff;
  if ((n&0x8000) != 0) n |= 0xffff0000;
  fprintf(asmstream, "\t%s\t",op);
  switch (w & 0xfc000000)
  { case OP_LWC1: case OP_SWC1: fprintf(asmstream, "$f%ld", rt); break;
    default: decode_regname(rt); break;
  }
  fprintf(asmstream, ",0x%lx(", n);
  decode_regname(rs);
  fprintf(asmstream, ")");
}

static void op_special(int32 w)
{
  switch (w & MASK_SPECIAL) {
  case OP_SLL:
    op_rrs("sll", w);
    break;
  case OP_SRL:
    op_rrs("srl", w);
    break;
  case OP_SRA:
    op_rrs("sra", w);
    break;
  case OP_SLLV:
    op_dts("sll",w);
    break;
  case OP_SRLV:
    op_dts("srl",w);
    break;
  case OP_SRAV:
    op_dts("sra",w);
    break;
  case OP_JR:
    fprintf(asmstream,"\tj\t"); /* Use J rather than JR for Ultrix? */
    decode_regname(D_RS(w));
    break;
  case OP_JALR:
    fprintf(asmstream,"\tjal\t");
    decode_regname(D_RD(w));
    fprintf(asmstream, ",");
    decode_regname(D_RS(w));
    break;
  case OP_SYSCALL:
    fprintf(asmstream, "\tsyscall");
    break;
  case OP_BREAK:
    fprintf(asmstream,"\tBREAK");
    break;
  case OP_MFHI:
    op_rd("mfhi", w);
    break;
  case OP_MTHI:
    op_rs("mthi", w);
    break;
  case OP_MFLO:
    op_rd("mflo", w);
    break;
  case OP_MTLO:
    op_rs("mtlo", w);
    break;
  case OP_MULT:
    /* mult/div/rem are given as .word to stop macro expansion by 'as'. */
    fprintf(asmstream, "\t.word\t0x%.8lx # mult ", w);
    decode_regname(D_RS(w));
    fprintf(asmstream, ",");
    decode_regname(D_RT(w));
    break;
  case OP_MULTU:
    fprintf(asmstream, "\t.word\t0x%.8lx # multu ", w);
    decode_regname(D_RS(w));
    fprintf(asmstream, ",");
    decode_regname(D_RT(w));
    break;
  case OP_DIV:
    fprintf(asmstream, "\t.word\t0x%.8lx # div ", w);
    decode_regname(D_RS(w));
    fprintf(asmstream, ",");
    decode_regname(D_RT(w));
    break;
  case OP_DIVU:
    fprintf(asmstream, "\t.word\t0x%.8lx # divu ", w);
    decode_regname(D_RS(w));
    fprintf(asmstream, ",");
    decode_regname(D_RT(w));
    break;
  case OP_ADD:
    op_dst("add",w);
    break;
  case OP_ADDU:
    op_dst("addu",w);
    break;
  case OP_SUB:
    op_dst("sub",w);
    break;
  case OP_SUBU:
    op_dst("subu",w);
    break;
  case OP_AND:
    op_dst("and",w);
    break;
  case OP_OR:
    op_dst("or",w);
    break;
  case OP_XOR:
    op_dst("xor",w);
    break;
  case OP_NOR:
    op_dst("nor",w);
    break;
  case OP_SLT:
    op_dst("slt",w);
    break;
  case OP_SLTU:
    op_dst("sltu",w);
    break;
  default:
    asm_error = 1; fprintf(asmstream,"\t?.word\t0x%.8lx\n", w);
    break;
  }
}

static void op_bcond(int32 op, int32 p)
{
  switch (op & MASK_PRI+MASK_COND) {
  case OP_BLTZ:
    op_rro("bltz", op, p, 2);
    break;
  case OP_BGEZ:
    op_rro("bgez", op, p, 2);
    break;
  case OP_BLTZAL:
    op_rro("bltzal", op, p, 2);
    break;
  case OP_BGEZAL:
    op_rro("bgezal", op, p, 2);
    break;
  default:
    asm_error = 1; fprintf(asmstream,"\t?.word\t0x%.8lx", op);
    break;
  }
}

static void op_td(int32 op)
{
  decode_regname(D_RT(op));
  fprintf(asmstream,",");
  decode_regname(D_RD(op));
}

static void op_flt(int32 op)
{
  int32 f = (op&0x00a00000)>>21;
  char fmt = (f==0 ? 's' : (f==1 ? 'd' : 'w'));
  switch (op & 0x3f) {
  case (OP_ADD_S&0x3f):
    fprintf(asmstream,"\tadd.%c\t$f%ld,$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op), D_FT(op));
    return;
  case (OP_SUB_S&0x3f):
    fprintf(asmstream,"\tsub.%c\t$f%ld,$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op), D_FT(op));
    return;
  case (OP_MUL_S&0x3f):
    fprintf(asmstream,"\tmul.%c\t$f%ld,$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op), D_FT(op));
    return;
  case (OP_DIV_S&0x3f):
    fprintf(asmstream,"\tdiv.%c\t$f%ld,$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op), D_FT(op));
    return;
  case (OP_ABS_S&0x3f):
    fprintf(asmstream,"\tabs.%c\t$f%ld,$f%ld", fmt, D_FD(op), D_FS(op));
    return;
  case (OP_MOV_S&0x3f):
    fprintf(asmstream,"\tmov.%c\t$f%ld,$f%ld", fmt, D_FD(op), D_FS(op));
    return;
  case (OP_NEG_S&0x3f):
    fprintf(asmstream,"\tneg.%c\t$f%ld,$f%ld", fmt, D_FD(op), D_FS(op));
    return;
  case (OP_CVT_S_S&0x3f):
    fprintf(asmstream,"\tcvt.s.%c\t$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op));
    return;
  case (OP_CVT_D_S&0x3f):
    fprintf(asmstream,"\tcvt.d.%c\t$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op));
    return;
  case (OP_CVT_W_S&0x3f):
    fprintf(asmstream,"\tcvt.w.%c\t$f%ld,$f%ld", fmt,
            D_FD(op), D_FS(op));
    return;
  case (OP_C_F_S&0x3f):
    fprintf(asmstream,"\tc.f.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_UN_S&0x3f):
    fprintf(asmstream,"\tc.un.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_EQ_S&0x3f):
    fprintf(asmstream,"\tc.eq.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_UEQ_S&0x3f):
    fprintf(asmstream,"\tc.ueq.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_OLT_S&0x3f):
    fprintf(asmstream,"\tc.olt.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_ULT_S&0x3f):
    fprintf(asmstream,"\tc.ult.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_OLE_S&0x3f):
    fprintf(asmstream,"\tc.ole.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_ULE_S&0x3f):
    fprintf(asmstream,"\tc.ule.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_SF_S&0x3f):
    fprintf(asmstream,"\tc.sf.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_NGLE_S&0x3f):
    fprintf(asmstream,"\tc.ngle.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_LT_S&0x3f):
    fprintf(asmstream,"\tc.lt.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_NGE_S&0x3f):
    fprintf(asmstream,"\tc.nge.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  case (OP_C_LE_S&0x3f):
    fprintf(asmstream,"\tc.le.%c\t$f%ld,$f%ld", fmt, D_FS(op), D_FT(op));
    return;
  case (OP_C_NGT_S&0x3f):
    fprintf(asmstream,"\tc.ngt.%c\t$f%ld,$f%ld", fmt,
            D_FS(op), D_FT(op));
    return;
  default:
    asm_error = 1; fprintf(asmstream,"\t?.word\t0x%.8lx # Floating", op);
    return;
  }
}

static void op_copz(int coproc, int32 op)
{
  switch (op & MASK_COPZ) {
  case OP_MFCZ:
    fprintf(asmstream,"\tmfc%d\t", coproc);
    op_td(op);
    break;
  case OP_MTCZ:
    fprintf(asmstream,"\tmtc%d\t", coproc);
    op_td(op);
    break;
  case OP_CFCZ:
    fprintf(asmstream,"\tcfc%d\t", coproc);
    op_td(op);
    break;
  case OP_CTCZ:
    fprintf(asmstream,"\tctc%d\t", coproc);
    op_td(op);
  case OP_BCZF:
    fprintf(asmstream,"\tbc%dF\t%ld", coproc, s16_(op) << 2);
    break;
  case OP_BCZT:
    fprintf(asmstream,"\tbc%dZ\t%ld", coproc, s16_(op) << 2);
    break;
  default:
    asm_error = 1; fprintf(asmstream,"\t?.word\t0x%.8lx", op);
    break;
  }
}

static void op_cop1(int32 op, int32 p)
{
  switch (op & MASK_COPZ) {
  case OP_MFCZ:
    fprintf(asmstream,"\tmfc1\t");
    decode_regname(D_RT(op));
    fprintf(asmstream,",$f%ld",D_FS(op));
    break;
  case OP_MTCZ:
    fprintf(asmstream,"\tmtc1\t");
    decode_regname(D_RT(op));
    fprintf(asmstream,",$f%ld",D_FS(op));
    break;
  case OP_CFCZ:
    fprintf(asmstream,"\tcfc1\t");
    decode_regname(D_RT(op));
    fprintf(asmstream,",$%ld",D_FS(op));
    break;
  case OP_CTCZ:
    fprintf(asmstream,"\tctc1\t");
    decode_regname(D_RT(op));
    fprintf(asmstream,",$%ld",D_FS(op));
    break;
  case OP_BCZF:
    { int32 n = p+4+(s16_(op) << 2);
      fprintf(asmstream,"\tbc1%c\t", (D_RT(op) & 1) ? 't':'f');
      printlabelname(n, p, 0);
    }
    break;
  default:
    asm_error = 1; fprintf(asmstream,"\t?.word\t0x%.8lx", op);
    break;
  }
}

static void op_cop0(int32 op)
{
  switch (op & 0x1f) {
  case OP_TLBR:
    fprintf(asmstream,"\ttlbr");
    break;
  case OP_TLBWI:
    fprintf(asmstream,"\ttlbwi");
    break;
  case OP_TLBWR:
    fprintf(asmstream,"\ttlbwr");
    break;
  case OP_TLBP:
    fprintf(asmstream,"\ttlbp");
    break;
  case OP_RFE:
    fprintf(asmstream,"\trfe");
    break;
  default:
    op_copz(0, op);
    break;
  }
}

static void decode_instruction(int32 w, int32 p)
{
    switch (w & 0xfc000000)
    {
    case OP_SPECIAL:
      op_special(w);
      break;
    case OP_BCOND:
      op_bcond(w, p);
      break;
    case OP_J:
      fprintf(asmstream, "\tj\t");
      decode_addr(w&0x03ffffff, p);
      break;
    case OP_JAL:
      fprintf(asmstream, "\tjal\t");
      decode_addr(w&0x03ffffff, p);
      break;
    case OP_BEQ:
      op_rro("beq",w,p,3);
      break;
    case OP_BNE:
      op_rro("bne",w,p,3);
      break;
    case OP_BLEZ:
      op_rro("blez",w,p,2);
      break;
    case OP_BGTZ:
      op_rro("bgtz",w,p,2);
      break;
    case OP_ADDI:
      op_rri("addi", w, 1);
      break;
    case OP_ADDIU:
      op_rri("addiu", w, 1);
      break;
    case OP_SLTI:
      op_rri("slti", w, 1);
      break;
    case OP_SLTIU:
      op_rri("sltiu", w, 1);
      break;
    case OP_ANDI:
      op_rri("andi", w, 0);
      break;
    case OP_ORI:
      op_rri("ori", w, 0);
      break;
    case OP_XORI:
      op_rri("xori", w, 0);
      break;
    case OP_LUI:
      {
        int32 rt = D_RT(w);
        int32 n = w&0xffff;
        /* if ((n&0x8000) != 0) n |= 0xffff0000; Surely not true */
        fprintf(asmstream, "\tlui\t");
        decode_regname(rt);
        fprintf(asmstream, ",0x%lx", n);
      }
      break;
    case OP_COP0:
      op_cop0(w);
      break;
    case OP_COP1:
      if ((w&0x02000000)!=0) op_flt(w);
      else op_cop1(w,p);
      break;
    case OP_COP2:
      op_copz(2,w);
      break;
    case OP_COP3:
      op_copz(3,w);
      break;
    case OP_LB:
      op_rbn("lb", w);
      break;
    case OP_LH:
      op_rbn("lh", w);
      break;
    case OP_LWL:
      op_rbn("lwl", w);
      break;
    case OP_LW:
      op_rbn("lw", w);
      break;
    case OP_LBU:
      op_rbn("lbu", w);
      break;
    case OP_LHU:
      op_rbn("lhu", w);
      break;
    case OP_LWR:
      op_rbn("lwr", w);
      break;
    case OP_SB:
      op_rbn("sb", w);
      break;
    case OP_SH:
      op_rbn("sh", w);
      break;
    case OP_SWL:
      op_rbn("swl", w);
      break;
    case OP_SW:
      op_rbn("sw", w);
      break;
    case OP_SWR:
      op_rbn("swr", w);
      break;
    case OP_LWC0:
      op_rbn("lwc0", w);
      break;
    case OP_LWC1:
      op_rbn("lwc1", w);
      break;
    case OP_LWC2:
      op_rbn("lwc2", w);
      break;
    case OP_LWC3:
      op_rbn("lwc3", w);
      break;
    case OP_SWC0:
      op_rbn("swc0", w);
      break;
    case OP_SWC1:
      op_rbn("swc1", w);
      break;
    case OP_SWC2:
      op_rbn("swc2", w);
      break;
    case OP_SWC3:
      op_rbn("swc3", w);
      break;
    case OP_pseudoADDIU_PC:
      op_rro("ADDIU[PC]",w,p,1);
      break;
    case OP_pseudoLW_PC:
      op_rro("LW[PC]",w,p,1);
      break;
    default:
      asm_error = 1; fprintf(asmstream, "\t?.word\t0x%.8lx", (long)w);
    }
}

static void decode_extended(int32 w1, int32 q, int32 lui_rt)
{
  int32 rt = D_RT(w1);
  int32 rs = D_RS(w1);
  int32 n = s16_(w1);
    bool flt=0;
    char *ww;
    switch (w1 & MASK_PRI) {
    case OP_ADDIU: ww = "la"; goto r_check;
    case OP_LW: ww = "lw"; goto r_check;
    case OP_SW: ww = "sw"; goto w_check;
    case OP_LB: ww = "lb"; goto r_check;
    case OP_LBU: ww = "lbu"; goto r_check;
    case OP_LH: ww = "lh"; goto r_check;
    case OP_LHU: ww = "lhu"; goto r_check;
    case OP_SB: ww = "sb"; goto w_check;
    case OP_SH: ww = "sh"; goto w_check;
    case OP_LWC1: ww = "lwc1"; goto f_check;
    case OP_SWC1: ww = "swc1"; goto f_check;
    default: asm_error = 1; ww = "??"; break;
    w_check: if (rs == rt || rs != 1) goto err; break;
    r_check: if (rs != rt && rs != 1) goto err; break;
    f_check: flt = 1; if (rs != 1) goto err; break;
    err:     syserr("asm.c(bad LUI)");
    }
    fprintf(asmstream, "\t%s\t",ww);
    if (flt) fprintf(asmstream, "$f%ld",rt); else decode_regname(rt);
    fprintf(asmstream, ",");
    decode_addr(n, q);
    if (rs != lui_rt) {
      syserr("asm.c(bad LUI2)");
      fprintf(asmstream, "(");
      decode_regname(rs);
      fprintf(asmstream, ")");
    }
}

/* ===================== The MIPS16 disassembler ====================== */

static m32r[8] = { 16,17,2,3,4,5,6,7 };

#define RY_LR 1
#define RY_SP 2
#define RY_PC 4
#define RX_SP 8

static void op16_rbn(char *op, int32 w, int f, int32 n)
{
  int32 rx = m32r[w>>8 & 7];
  int32 rb = f & RX_SP ? 29 : rx;
  int32 ry = f & RY_LR ? 31 : f ? rx : m32r[w>>5 & 7];
  if ((n&0x8000) != 0) n |= 0xffff0000;
  fprintf(asmstream, "\t%s\t",op);
  decode_regname(ry);
  fprintf(asmstream, ",0x%lx(", n);
  decode_regname(rb);
  fprintf(asmstream, ")");
}

static void op16_ro(char *op, int32 w, int32 d, int32 p_eff, bool waste)
{
  int32 rx = m32r[w>>8 & 7];
  fprintf(asmstream, "\t%s\t", op);
  if (w != 0)
  { decode_regname(rx);
    fprintf(asmstream, ",");
  }
  printlabelname(p_eff+d, p_eff, 1);
  if (waste) fprintf(asmstream, "\t# spurious EXT");
}

static void op16_lit(char *op, int32 w, int32 d, int32 p, int32 p_eff,
                     bool waste)
{
  int32 rx = m32r[w>>8 & 7];
  fprintf(asmstream, "\t%s\t", op);
  decode_regname(rx);
  fprintf(asmstream, ",");
  if (decode_external(codebase+p)) decode_addr(0, p);
  else { printlabelname(p_eff+d, p_eff, 0);
         if (waste) fprintf(asmstream, "\t# spurious EXT");
       }
}

static void op16_shift(char *op, int32 w, int32 shamt)
{   /* Operand ordering follows case 'diad' in op16_rr().               */
    int32 rx = m32r[w>>8 & 7];
    int32 ry = m32r[w>>5 & 7];
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rx);
    fprintf(asmstream, ",");
    decode_regname(ry);
    fprintf(asmstream, ",%ld", shamt);
}

static void op16_rr(int32 w)
{
  char *op;
  int32 rx = m32r[w>>8 & 7];
  int32 ry = m32r[w>>5 & 7];
  switch (w & 0x1f)
  {
case OP16_JR & 31:
    if (w & 0x20) rx = 31;      /* R_LR */
    op = (w >> 6 & 3) * 6 + "jr\0\0\0\0jalr\0\0jrc\0\0\0jalrc";
    goto monad;

case OP16_SDBBP & 31:       /* u6 */
    op = "sdbbp"; goto nilad;
case OP16_SLT & 31:         /* T = R3 op R3 */
    op = "slt"; goto diad;
case OP16_SLTU & 31:        /* T = R3 op R3 */
    op = "sltu"; goto diad;
case OP16_SLLV & 31:        /* R3 op= R3 */
    op = "sllv"; goto xdiad;
case OP16_BREAK & 31:       /* breakpt */
    op = "break"; goto nilad;
case OP16_SRLV & 31:        /* R3 op= R3 */
    op = "srlv"; goto xdiad;
case OP16_SRAV & 31:        /* R3 op= R3 */
    op = "srav"; goto xdiad;
case OP16_DSRL & 31:        /* R3 >>= u3 */
    op = "dsrl"; goto dshifti;
case OP16_CMP & 31:         /* T = R3 op R3 */
    op = "cmp"; goto diad;
case OP16_NEG & 31:         /* R3 op= R3 */
    op = "neg"; goto diad;
case OP16_AND & 31:         /* R3 op= R3 */
    op = "and"; goto diad;
case OP16_OR & 31:          /* R3 op= R3 */
    op = "or"; goto diad;
case OP16_XOR & 31:         /* R3 op= R3 */
    op = "xor"; goto diad;
case OP16_NOT & 31:         /* R3 op= R3 */
    op = "not"; goto diad;
case OP16_MFHI & 31:        /* R3 = HI */
    op = "mfhi"; goto monad;

case OP16_ZEB & 31:         /* R3 */
    op = (w>>5 & 7) * 4 + "zeb\0zeh\0zew\0?zx\0seb\0seh\0sew\0?sx"; goto monad;
case OP16_MFLO & 31:        /* R3 = LO */
    op = "mflo"; goto monad;
case OP16_DSRA & 31:        /* R3 >>= u3 */
    op = "dsra"; goto dshifti;
case OP16_DSLLV & 31:       /* R3 op= R3 */
    op = "dsllv"; goto xdiad;
case OP16_DSRLV & 31:       /* R3 op= R3 */
    op = "dsrlv"; goto xdiad;
case OP16_DSRAV & 31:       /* R3 op= R3 */
    op = "dsrav"; goto xdiad;
case OP16_MULT & 31:        /* HI,LO = R3 op R3 */
    op = "mult"; goto diad;
case OP16_MULTU & 31:       /* HI,LO = R3 op R3 */
    op = "multu"; goto diad;
case OP16_DIV & 31:         /* HI,LO = R3 op R3 */
    op = "div"; goto diad;
case OP16_DIVU & 31:        /* HI,LO = R3 op R3 */
    op = "divu"; goto diad;
case OP16_DMULT & 31:       /* HI,LO = R3 op R3 */
    op = "dmult"; goto diad;
case OP16_DMULTU & 31:      /* HI,LO = R3 op R3 */
    op = "dmultu"; goto diad;
case OP16_DDIV & 31:        /* HI,LO = R3 op R3 */
    op = "ddiv"; goto diad;
case OP16_DDIVU & 31:       /* HI,LO = R3 op R3 */
    op = "ddivu"; goto diad;

default: 
    asm_error = 1;
    fprintf(asmstream, "\t?bad MIPS16_RR instruction (%.4lx)", w);
    break;

nilad:
    fprintf(asmstream, "\t%s", op);
    break;
monad:
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rx);
    break;
diad:
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rx);
    fprintf(asmstream, ",");
    decode_regname(ry);
    break;
xdiad:
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(ry);
    fprintf(asmstream, ",");
    decode_regname(rx);
    break;
dshifti:
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rx);
    fprintf(asmstream, ",%ld", ((w>>5)-1 & 7) + 1);
    break;
  }
}

static void op16_rrr(int32 w)
{   char *op = "daddu\0addu\0\0dsubu\0subu\0" + 6*(w&3);
    int32 rx = m32r[w>>8 & 7];
    int32 ry = m32r[w>>5 & 7];
    int32 rz = m32r[w>>2 & 7];
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rz);
    fprintf(asmstream, ",");
    decode_regname(rx);
    fprintf(asmstream, ",");
    decode_regname(ry);
}

static void op16_i(char *op, int32 w, int32 f, int32 imm)
{
    int32 rx = f & RX_SP ? 29 : m32r[w>>8 & 7];
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(rx);
    if (f & RY_SP) { fprintf(asmstream, ","); decode_regname(29); }
    if (f & RY_PC) { fprintf(asmstream, ",%s", "pc"); }
    fprintf(asmstream, ",0x%.4lx", imm);
}

static void op16_addiu3(char *op, int32 w, int32 imm)
{
    int32 rx = m32r[w>>8 & 7];
    int32 ry = m32r[w>>5 & 7];
    /* special coding for imm in ADDIU3 */
    imm = s15_(imm & ~16 | (imm >> 11) & 16);
    fprintf(asmstream, "\t%s\t", op);
    decode_regname(ry);
    fprintf(asmstream, ",");
    decode_regname(rx);
    fprintf(asmstream, ",0x%.4lx", imm);
}

static void op16_jal(char *op, int32 w, int32 p)
{
    fprintf(asmstream, "\t%s\t", op);
    decode_addr(w & 0x03ffffff, p);
}

static void decode_mov(char *op, int32 rd, int32 rs)
{   fprintf(asmstream, "\t%s\t", op);
    decode_regname(rd);
    fprintf(asmstream, ",");
    decode_regname(rs);
#ifdef JUGGLECNT
    {   static char m16r[32] = {
             0, 0, 3, 3, 3, 3, 3, 3,
             0, 0, 0, 0, 0, 0, 0, 0,
             3, 3, 0, 0, 0, 0, 0, 0,
             1, 0, 0, 0, 0, 1, 0, 0 };          /* $24,$29 OK as rs.    */
        if (!(m16r[rs] & m16r[rd]>>1))
        {   jugglecnt++;
            fprintf(asmstream, " # juggle MIPS16");
        }
    }
#endif
}

static int savetab[16] =
{ 0x00, 0x80, 0xc0, 0xe0,
  0x01, 0x81, 0xc1, 0xe1,
  0x03, 0x83, 0xc3,
                          0xf0,
  0x07, 0x87,
  0x0f, -1 };

#define decode_rncomma(x) (decode_regname(x), fprintf(asmstream, "%c", ','))
#define decode_commarn(x) (fprintf(asmstream, "%c", ','), decode_regname(x))

static void op16_save(char *op, int32 m, int32 size)
{   int32 i;
    int32 xsregs = m >> 8;
    int32 amask = savetab[m & 15];
    if (amask < 0) asm_error = 1, fprintf(asmstream, "\t?bad SAVE/RESTORE");
    fprintf(asmstream, "\t%s\t", m & SR_SAVE ? "save" : "restore");
    if (m & SR_RA) decode_rncomma(31);
    for (i = xsregs-1; i>=0; i--) decode_rncomma(i==6? 30 : 18+i);
    if (m & SR_S1) decode_rncomma(17);
    if (m & SR_S0) decode_rncomma(16);
    for (i = 7; i>=4; i--) if (amask>>i & 1) decode_rncomma(i);
    fprintf(asmstream, "%ld", size);
    for (i = 4; i<=7; i++) if (amask>>(i-4) & 1) decode_commarn(i);
}

#define NOEXT 0x5000000

static bool op16_i8(int32 w, int32 ext, int32 w1, int32 p, int32 ilen)
{
    switch (w & 0xff00)
    {
case OP16_BTEQZ:        /* if (T) PC += s8<<1 */
	op16_ro("bteqz", 0, (ext==NOEXT ? s8_(w) : ext) << 1, p+ilen,
                ext!=NOEXT && s8_(ext) == ext); break;
case OP16_BTNEZ:        /* if (T) PC += s8<<1 */
	op16_ro("btnez", 0, (ext==NOEXT ? s8_(w) : ext) << 1, p+ilen,
                ext!=NOEXT && s8_(ext) == ext); break;
case OP16_SW_SP_LR:     /* u8<<2(SP) = LR */
	op16_rbn("sw", w, RY_LR|RX_SP, ext==NOEXT ? u8_(w)<<2 : ext); break;
case OP16_ADJSP:        /* SP += s8<<3 */
	op16_i("addiu", w, RX_SP, ext==NOEXT ? s8_(w)<<3 : ext); break;
case OP16_RESTORE:      /* i7 */
	op16_save((w & SR_SAVE ? "save" : "restore"),
                  (ext==NOEXT ? 0 : w1 & 0x070f) | (w & 0xf0),
                  (w&0x0f | (ext==NOEXT ? (w&0x0f ? 0 : 16) : w1&0xf0)) << 3);
        break;
case OP16_spare66:
        fprintf(asmstream, "\ti8:unimp\t0x%.4lx", w); break;
case OP16_MOV32R:       /* R5 = R3, nop if both zero */
        if (ext != NOEXT) return 0;
        if ((w & 255) == 0) fprintf(asmstream, "\tnop");
        else decode_mov("mov", (w>>5 & 7) | w & 24, m32r[w & 7]);
        break;
case OP16_MOVR32:       /* R3 = R5 */
        if (ext != NOEXT) return 0;
        decode_mov("mov", m32r[w>>5 & 7], w & 31);
        break;
    }
    return 1; /* success */
}

static void decode_instr16(int32 w1, int32 w2, int32 p, int32 ilen)
{
    int32 w = w1;
    int32 ext = NOEXT;
    if ((w1 & MASK16_PRI) == OP16_EXT)
        w = w2, ext = s16_((w1 & 0x1f) << 11 | (w1 & 0x07e0) | (w2 & 0x1f));
    switch (w & MASK16_PRI)
    {
case OP16_ADDIU_SP:     /* R3 = SP op u8<<2    */
	op16_i("addiu", w, RY_SP, ext==NOEXT ? u8_(w)<<2 : ext); break;
case OP16_ADDIU_PC:     /* R3 = (PC op u8<<2) & ~3 */
	/* op16_i("addiu", w, RY_PC, ext==NOEXT ? u8_(w)<<2 : ext); break; */
	op16_lit("la", w, ext==NOEXT ? u8_(w)<<2 : ext, p, p&~3,
                ext!=NOEXT && (ext & 0x3fc) == ext); break;
case OP16_B:            /* PC += s11<<1     */
	op16_ro("b", 0, (ext==NOEXT ? s11_(w) : ext) << 1, p+ilen,
                ext!=NOEXT && s11_(ext) == ext); break;
case OP16_JAL:          /* PC = [PCHI:imm26] */
        if (ext != NOEXT) goto badext;
        op16_jal(w1 & 0x0400 ? "jalx" : "jal",
                (w1 & 0x1f) << 21 | (w1 & 0xe3e) << 11 | w2, p); break;
case OP16_BEQZ:         /* if (R3) PC += s8<<1 */
	op16_ro("beqz", w, (ext==NOEXT ? s8_(w) : ext) << 1, p+ilen,
                ext!=NOEXT && s8_(ext) == ext); break;
case OP16_BNEZ:         /* if (R3) PC += s8<<1 */
	op16_ro("bnez", w, (ext==NOEXT ? s8_(w) : ext) << 1, p+ilen,
                ext!=NOEXT && s8_(ext) == ext); break;
case OP16_SHIFTform:    /* R3 = R3 << u3 */
	op16_shift((w&3)*6 + "sll\0\0\0dsll\0\0srl\0\0\0sra", w,
                   ext==NOEXT ? ((w>>2)-1 & 7) + 1 : ext>>6); break;

case OP16_LD:           /* R3 =64 u5(R3) */
	op16_rbn("ld", w, 0, ext==NOEXT ? u5_(w)<<3 : ext); break;
case OP16_ADDIU3:       /* R3 = R3 op s4 */
/* case OP16_DADDIU3: */  /* R3 = R3 op s4 */
        op16_addiu3(w & 16 ? "daddiu" : "addiu", w, ext==NOEXT ? s4_(w) : ext);
        break;
case OP16_ADDIU2:       /* R3 op= s8     */
	op16_i("addiu", w, 0, ext==NOEXT ? s8_(w) : ext); break;
case OP16_SLTI:         /* T = R3 op u8  */
	op16_i("slti", w, 0, ext==NOEXT ? u8_(w) : ext); break;
case OP16_SLTIU:        /* T = R3 op u8  */
	op16_i("sltiu", w, 0, ext==NOEXT ? u8_(w) : ext); break;

case OP16_I8form:
	if (!op16_i8(w, ext, w1, p, ilen)) goto badext;
        break;

case OP16_LI:           /* R3 = u8 (ext=u16) */
	op16_i("li", w, 0, ext==NOEXT ? u8_(w) : u16_(ext)); break;
case OP16_CMPI:         /* T = R3 op u8 (ext=u16) */
	op16_i("cmpi", w, 0, ext==NOEXT ? u8_(w) : u16_(ext)); break;

case OP16_SD:           /* u5(R3) =64 R3 */
	op16_rbn("sd", w, 0, ext==NOEXT ? u5_(w)<<3 : ext); break;
case OP16_LB:           /* R3 = u5(R3) */
	op16_rbn("lb", w, 0, ext==NOEXT ? u5_(w)<<0 : ext); break;
case OP16_LH:           /* R3 = u5(R3) */
	op16_rbn("lh", w, 0, ext==NOEXT ? u5_(w)<<1 : ext); break;
case OP16_LW_SP:        /* R3 = u8<<2(SP) */
	op16_rbn("lw", w, RX_SP, ext==NOEXT ? u8_(w)<<2 : ext); break;
case OP16_LW:           /* R3 = u5(R3) */
	op16_rbn("lw", w, 0, ext==NOEXT ? u5_(w)<<2 : ext); break;
case OP16_LBU:          /* R3 = u5(R3) */
	op16_rbn("lbu", w, 0, ext==NOEXT ? u5_(w)<<0 : ext); break;
case OP16_LHU:          /* R3 = u5(R3) */
	op16_rbn("lhu", w, 0, ext==NOEXT ? u5_(w)<<1 : ext); break;
case OP16_LW_PC:        /* R3 = u8<<2(PC) */
	/* op16_i("lw", w, RY_PC, ext==NOEXT ? u8_(w)<<2 : ext); break; */
	op16_lit("lw", w, ext==NOEXT ? u8_(w)<<2 : ext, p, p&~3,
                ext!=NOEXT && (ext & 0x3fc) == ext); break;
case OP16_LWU:          /* R3 =64 u5(R3) */
	op16_rbn("lwu", w, 0, ext==NOEXT ? u5_(w)<<2 : ext); break;
case OP16_SB:           /* u5(R3) = R3 */
	op16_rbn("sb", w, 0, ext==NOEXT ? u5_(w)<<0 : ext); break;
case OP16_SH:           /* u5(R3) = R3 */
	op16_rbn("sh", w, 0, ext==NOEXT ? u5_(w)<<1 : ext); break;
case OP16_SW_SP:        /* u8<<2(SP) = R3 */
	op16_rbn("sw", w, RX_SP, ext==NOEXT ? u8_(w)<<2 : ext); break;
case OP16_SW:           /* u5(R3) = R3 */
	op16_rbn("sw", w, 0, ext==NOEXT ? u5_(w)<<2 : ext); break;

case OP16_RRRform:      /* R3 = R3 op R3 */
        if (ext != NOEXT) goto badext;
        op16_rrr(w); break;
case OP16_RRform:
        if (ext != NOEXT) goto badext;
        op16_rr(w); break;
case OP16_EXT:          /* extend */
        goto badext;
case OP16_MIPS64form:   /* mips64 */
        asm_error = 1;
        fprintf(asmstream, "\t?bad MIPS64 instruction (%.4lx:%.4lx)", w1, w2);
        break;
badext:
        asm_error = 1;
        fprintf(asmstream, "\t?bad EXTEND instruction (%.4lx:%.4lx)", w1, w2);
        break;
    }
}

static void decode_DC(int32 w)
{   int32 col;
    asm_padcol8(0);
    col = fprintf(asmstream, ".word");
    col = asm_padcol8(col);
    fprintf(asmstream, "0x%.8lx", (long)w);
    dotwordcnt++;
}

static void decode_DCA(Symstr *s, int32 w)
{   int32 col;
    asm_padcol8(0);
    col = fprintf(asmstream, ".word");
    col = asm_padcol8(col);
    pr_asmname(s);
    if (w!=0) fprintf(asmstream, "%+ld", (long)w);
    dotwordcnt++;
}

static void maybe_export(Symstr *sym)
{   char  *p = symname_(sym);
    char  c;
    ExtRef *x;
/* Unless external there is nothing to do here. */
    if ((x = symext_(sym)) != 0 &&
        (x->extflags & xr_defext) == 0) return;
/*@@@ AM does not see how the following can ever now happen as x$dataseg etc. */
/*@@@ are very local statics.  Is this if error recovery inserted gensyms?    */
    while ((c = *p++) != 0) { /* look for odd characters in x$dataseg etc */
        if (!(isalnum(c) || (c == '_'))) return;
    }
    {
        FILE  *as = asmstream;
        fprintf(as, "\t.globl\t"); pr_asmname(sym); fputs("\n", as);
    }
}

/* exported functions ...*/
extern LabelNumber *returnlab;

void display_assembly_code(Symstr *name)
{   int32 q, ilen;
    LabList *asm_lablist2 = 0;
    litlabno = 0;
    if (name != 0)   /* may be 0 for string literals from static inits   */
    {   asm_lablist2 = asm_lablist;
        fncount++;
	fprintf(asmstream, "\n");
	fprintf(asmstream, ".ent ");
        pr_asmname(name);
        fprintf(asmstream, ",3\n");
        maybe_export(name);
        if (annotations)
            fprintf(asmstream, "%.6lx  %20s", (long)codebase, "");
        pr_asmname(name);
        fprintf(asmstream, ":\n");
    }
    for (q=0; q < codep; q+=ilen)    /* q is now a BYTE offset */
    {   int32 w = code_inst_(q);
        const int32 f = code_flag_(q);
        VoidStar aux = code_aux_(q);
        ilen = 4;
        if ((mips_opt & 2) && f == LIT_OPCODE)
        {   w = code_hword_(q);
            switch (w & MASK16_PRI)
            {
    default: ilen = 2; break;
    case OP16_JAL:
    case OP16_JALX:
    case OP16_EXT: break;
            }
        }
        {   int32 labq;
            LabelNumber *t;
            while (asm_lablist2 &&
                      (t = asm_lablist2->labcar,
                       (labq = (t->u.defn & 0x00ffffff))) <= q)
            {   if (annotations)
                    fprintf(asmstream, "%28s", "");
                litlabdef = codebase+labq;
                litlabno = lab_name_(t) & 0x7fffffff;
                fprintf(asmstream, "L%ldF%ld: \n",
                        (long)(lab_name_(t) & 0x7fffffff),
                        (long)fncount);
                if (labq != q)
                    asm_error = 1,
                    fprintf(asmstream,"?? labq=%lx q=%lx\n",(long)labq, q);
                asm_lablist2 = asm_lablist2->labcdr;
            }
        }
        if (annotations)
        {   int32 i;
            fprintf(asmstream, "%.6lx  ", (long)(q + codebase));
            switch (f)
            { case LIT_OPCODE:
                if (mips_opt & 2)
                  for (i = 0; i < 4; i += 2)
                    if (i < ilen)
                        fprintf(asmstream, "%.4lx ", (long)code_hword_(q+i));
                    else
                        fprintf(asmstream, "     ");
                  else for (i = 0; i < 8; i += 4)
                    if (i < 4)
                        fprintf(asmstream, "%.8lx ", (long)code_inst_(q+i));
                    else
                        fprintf(asmstream, "     ");
                break;
              case LIT_RELADDR:
                for (i = 0; i < 8; i += 4)
                        fprintf(asmstream, "%.8lx ", (long)code_inst_(q+i));
                break;
              case LIT_STRING:
                fprintf(asmstream, "%.8lx", (long)totargetsex(w,LIT_BBBB));
                break;
              default:
                fprintf(asmstream, "%.8lx", (long)w);
                break;
            }
        }
        switch (f)
        {
    case LIT_RELADDR:
            if ((w & MASK_PRI)==OP_LUI) {
              /* the next could use decode_external, may be cleaner.    */
              q +=4;
              decode_extended(code_inst_(q), q, D_RT(w));
            }
            else syserr("MIPS(extended LA/LW/SW lacks LUI)");
            break;
    case LIT_OPCODE:
            if (mips_opt & 2)
              /* FIXME: sometimes q should be preceding J (non-annul).  */
              decode_instr16(w, (ilen==4 ? code_hword_(q+2) : 0), q, ilen);
            else
              decode_instruction(w, q);
            break;
    case LIT_STRING:
            decode_DC(totargetsex(w,LIT_BBBB));
#ifdef never
            fprintf(asmstream, ".byte %#.2x,%#.2x,%#.2x,%#.2x",
                    (int)((w>>24)&0xff),(int)((w>>16)&0xff),
                    (int)((w>>8)&0xff),(int)(w&0xff));
#endif
            if (annotations) fprintf(asmstream, " # "), pr_chars(w);
            break;
    case LIT_NUMBER:
            decode_DC(w);
            break;
    case LIT_ADCON:
            decode_DCA(decode_external(codebase+q), w);
            notelitadcon();
            break;
    case LIT_FPNUM:
            decode_DC(w);
            if (annotations)
                fprintf(asmstream, " # E'%s'", (char *)aux);
            break;
    case LIT_FPNUM1:
            decode_DC(w);
            if (annotations)
                fprintf(asmstream, " # D'%s'",(char *)aux);
            break;
    case LIT_FPNUM2:    /* all printed by the FPNUM1 */
            decode_DC(w);
            break;
    default:
            decode_DC(w);
            fprintf(asmstream, "    ?%ld",f);
            asm_error = 1;
        }
        fprintf(asmstream, "\n");
    }
    if (asm_lablist2) syserr(syserr_asmlab,0L);
    asm_lablist = 0;    /* stop confusion when called from vargen.c  */
    if (name != 0)
    {
        fprintf(asmstream, ".end ");
        pr_asmname(name);
        fprintf(asmstream, "\n");
    }
}

void asm_header()
{
/*  int32 i; */
    asm_error = 0;
    fncount = 0;
    litlabels = 0;
    fprintf(asmstream," # generated by %s\n", CC_BANNER);
    fprintf(asmstream," # target %s, %s endian, compiler option code %.4lx\n",
                       mips_opt & 2 ? "MIPS16":"MIPS-1",
                       target_lsbytefirst ? "little" : "big",
                       mips_opt);
    fprintf(asmstream, "\t.set noreorder\n");
    if (!(mips_opt & 2)) fprintf(asmstream, "\t.set noat\n");
    fprintf(asmstream, "\t.text\n");
    if (mips_opt & 2) fprintf(asmstream, "\t.set mips16\n");
    dotwordcnt = 0;
    jugglecnt = 0;
}

/* (not exported) */

static void asm_outextern()
{
#ifdef never
    ExtRef *x;
    for (x = obj_symlist; x != 0; x = x->extcdr)
    {   int32 flags = x->extflags;
        if (!(flags & xr_defloc) && (flags & xr_defext))
        {   fprintf(asmstream, " .globl ");
            pr_asmname(x->extsym);
            fprintf(asmstream, "\n");
        }
    }
    for (x = obj_symlist; x != 0; x = x->extcdr)
    {   int32 flags = x->extflags;
        if (!(flags & xr_defloc) && !(flags & xr_defext))
        {   fprintf(asmstream, "# .extern ");
            pr_asmname(x->extsym);
        }
    }
#endif
}

static void pr_common_defs(void)
{
  ExtRef *x;
  bool seen = 0;
  /* AM: I hope the next line does not upset anyone	*/
  obj_symlist = (ExtRef *) dreverse((List *)obj_symlist);
				/* oldest = smallest numbered first */
/* This next stuff is really OBSOLETE_BSS ...                       */
/* It works for PCC mode bss, but omits non-PCC mode bss.           */
/* Really we should shop using OBSOLETE_BSS and then we'll get      */
/* the BSS done right in both cases.                                */
  for (x = obj_symlist; x!=0; x = x->extcdr) {
    int32 sz = x->extoffset;
    if (sz>0 && !(x->extflags & (xr_defloc|xr_defext))) {
      fprintf(asmstream, "\t.comm\t");
      pr_asmname(x->extsym);
      fprintf(asmstream, ",%lu\n", (long)sz);
      seen = 1;
    }
  }
}

static void asm_datasect(DataInit *p, int genflag, char *segname)
{ fprintf(asmstream, "\n");
  asm_padcol8(0), fprintf(asmstream, "%s\n", segname);
#ifdef never
  fprintf(asmstream, "\t%s\n", segname);    /* .segment/.SEGMENT */
#endif
  for (; p != 0; p = p->datacdr)
  { int32 rpt = p->rpt, sort = p->sort, len = p->len, val = p->val;
    switch (sort)
    {   case LIT_LABEL:
            maybe_export((Symstr *)rpt);
            pr_asmname((Symstr *)rpt);
            fprintf(asmstream, ":");
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
                    if (rpt != 0) fprintf(asmstream, "\n");
                }
            }
            else syserr(syserr_asm_trailer1, (long)rpt, (long)val);
            break;
        case LIT_FPNUM:
        {   int32 *p = ((FloatCon *)val) -> floatbin.irep;
            decode_DC(p[0]);
            if (annotations)
                fprintf(asmstream, " # %s", ((FloatCon *)val) -> floatstr);
            if (len == 8) fprintf(asmstream, "\n"), decode_DC(p[1]);
            break;
        }
        case LIT_ADCON:              /* (possibly external) name + offset */
            if (rpt != 1) syserr(syserr_asm_trailer2);
            decode_DCA((Symstr *)len, val);
            break;
    }
    fprintf(asmstream, "\n");
  }
}

void asm_trailer()
{
  int32 code_dotwordcnt = dotwordcnt;
  if (vardata.size != 0) asm_datasect(vardata.head, 3, ".data");
#ifdef CONST_DATA_IN_CODE
  if (constdata.size != 0) asm_datasect(constdata.head, 3, ".rodata");
#endif
#ifdef TARGET_HAS_BSSxxxxxxxxXAP_OR_NEC
  if (bssdata.size != 0) asm_datasect(bssdata.head, 2, ".bss");
#else
  pr_common_defs();		/* Any Common stuff output now */
#endif

  asm_outextern();

  if (mips_opt & 2)
  { fprintf(asmstream,
"\n # MIPS16 code size %ld (incl. %ld as .word), \
data size %ld+%ld+%ld (bytes)\n",      /* .data,.rodata,.bss */
            codebase, code_dotwordcnt*4, vardata.size,
#ifdef CONST_DATA_IN_CODE
            constdata.size,
#else
            0L,
#endif
#ifdef TARGET_HAS_BSSxxxxxxxxXAP_OR_NEC
            bssdata.size
#else
            0L
#endif
           );
    fprintf(asmstream,
" # Bytes wasted because couldn't use: SAVE %ld, RESTORE %ld\n",
                       wasted_in_save, wasted_in_restore);
#ifdef JUGGLECNT
    fprintf(asmstream, " # Bytes spent juggling registers %ld\n",
                       jugglecnt*2);
#endif

  }

  if (asm_error) syserr(syserr_asm_confused);
}

#endif

/* end of mips/asm.c */
