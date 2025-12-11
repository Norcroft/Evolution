
/* c4p/asm.c: Copyright (C) Codemist Ltd., 1994-1999.                   */

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

static int32 fncount;         /* maybe should be more global */
static bool asm_error;
char asm_codesegname[sizeof asm_codesegname];

static void asm_blank(int32 n)
{   while (n-- > 0) fprintf(asmstream, ";\n");
}

static int32 asm_padcol8(int32 n)
{   if (!annotations) n = 7;      /* compact the asm file */
    while (n<8) fputc(annotations ? ' ':'\t', asmstream), n++;
    return n;
}

static int asm_prchar(char *d, int c)
{   switch (c)
    {   case '\n': c = 'n'; break;
        case '\r': c = 'r'; break;
        case '\0': c = '0'; break;
        case '\\': break;
        case '\'': break;
        case '\"': break;
        default: if (isprint(c))
                 {   if (d) d[0] = c, d[1] = 0;
                     else fputc(c, asmstream);
                     return 1;
                 }
                 if (d) sprintf(d, "\\x%.2X", c);
                 else fprintf(asmstream, "\\x%.2X", c);
                 return 4;
    }
    if (d) sprintf(d, "\\%c", c); else fprintf(asmstream, "\\%c", c);
    return 2;
}

static int32 asm_prname(Symstr *sym)
{   char *s = sym == 0 ? (asm_error = 1, "?") : symname_(sym);
    /* On C4P all C names are prefixed with '_', so they  can't         */
    /* clash with register names; otherwise add anti-clash code here.   */
    return fprintf(asmstream, "_%s", s);
}

/* asm_define() does "sym:" and does ".export sym" if needed.           */
static void asm_define(Symstr *sym)
{   asm_prname(sym);
    fprintf(asmstream, ":\n");
    /* fputc(':', asmstream); */
}

static void asm_reference(Symstr *sym)
{    asm_prname(sym);
}

static unsigned32 xroffset = 0, xrflavour = 0;

/* decode_external checks if (and by what) a location is to be relocated.  */
/* Since it exploits xroffset only call via decode_as_addr.                */
static Symstr *decode_external(int32 p)
{
    CodeXref *x;
    for (x = codexrefs; x!=NULL; x = x->codexrcdr) {
      int32 q = x->codexroff & 0x00ffffff;
      if (q < p) break;
      if (q == p) {
        switch (xrflavour = x->codexroff & 0xff000000) {
        case X_DataAddr:
        case X_ZPOFFb: case X_ZPOFFh: case X_ZPOFFw:
        case X_PCreloc:
        case X_absreloc:
          xroffset = x->codexrlitoff; break;
        default:
          xroffset = 0; break;
        }
/*          fprintf(stderr,"found %s + %lx\n",symname_(x->codexrsym), */
/*                  xroffset);                                        */
        return x->codexrsym;
      }
    }
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
      return fprintf(asmstream, "L%ldF%ld",
                     (long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
  }
  asm_error = 1;
  return fprintf(asmstream, "?L%06lx ; %lx -> %lx",
                 offset+codebase, loc, offset);
}

static bool decode_as_addr(int32 addr, int32 off)
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
      addr = 0;         /* /* */

      for ( p = asm_lablist ; p != NULL ; p = p->labcdr) {
        LabelNumber *lab = p->labcar;
/*          printf("cmp %ld %ld\n", codebase+(lab->u.defn & 0x00ffffff)+addr, xroffset); */
        if (codebase+(lab->u.defn & 0x00ffffff)+addr == xroffset) {
          fprintf(asmstream, "L%ldF%ld",
                  (long)(lab_name_(lab) & 0x7fffffff), (long)fncount);
          if (addr != 0) fprintf(asmstream, "%+ld", (long)addr);
          return 1;
        }
      }
    }

  if (name != 0) {
    asm_reference(name);   /* maybe_import(first use) */
    if (xroffset != 0) fprintf(asmstream, "%+ld", (long)xroffset);
    return 1;
  }

  return 0;               /* doesn't decode as an ext. addr. */
}

/* Disassembler routines                                                 */

static int instruction_length(int32 w)
{
    int32 ww = w&0xF800;
    if (ww == OP_MOVI_L || ww == OP_JUMP_L || ww == OP_CALL_L) return 4;
    if (ww== OP_ORr && (w&0x1f) == 5 && (w&0xff) == 0xa5) return 6;
    return 2;
}

static void decode0(char *ops, int r1, int r2, int w)
{
    fprintf(asmstream, "%s", ops);
}

static void decode_v(char *ops)
{
    fprintf(asmstream, "%s", ops);
}

static void decode_i(char *ops, int i)
{
    fprintf(asmstream, "%s\t%d", ops, i);
}

static void decode_r(char *ops, int rd)
{
    fprintf(asmstream, "%s\tr%d", ops, rd);
}

static void decode_rr(char *ops, int rd, int rs)
{
    fprintf(asmstream, "%s\tr%d,r%d", ops, rd, rs);
}

static void decode_rri(char *ops, int rd, int rs, int d)
{
    fprintf(asmstream, "%s\tr%d,(r%d%+d)", ops, rd, rs, d);
}

static void decode_ri(int32 p, char *ops, int rd, int d)
{
    fprintf(asmstream, "%s\tr%d,(", ops, rd);
    if (!decode_as_addr(d, p)) fprintf(asmstream, "%ld", (long)d);
    fprintf(asmstream, ")");
}

static void decode_ri2(int32 p, char *ops, int rd, int d)
{
    fprintf(asmstream, "%s\tr%d,", ops, rd);
    if (!decode_as_addr(d, p)) fprintf(asmstream, "%ld", (long)d);
}

static void decode_rir(char *ops, int rd, int rs, int d)
{
    fprintf(asmstream, "%s\t(r%d%+d),r%d", ops, rs, d, rd);
}

static void decode_ir(int32 p, char *ops, int rd, int d)
{
    fprintf(asmstream, "%s\t(", ops);
    if (!decode_as_addr(d, p)) fprintf(asmstream, "%ld", (long)d);
    fprintf(asmstream, "),r%d", rd);
}

#define u5_(w) (int)((w) & 0x1f)
#define u8_(w) (int)((w) & 0xff)
#define s8_(w) (int)((((w) & 0xff) ^ 0x80) - 0x80)

/* @@@ Beware code below -- int shifts asssumed *signed*.               */

static int decode_instruction(int32 w, int32 w1, int32 w2, int32 p)
{
    int32 wop = (w & 0xF800);
    int rd = (int)((w>>8) & 7);
    int rs = (int)((w>>5) & 7);
    char *ops;
    switch (wop) {
    default:
      ops = "?unknown";
      break;
    case OP_LD_Br:
      decode_rri("load.b", rd, rs, u5_(w));
      return 2;
    case OP_LD_Bs:
      decode_ri(p, "load.b", rd, u8_(w));
      return 2;
    case OP_LD_Wr:
      decode_rri("load.w", rd, rs, u5_(w));
      return 2;
    case OP_LD_Ws:
      decode_ri(p, "load.w", rd, u8_(w));
      return 2;
    case OP_LD_Lr:
      decode_rri("load.d", rd, rs, u5_(w));
      return 2;
    case OP_LD_Ls:
      decode_ri(p, "load.d", rd, u8_(w));
      return 2;
    case OP_ST_Br:
      decode_rir("store.b", rd, rs, u5_(w));
      return 2;
    case OP_ST_Bs:
      decode_ir(p, "store.b", rd, u8_(w));
      return 2;
    case OP_ST_Wr:
      decode_rir("store.w", rd, rs, u5_(w));
      return 2;
    case OP_ST_Ws:
      decode_ir(p, "store.w", rd, u8_(w));
      return 2;
    case OP_ST_Lr:
      decode_rir("store.d", rd, rs, u5_(w));
      return 2;
    case OP_ST_Ls:
      decode_ir(p, "store.d", rd, u8_(w));
      return 2;
    case OP_MOVI_S:
      decode_ri2(p, "movi.b", rd, s8_(w));
      return 2;
    case OP_MOVI_L:
      decode_ri2(p, "movi.l", rd, (int)((w<<24)>>8 | w1));
      return 4;
    case OP_JUMP_S:
      fprintf(asmstream, "jump.s\t%ld,", (w>>8)&7);
      printlabelname((((w&0xff)<<24)>>23)+p+2, p);
      return 2;
    case OP_CALL_S:
      fprintf(asmstream, "call.s\t%ld,", (w>>8)&7);
      decode_as_addr((((w&0xff)<<24)>>23)+p+2, p);
      return 2;
    case OP_JUMP_L:
      fprintf(asmstream, "jump.l\t%ld,", (w>>8)&7);
      if (!decode_as_addr(((w<<24)>>8 | w1)+4+p, p))
        printlabelname(((w<<24)>>8 | w1)*2+4+p, p);
      return 4;
    case OP_CALL_L:
      fprintf(asmstream, "call.l\t%ld,", (w>>8)&7);
      decode_as_addr(((w<<24)>>8 | w1)*2+4+p, p);
      return 4;
    case OP_ADDr:
      if (w&1) decode_ri2(p, "add", rd, u8_(w) >> 1);
      else decode_rr("add", rd, rs);
      return 2;
    case OP_SUBr:
      if (w&1) decode_ri2(p, "sub", rd, u8_(w) >> 1);
      else decode_rr("sub", rd, rs);
      return 2;
    case OP_ADCr:
      if (w&1) decode_ri2(p, "adc", rd, u8_(w) >> 1);
      else decode_rr("adc", rd, rs);
      return 2;
    case OP_SBCr:
      if (w&1) decode_ri2(p, "sbc", rd, u8_(w) >> 1);
      else decode_rr("sbc", rd, rs);
      return 2;
    case OP_CMPr:
      if (w&1) decode_ri2(p, "cmp", rd, u8_(w) >> 1);
      else decode_rr("cmp", rd, rs);
      return 2;
    case OP_SHIFT:
      {
        int k = -1, imm = 0;
        switch (w&0x1f) {
        case 0x08:
          ops = "sla";
          k = (int)(w>>5)&0x7;
          imm = 1;
          break;
        case 0x0e:
          ops = "sra";
          k = (int)(w>>5)&0x7;
          imm = 1;
          break;
        case 0x10:
          ops = "sla";
          k = (int)(w>>5)&0x7;
          break;
        case 0x16:
          ops = "sra";
          k = (int)(w>>5)&0x7;
          break;

        case 0x18:
          ops = "sla";
          goto byteshift;
        case 0x1e:
          ops = "sra";
        byteshift:
          k = (int)(((w>>5)&0x7) * 8) + 16 - 1;  /* -1 for the +1 below */
          imm = 1;
          if (k<=24) break;
          /* drop through */
        default:
          ops = "src";
          if ((w&0x98)!=0) break;
          {
            char lorr = (w&4) ? 'r' : 'l';
            char newb = "zlmc"[w&3];
            char size = "bwl"[(w>>5)&3];
            fprintf(asmstream, "s%c%c.%c\tr%d", lorr, newb, size, rd);
            return 2;
          }
        }
        if (k>=0) {
          if (imm) decode_ri2(p, ops, rd, k+1);
          else     decode_rr(ops, rd, k);
          return 2;
        }
      }
      break;
    case OP_MUL:
      switch ((w>>8)&7) {
      case 0:
        decode_v("mul");
        return 2;
      case 1:
        decode_v("macc");
        return 2;
      case 2:
        decode0(ops="mulf", rd, rs, (int)w);
        break;
      case 3:
        decode0(ops="naccf", rd, rs, (int)w);
        break;
      case 4:
        decode0(ops="mulx", rd, rs, (int)w);
        break;
      case 5:
        decode0(ops="maccx", rd, rs, (int)w);
        break;
      default:
        ops = "?mul";
        break;
      }
      break;
    case OP_ORr:
      switch (w&0x1f) {
      case 0:
        decode_rr("or", rd, rs);
        return 2;
      case 1:
        decode_rr("and", rd, rs);
        return 2;
      case 2:
        decode_rr("xor", rd, rs);
        return 2;
      case 3:
        decode_rr("tst", rd, rs);
        return 2;
      case 5:
        switch (w&0xff) {
        case 5:
          decode_r("push", rd);
          return 2;
        case 0x25:
          decode_r("pop", rd);
          return 2;
        case 0xa5:
          decode_ri2(p, "movi.d", rd, (int)(w1 | (w2<<16)));
          return 6;
        case 0xe5:
          decode_i("int", rd);
          return 2;
        default:
          ops = "?5?";
        }
        break;
      case 7:
        decode_rr("mov", rd, rs);
        return 2;
      default:
        ops = "?or";
        break;
      }
      break;
    case OP_NOP:
      switch (w&0xff) {
      case 0x00:
        decode_v("nop");
        return 2;
      case 0x01:
        decode_v("ret");
        return 2;
      case 0x02:
        decode_v("reti");
        return 2;
      case 0x03:
        decode_v("ei");
        return 2;
      case 0x04:
        decode_v("di");
        return 2;
      case 0x07:
        decode_v("mov\tsp,r0");
        return 2;
      case 0x08:
        decode_v("mov\tr0,sp");
        return 2;
      case 0x09:
        decode_v("mov\tpc,r0");
        return 2;
      case 0x0A:
        decode_v("mov\tr0,pc");
        return 2;
      case 0x0B:
        decode_v("waitm");
        return 2;
      case 0x10:
        ops = "round";
        break;
      default:
        ops = "?nop";
        break;
      }
      break;
    }
    fprintf(asmstream, ".c4p\t0x%.4lx ?? (%s)", w, ops);
    asm_error = 1;
    return 2;
}

static Symstr *asm_DS_label;
static int32 asm_DS_count;

#define asm_align(len) ((len) & 1 ? 1 : (len) & 2 ? 2 : 4)
/* .comm name,size,alignment or .lcomm name,size,alignment */
/* BEWARE: AM assumes that preceding .sbss/.bss sets segment to be used. */
static void asm_comm(Symstr *sym)
{   int32 flags = symext_(sym) ? symext_(sym)->extflags : 0;
    fprintf(asmstream, "\t%s\t",
        (!(flags & xr_defloc) && (flags & xr_defext)) ? ".comm" : ".lcomm");
    asm_prname(sym);
    fprintf(asmstream, ",");
}

static void decode_DS(int32 len)
{   if (asm_DS_label && len == 0)
    {   asm_define(asm_DS_label);
        /* fprintf(asmstream, ":\n"); */
        asm_DS_label = 0;
        return;
    }
    if (len != 0)
    {   if (asm_DS_label) asm_comm(asm_DS_label); asm_DS_label = 0;
        /* asm_padcol8(0); */
        fprintf(asmstream, "%ld,%d\n", (long)len, asm_align(len));
    }
}

static void decode_DC(int32 len, int32 w1, int32 w2, int genflag, int32 sort)
{ if (genflag & 1)
  {   if (asm_DS_label) asm_define(asm_DS_label), asm_DS_label = 0;
      /* asm_padcol8(0); */
      switch (sort)
      {
case LIT_BBBB:
case LIT_HBB:
case LIT_BBH:
          fprintf(asmstream, "\t.byte\t0x%.2lx,0x%.2lx,0x%.2lx,0x%.2lx",
                              (long)w1>>0&255, (long)w1>>8&255,
                              (long)w1>>16&255, (long)w1>>24&255);
          break;
#if (alignof_toplevel <= 1)
case LIT_BBBX:
case LIT_HBX:
          fprintf(asmstream, "\t.byte\t0x%.2lx,0x%.2lx,0x%.2lx",
                              (long)w1>>0&255, (long)w1>>8&255,
                              (long)w1>>16&255);
          break;
case LIT_BX:
          fprintf(asmstream, "\t.byte\t0x%.2lx", (long)w1>>0&255);
          break;
case LIT_HH:
          fprintf(asmstream, "\t.word\t0x%.4lx,0x%.4lx",
                              (long)w1>>0&0xffff, (long)w1>>16&0xffff);
          break;
#endif
#if (alignof_toplevel <= 2)
case LIT_BBX:
          fprintf(asmstream, "\t.byte\t0x%.2lx,0x%.2lx",
                              (long)w1>>0&255, (long)w1>>8&255);
          break;

case LIT_HX:
          fprintf(asmstream, "\t.word\t0x%.4lx", (long)w1>>0&0xffff);
          break;
#endif
default:
          fprintf(asmstream, "\t.long\t0x%.8lx", (long)w1);
          break;
      }
      if (sort == LIT_FPNUM) fprintf(asmstream, ",0x%.8lx", (long)w2);
  }
  else asm_DS_count += len; /* decode_DS(len); */
}

static void decode_DCA(Symstr *s, int32 w, int genflag)
{
  if (genflag & 1)
  {   if (asm_DS_label) asm_define(asm_DS_label), asm_DS_label = 0;
      /* asm_padcol8(0); */
      fprintf(asmstream, "\t.long\t");
      asm_reference(s);         /* maybe_import(first use) */
      if (w!=0) fprintf(asmstream, "%+ld", (long)w);
  }
  else asm_DS_count += sizeof_ptr; /* decode_DS(sizeof_ptr); */
}

/* exported functions ...*/

void display_assembly_code(Symstr *name)
{   int32 q;
    int32 qsiz;
    LabList *asm_lablist2 = 0;
    asm_blank(2);
    if (asm_codesegname[0] != 0)
    {   fprintf(asmstream, "\t.section\t.%s\n", asm_codesegname);
        asm_codesegname[0] = 0;
        asm_blank(1);
    }
    if (name != 0)   /* may be 0 for string literals from static inits   */
    {   asm_lablist2 = asm_lablist;
        fncount++;
        if (annotations)
            fprintf(asmstream, "%.6lx  %20s", (long)codebase, "");
        asm_define(name);
        /* fprintf(asmstream, ":\n"); */
    }
    for (q=0; q < codep; q+=qsiz)    /* q is now a BYTE offset */
    {   const int32 w = code_hword_(q),
                    f = code_flag_(q);
        int32 w2, w3;
        qsiz = instruction_length(w);
        w2 = (q+2) < codep ? code_hword_(q+2) : 0;
        w3 = (q+4) < codep ? code_hword_(q+4) : 0;
        {
          int32 labq;
          LabelNumber *t;
          while (asm_lablist2 &&
                 (t = asm_lablist2->labcar,
                  labq = t->u.defn & 0x00ffffff) <= q)
            { if (annotations)
                fprintf(asmstream, "%18s", "");
              fprintf(asmstream, "L%ldF%ld:\n",
                      (long)(lab_name_(t) & 0x7fffffff), (long)fncount);
              if (labq != q)
                syserr(syserr_asmlab, (long)labq);
              asm_lablist2 = asm_lablist2->labcdr;
            }
        }
        if (annotations)
        {   fprintf(asmstream, "%.6lx  ", (long)(q + codebase));
            if (qsiz > 4)
                fprintf(asmstream, "%.4lx %.4lx %.4lx",
                                   (long)w, (long)w2, (long)w3);
            else if (qsiz > 2)
                fprintf(asmstream, "%.4lx %.4lx     ", (long)w, (long)w2);
            else
                fprintf(asmstream, "%.4lx          ", (long)w);
        }
        fputc('\t', asmstream);
        switch (f) {
          case LIT_OPCODE:
            qsiz = decode_instruction(w, w2, w3, q);
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

void asm_header()
{
    asm_error = 0;
    fncount = 0;
    if (annotations) return;   /* do not bore interactive user */
    fprintf(asmstream, "; generated by %s\n", CC_BANNER);
    asm_blank(1);
    fprintf(asmstream, "\t.file \"%s\"\n", sourcefile);
    fprintf(asmstream, "\t.text\n");
}

/* (not exported) */

static void asm_outextern()
{   ExtRef *x,*seen;
    for (x = obj_symlist, seen = 0; x != 0; x = x->extcdr)
    {   int32 flags = x->extflags;
        if (!(flags & (xr_defloc|xr_defext)))
        {   int32 sz = x->extoffset;
            seen = x;
            fprintf(asmstream, "\t%s\t", sz!=0 ? ".comm" : ".extern");
            asm_reference(x->extsym);
            if (sz!=0) fprintf(asmstream, ",%lu,%d",
                               (long)sz, asm_align(sz)); /* asm_comm() */
            fprintf(asmstream, "\n");
        }
    }
    if (seen) asm_blank(1);
    for (x = obj_symlist, seen = 0; x != 0; x = x->extcdr)
    {   int32 flags = x->extflags;
        if (!(flags & xr_defloc) && (flags & xr_defext) &&
            !(flags & xr_bss))
        {   seen = x;
            fprintf(asmstream, "\t.globl\t");
            asm_reference(x->extsym);
            fprintf(asmstream, "\n");
        }
    }
    if (seen) asm_blank(1);
}

static void asm_datasect(DataInit *p, int genflag, char *segname)
{ int32 i = 0;
  if (segname[0] == '.')
      fprintf(asmstream, "\t%s\n", segname);    /* standard section */
  else
      fprintf(asmstream, "\t.section\t.%s\n", segname); /* user section */
  asm_DS_label = 0;
  asm_DS_count = 0;
  for (; p != 0; p = p->datacdr)
  { int32 rpt = p->rpt, sort = p->sort, len = p->len, val = p->val;
    switch (sort)
    {   case LIT_LABEL:
            if (genflag & 2)
            {   decode_DS(asm_DS_count);
                asm_DS_label = (Symstr *)rpt, asm_DS_count = 0;
#ifdef TARGET_HAS_BSS
/* AM: the next line is a nasty hack for NEC assembler syntax.          */
                if (asm_DS_label == bssdata.segsym ||
#ifdef TARGET_HAS_C4P_SECTS
                    asm_DS_label == ibssdata.segsym ||
#endif
                    asm_DS_label == zbssdata.segsym)
                  asm_DS_label = 0;
#endif
            }
            continue;
        default:  syserr(syserr_asm_trailer, (long)sort);
        case LIT_BBBB:
        case LIT_BBH:
        case LIT_HBB:
        case LIT_HH:
            val = totargetsex(val, (int)sort);
        case LIT_NUMBER:
            if (len != 4) syserr(syserr_datalen, (long)len);
        case_any_number:
            for (; rpt > 0; rpt--, i+=len)
            {   decode_DC(len, val, 0, genflag, sort);
                if ((genflag & 1) && rpt > 1)
                    fprintf(asmstream, "\n");
            }
            break;
#if (alignof_toplevel <= 2)
        case LIT_BBX:
        case LIT_HX:
            val = totargetsex(val, (int)sort);
            if (len != 2) syserr(syserr_datalen, (long)len);
            goto case_any_number;
#endif
#if (alignof_toplevel <= 1)
        case LIT_BX:
            val = totargetsex(val, (int)sort);
            if (len != 1) syserr(syserr_datalen, (long)len);
            goto case_any_number;
        case LIT_BBBX:
        case LIT_HBX:
            val = totargetsex(val, (int)sort);
            if (len != 3) syserr(syserr_datalen, (long)len);
            goto case_any_number;
#endif
        case LIT_FPNUM:
        {   int32 *p = ((FloatCon *)val) -> floatbin.irep;
            if (rpt != 1) syserr(syserr_asm_trailer2);
            decode_DC(len, p[0], len==8 ? p[1]:0, genflag,
                      len==8 ? sort:LIT_NUMBER);
            if (genflag & 1)
                fprintf(asmstream, "\t-- %s", ((FloatCon *)val) -> floatstr);
            i += len;
            break;
        }
        case LIT_ADCON:              /* (possibly external) name + offset */
            if (rpt != 1) syserr(syserr_asm_trailer2);
            decode_DCA((Symstr *)len, val, genflag);
            i += sizeof_ptr;
            break;
    }
    if (genflag & 1) fprintf(asmstream, "\n");
  }
  decode_DS(asm_DS_count);
  asm_blank(1);
}

void asm_trailer()
{ /* AM: I hope the next line does not upset anyone	*/
  obj_symlist = (ExtRef *) dreverse((List *)obj_symlist);
                                /* oldest = smallest numbered first */
  asm_blank(1);

  if (zvardata.size != 0) asm_datasect(zvardata.head, 3, ".sdata");
  if (zconstdata.size != 0) asm_datasect(zconstdata.head, 3, ".sconst");
#ifdef TARGET_HAS_BSS
  if (zbssdata.size != 0) asm_datasect(zbssdata.head, 2, ".sbss");
#endif

  if (vardata.size != 0) asm_datasect(vardata.head, 3, ".data");
  if (constdata.size != 0) asm_datasect(constdata.head, 3, ".const");
#ifdef TARGET_HAS_BSS
  if (bssdata.size != 0) asm_datasect(bssdata.head, 2, ".bss");
#endif

#ifdef TARGET_HAS_C4P_SECTS
  {   int i;
      for (i = 6; i<TARGET_NUM_DATASECTS; i++)
        if (datasects[i].size != 0)
          asm_datasect(datasects[i].head, (datasects[i].xrarea & xr_bss) ? 2:3,
                       datasects[i].elfsegname);
/*
**  if (ivardata.size != 0) asm_datasect(ivardata.head, 3, ".section\t.idata");
**  if (iconstdata.size != 0) asm_datasect(iconstdata.head, 3, ".section\t.iconst");
**  if (ibssdata.size != 0) asm_datasect(ibssdata.head, 2, ".section\t.ibss");
*/
  }
#endif

  asm_outextern();
/*  fprintf(asmstream, "\t.end\n"); */
  if (asm_error) syserr(syserr_asm_confused);
}

#endif

/* end of c4p/asm.c */
