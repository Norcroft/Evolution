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
#include "mcdpriv.h"
#include "builtin.h"
#include "version.h"
#include "errors.h"

#include "gen.h"

FILE *asmstream;

static void display_label (Symstr *name, char *after) {
    ExtRef *ext = symext_(name);
    if (ext && (ext->extflags & xr_defext))
	fprintf(asmstream, "global $%s\n", symname_(name));
    fprintf(asmstream, "$%s%s", symname_(name), after);
}

void asm_header() {
    fprintf(asmstream, "section .text align=%d\n\n",
	    uncond_align > cond_align ? uncond_align : cond_align);
}

void asm_trailer() {
    ExtRef *x;
    DataInit *d;
    int32 *ip;

    for (x = obj_symlist; x != NULL; x = x->extcdr)
	if (!(x->extflags & (xr_defext | xr_defloc)))
	    fprintf(asmstream, "extern $%s\n", symname_(x->extsym));

    fprintf(asmstream, "\nsection .data\n\n");

    for (d = datainitp; d != NULL; d = d->datacdr) {
	switch (d->sort) {
	  case LIT_LABEL:
	    display_label((Symstr *)(d->rpt), ":\n");
	    break;
	  case LIT_BBBB: case LIT_HH: case LIT_BBH: case LIT_HBB:
	    if (d->rpt != 1)
		fprintf(asmstream, "times %d ", d->rpt);
	    fprintf(asmstream, "dd 0x%08lX\n",
		    totargetsex(d->val, d->sort));
	    break;
	  case LIT_NUMBER:
	    if (d->rpt != 1)
		fprintf(asmstream, "times %d ", d->rpt);
	    fprintf(asmstream, "dd 0x%08lX\n", d->val);
	    break;
	  case LIT_ADCON:
	    if (d->rpt != 1)
		fprintf(asmstream, "times %d ", d->rpt);
	    fprintf(asmstream, "dd %d+$%s\n", d->val,
		    symname_(((Symstr *)(d->len))));
	    break;
	  case LIT_FPNUM:
	    ip = ((FloatCon *)(d->val))->floatbin.irep;
            if (d->len == 8)
		fprintf(asmstream, "dd 0x%08lX,0x%08lX\n", ip[1], ip[0]);
	    else
		fprintf(asmstream, "dd 0x%08lX\n", ip[0]);
            break;
	  default:
	    fprintf(asmstream, "%%error data sort=%d\n", d->sort);
	}
    }

    if (bss_size != 0) {
	ExtRef *x;

	fprintf(asmstream, "\nsection .bss\n\nresb %d-$+$$\n\n", bss_size);

	for (x = obj_symlist; x != NULL; x = x->extcdr)
	    if (x->extflags & xr_bss) {
		display_label(x->extsym, " equ $$+");
		fprintf(asmstream, "%d\n", x->extoffset);
	    }
    }
}

void display_assembly_code(Symstr *name) {
    i386ins *p;

    /*
     * Align function entry point
     */
    fprintf(asmstream, "times ($$-$) & %d nop\n", uncond_align-1);

    display_label (name, ":\n");

    for (p = funhead; p; p = p->next) {
	if (p->scheduled) {
	    if (p->scheduled != NO_INSN)
		fprintf(asmstream, "%s\n", p->scheduled->text);
	} else if (!(p->peepflags & PEEP_DISCARDED))
	    fprintf(asmstream, "%s\n", p->text);
    }

    while (funhead) {
	p = funhead;
	funhead = funhead->next;
	free(p);
    }

    fprintf(asmstream, "\n");

    funhead = funtail = NULL;
}
