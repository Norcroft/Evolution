#include <ctype.h>
#include <signal.h>

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "host.h"
#include "gen.h"

int32 config;

bool mcdep_config_option(char name, char tail[]) {
    if (safe_toupper(name) != 'M')
	return NO;
    else {
	int c = safe_toupper(tail[0]);
	int cc;
	int value;

	switch (c) {
	  case 'M': case 'P': case 'C': case 'S':   /* boolean options */
	    if (tail[1] == '1')
		value = 1;
	    else if (tail[1] == '0')
		value = 0;
	    else
		return NO;
	    switch (c) {
	      case 'M': peep_mmx   = value; break;   /* MMX scheduling */
	      case 'P': peep_sched = value; break;   /* scheduling at all */
	      case 'C': peep_const = value; break;   /* const peepholing */
	      case 'S': peep_slave = value; break;   /* register slaving */
	    }
	    return YES;
	  case 'A':		       /* code alignment */
	    cc = safe_toupper(tail[1]);
	    if (cc == 'C' || cc == 'U') {
		if (tail[2+strspn(tail+2, "0123456789")])
		    return NO;
		value = atoi(tail+2);
		if (value & (value-1))
		    return NO;	       /* not a power of two */
		if (cc == 'C')
		    cond_align = value;
		else
		    uncond_align = value;
	    } else
		return NO;
	}
    }
}

void config_init(void) {
    				       /* stub */
}

KW_Status mcdep_keyword(const char *key, int *argp, char **argv) {
    return KW_NONE;		       /* stub, nicked from alpha, looks ok */
}
