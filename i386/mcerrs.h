
/*
 * i386/mcerrs.h - prototype for machine-specific error messages file
 */

%O  /* Ordinary error messages - mapped onto numeric codes */

/*
 * The following message is always machine specific since it may include
 * information about the source of support for the product.
 */
#define misc_disaster_banner   "\n\
*************************************************************************\n\
* The compiler has detected an internal inconsistency.  This can occur  *\n\
* because it has run out of a vital resource such as memory or disk     *\n\
* space or because there is a fault in it.  If you cannot easily alter  *\n\
* your program to avoid causing this rare failure, please contact your  *\n\
* dealer.  The dealer may be able to help you immediately and will be   *\n\
* able to report a suspected compiler fault to the support centre.      *\n\
*************************************************************************\n\
\n"

%S  /* System failure messages - text not preserved */

#define syserr_local_address "local_address %lx"

/* end of i386/mcerrs.h */
