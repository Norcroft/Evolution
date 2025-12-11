/* Do this first since needs #include with DEFINE_MSG_COMPRESSION_TABLE */
#include "../mip/misc.c"
/* Do this second since needs #include with DEFINE_JOPTABLE             */
#include "../mip/jopprint.c"

#include "../cfe/lex.c"
#include "../cfe/pp.c"
#include "../cfe/sem.c"
#include "../cfe/simplify.c"
#include "../cfe/syn.c"
#include "../cfe/vargen.c"

#include "../c4p/asm.c"
#include "../c4p/gen.c"
#include "../c4p/mcdep.c"

#include "../mip/elfobj.c"
#include "../mip/dbx.c"

#include "../mip/aetree.c"
#include "../mip/bind.c"
#include "../mip/builtin.c"
#include "../mip/cg.c"
#include "../mip/codebuf.c"
#include "../mip/compiler.c"
#include "../mip/cse.c"
#include "../mip/csescan.c"
#include "../mip/driver.c"
#include "../mip/flowgraf.c"
#include "../mip/fname.c"
#include "../mip/ieeeflt.c"
#include "../mip/regalloc.c"
#include "../mip/regsets.c"
#include "../mip/sr.c"
#include "../mip/store.c"
#include "../mip/version.c"

/* stop name clashes between compiler and library */
#undef intofdigit
#undef memclr
#undef int32
#undef unsigned32
#undef bool
#undef _fprintf
#undef _sprintf
#undef _vfprintf
#undef _vsprintf
#define flt_sum _flt_sum
#define flt_difference _flt_difference

#include "../../c4plib/liball.c"
