/*
 * Copyright (C) Advanced RISC Machines Limited and
 *               Codemist Limited, 19106.
 * All rights reserved.
 *
 * errors.h, created by genhdrs on Sun Feb  5 16:22:17 2006
 */

#ifndef _msgs_LOADED
#define _msgs_LOADED 1

typedef char *syserr_message_type;
extern void syserr(syserr_message_type errcode, ...);

/*
 * C compiler error prototype file (miperrs.h)
 * Copyright (C) Codemist Ltd, 1988.
 */

/*
 * RCS $Revision: 1.9 $ Codemist 152
 * Checkin $Date: 93/09/29 16:21:58 $
 * Revising $Author: lsmith $
 */

/*
 * This file is input to the genhdrs utility, which can compress error
 * strings (but leaving escape sequences alone so that format checking can
 * occur) and optionally mapping syserr messages onto numeric codes
 * (in case somebody wants to save the about 4Kbytes of memory involved).
 */

/* AM: (after discussion with LDS) It would seem that error texts below */
/* which (seriously) take 2 or more arguments should be of the form     */
/*    #define ermsg(a,b,c) "ho hum %s had a %s %s", a, b, c             */
/* etc. to allow different sentence order in other (natural) languages. */

/* One nice thing would be to have a variant form of $r (etc) which did */
/* not quote its arg to avoid many uses of symname_() in the code.      */

#ifdef __CC_NORCROFT
  /*
   * The next procedure takes a string as a format... check args.
   */
#pragma -v3
#endif

/* cc_msg has been left in globals.h since it takes an uncompressed string */
extern void cc_rerr(char *errcode, ...);
extern void cc_ansi_rerr(char *errcode, ...);
extern void cc_warn(char *errcode, ...);
extern void cc_ansi_warn(char *errcode, ...);
extern void cc_pccwarn(char *errcode, ...);
extern void cc_err(char *errcode, ...);
extern void cc_fatalerr(char *errcode, ...);

#ifdef __CC_NORCROFT
  /*
   * End of procedures that take error strings or codes.
   */
#pragma -v0
#endif


      /* Map strings to offsets in compressed string table */

#define warn_usage_rw \
        "\027\235\013\361havi\216r@$b wr\215t\030 \201\200\020a\200\341\
\216\002\001\017v\030\035\274\364\030c\004\230\236"    /* "undefined behaviour: $b written and read without intervening sequence point" */
#define warn_usage_ww \
        "\027\235\013\361havi\216r@$b wr\215t\030 tw\237\004\341\216\002\
\001\017v\030\035\274\364\030c\004\230\236"    /* "undefined behaviour: $b written twice without intervening sequence point" */

#define bind_warn_extern_clash \
        "\207\017n \227\177h $r\305$r \227\177h\375ANSI 6 \252\016 m\006\
o\267e)"    /* "extern clash $r, $r clash (ANSI 6 char monocase)" */
#define bind_warn_unused_static_decl "\027\256\013e\016li\244\204a\015c \
\303\233\213\203$r"    /* "unused earlier static declaration of $r" */
#define bind_warn_not_in_hdr "\207\017n $r \212\303\016\013\036head\003"    /* "extern $r not declared in header" */
#define bind_warn_main_not_int "\207\017n '\242\001\026ne\005\014\217\346\
\376\026\270\272"    /* "extern 'main' needs to be 'int' function" */
#define bind_warn_label_not_used "l\320\253 $r w\240\235\013\353\212\256\
\005"    /* "label $r was defined but not used" */
/*
 * Note that when part of an error string MUST be stored as a regular
 * non-compressed string I have to inform the GenHdrs utility with %Z and %O
 * This arises when a string contains literal strings as extra sub-args.
 */
#define bind_warn_not_used(is_typedef,is_fn,binder) \
        "%s $b \303\016\013\353\212\256\005"    /* "%s $b declared but not used" */, \
      ((is_typedef) ? "typedef" : ((is_fn) ? "function" : "variable")),\
      binder
#define bind_warn_static_not_used "\204a\015c $b \303\016\013\353\212\256\
\005"    /* "static $b declared but not used" */
#define cg_warn_implicit_return "i\246\271\254\020t\367n\327\331\335\200\
%s()"    /* "implicit return in non-void %s()" */
#define flowgraf_warn_implicit_return "i\246\271\254\020t\367n\327\331\335\
\200\270\272"    /* "implicit return in non-void function" */
#define pp_warn_triglyph \
        "ANSI '%c%c%c\026tr\220\323ph \260'%c\026\300\344\206w\240\275\362\
\236\030d\005?"    /* "ANSI '%c%c%c' trigraph for '%c' found - was this intended?" */
#define pp_warn_nested_comment "\252\233c\214\274\364\030c\004%s \001\255\
d\004\266mm\360"    /* "character sequence %s inside comment" */
#define pp_warn_many_arglines \
        "(\230s\255b\273\356\010)@>= %lu \347e\014\203\242cr\021\357\221\
\360s"    /* "(possible error): >= %lu lines of macro arguments" */
#define pp_warn_redefinition "\020pe\304\235i\213\203#\235\004\242cr\021\
%s"    /* "repeated definition of #define macro %s" */
#define pp_rerr_redefinition "diff\003\035r\005ef\314\213\203#\235\004\242\
cr\021%s"    /* "differing redefinition of #define macro %s" */
#define pp_rerr_nonunique_formal "d\311\271\032\004\242cr\021\351\037\374\
\017@'%s'"    /* "duplicate macro formal parameter: '%s'" */
#define pp_rerr_define_hash_arg "\333\003\201\200\203# \212\242cr\021\351\
\037\374\017"    /* "operand of # not macro formal parameter" */
#define pp_rerr_define_hashhash "## \310rs\002\034l\177\002tok\030\327#\
\235\004body"    /* "## first or last token in #define body" */
#define pp_warn_ifvaldef "#if\024\025%s \242\210\001d\237\032\004tr\216\
b\202\350"    /* "#ifdef %s may indicate trouble..." */ /* MACH_EXTNS */
#define pp_warn_nonansi_header "N\302ANSI #\001\227ud\004<%s>"    /* "Non-ANSI #include <%s>" */
#define pp_warn_bad_pragma "Un\020\266gn\033\013#p\323g\242\375n\021'-\026\
\034\027k\205wn w\010d)"    /* "Unrecognised #pragma (no '-' or unknown word)" */
#define pp_warn_bad_pragma1 "Un\020\266gn\033\013#p\323g\242 -%c"    /* "Unrecognised #pragma -%c" */
#define pp_warn_unused_macro "#\235\004\242cr\021'%s\026\235\013\353\212\
\256\005"    /* "#define macro '%s' defined but not used" */
#define regalloc_warn_use_before_set "$b \242\210\346\256\013\361\241\004\
\361\035\274t"    /* "$b may be used before being set" */
#define regalloc_warn_never_used "$b \362\274\002\353nev\244\256\005"    /* "$b is set but never used" */
#define sem_warn_unsigned "ANSI s\367pr\033e@'l\006g\026$s '\027s\231\005\
\026yi\253d\014'l\006g'"    /* "ANSI surprise: 'long' $s 'unsigned' yields 'long'" */
#define sem_warn_format_type "actu\037\337$t m\033m\032\252e\014\241\242\
\002'%.*s'"    /* "actual type $t mismatches format '%.*s'" */
#define sem_warn_bad_format "Il\232\241\242\002\226\262\255\031'%%%c'"    /* "Illegal format conversion '%%%c'" */
#define sem_warn_incomplete_format "In\266\246\202t\004\241\242\002\204\
r\001g"    /* "Incomplete format string" */
#define sem_warn_format_nargs "F\010\242\002\020\364i\020\014%ld \374\017\
%s\305\353%ld giv\030"    /* "Format requires %ld parameter%s, but %ld given" */
#define sem_warn_addr_array "'&\026\027n\223\322s\016\210\260\325\034\016\
\323\210$e"    /* "'&' unnecessary for function or array $e" */
#define sem_warn_bad_shift(_m,_n) "shif\002\203$m b\210%ld \027\235\013\
\036ANSI C"    /* "shift of $m by %ld undefined in ANSI C" */,_m,_n
#define sem_warn_divrem_0 "div\033i\031b\210z\003o@$s"    /* "division by zero: $s" */
#define sem_warn_ucomp_0 "od\200\027s\231\013\266\246\016\033\031\341 0\
@$s"    /* "odd unsigned comparison with 0: $s" */
#define sem_warn_fp_overflow(op) "\306\032\035\245\002\370\201\002\363\306\
w@$s"    /* "floating point constant overflow: $s" */,op
#define sem_rerr_udiad_overflow(op,_a,_b,_c) "\027s\231\013\370\201\002\
\363\306w@$s"    /* "unsigned constant overflow: $s" */,op
#define sem_rerr_diad_overflow(op,_a,_b,_c) "s\231\013\370\201\002\363\306\
w@$s"    /* "signed constant overflow: $s" */,op
#define sem_rerr_umonad_overflow(op,_a,_b) "\027s\231\013\370\201\002\363\
\306w@$s"    /* "unsigned constant overflow: $s" */,op
#define sem_rerr_monad_overflow(op,_a,_b) "s\231\013\370\201\002\363\306\
w@$s"    /* "signed constant overflow: $s" */,op
#define sem_rerr_implicit_cast_overflow(_t,_a,_b) \
                                "i\246\271\254\340(\217$t\265\363\306w"    /* "implicit cast (to $t) overflow" */,_t
#ifdef EXTENSION_FRAC
#define sem_warn_fix_fail "\306\032\035\217\236egr\037(\034f\323c\265\226\
\262\255\031\363\306w"    /* "floating to integral (or frac) conversion overflow" */
#else
#define sem_warn_fix_fail "\306\032\035\217\236egr\037\226\262\255\031\363\
\306w"    /* "floating to integral conversion overflow" */
#endif
#define sem_warn_index_ovfl "\216t-of-bo\344off\274\002%ld\327add\020ss"    /* "out-of-bound offset %ld in address" */
#define sem_warn_low_precision "\211w\244p\020c\033i\031\036wid\244\226\
t\207t@$s"    /* "lower precision in wider context: $s" */
#define sem_warn_odd_condition "\256\004\203$s\327\226di\213\226t\207t"    /* "use of $s in condition context" */
#define sem_warn_void_context "n\021\255d\004eff\223\002\036\335\200\226\
t\207t@$s"    /* "no side effect in void context: $s" */
#define sem_warn_olde_mismatch "\357\221\030\002\201\200old-\204y\273\374\
\214m\033m\032\252@$e"    /* "argument and old-style parameter mismatch: $e" */
#define sem_warn_uncheckable_format \
        "'\351\032\026\357. \217pr\236f/sc\201\025etc. \362v\016i\320\202\
\305s\021c\201\212\346\252\223k\005"    /* "'format' arg. to printf/scanf etc. is variable, so cannot be checked" */
#define sem_warn_narrow_voidstar "i\246\271\254\340f\365m\375\335\200*)\
\305C++ \241bids"    /* "implicit cast from (void *), C++ forbids" */
#define sem_warn_narrowing "i\246\271\254n\016\365w\035\267t@$s"    /* "implicit narrowing cast: $s" */
#define sem_warn_fn_cast \
        "$s@\340\361twe\030 \325\245\214\201\200\331\325obj\223t"    /* "$s: cast between function pointer and non-function object" */
#define sem_warn_pointer_int "\276\271\254\340\203\245\214\217\376'"    /* "explicit cast of pointer to 'int'" */
#define bind_err_extern_clash "\207\017n \227\177h $r\305$r\375\347k\244\
%ld \252\016%s)"    /* "extern clash $r, $r (linker %ld char%s)" */
#define bind_err_duplicate_tag "d\311\271\032\004\235i\213\203$s ta\023\
$b"    /* "duplicate definition of $s tag $b" */
#define bind_err_reuse_tag "\020-\256\035$s ta\023$b \240$s tag"    /* "re-using $s tag $b as $s tag" */
#define bind_err_incomplete_tentative \
        "\001\266\246\202t\004t\360a\015v\004\303\233\213\203$r"    /* "incomplete tentative declaration of $r" */
#define bind_err_type_disagreement "\337d\033ag\020em\030\002\260$r"    /* "type disagreement for $r" */
#define bind_err_duplicate_definition "d\311\271\032\004\235i\213\203$r"    /* "duplicate definition of $r" */
#define bind_err_duplicate_label "d\311\271\032\004\235i\213\203l\320\253 \
$r\330\277"    /* "duplicate definition of label $r - ignored" */
#define bind_err_unset_label "l\320\253 $r h\240\212\361\030 \274t"    /* "label $r has not been set" */
#define bind_err_undefined_static \
        "\204a\015c \325$b \212\235\013\206\354\207\017n"    /* "static function $b not defined - treated as extern" */
#define bind_err_conflicting_globalreg \
        "\226f\271t\035g\211b\037\020g\033\214\303\233\272\014\260$b"    /* "conflicting global register declarations for $b" */
#define fp_err_very_big "O\262l\357\004\306\032\035\245\002\373\004\300\
\027d"    /* "Overlarge floating point value found" */
#define fp_err_big_single \
        "O\262l\357\004(s\001g\273p\020c\033i\006\265\306\032\035\245\002\
\373\004\300\027d"    /* "Overlarge (single precision) floating point value found" */
#define pp_err_eof_comment "EOF\327\266mm\360"    /* "EOF in comment" */
#define pp_err_eof_string "EOF\327\204r\001g"    /* "EOF in string" */
#define pp_err_eol_string "\364ot\004(%c\265\001s\003\243\361\241\004ne\
w\347e"    /* "quote (%c) inserted before newline" */
#define pp_err_eof_escape "EOF\327\204r\035\322cape"    /* "EOF in string escape" */
#define pp_err_missing_quote "M\355'%c\026\036p\020-p\365c\322s\034\266\
mm\201\200\347e"    /* "Missing '%c' in pre-processor command line" */
#define pp_err_if_defined "N\021i\024n\015\310\244\326#i\025\235\005"    /* "No identifier after #if defined" */
#define pp_err_if_defined1 "N\021')\026\326#i\025\235\005(\350"    /* "No ')' after #if defined(..." */
#define pp_err_rpar_eof "M\355')\026\326%s(\350 \031\347\004%ld"    /* "Missing ')' after %s(... on line %ld" */
#define pp_err_many_args "To\021m\201\210\357\221\360\014\217\242cr\021\
%s(\350 \031\347\004%ld"    /* "Too many arguments to macro %s(... on line %ld" */
#define pp_err_few_args "To\021few \357\221\360\014\217\242cr\021%s(\350 \
\031\347\004%ld"    /* "Too few arguments to macro %s(... on line %ld" */
#define pp_err_missing_identifier "M\355i\024n\015\310\244\326#\235e"    /* "Missing identifier after #define" */
#define pp_err_missing_parameter "M\355\374\214n\377\004\036#\235\004%s\
(\350"    /* "Missing parameter name in #define %s(..." */
#define pp_err_missing_comma "M\355',\026\034')\026\326#\235\004%s(\350"    /* "Missing ',' or ')' after #define %s(..." */
#define pp_err_undef "M\355i\024n\015\310\244\326#\027\224"    /* "Missing identifier after #undef" */
#define pp_err_ifdef "M\355i\024n\015\310\244\326#if\224"    /* "Missing identifier after #ifdef" */
#define pp_err_include_quote "M\355'<\026\034'\"\026\326#\001\227u\024"    /* "Missing '<' or '\"' after #include" */
#define pp_err_include_junk "J\027k \326#\001\227ud\004%c%s%c"    /* "Junk after #include %c%s%c" */
#define pp_err_include_file "#\001\227ud\004f`\004%c%s%c w\216ldn'\002\333\
\030"    /* "#include file %c%s%c wouldn't open" */
#define pp_err_unknown_directive "Unk\205wn di\020c\015ve@#%s"    /* "Unknown directive: #%s" */
#define pp_err_endif_eof "M\355#\030di\025a\002EOF"    /* "Missing #endif at EOF" */
#define sem_err_typeclash "Il\232\250e\014\260\333\003\201ds@$s"    /* "Illegal types for operands: $s" */
#define sem_err_sizeof_struct "\255z\004\203$c ne\005\013\353\212ye\002\
\235\005"    /* "size of $c needed but not yet defined" */
#define sem_err_lvalue "Il\232\036l\373e@\325\034\016\323\210$e"    /* "Illegal in lvalue: function or array $e" */
#define sem_err_bitfield_address "b\254\310\253d\014d\021\212hav\004add\
\020s\274s"    /* "bit fields do not have addresses" */
#define sem_err_lvalue1 "Il\232\036l-\373e@'\030\221\026\370\201\002$b"    /* "Illegal in l-value: 'enum' constant $b" */
#define sem_err_lvalue2 "Il\232\036\275\004\226t\207\002\203\201 l-\373\
e@$s"    /* "Illegal in the context of an l-value: $s" */
#define sem_err_nonconst "\251\036%s@<\027k\205wn>"    /* "illegal in %s: <unknown>" */
#define sem_err_nonconst1 "\251\036%s@n\031\370\201\002$b"    /* "illegal in %s: non constant $b" */
#define sem_err_nonconst2 "\251\036%s@$s"    /* "illegal in %s: $s" */
#define sem_err_nonfunction "\032te\246\002\217appl\210a \331\270\272"    /* "attempt to apply a non-function" */
#define sem_err_void_argument "'\335d\026\373e\014\242\210\212\346\357\221\
\360s"    /* "'void' values may not be arguments" */
#define sem_err_bad_cast "$s@\251\340\203$t \217\371"    /* "$s: illegal cast of $t to pointer" */
#define sem_err_bad_cast1 "$s@\251\340\217$t"    /* "$s: illegal cast to $t" */
#define sem_err_bad_cast2 "$s@\340\217\331e\364\037$t `\225\011"    /* "$s: cast to non-equal $t illegal" */
#define sem_err_undef_struct \
        "$c \212ye\002\235\013\206c\201\212\346\274\202c\243f\365m"    /* "$c not yet defined - cannot be selected from" */
#define sem_err_unknown_field "$c h\240n\021$r \310\253d"    /* "$c has no $r field" */
#define errs_membobj(_m)\
  (_m ? "member":"object")


#define bind_rerr_undefined_tag "$s ta\023$b \212\235\005"    /* "$s tag $b not defined" */
#define bind_rerr_linkage_disagreement \
        "\347kag\004d\033ag\020em\030\002\260$r\330\354$g"    /* "linkage disagreement for $r - treated as $g" */
#define bind_rerr_duplicate_typedef "d\311\271\032\004\250\005e\025$r"    /* "duplicate typedef $r" */
#define bind_rerr_local_extern "\207\017n $r m\033m\032\252e\014t\333-\202\
v\253 \303\233\272"    /* "extern $r mismatches top-level declaration" */
#define fp_rerr_very_small "sm\011l \306\032\035\245\002\373\004\226\262\
\243\2170.0"    /* "small floating point value converted to 0.0" */
#define fp_rerr_small_single \
        "sm\011l\375s\001g\273p\020c\033i\006\265\306\032\035\373\004\226\
\262\243\2170.0"    /* "small (single precision) floating value converted to 0.0" */
#define pp_rerr_newline_eof "m\355new\347\004\361\241\004EOF\330\001s\003\
t\005"    /* "missing newline before EOF - inserted" */
#define pp_rerr_nonprint_char "\027pr\236\320\273\252\016 %#.2x \300\344\
\206\277"    /* "unprintable char %#.2x found - ignored" */
#define pp_rerr_illegal_option "\251\333\213-D%s%s"    /* "illegal option -D%s%s" */
#define pp_rerr_spurious_else "sp\367i\216\014#\253s\004\277"    /* "spurious #else ignored" */
#define pp_rerr_spurious_elif "sp\367i\216\014#\253i\025\277"    /* "spurious #elif ignored" */
#define pp_rerr_spurious_endif "sp\367i\216\014#\030di\025\277"    /* "spurious #endif ignored" */
#define pp_rerr_hash_line "n\221b\244m\355\036#\347e"    /* "number missing in #line" */
#define pp_rerr_hash_error "#\356\034\030\266\027\017\013\"%s\""    /* "#error encountered \"%s\"" */
#define pp_rerr_hash_ident "#i\024n\002\362\212\036ANSI C"    /* "#ident is not in ANSI C" */
#define pp_rerr_junk_eol "j\027k a\002\030\200\203#%s \347\004\206\277"    /* "junk at end of #%s line - ignored" */
#define sem_rerr_sizeof_bitfield \
        "\255ze\203<b\254\310\253d\257\251\206\255zeof(\236\265\342\005"    /* "sizeof <bit field> illegal - sizeof(int) assumed" */
#define sem_rerr_sizeof_void "\255z\004\203'\335d\026\020\364ir\013\206\
\3541"    /* "size of 'void' required - treated as 1" */
#define sem_rerr_sizeof_array "\255z\004\203a [] \016\323\210\020\364ir\
\005\305\354[1]"    /* "size of a [] array required, treated as [1]" */
#define sem_rerr_sizeof_function \
        "\255z\004\203\325\020\364ir\013\206\354\255z\004\203\371"    /* "size of function required - treated as size of pointer" */
#define sem_rerr_pointer_arith \
        "<\236\257$s <\371\257\354<\236\257$s\375\236)<\371>"    /* "<int> $s <pointer> treated as <int> $s (int)<pointer>" */
#define sem_rerr_pointer_arith1 \
        "<\371\257$s <\236\257\354(\236)<\371\257$s <\236>"    /* "<pointer> $s <int> treated as (int)<pointer> $s <int>" */
#define sem_rerr_assign_const "\313\231m\030\002\217'\370\026obj\223\002\
$e"    /* "assignment to 'const' object $e" */
#define sem_rerr_addr_regvar \
        "'\020g\033\017\026\032tri\307t\004\260$b \263\013wh\030 add\020\
s\014tak\030"    /* "'register' attribute for $b ignored when address taken" */
#define sem_rerr_lcast "obj\223t\014\275a\002hav\004\361\030 \340\016\004\
\212l-\373\322"    /* "objects that have been cast are not l-values" */
#define sem_rerr_pointer_compare \
"\266\246\016\033\031$s \203\245\214\201\200\236:\n\317li\017\0370\375\260\
== \201\200!=\265\362\006l\210\232\267e"    /* "comparison $s of pointer and int:\n\
  literal 0 (for == and !=) is only legal case" */
#define sem_rerr_different_pointers "diff\003\035\245\214\250\322@$s"    /* "differing pointer types: $s" */
#define sem_rerr_wrong_no_args "wr\006\023n\221b\244\203\374\017\014\217\
$e"    /* "wrong number of parameters to $e" */
#define sem_rerr_casttoenum "$s@\340\203$m \217diff\003\035\030\221"    /* "$s: cast of $m to differing enum" */ /* warn in C */
#define sem_rerr_valcasttoref "$s@\331l\373\004\340\217\331\226s\002\020\
f\003\030ce"    /* "$s: non-lvalue cast to non-const reference" */
#define sem_rerr_implicit_cast1 \
        "$s@i\246\271\254\340\203\245\214\217\331e\364\037\371"    /* "$s: implicit cast of pointer to non-equal pointer" */
#define sem_rerr_implicit_cast2 "$s@i\246\271\254\340\203\3310 \001\002\
\217\371"    /* "$s: implicit cast of non-0 int to pointer" */
#define sem_rerr_implicit_cast3 "$s@i\246\271\254\340\203\245\214\217\376\
'"    /* "$s: implicit cast of pointer to 'int'" */
#define sem_rerr_implicit_cast4 "$s@i\246\271\254\340\203$t \217\376'"    /* "$s: implicit cast of $t to 'int'" */
#define sem_rerr_nonpublic "$r \362\331pub\271 \261mb\244\203$c"    /* "$r is non-public member of $c" */
#define sem_rerr_cant_balance "diff\003\035\245\214\250\322@$s"    /* "differing pointer types: $s" */

#define sem_rerr_void_indirection "\251\001di\020c\213\031(\335\200*)@'\
*'"    /* "illegal indirection on (void *): '*'" */
#define obj_fatalerr_io_object "I/O \356\034\031obj\223\002\204\020\377"    /* "I/O error on object stream" */
#define compiler_rerr_no_extern_decl\
    "n\021\207\017n\037\303\233\213\036tr\201sla\213\027\215"    /* "no external declaration in translation unit" */
#define compiler_fatalerr_io_error "I/O \356\034wr\215\035'%s'"    /* "I/O error writing '%s'" */
#define driver_fatalerr_io_object "I/O \356\034\031obj\223\002\204\020\377"    /* "I/O error on object stream" */
#define driver_fatalerr_io_asm "I/O \356\034\031\177\274mbl\244\216tpu\002\
\204\020\377"    /* "I/O error on assembler output stream" */
#define driver_fatalerr_io_listing "I/O \356\034\031l\033t\035\204\020\377"    /* "I/O error on listing stream" */
#ifdef TARGET_HAS_AOUT
#define aout_fatalerr_toomany "To\021m\201\210symbol\014\260'a.\216t\026\
\216tput"    /* "Too many symbols for 'a.out' output" */
#define aout_fatalerr_toobig "Modu\273to\021bi\023\260a.\216\002\351\032\
\017"    /* "Module too big for a.out formatter" */
#endif
#ifdef TARGET_HAS_COFF
#define coff_fatalerr_toomany "To\021m\201\210\020\211ca\272\014\260COF\
F \241\242\002\036.\021f`e"    /* "Too many relocations for COFF format in .o file" */
#define coff_fatalerr_toobig "Modu\273to\021bi\023\260COFF \351\032\017"    /* "Module too big for COFF formatter" */
#endif
#ifdef TARGET_IS_HELIOS
#define heliobj_warn_12bits "Off\274\002%ld \25712 b\215s"    /* "Offset %ld > 12 bits" */
#define heliobj_warn_16bits "Off\274\002%ld \25716 b\215s"    /* "Offset %ld > 16 bits" */
#define heliobj_warn_24bits "Off\274\002%ld \25724 b\215s"    /* "Offset %ld > 24 bits" */
#endif
#define misc_fatalerr_space1 "\216\002\203\204\010\004(\260\356\034\307\
ff\003)"    /* "out of store (for error buffer)" */
#define misc_fatalerr_toomanyerrs "To\021m\201\210\356\010s"    /* "Too many errors" */
#define misc_fatalerr_space2 "\216\002\203\204\010\004(\036cc_\011\211c\
)\n(Co\246`a\213\203\275\004\024\307gg\035t\320\202\014\020\364e\204\013\
\341 \275\004-\023\333\272\n \020\364i\020\014a g\020a\002\024\037\203\261\
m\010y. R\223o\246`\035\341\216\002-g\305\341\n \275\004m\010\004\020\204\
r\237\243-g\025\333\272\305\034\341 \275\004p\365g\323m b\365k\030 \236\
o\n sm\011l\244pi\223\322\305\242\210h\253p.)"    /* "out of store (in cc_alloc)\n\
(Compilation of the debugging tables requested with the -g option\n\
 requires a great deal of memory. Recompiling without -g, with\n\
 the more restricted -gf option, or with the program broken into\n\
 smaller pieces, may help.)" */
#define misc_fatalerr_space3 "\216\002\203\204\010\004(\036cc_\011\211c\
)"    /* "out of store (in cc_alloc)" */
#define pp_fatalerr_hash_error "#\356\034\030\266\027\017\013\"%s\""    /* "#error encountered \"%s\"" */

/* Beware: none of the following driver_message #defined are used!      */
#define driver_message_nolisting \
        "Un\320\273\217\333\030 %s \260l\033t\001g@-l \333\213\277\n"    /* "Unable to open %s for listing: -l option ignored\n" */
#ifdef NO_ASSEMBLER_OUTPUT
#define driver_message_noasm \
        "Th\362\262\255\031\203\275\004\266\246`\244doe\014\212s\311p\010\
\002-s\n"    /* "This version of the compiler does not support -s\n" */
#endif
#define driver_message_writefail "C\216ldn'\002wr\215\004f`\004'%s'\n"    /* "Couldn't write file '%s'\n" */
#define driver_message_oddoption "Un\020\266gniz\013\333\213'%c'@\277\n"    /* "Unrecognized option '%c': ignored\n" */
#define driver_message_readfail "C\216ldn'\002\020a\200f`\004'%s'\n"    /* "Couldn't read file '%s'\n" */
/* NB the next error can not arise with the current ARM driver */
#define driver_message_toomanyfiles "To\021m\201\210f`\004\357s"    /* "Too many file args" */
#define driver_message_asmstdout "As\274mbl\210\266d\004w`l g\021\217\204\
d\216t\n"    /* "Assembly code will go to stdout\n" */
#define driver_message_no_listing \
        "-m \333\213\256e\202s\014\341\216\002s\216rc\004l\033t\001g. I\
gn\010\005\n"    /* "-m option useless without source listing. Ignored\n" */
#define driver_message_nomap "-m f`\004\212ava`\320\273\034c\010r\311\002\
\206\277\n"    /* "-m file not available or corrupt - ignored\n" */
#define driver_message_notest \
        "Th\362\262\255\031\203\275\004\266\246`\244doe\014\212s\311p\010\
\002\275\004-t\322\002\333\272\n"    /* "This version of the compiler does not support the -test option\n" */
#define driver_message_needfile "A\002\202\177\002\006\004f`\004\357\221\
\030\002w\201t\005\n"    /* "At least one file argument wanted\n" */
#ifndef COMPILING_ON_ARM_OS
#define driver_message_spool "\216tpu\002\217c\211g1.\211\023& c\211g2.\
\211g\n"    /* "output to clog1.log & clog2.log\n" */
#endif
#define driver_message_testfile "N\021f`e\014\011\211w\013\341 -te\204\n"    /* "No files allowed with -test\n" */

/* messages generated by misc.c */
#ifndef TARGET_IS_UNIX
#ifndef COMPILING_ON_MPW
#define misc_message_lineno(_f,_l,_s) "\"%s\"\305\347\004%ld@%s@"    /* "\"%s\", line %ld: %s: " */,_f,_l,_s
#else
#define misc_message_lineno(_f,_l,_s) "F`\004\"%s\"; L\001\004%ld # %s@"    /* "File \"%s\"; Line %ld # %s: " */,_f,_l,_s
#endif
#else
#define misc_message_lineno(_f,_l,_s) "%s@%ld@%s@"    /* "%s: %ld: %s: " */,_f,_l,_s
#endif
#ifndef COMPILING_ON_MPW
#define misc_message_sum1(_f,nx,neq1) "%s@%ld w\016n\001g%s"    /* "%s: %ld warning%s" */, _f, nx, \
 neq1 ? "":"s"

#else
#define misc_message_sum1(_f,nx,neq1) "### \"%s\"@%ld w\016n\001g%s"    /* "### \"%s\": %ld warning%s" */, _f, nx, \
 neq1 ? "":"s"

#endif
#define misc_message_sum2 "\375+ %ld s\311p\020ss\005)"    /* " (+ %ld suppressed)" */
#define misc_message_sum3(nx,neq1) "\305%ld \356\010%s"    /* ", %ld error%s" */, nx, \
 neq1 ? "":"s"

#define misc_message_sum5(nx,neq1) "\305%ld s\003i\216\014\356\010%s\n"    /* ", %ld serious error%s\n" */, nx, \
 neq1 ? "":"s"


#ifdef TARGET_STACK_MOVES_ONCE
/* Cannot be issued if NARGREGS==0 */
#define warn_untrustable "\027tru\204\320\273\266d\004g\030\003\304\260\
$r"    /* "untrustable code generated for $r" */
#endif

 /* The next batch of things just get mapped onto syserr codes */

#define syserr_removepostincs "unexpected op in RemovePostIncs"
#define syserr_mkqualifiedtype "mkqualifiedtype(..., %ld)"
#define syserr_unbitfield "unbitfield_type $t"
#define syserr_bf_promote "bf_promoted_type $t"
#define syserr_typeof "typeof(%ld)"
#define syserr_alignoftype "alignoftype(%ld,%#lx)"
#define syserr_sizeoftype "sizeoftype(%ld,%#lx)"
#define syserr_codeoftype "codeoftype"
#define syserr_equivtype "equivtype(%ld)"
#define syserr_compositetype "compositetype(%ld)"
#define syserr_trydiadicreduce "trydiadreduce(unsigned op %ld)"
#define syserr_trydiadicreduce1 "trydiadreduce(signed op %ld)"
#define syserr_trydiadicreduce2 "trydiadreduce(float op %ld)"
#define syserr_fp_op "FP op %ld unknown"
#define syserr_trymonadicreduce "trymonadreduce(int op %ld)"
#define syserr_trymonadicreduce1 "trymonadreduce(float op %ld)"
#define syserr_bf_container "bf_container"
#define syserr_coerceunary1 "coerceunary(%ld,%#lx)"
#define syserr_bitfieldassign "bitfieldassign"
#define syserr_mkindex "sem(mkindex)"
#define syserr_ptrdiff "sem(mkbinary/ptrdiff)"
#define syserr_va_arg_fn "sem(odd va_arg fn)"
#define syserr_mkcast "mkcast(%ld,%#lx)"
#define syserr_mkcast1 "mkcast(%ld)"
#define syserr_te_plain "te_plain(%ld)"
#define syserr_clone_node "clone_node(%ld)"
#define syserr_optimise "optimise &(%ld)"
#define syserr_optimise1 "optimise(%ld)"
#define syserr_mcrepofexpr "mcrepofexpr(%ld,%#lx)"
#define syserr_mcreparray "mcrep(array %ld)"
#define syserr_newdbuf "pp_newdbuf(%ld,%ld)"
#define syserr_pp_recursion "pp recursive sleep: '%s'"
#define syserr_pp_special "pp_special(%ld)"
#define syserr_overlarge_store1 "Overlarge storage request (binder %ld)"
#define syserr_overlarge_store2 "Overlarge storage request (local %ld)"
#define syserr_discard2 "discard2 %p"
#define syserr_discard3 "discard3 %p"
#define syserr_alloc_unmark "alloc_unmark - called too often"
#define syserr_alloc_unmark1 "alloc_unmark(no drop_local_store())"
#define syserr_alloc_reinit "alloc_reinit(no drop_local_store())"
#define syserr_addclash "add_clash (0x%lx, 0x%lx)"
#define syserr_forget_slave "forget_slave(%ld, %ld) %ld"
#define syserr_GAP "GAP in reference_register"
#define syserr_corrupt_register "corrupt_register %ld %p"
#define syserr_regalloc "regalloc(corrupt/alloc)"
#define syserr_regalloc_typefnaux "regalloc(typefnaux)"
#define syserr_regalloc_POP "regalloc(POP)"
#define syserr_call2 "CALL2 %ld"
#define syserr_dataflow "dataflow &-var"
#define syserr_choose_real_reg "choose_real_reg %lx"
#define syserr_fail_to_spill "Failed to spill register for %ld"
#define syserr_regalloc_reinit2 "regalloc_reinit2"
#define syserr_regheap "Register heap overflow"
#define syserr_bad_fmt_dir "bad fmt directive"
#define syserr_syserr "syserr simulated"
#define syserr_r1r "r1r %ld"
#define syserr_r2r "r2r %ld"
#define syserr_mr "mr %ld"
#define syserr_expand_jop "expand_jop(2address)"
#define syserr_nonauto_active "Non auto 'active_binders' element"
#define syserr_size_of_binder "size_of_binder"
#define syserr_insertblockbetween "insertblockbetween(%ld, %ld)"
#define syserr_reopen_block "reopen_block called"
#define syserr_scaled_address "emit5(scaled address)"
#define syserr_expand_pushr "expand_jop_macro(PUSHR)"
#define syserr_remove_noop_failed "remove_noop failed"
#define syserr_remove_noop_failed2 "remove_noop failed2"
#define syserr_bad_bindaddr "Bad bindaddr_() with LDRVx1"
#define syserr_ldrfk "duff LDRF/DK %lx"
#define syserr_ldrk "duff LDR/B/WK %lx"
#define syserr_branch_backptr "Bad back-pointer code in branch_chain"
#define syserr_no_main_exit "use_cond_field(no main exit)"
#define syserr_two_returns "Two return exits from a block"
#define syserr_unrefblock "unrefblock"
#define syserr_zip_blocks "zip_blocks(SETSP confused %ld!=%ld)"
#define syserr_live_empty_block "ALIVE empty block L%ld"
#define syserr_loctype "loctype"
#define syserr_adconbase "cse_adconbase"
#define syserr_find_exprn "CSE: find_exprn %ld"
#define syserr_removecomparison "CSE: removecomparison %lx"
#define syserr_evalconst "CSE: evalconst %lx"
#define syserr_scanblock "cse_scanblock %08lx"
#define syserr_prune "csescan(prune)"
#define syserr_globalize "globalize_declaree1(%p,%ld)"
#define syserr_globalize1 "globalize_typeexpr(%p,%ld)"
#define syserr_copy_typeexpr "copy_typeexpr(%p,%ld)"
#define syserr_tentative "is_tentative(tmpdataq == NULL)"
#define syserr_tentative1 "is_tentative(ADCON)"
#define syserr_tentative2 "tentative definition confusion"
#define syserr_instate_decl "instate_decl %ld"
#define syserr_totarget "totargetsex(%d)"
#define syserr_vg_wpos "vg_wpos(%ld)"
#define syserr_vg_wflush "vg_wflush(type=0x%x)"
#define syserr_gendcI "gendcI(%ld,%ld)"
#define syserr_vg_wtype "vg_wtype=0x%x"
#define syserr_codevec "code vector overflow"
#define syserr_nonstring_lit "non-string literal: %.8lx"
#define syserr_addr_lit "Address-literals should not arise in HELIOS mode"
#define syserr_dumplits "dumplits(codep&3)"
#define syserr_dumplits1 "codebuf(dumplits1)"
#define syserr_dumplits2 "codebuf(dumplits2)"
#define syserr_outlitword "outlitword confused"
#define syserr_dumplits3 "codebuf(dumplits3)"
#define syserr_addlocalcse "addlocalcse %ld"
#define syserr_cse_lost_def "cse: def missing"
#define syserr_cse_lost_use "cse: use missing"
#define syserr_cse_linkrefstodefs "CSE: linkrefstodefs"
#define syserr_linkrefstodefs "cse_linkrefstodefs"
#define syserr_safetolift "cse_safetolift"
#define syserr_storecse "storecse %ld %ld\n"
#define syserr_baseop "CSE: baseop %lx"
#define syserr_cse_wordn "CSE_WORDn"
#define syserr_addcsedefs "addcsedefs"
#define syserr_cse_preheader "CSE: loop preheader %d != %ld"
#define syserr_modifycode "modifycode %ld %ld!=%ld"
#define syserr_modifycode_2 "compare ref L%ld not reachable from def L%ld"
#define syserr_regtype "ensure_regtype(%lx)"
#define syserr_struct_val "Value of structure requested improperly"
#define syserr_missing_expr "missing expr"
#define syserr_checknot "s_checknot"
#define syserr_structassign_val "value of structure assignment needed"
#define syserr_structdot "Struct returning function (with '.') reaches cg"
#define syserr_floating "Float %%"
#define syserr_cg_expr  "cg_expr(%ld = $s)"
#define syserr_bad_reg "bad reg %lx in use"
#define syserr_bad_fp_reg "fp reg in use"
#define syserr_cg_fnarg "cg_fnarg(odd rep %lx)"
#define syserr_fnarg_struct "cg(struct arg confused)"
#define syserr_cg_fnarg1 "cg_fnargs confused"
#define syserr_cg_argcount "arg count confused"
#define syserr_cg_fnarg2 "cg_fnargs tidy"
#define syserr_padbinder "odd padbinder$b in cg_fnargs()"
#define syserr_cg_fnap "cg_fnap"
#define syserr_cg_cmd "cg_cmd(%ld = $s)"
#define syserr_cg_endcase "cg_cmd(endcase)"
#define syserr_cg_break "cg_cmd(break)"
#define syserr_cg_cont "cg_cmd(cont)"
#define syserr_cg_switch "switch expression must have integer type"
#define syserr_cg_caselist "cg_caselist"
#define syserr_cg_case "cg_cmd(case)"
#define syserr_unset_case "Unset case_lab"
#define syserr_cg_default "cg_cmd(default)"
#define syserr_cg_badrep "rep bad in comparison %.8lx"
#define syserr_cg_plain "(plain) qualifies non-<narrow-int-binder>"
#define syserr_cg_cast "Illegal cast involving a structure or union"
#define syserr_cg_fpsize "fp sizes are wrong %ld %ld"
#define syserr_cg_cast1 "bad mode %ld in cast expression"
#define syserr_cg_cast2 "cast %ld %ld %ld %ld"
#define syserr_cg_indexword "Indexed address mode with word-store"
#define syserr_cg_bad_width "bad width %ld in cg_stind"
#define syserr_cg_bad_mode "bad mcmode %ld in cg_stind"
#define syserr_chroma "chroma_check(target.h setup wrong or multi-temp op confused)"
#define syserr_Q_swap "Q_swap(%lx)"
#define syserr_cg_stgclass "Funny storage class %#lx"
#define syserr_cg_storein "cg_storein(%ld)"
#define syserr_cg_addr "p nasty in '&(p=q)'"
#define syserr_cg_addr1 "cg_addr(%ld)"
#define syserr_cg_shift0 "n=0 in shift_op1"
#define syserr_not_shift "not a shift in shift_operand()"
#define syserr_not_shift1 "not a shift in shift_amount()"
#define syserr_integer_expected "integer expression expected"
#define syserr_nonauto_arg "Non-auto arg!"
#define syserr_struct_result "Unexpected struct result"
#define syserr_cg_topdecl "cg_topdecl(not fn type)"
#define syserr_cg_unknown "unknown top level %ld"
#define syserr_cg_narrowformal "unwidened formal"

#ifdef TARGET_HAS_AOUT
#define syserr_aout_reloc "relocate_code_to_data(PCreloc)"
#define syserr_aout_checksym "obj_checksym(%s)"
#define syserr_aout_reloc1 "obj_coderelocation %.8lx"
#define syserr_aout_gendata "obj_gendata(%ld)"
#define syserr_aout_datalen "obj_data len=%ld"
#define syserr_aout_data "obj_data %ldEL%ld'%s'"
#define syserr_aout_debug "writedebug: aoutobj linked with xxxdbg not dbx"
#endif

#ifdef TARGET_HAS_COFF
#define syserr_coff_reloc "relocate_code_to_data(PCreloc)"
#define syserr_coff_pcrel "coff(unexpected X_PCreloc)"
#define syserr_coff_m88000 "coffobj(X_DataAddr needs extending)"
#define syserr_coff_toobig "coffobj(Module over 64K -- fix)"
#define syserr_coff_checksym "obj_checksym($r)"
#define syserr_coff_reloc1 "obj_coderelocation(%.8lx)"
#define syserr_coff_gendata "obj_gendata(%ld)"
#define syserr_coff_datalen "obj_data len=%ld"
#define syserr_coff_data "obj_data %ldEL%ld'%s'"
#endif

#ifdef TARGET_HAS_DEBUGGER  /* dbx.c syserrs */
#  ifndef TARGET_HAS_NON_DBX_DEBUGGER
#define syserr_too_many_types "too many types in dbx"
#define syserr_addcodep "bad pointer in dbg_addcodep"
#define syserr_tagbindsort "bad tagbindsort 0x%08lx"
#define syserr_sprinttype "sprinttype(%p,0x%lx)"
#define syserr_dbx_locvar "debugger table confusion(local variable $r %lx %lx)"
#define syserr_dbx_scope "dbg_scope"
#define syserr_dbx_proc "dbg_proc"
#define syserr_dbx_proc1 "dbg_proc confused"
#define syserr_dbx_write "dbg_write(%lx)"
#  endif
#endif

#ifdef TARGET_IS_HELIOS
#define syserr_heliobj_bad_xref "Invalid external reference $r %#lx"
#define syserr_heliobj_dataseggen "Data seg generation confused"
#define syserr_heliobj_gendata "obj_gendata(%ld)"
#define syserr_heliobj_datalen "obj_data len=%ld"
#define syserr_heliobj_data "obj_data %ldEL%ld'%s'"
#define syserr_heliobj_align "Helios obj align"
#define syserr_heliobj_2def "double definition in obj_symref $r"
#define syserr_heliobj_codedata "code/data confusion for $r"
#endif

      /* The following remain as ordinary (uncompressed) strings */

#define misc_message_announce "+++ %s: "
#define misc_message_announce1 "+++ %s: %ld: %s: "

/*
 * @@@ Wording here is subject to change...
 */
#define misc_message_warning   "Warning"
#define misc_message_error     "Error"
#define misc_message_serious   "Serious error"
#define misc_message_fatal     "Fatal error"
#define misc_message_fatal_internal "Fatal internal error"
#define misc_message_abandoned "\nCompilation abandoned.\n"

#define bind_msg_const "constant expression"

#define moan_floating_type "floating type initialiser"
#define moan_static_int_type "static integral type initialiser"

/*
 * The following are used in init_sym_name_table() and/or ctxtofdeclflag()
 * and eventually find their ways into various error messages.
 */

#define errname_aftercommand       " after command"
#define errname_unset              "<?>"
#define errname_pointertypes       "<after * in declarator>"
#define errname_toplevel           "<top level>"
#define errname_structelement      "<structure component>"
#define errname_formalarg          "<formal parameter>"
#define errname_formaltype         "<formal parameter type declaration>"
#define errname_blockhead          "<head of block>"
#define errname_typename           "<type-name>"
#define errname_unknown            "<unknown context>"
#define errname_error              "<previous error>"
#define errname_invisible          "<invisible>"
#define errname_let                "<let>"
#define errname_character          "<character constant>"
#define errname_wcharacter         "<wide character constant>"
#define errname_integer            "<integer constant>"
#define errname_floatcon           "<floating constant>"
#define errname_string             "<string constant>"
#define errname_wstring            "<wide string constant>"
#define errname_identifier         "<identifier>"
#define errname_binder             "<variable>"
#define errname_tagbind            "<struct/union tag>"
#define errname_cond               "_?_:_"
#define errname_displace           "++ or --"
#define errname_postinc            "++"
#define errname_postdec            "--"
#define errname_arrow              "->"
#define errname_addrof             "unary &"
#define errname_content            "unary *"
#define errname_monplus            "unary +"
#define errname_neg                "unary -"
#define errname_fnap               "<function argument>"
#define errname_subscript          "<subscript>"
#define errname_cast               "<cast>"
#define errname_sizeoftype         "sizeof"
#define errname_sizeofexpr         "sizeof"
#define errname_ptrdiff            "-"   /* for (a-b)=c msg */
#define errname_endcase            "break"
#define errname_block              "<block>"
#define errname_decl               "decl"
#define errname_fndef              "fndef"
#define errname_typespec           "typespec"
#define errname_typedefname        "typedefname"
#define errname_valof              "valof"
#define errname_ellipsis           "..."
#define errname_eol                "\\n"
#define errname_eof                "<eof>"

#ifdef RANGECHECK_SUPPORTED
#  define errname_rangecheck       "<rangecheck>"
#  define errname_checknot         "<checknot>"
#endif

/* end of miperrs.h */
/*
 * cfe/feerrs.h - prototype for front-end error messages file
 * version 3b.
 */

  /* Ordinary error messages - mapped onto numeric codes */

#define lex_warn_force_unsigned "%s \354%sul\32732-b\254i\246\202m\360a\
\272"    /* "%s treated as %sul in 32-bit implementation" */
#define lex_warn_multi_char "\331p\010t\320\273\206\2121 \252\016\327'\350\
'"    /* "non-portable - not 1 char in '...'" */
#define lex_warn_cplusplusid "C++ keyw\010\200\256\013\240i\024n\015\310\
\003@$r"    /* "C++ keyword used as identifier: $r" */

#define syn_warn_hashif_undef "Un\235\013\242cr\021'%s\026\036#i\025\206\
\3540"    /* "Undefined macro '%s' in #if - treated as 0" */
#define syn_warn_invent_extern "\001v\360\035'\207\017n \001\002%s();'"    /* "inventing 'extern int %s();'" */
#define syn_warn_unary_plus "Un\016\210'+\026\362a fe\032\367\004\203AN\
SI C"    /* "Unary '+' is a feature of ANSI C" */
#define syn_warn_spurious_braces "sp\367i\216\014{} \016o\344sc\011\016 \
\343\003"    /* "spurious {} around scalar initialiser" */
#define syn_warn_dangling_else "D\201gl\035'\253\274\026\001d\237\032e\014\
\230s\255b\273\356\010"    /* "Dangling 'else' indicates possible error" */
#define syn_warn_void_return "\331\373\004\020t\367n\327\331\335\200\270\
\272"    /* "non-value return in non-void function" */
#define syn_warn_use_of_short \
        "'sh\010t\026s\211w\244\275\201 \376\026\031\275\362\242\252\001\
\004(\274\004m\201u\011)"    /* "'short' slower than 'int' on this machine (see manual)" */
#define syn_warn_undeclared_parm \
        "\351\037\374\214$r \212\303\016\013\206\376\026\342\005"    /* "formal parameter $r not declared - 'int' assumed" */
#define syn_warn_old_style "Old-\204y\273\325$r"    /* "Old-style function $r" */
#define syn_warn_give_args "Dep\020c\304\303\233\213%s(\265\206giv\004\016\
\023\250\322"    /* "Deprecated declaration %s() - give arg types" */
#define syn_warn_ANSI_decl "ANSI \204y\273\325\303\233\213\256\005\305'\
%s(\350)'"    /* "ANSI style function declaration used, '%s(...)'" */
#define syn_warn_archaic_init "Anci\030\002\351 \203\343a\272\305\256\004\
'='"    /* "Ancient form of initialisation, use '='" */
#define syn_warn_untyped_fn "'\001\002%s()\026\342\013\206'\335d\026\236\
\030d\005?"    /* "'int %s()' assumed - 'void' intended?" */
#define syn_warn_no_named_member "$c h\240n\021n\377\013\261mb\003"    /* "$c has no named member" */
#define syn_warn_extra_comma "S\311\003flu\216\014',\026\036'\030\221\026\
\303\233\272"    /* "Superfluous ',' in 'enum' declaration" */
#define syn_warn_struct_padded "padd\035\001s\003\243\036\204ruc\002$b"    /* "padding inserted in struct $b" */
#define syn_warn_switch_funny "'s\301\252\375e)\026\212\300l\211w\013b\210\
'{'"    /* "'switch (e)' not followed by '{'" */

#define vargen_warn_nonull "om\215t\035t\323`\035'\\0\026\260%s [%ld]"    /* "omitting trailing '\\0' for %s [%ld]" */
#define vargen_warn_unnamed_bitfield \
        "Unn\377\013b\254\310\253\200\343\013\2170"    /* "Unnamed bit field initialised to 0" */
#define vargen_warn_init_non_aggregate \
        "Atte\246\002\217\343\004\331agg\020g\032e"    /* "Attempt to initialise non-aggregate" */

#define lex_err_ioverflow "N\221b\244%s to\021l\357\004\26032-b\254i\246\
\202m\360a\272"    /* "Number %s too large for 32-bit implementation" */
#define lex_err_overlong_fp "G\365ssl\210\363-l\006\023\306\032\035\245\
\002n\221b\003"    /* "Grossly over-long floating point number" */
#define lex_err_fp_syntax1 "D\220\254\020\364ir\013\326\276\006\030\002\
m\016k\003"    /* "Digit required after exponent marker" */
#define lex_err_overlong_hex "G\365ssl\210\363-l\006\023h\207a\024cim\037\
\370\201t"    /* "Grossly over-long hexadecimal constant" */
#define lex_err_overlong_int "G\365ssl\210\363-l\006\023n\221b\003"    /* "Grossly over-long number" */
#define lex_err_need_hex_dig "H\207 d\220\254ne\005\013\3260x \0340X"    /* "Hex digit needed after 0x or 0X" */
#define lex_err_need_hex_dig1 "M\355h\207 d\220\215(s\265\326\\x"    /* "Missing hex digit(s) after \\x" */
#define lex_err_backslash_blank \
        "\\<space\257\201\200\\<t\320\257\016\004\001\312i\200\204r\035\
\322cap\322"    /* "\\<space> and \\<tab> are invalid string escapes" */
#define lex_err_unterminated_string "New\347\004\034\030\200\203f`\004\341\
\036\204r\001g"    /* "Newline or end of file within string" */
#define lex_err_bad_hash "m\033plac\013p\020p\365c\322s\034\252\233c\214\
'%c'"    /* "misplaced preprocessor character '%c'" */
#define lex_err_bad_char "\251\252\233c\214(0x%lx = '%c'\265\036s\216rc\
e"    /* "illegal character (0x%lx = \'%c\') in source" */
#define lex_err_bad_noprint_char "\251\252\233c\214(h\207 \266d\0040x%x\
\265\036s\216rce"    /* "illegal character (hex code 0x%x) in source" */
#define lex_err_ellipsis "(\350\265m\256\002hav\004\207actl\2103 dots"    /* "(...) must have exactly 3 dots" */
#define lex_err_illegal_whitespace "$s \242\210\212hav\004wh\215\322pac\
\004\036\215"    /* "$s may not have whitespace in it" */

#define syn_err_illdtor "\251\024\204ruct\034~$l"    /* "illegal destructor ~$l" */
#define syn_err_bitsize "b\254\255z\004%ld \251\2061 \342\005"    /* "bit size %ld illegal - 1 assumed" */
#define syn_err_zerobitsize "z\003\021wid\275 n\377\013b\254\310\253\200\
\2061 \342\005"    /* "zero width named bit field - 1 assumed" */
#define syn_err_arraysize "Ar\323\210\255z\004%ld \251\2061 \342\005"    /* "Array size %ld illegal - 1 assumed" */
#define syn_err_expected "\372$s\330\001s\003\243\361\241\004$l"    /* "expected $s - inserted before $l" */
#define syn_err_expected1 "\372$s%s\330\001s\003\243\361\241\004$l"    /* "expected $s%s - inserted before $l" */
#define syn_err_expected2 "\372$s \034$s\330\001s\003\243$s \361\241\004\
$l"    /* "expected $s or $s - inserted $s before $l" */
#define syn_err_expecteda "\372$s"    /* "expected $s" */
#define syn_err_expected1a "\372$s%s"    /* "expected $s%s" */
#define syn_err_expected2a "\372$s \034$s"    /* "expected $s or $s" */
#define syn_err_mix_strings "\252\016 \201\200wid\004(L\"\350\"\265\204\
r\001g\014d\021\212\226c\032\030\032e"    /* "char and wide (L\"...\") strings do not concatenate" */
#define syn_err_expected_expr "<\276\020s\255\006\257\372\353\300\344$l"    /* "<expression> expected but found $l" */
#ifdef EXTENSION_VALOF
#define syn_err_valof_block \
        "{ \300l\211w\035a \340w`l \346\354VALOF b\211ck"    /* "{ following a cast will be treated as VALOF block" */
#endif
#define syn_err_typedef "\250\005e\025n\377\004$r \256\013\036\276\020s\
\255\031\226t\207t"    /* "typedef name $r used in expression context" */
#define syn_err_assertion "___\313\003t(0\305$e)"    /* "___assert(0, $e)" */
#define syn_err_expected_member "Exp\336<\261mb\003\257\353\300\344$l"    /* "Expected <member> but found $l" */
#define syn_err_hashif_eof "EOF \212new\347\004\326#i\025\350"    /* "EOF not newline after #if ..." */
#define syn_err_hashif_junk "J\027k \326#i\025<\276\020s\255\006>"    /* "Junk after #if <expression>" */
#define syn_err_initialisers "to\021m\201\210\343\003\014\036{} \260agg\
\020g\032e"    /* "too many initialisers in {} for aggregate" */
#define syn_err_initialisers1 "{} m\256\002hav\0041 e\202m\030\002\217\343\
\004sc\011\016"    /* "{} must have 1 element to initialise scalar" */
#define syn_err_default "'\224ault\026\212\036s\301\252\330\277"    /* "'default' not in switch - ignored" */
#define syn_err_default1 "d\311\271\032\004'\224ault\026\267\004\277"    /* "duplicate 'default' case ignored" */
#define syn_err_case "'\267e\026\212\036s\301\252\330\277"    /* "'case' not in switch - ignored" */
#define syn_err_case1 "d\311\271\304\267\004\370\201t@%ld"    /* "duplicated case constant: %ld" */
#define syn_err_expected_cmd "<\266mm\201d\257\372\353\300\344$l"    /* "<command> expected but found $l" */
#define syn_err_expected_while "'wh`e\026\372\326'do\026\353\300\344$l"    /* "'while' expected after 'do' but found $l" */
#define syn_err_else "M\033plac\013'\253\274\026\277"    /* "Misplaced 'else' ignored" */
#define syn_err_continue "'\226t\001ue\026\212\036\211\333\330\277"    /* "'continue' not in loop - ignored" */
#define syn_err_break "'b\020ak\026\212\036\211\333 \034s\301\252\330\277"    /* "'break' not in loop or switch - ignored" */
#define syn_err_no_label "'goto\026\212\300l\211w\013b\210l\320\253\330\
\277"    /* "'goto' not followed by label - ignored" */
#define syn_err_no_brace "'{\026\203\325bod\210\372\206\300\344$l"    /* "'{' of function body expected - found $l" */
#define syn_err_stgclass \
        "\204\010ag\004\227\177\014$s \212p\003m\215\243\036\226t\207\002\
%s\330\277"    /* "storage class $s not permitted in context %s - ignored" */
#define syn_err_stgclass1 "\204\010ag\004\227\177\014$s \001\266\246a\015\
b\273\341 $g\330\277"    /* "storage class $s incompatible with $g - ignored" */
#define syn_err_typeclash "\337$s \001\226s\033t\030\002\341 $m"    /* "type $s inconsistent with $m" */
#define syn_err_tag_brace \
        "'{\026\034<i\024n\015\310\003\257\372\326$s \353\300\344$l"    /* "'{' or <identifier> expected after $s but found $l" */
#define syn_err_expected3 "Exp\223t\035<\303\016\032\010\257\034<\250e\257\
\353\300\344$l"    /* "Expecting <declarator> or <type> but found $l" */
#define syn_err_unneeded_id \
        "s\311\003flu\216\014$l\327<\320\204\323c\002\303\016\032\010\257\
\206\277"    /* "superfluous $l in <abstract declarator> - ignored" */
#define syn_err_undef_struct(_m,_b,_s) \
        "\027\235\013$c %s@$r"    /* "undefined $c %s: $r" */, _b, errs_membobj(_m), _s
#define syn_err_selfdef_struct(_m,_b,_s) \
        "\032te\246\002\217\001\227ud\004$c %s@$r \341\036\215s\253f"    /* "attempt to include $c %s: $r within itself" */, \
        _b, errs_membobj(_m), _s
#define syn_err_void_object(_m,_s) "\251'\335d\026%s@$r"    /* "illegal 'void' %s: $r" */, errs_membobj(_m), _s
#define syn_err_duplicate_type \
        "d\311\271\032\004\337sp\223if\237a\213\203\351\037\374\214$r"    /* "duplicate type specification of formal parameter $r" */
#define syn_err_not_a_formal "N\302\351\037$r\327\374\017-\250e-sp\223i\
\310\003"    /* "Non-formal $r in parameter-type-specifier" */
#define syn_err_cant_init "$g na\261\014\242\210\212\346\343\005"    /* "$g names may not be initialised" */
#define syn_err_enumdef \
        "<i\024n\015\310\003\257\372\353\300\344$l\327'\030\221\026\235\
i\272"    /* "<identifier> expected but found $l in 'enum' definition" */
#define syn_err_undef_enum "Un\235\013\030\221 $r"    /* "Undefined enum $r" */
#define syn_err_misplaced_brace "M\033plac\013'{\026a\002t\333 \202v\253\
\330\263\035b\211ck"    /* "Misplaced '{' at top level - ignoring block" */

#define vargen_err_long_string "\204r\035\343\244l\006g\244\275\201 %s \
[%ld]"    /* "string initialiser longer than %s [%ld]" */
#define vargen_err_nonstatic_addr \
        "\331\204a\015c add\020s\014$b\327\245\214\343\003"    /* "non-static address $b in pointer initialiser" */
#define vargen_err_bad_ptr "$s@\251\256\004\036\245\214\343\003"    /* "$s: illegal use in pointer initialiser" */
#define vargen_err_init_void "obj\223t\014\203\337'\335d\026c\201 \212\346\
\343\005"    /* "objects of type 'void' can not be initialised" */
#define vargen_err_undefined_struct \
        "$c m\256\002\346\235\013\260(\204a\015c\265v\016i\320\273\303\233\
\272"    /* "$c must be defined for (static) variable declaration" */
#define vargen_err_open_array "Un\343\013\204a\015c [] \016\323y\014`\225\011"    /* "Uninitialised static [] arrays illegal" */
#define vargen_err_overlarge_reg "\001\312i\200g\211b\037\020g\033\214n\
\221b\003"    /* "invalid global register number" */
#define vargen_err_not_int "\001\312i\200\337\260g\211b\037\001\002\020\
g\033\017"    /* "invalid type for global int register" */
#define vargen_err_not_float "\001\312i\200\337\260g\211b\037\306a\002\020\
g\033\017"    /* "invalid type for global float register" */
#ifdef TARGET_CALL_USES_DESCRIPTOR
#define vargen_err_badinit "\251\343a\213\217$r%+ld"    /* "illegal initialisation to $r%+ld" */
#endif
#ifdef TARGET_IS_HELIOS
#define vg_err_dynamicinit "Ini\316\013dyn\377\237 \016\323\210\341 -ZR \
\034-ZL"    /* "Initialised dynamic array with -ZR or -ZL" */
#endif
#define vargen_rerr_nonaligned \
        "N\302\011\231\013ADCON a\002d\032a+0x%lx\375\373\004$r+0x%lx\265\
\274\002\217NULL"    /* "Non-aligned ADCON at data+0x%lx (value $r+0x%lx) set to NULL" */
#define vargen_rerr_datadata_reloc \
       "RISC OS\375\034o\275\003\265\020\360r\201\002modu\273h\240\204a\015\
c \001\215. \217d\032a $r"    /* "RISC OS (or other) reentrant module has static init. to data $r" */

#define lex_rerr_8_or_9 "d\220\2548 \0349 \300\344\036oct\037n\221b\003"    /* "digit 8 or 9 found in octal number" */
#define lex_rerr_pp_number "n\221b\244`\225\011l\210\300l\211w\013b\210\
\202t\017"    /* "number illegally followed by letter" */
#define lex_rerr_hex_exponent "h\207 n\221b\244c\201\212hav\004\276\006\
\360"    /* "hex number cannot have exponent" */
#define lex_rerr_esc16_truncated \
        "\363l\357\004\322cap\004'\\x%s%lx\026\354'\\x%lx'"    /* "overlarge escape '\\x%s%lx' treated as '\\x%lx'" */
#define lex_rerr_esc8_truncated "\363l\357\004\322cap\004'\\%o\026\354'\\\
%o'"    /* "overlarge escape '\\%o' treated as '\\%o'" */
#define lex_rerr_illegal_esc "\251\204r\035\322cap\004'\\%c\026\206\354\
%c"    /* "illegal string escape '\\%c' - treated as %c" */
#define lex_rerr_not1wchar "L'\350\026ne\005\014\207actl\2101 wid\004\252\
\233c\017"    /* "L'...' needs exactly 1 wide character" */
#define lex_rerr_empty_char "n\021\252\016\014\036\252\233c\214\370\201\
\002''"    /* "no chars in character constant ''" */
#define lex_rerr_overlong_char "m\010\004\275\201 4 \252\016\014\036\252\
\233c\214\370\201t"    /* "more than 4 chars in character constant" */

#define syn_rerr_array_0 "\016\323\210[0] \300\027d"    /* "array [0] found" */
#ifdef EXTENSION_VALOF
#define syn_rerr_void_valof "\335\200\312\203b\211ck\014\016\004\212p\003\
m\215t\005"    /* "void valof blocks are not permitted" */
#endif
#define syn_rerr_undeclared "\027\303\016\013na\261\305\001v\360\035'\207\
\017n \001\002%s'"    /* "undeclared name, inventing 'extern int %s'" */
#define syn_rerr_insert_parens \
        "p\016\030\275e\274\014(\234\265\001s\003\243\016o\344\276\020s\
\255\031\300l\211w\035$s"    /* "parentheses (..) inserted around expression following $s" */
#define syn_rerr_return "\020t\367n <\276r\257\251\260\335\200\270\272"    /* "return <expr> illegal for void function" */
#define syn_rerr_qualified_typedef(_b,_m) \
        "$m \250\005e\025$b h\240$m \020-sp\223i\310\005"    /* "$m typedef $b has $m re-specified" */, _m, _b, _m
#define syn_rerr_missing_type "m\355\337sp\223if\237a\213\206\376\026\342\
\005"    /* "missing type specification - 'int' assumed" */
#define syn_rerr_long_float "ANSI C doe\014\212s\311p\010\002'l\006\023\
\306\032'"    /* "ANSI C does not support 'long float'" */
#define syn_rerr_missing_type1 \
        "om\215\243<\250e\257\361\241\004\351\037\303\016\032\034\206\376\
\026\342\005"    /* "omitted <type> before formal declarator - 'int' assumed" */
#define syn_rerr_missing_type2 \
        "\325p\365to\337\351\037$r ne\005\014\337\034\227\177\014\206\376\
\026\342\005"    /* "function prototype formal $r needs type or class - 'int' assumed" */
#define syn_rerr_ellipsis_first "\253lip\255\014(\350\265c\201\212\346\006\
l\210\374\017"    /* "ellipsis (...) cannot be only parameter" */
#define syn_rerr_mixed_formals "p\365to\337\201\200old-\204y\273\374\017\014\
mix\005"    /* "prototype and old-style parameters mixed" */
#define syn_rerr_open_member "\251[] \261mb\003@$r"    /* "illegal [] member: $r" */
#define syn_rerr_ref_void "\251\337(\335\200&\265\354(\001\002&)"    /* "illegal type (void &) treated as (int &)" */
#define syn_rerr_ill_ref "$t \203\020f\003\030c\004\251-\206'&\026\277"    /* "$t of reference illegal -- '&' ignored" */
#define syn_rerr_fn_returntype "\325\020t\367n\035$t \251-\206\342\035\371"    /* "function returning $t illegal -- assuming pointer" */
#define syn_rerr_array_elttype "\016\323\210\203$t \251-\206\342\035\371"    /* "array of $t illegal -- assuming pointer" */
#define syn_rerr_fn_ptr(_m,_s) \
   "%s $r \242\210\212\346\325-\206\342\035\371"    /* "%s $r may not be function -- assuming pointer" */, errs_membobj(_m), _s
#define syn_rerr_fn_ptr1 \
        "\325$r \242\210\212\346\343\013\206\342\035\325\371"    /* "function $r may not be initialised - assuming function pointer" */
#define syn_rerr_archaic_init "Anci\030\002\351 \203\343a\272\305\256\004\
'='"    /* "Ancient form of initialisation, use '='" */
#define syn_rerr_bitfield "\251b\254\310\253\200\337$t\330\376\026\342\005"    /* "illegal bit field type $t - 'int' assumed" */
#define syn_rerr_ANSIbitfield "ANSI C \241bid\014b\254\310\253\200\337$\
t"    /* "ANSI C forbids bit field type $t" */
#define syn_rerr_missing_formal "\351\037n\377\004m\355\036\325\235i\272"    /* "formal name missing in function definition" */
#define syn_rerr_ineffective "\303\233\213\341 n\021eff\223t"    /* "declaration with no effect" */
#define syn_rerr_duplicate_member(sv,_b) "d\311\271\032\004\261mb\244$r \
\203$c"    /* "duplicate member $r of $c" */, sv, _b
#define syn_rerr_missing_type3 \
        "\337\034\227\177\014ne\005\013(\207cep\002\036\325\235i\272\265\
\206\376\026\342\005"    /* "type or class needed (except in function definition) - 'int' assumed" */
#define syn_rerr_semicolon_in_arglist \
        "',\026(\212';'\265\274p\016\032e\014\351\037\374\017s"    /* "',' (not ';') separates formal parameters" */
#define syn_rerr_no_members "$c h\240n\021\261mb\003s"    /* "$c has no members" */

      /* The following remain as ordinary (uncompressed) strings */

#define syn_moan_hashif "#if <expression>"
#define syn_moan_case "case expression (ignored)"

 /* The next batch of things just get mapped onto syserr codes */

#define syserr_genpointer "genpointer&(%ld)"
#define syserr_initsubstatic "initsubstatic(bit)"
#define syserr_initstatic "initstatic(%ld,%#lx)"
#define syserr_initstatic1 "initstatic(%ld)"
#define syserr_rd_decl_init "rd_decl/init(%#lx)"
#define syserr_rd_typename "rd_typename()=0"
#define syserr_rdinit "syn_rdinit"
#define syserr_rd_declarator "rd_declarator(%ld)"
#define syserr_defaultstgclass "defaultstorageclass(%#x)"
#define syserr_rd_declrhslist "rd_declrhslist confused"
#define syserr_rd_decl2 "rd_decl2(%p,%ld)"
#define syserr_rd_strdecl "rd_strdecl"
#define syserr_lex_string "lex_string"

/* end of cfe/feerrs.h */
/*
 * c4p/mcerrs.h - prototype for machine-specific error messages file
 */

  /* Ordinary error messages - mapped onto numeric codes */

/*
 * The following message is always machine specific since it may include
 * information about the source of support for the product.
 */
#define misc_disaster_banner   "\n\324\324\324\324\222*\n* Th\004\266\246\
`\244h\240\024t\336\201 \001\017n\037\001\226s\033t\030cy.\317Th\362c\201 \
occ\367\317*\n* b\223a\256\004\254h\240r\027 \216\002\203a v\215\037\020\
s\216rc\004su\252 \240\261m\010\210\034d\033k\317\317 *\n* spac\004\034\
b\223a\256\004\275\003\004\362a faul\002\036\215.\317I\025y\216 c\201\212\
e\177`\210\011\214 *\n* y\216r p\365g\323m \217a\335\200ca\256\035\275\362\
r\016\004fa`u\020\305p\202\177\004\226tac\002y\216r\317*\n* \024\011\003\
.\317Th\004\024\011\244\242\210\346\320\273\217h\253p y\216 imm\005i\032\
\253\210\201\200w`l \346\317*\n* \320\273\217\020p\010\002a s\256p\336\266\
\246`\244faul\002\217\275\004s\311p\010\002c\030\334.\317\317\317*\n\324\
\324\324\324\222*\n\n"    /* "\n\
*************************************************************************\n\
* The compiler has detected an internal inconsistency.  This can occur  *\n\
* because it has run out of a vital resource such as memory or disk     *\n\
* space or because there is a fault in it.  If you cannot easily alter  *\n\
* your program to avoid causing this rare failure, please contact your  *\n\
* dealer.  The dealer may be able to help you immediately and will be   *\n\
* able to report a suspected compiler fault to the support centre.      *\n\
*************************************************************************\n\
\n" */

  /* System failure messages - text not preserved */

#define syserr_fromq "C_FROMQ(%lx)"
#define syserr_local_address "local_address %lx"
#define syserr_local_addr "local_addr"
#define syserr_local_base "local_base %lx"
#define syserr_firstbit "firstbit"
#define syserr_outHW "outHW(%lx)"
#define syserr_litaddr "code/literal addressing error %lx"
#define syserr_unknown_labref_type "unknown label reference type %.8lx"
#define syserr_push "push(VARREGS)"
#define syserr_pop "pop(VARREGS)"
#define syserr_back_coderef "back. code. ref. off %#lx"
#define syserr_movc "MOVC overlong"
#define syserr_movr "movr r,r"
#define syserr_asymrrop "asymrrop"
#define syserr_movdr1 "MOVDR1 not finished"
#define syserr_movfdr "MOVF/DR r,r"
#define syserr_frrop "frrop"
#define syserr_show_inst "show_inst(%#lx)"
#define syserr_asmlab "odd asmlab(%lx)"
#define syserr_display_asm "display_asm(%lx)"
#define syserr_asm_trailer "asm_trailer(%ld)"
#define syserr_datalen "asm_data len=%ld"
#define syserr_asm_trailer1 "asm_trailer(%ldF%ld)"
#define syserr_asm_trailer2 "asm_trailer(LIT_ADCON rpt)"
#define syserr_asm_confused "Assembler output confused - find '?'"
#define syserr_debug_addr "local_fpaddress"

/* end of c4p/mcerrs.h */

#define COMPRESSED_MESSAGES 1

#define MSGSTACKDEPTH 5

#ifdef DEFINE_MSG_COMPRESSION_TABLE

static unsigned short int ecompression_info[256] = {
    0x0000, 0x696e, 0x7420, 0x6572, 0x6520, 0x6564, 0x6f6e, 0x2a2a, 
    0x6f72, 0x616c, 0x000a, 0x0520, 0x7320, 0x7469, 0x6172, 0x7403, 
    0x7265, 0x6f20, 0x0707, 0x6720, 0x6465, 0x6620, 0x2720, 0x756e, 
    0x656e, 0x0620, 0x6174, 0x6973, 0x0820, 0x0113, 0x0120, 0x0920, 
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
    0x0028, 0x0029, 0x002a, 0x002b, 0x002c, 0x002d, 0x002e, 0x002f, 
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
    0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f, 
    0x3a20, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
    0x0048, 0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f, 
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
    0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 
    0x696c, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 
    0x0068, 0x0069, 0x006a, 0x006b, 0x006c, 0x006d, 0x006e, 0x006f, 
    0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 
    0x0078, 0x0079, 0x007a, 0x007b, 0x007c, 0x007d, 0x007e, 0x6173, 
    0x6420, 0x616e, 0x6c65, 0x6f15, 0x7374, 0x6e6f, 0x2d20, 0x6578, 
    0x7920, 0x6c6f, 0x8502, 0x0d19, 0x0f20, 0x6974, 0x6f75, 0x7411, 
    0x6967, 0x756d, 0x1212, 0x6563, 0x1466, 0x8267, 0x6306, 0x636c, 
    0x706f, 0x906e, 0x951f, 0x0e61, 0x2e2e, 0x9401, 0x0174, 0x6963, 
    0x610c, 0x6608, 0x6d61, 0x740b, 0x0320, 0x9801, 0x6d70, 0x7479, 
    0xa770, 0x609a, 0x6368, 0x656c, 0x6902, 0x7369, 0x7573, 0x3e20, 
    0x661c, 0x6d65, 0x7603, 0x9908, 0x1763, 0x2920, 0x636f, 0x637f, 
    0x66b4, 0x6c9f, 0x0d06, 0x6c04, 0x7365, 0x7468, 0x8770, 0xb305, 
    0x666f, 0x778d, 0x062d, 0x1497, 0x1a0b, 0x2c20, 0x6689, 0x6275, 
    0x6669, 0x7570, 0x7609, 0x7f73, 0x0169, 0x091b, 0x0dcd, 0x2020, 
    0x6162, 0x6166, 0x6573, 0x7261, 0x9292, 0xb88b, 0xd18c, 0x201e, 
    0x2086, 0x6ec2, 0x6f69, 0x6f70, 0x7410, 0x76da, 0x93a3, 0xa804, 
    0xb702, 0xc168, 0xcb91, 0xccce, 0x1780, 0x1b73, 0x6204, 0x6c01, 
    0x9c2e, 0xa16d, 0xc4a0, 0xc702, 0xdcea, 0xe51d, 0x0372, 0x0e67, 
    0x1874, 0x6265, 0x690c, 0x6fb2, 0x7175, 0x726f, 0x709b, 0x7572, 
    0x9684, 0xa50f, 0xbede, 0xca75, 0xf6b1, 0x2028, 0x279e, 0x616d};


#endif

#endif  /* _msgs_LOADED */
/* end of errors.h */
