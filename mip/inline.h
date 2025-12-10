/*
 * inline.h: inline function expansion
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 */

/*
 * RCS $Revision: 1.13 $
 * Checkin $Date: 1995/10/31 15:39:23 $
 * Revising $Author: hmeekings $
 */

#ifndef _cgdefs_h
#  include "cgdefs.h"
#endif
#ifndef _jopcode_h
#  include "jopcode.h"
#endif

typedef struct Inline_SavedFn {
  struct CurrentFnDetails fndetails;
  BlockHead *top_block, *bottom_block;
  BindList *var_binders, *reg_binders;
} Inline_SavedFn;

typedef enum {
  T_Binder,
  T_AdconV,
  T_Adcon,
  T_Int,
  T_Plus
} Inline_ArgSubstSort;

typedef struct Inline_ArgSubstList Inline_ArgSubstList;
struct Inline_ArgSubstList {
  Inline_ArgSubstList *cdr;
  Binder *arg;
  union { Binder *b; Expr *ex; } replacement;
  Expr *rest;
  Inline_ArgSubstSort sort;
  int32 size;
  bool refsleft;
  bool notnull;
};

typedef struct {
  LabelNumber *exitlabel;
  BindList *env;
  int nresults;
  VRegnum resultregs[NARGREGS],
          newresultregs[NARGREGS];
  Inline_ArgSubstList *argreplace;
} Inline_RestoreControl;

Inline_SavedFn *Inline_FindFn(Binder *b);

bool Inline_Save(Binder *b, BindList *local_binders, BindList *regvar_binders);

#ifdef TARGET_IS_INTERPRETER
#define Inline_RealUse(s) ((void)(s))
#else
void Inline_RealUse(Binder *b);
#endif

void Inline_Restore(Inline_SavedFn *p, Inline_RestoreControl *rc);

void Inline_Init(void);
void Inline_Tidy(void);
