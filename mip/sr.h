/*
 * sr.h: binder live range splitting
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 */

/*
 * RCS $Revision: 1.4 $
 * Checkin $Date: 1995/02/06 15:22:02 $
 * Revising $Author: enevill $
 */

struct SuperBinder {
  SuperBinder *cdr;
  Binder *binder;
  int32 spillcount;
};

extern SuperBinder *superbinders;

extern BindList *splitranges(BindList *local_binders, BindList *regvar_binders);

extern void splitrange_init(void);
