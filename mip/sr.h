/*
 * sr.h: binder live range splitting
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 * Copyright (C) Codemist Ltd., 1996
 */

/*
 * RCS $Revision: 1.2 $
 * Checkin $Date: 93/10/07 17:42:24 $
 * Revising $Author: irickard $
 */

#ifndef _sr_h
#define _sr_h 1

struct SuperBinder {
  SuperBinder *cdr;
  Binder *binder;
  int32 spillcount;
};

extern SuperBinder *superbinders;

extern BindList *splitranges(void);

extern void splitrange_init(void);

#endif
