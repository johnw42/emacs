/* Replacement sys/ioctl.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _SYS_IOCTL_H
#define _SYS_IOCTL_H

int ioctl(int, int, void *);

#define FIONREAD 1
#define TCGETA 2

#endif  /* _SYS_IOCTL_H */

/* arch-tag: fa0c3dda-dbe0-4a49-86c4-7516c83c3c8c
   (do not change this comment) */
