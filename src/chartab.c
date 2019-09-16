/* chartab.c -- char-table support
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include "lisp.h"
#include "character.h"
#include "charset.h"

/* 64/16/32/128 */

/* Number of elements in Nth level char-table.  */
const int chartab_size[4] =
  { (1 << CHARTAB_SIZE_BITS_0),
    (1 << CHARTAB_SIZE_BITS_1),
    (1 << CHARTAB_SIZE_BITS_2),
    (1 << CHARTAB_SIZE_BITS_3) };

/* Number of characters each element of Nth level char-table
   covers.  */
static const int chartab_chars[4] =
  { (1 << (CHARTAB_SIZE_BITS_1 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_3)),
    (1 << (CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_3)),
    (1 << CHARTAB_SIZE_BITS_3),
    1 };

/* Number of characters (in bits) each element of Nth level char-table
   covers.  */
static const int chartab_bits[4] =
  { (CHARTAB_SIZE_BITS_1 + CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_3),
    (CHARTAB_SIZE_BITS_2 + CHARTAB_SIZE_BITS_3),
    CHARTAB_SIZE_BITS_3,
    0 };

#define CHARTAB_IDX(c, depth, min_char)		\
  (((c) - (min_char)) >> chartab_bits[(depth)])


/* Preamble for uniprop (Unicode character property) tables.  See the
   comment of "Unicode character property tables".  */

/* Types of decoder and encoder functions for uniprop values.  */
typedef Lisp_Object (*uniprop_decoder_t) (Lisp_Object, Lisp_Object);
typedef Lisp_Object (*uniprop_encoder_t) (Lisp_Object, Lisp_Object);

static Lisp_Object uniprop_table_uncompress (Lisp_Object, int);
static uniprop_decoder_t uniprop_get_decoder (Lisp_Object);

/* 1 iff TABLE is a uniprop table.  */
#define UNIPROP_TABLE_P(TABLE)					\
  (EQ (XCHAR_TABLE (TABLE)->purpose, Qchar_code_property_table)	\
   && CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (TABLE)) == 5)

/* Return a decoder for values in the uniprop table TABLE.  */
#define UNIPROP_GET_DECODER(TABLE)	\
  (UNIPROP_TABLE_P (TABLE) ? uniprop_get_decoder (TABLE) : NULL)

/* Nonzero iff OBJ is a string representing uniprop values of 128
   succeeding characters (the bottom level of a char-table) by a
   compressed format.  We are sure that no property value has a string
   starting with '\001' nor '\002'.  */
#define UNIPROP_COMPRESSED_FORM_P(OBJ)	\
  (STRINGP (OBJ) && SCHARS (OBJ) > 0	\
   && ((SREF (OBJ, 0) == 1 || (SREF (OBJ, 0) == 2))))

static void
CHECK_CHAR_TABLE (Lisp_Object x)
{
  ENTER_LISP_FRAME (x);
  CHECK_TYPE (CHAR_TABLE_P (x), Qchar_table_p, x);
  EXIT_LISP_FRAME_VOID ();
}

static void
set_char_table_ascii (Lisp_Object table, Lisp_Object val)
{
  ENTER_LISP_FRAME (table, val);
  XCHAR_TABLE (table)->ascii = val;
  EXIT_LISP_FRAME_VOID ();
}
static void
set_char_table_parent (Lisp_Object table, Lisp_Object val)
{
  ENTER_LISP_FRAME (table, val);
  XCHAR_TABLE (table)->parent = val;
  EXIT_LISP_FRAME_VOID ();
}

DEFUN ("make-char-table", Fmake_char_table, Smake_char_table, 1, 2, 0,
       doc: /* Return a newly created char-table, with purpose PURPOSE.
Each element is initialized to INIT, which defaults to nil.

PURPOSE should be a symbol.  If it has a `char-table-extra-slots'
property, the property's value should be an integer between 0 and 10
that specifies how many extra slots the char-table has.  Otherwise,
the char-table has no extra slot.  */)
  (Lisp_Object purpose, Lisp_Object init)
{
  ENTER_LISP_FRAME (purpose, init);
  LISP_LOCALS (vector, n);
  int n_extras;
  int size;

  CHECK_SYMBOL (purpose);
  n = Fget (purpose, Qchar_table_extra_slots);
  if (NILP (n))
    n_extras = 0;
  else
    {
      CHECK_NATNUM (n);
      if (XINT (n) > 10)
	args_out_of_range (n, Qnil);
      n_extras = XINT (n);
    }

  size = CHAR_TABLE_STANDARD_SLOTS + n_extras;
  vector = Fmake_vector (make_number (size), init);
  SETPVECTYPE (vector, PVEC_CHAR_TABLE);
  set_char_table_parent (vector, Qnil);
  set_char_table_purpose (vector, purpose);
  XSETCHAR_TABLE (vector, XCHAR_TABLE (vector));
  EXIT_LISP_FRAME (vector);
}

static Lisp_Object
make_sub_char_table (int depth, int min_char, Lisp_Object defalt)
{
  ENTER_LISP_FRAME (defalt);
  LISP_LOCALS (table);
  int i;
  table = make_uninit_sub_char_table (depth, min_char);


  for (i = 0; i < chartab_size[depth]; i++)
    XSUB_CHAR_TABLE (table)->contents[i] = defalt;
  EXIT_LISP_FRAME (table);
}

static Lisp_Object
char_table_ascii (Lisp_Object table)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (sub, val);

  sub = XCHAR_TABLE (table)->contents[0];
  if (! SUB_CHAR_TABLE_P (sub))
    EXIT_LISP_FRAME (sub);
  sub = XSUB_CHAR_TABLE (sub)->contents[0];
  if (! SUB_CHAR_TABLE_P (sub))
    EXIT_LISP_FRAME (sub);
  val = XSUB_CHAR_TABLE (sub)->contents[0];
  if (UNIPROP_TABLE_P (table) && UNIPROP_COMPRESSED_FORM_P (val))
    val = uniprop_table_uncompress (sub, 0);
  EXIT_LISP_FRAME (val);
}

static Lisp_Object
copy_sub_char_table (Lisp_Object table)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (copy, val);
  int depth = XSUB_CHAR_TABLE (table)->depth;
  int min_char = XSUB_CHAR_TABLE (table)->min_char;
  copy = make_sub_char_table (depth, min_char, Qnil);

  int i;

  /* Recursively copy any sub char-tables.  */
  for (i = 0; i < chartab_size[depth]; i++)
    {
      val = XSUB_CHAR_TABLE (table)->contents[i];

      set_sub_char_table_contents
	(copy, i, SUB_CHAR_TABLE_P (val) ? copy_sub_char_table (val) : val);
    }

  EXIT_LISP_FRAME (copy);
}


Lisp_Object
copy_char_table (Lisp_Object table)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (copy);
  int size = PVSIZE (table);
  int i;

  copy = Fmake_vector (make_number (size), Qnil);
  SETPVECTYPE (copy, PVEC_CHAR_TABLE);
  set_char_table_defalt (copy, XCHAR_TABLE (table)->defalt);
  set_char_table_parent (copy, XCHAR_TABLE (table)->parent);
  set_char_table_purpose (copy, XCHAR_TABLE (table)->purpose);
  for (i = 0; i < chartab_size[0]; i++)
    set_char_table_contents
      (copy, i,
       (SUB_CHAR_TABLE_P (XCHAR_TABLE (table)->contents[i])
	? copy_sub_char_table (XCHAR_TABLE (table)->contents[i])
	: XCHAR_TABLE (table)->contents[i]));
  set_char_table_ascii (copy, char_table_ascii (copy));
  size -= CHAR_TABLE_STANDARD_SLOTS;
  for (i = 0; i < size; i++)
    set_char_table_extras (copy, i, XCHAR_TABLE (table)->extras[i]);

  XSETCHAR_TABLE (copy, XCHAR_TABLE (copy));
  EXIT_LISP_FRAME (copy);
}

static Lisp_Object
sub_char_table_ref (Lisp_Object table, int c, bool is_uniprop)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (val);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int idx = CHARTAB_IDX (c, tbl->depth, tbl->min_char);

  val = tbl->contents[idx];
  if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (val))
    val = uniprop_table_uncompress (table, idx);
  if (SUB_CHAR_TABLE_P (val))
    val = sub_char_table_ref (val, c, is_uniprop);
  EXIT_LISP_FRAME (val);
}

Lisp_Object
char_table_ref (Lisp_Object table, int c)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (val);
  struct Lisp_Char_Table *tbl = XCHAR_TABLE (table);

  if (ASCII_CHAR_P (c))
    {
      val = tbl->ascii;
      if (SUB_CHAR_TABLE_P (val))
	val = XSUB_CHAR_TABLE (val)->contents[c];
    }
  else
    {
      val = tbl->contents[CHARTAB_IDX (c, 0, 0)];
      if (SUB_CHAR_TABLE_P (val))
	val = sub_char_table_ref (val, c, UNIPROP_TABLE_P (table));
    }
  if (NILP (val))
    {
      val = tbl->defalt;
      if (NILP (val) && CHAR_TABLE_P (tbl->parent))
	val = char_table_ref (tbl->parent, c);
    }
  EXIT_LISP_FRAME (val);
}

static Lisp_Object
sub_char_table_ref_and_range (Lisp_Object table, int c, int *from, int *to,
			      Lisp_Object defalt, bool is_uniprop)
{
  ENTER_LISP_FRAME (table, defalt);
  LISP_LOCALS (val, this_val);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int depth = tbl->depth, min_char = tbl->min_char;
  int chartab_idx = CHARTAB_IDX (c, depth, min_char), idx;

  val = tbl->contents[chartab_idx];
  if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (val))
    val = uniprop_table_uncompress (table, chartab_idx);
  if (SUB_CHAR_TABLE_P (val))
    val = sub_char_table_ref_and_range (val, c, from, to, defalt, is_uniprop);
  else if (NILP (val))
    val = defalt;

  idx = chartab_idx;
  while (idx > 0 && *from < min_char + idx * chartab_chars[depth])
    {

      c = min_char + idx * chartab_chars[depth] - 1;
      idx--;
      this_val = tbl->contents[idx];
      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (this_val))
	this_val = uniprop_table_uncompress (table, idx);
      if (SUB_CHAR_TABLE_P (this_val))
	this_val = sub_char_table_ref_and_range (this_val, c, from, to, defalt,
						 is_uniprop);
      else if (NILP (this_val))
	this_val = defalt;

      if (! EQ (this_val, val))
	{
	  *from = c + 1;
	  break;
	}
    }
  while (((c = (chartab_idx + 1) * chartab_chars[depth])
	  < chartab_chars[depth - 1])
	 && (c += min_char) <= *to)
    {

      chartab_idx++;
      this_val = tbl->contents[chartab_idx];
      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (this_val))
	this_val = uniprop_table_uncompress (table, chartab_idx);
      if (SUB_CHAR_TABLE_P (this_val))
	this_val = sub_char_table_ref_and_range (this_val, c, from, to, defalt,
						 is_uniprop);
      else if (NILP (this_val))
	this_val = defalt;
      if (! EQ (this_val, val))
	{
	  *to = c - 1;
	  break;
	}
    }

  EXIT_LISP_FRAME (val);
}


/* Return the value for C in char-table TABLE.  Shrink the range *FROM
   and *TO to cover characters (containing C) that have the same value
   as C.  It is not assured that the values of (*FROM - 1) and (*TO +
   1) are different from that of C.  */

Lisp_Object
char_table_ref_and_range (Lisp_Object table, int c, int *from, int *to)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (val, this_val);
  struct Lisp_Char_Table *tbl = XCHAR_TABLE (table);
  int chartab_idx = CHARTAB_IDX (c, 0, 0), idx;
  bool is_uniprop = UNIPROP_TABLE_P (table);

  val = tbl->contents[chartab_idx];
  if (*from < 0)
    *from = 0;
  if (*to < 0)
    *to = MAX_CHAR;
  if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (val))
    val = uniprop_table_uncompress (table, chartab_idx);
  if (SUB_CHAR_TABLE_P (val))
    val = sub_char_table_ref_and_range (val, c, from, to, tbl->defalt,
					is_uniprop);
  else if (NILP (val))
    val = tbl->defalt;
  idx = chartab_idx;
  while (*from < idx * chartab_chars[0])
    {

      c = idx * chartab_chars[0] - 1;
      idx--;
      this_val = tbl->contents[idx];
      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (this_val))
	this_val = uniprop_table_uncompress (table, idx);
      if (SUB_CHAR_TABLE_P (this_val))
	this_val = sub_char_table_ref_and_range (this_val, c, from, to,
						 tbl->defalt, is_uniprop);
      else if (NILP (this_val))
	this_val = tbl->defalt;

      if (! EQ (this_val, val))
	{
	  *from = c + 1;
	  break;
	}
    }
  while (*to >= (chartab_idx + 1) * chartab_chars[0])
    {

      chartab_idx++;
      c = chartab_idx * chartab_chars[0];
      this_val = tbl->contents[chartab_idx];
      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (this_val))
	this_val = uniprop_table_uncompress (table, chartab_idx);
      if (SUB_CHAR_TABLE_P (this_val))
	this_val = sub_char_table_ref_and_range (this_val, c, from, to,
						 tbl->defalt, is_uniprop);
      else if (NILP (this_val))
	this_val = tbl->defalt;
      if (! EQ (this_val, val))
	{
	  *to = c - 1;
	  break;
	}
    }

  EXIT_LISP_FRAME (val);
}


static void
sub_char_table_set (Lisp_Object table, int c, Lisp_Object val, bool is_uniprop)
{
  ENTER_LISP_FRAME (table, val);
  LISP_LOCALS (sub);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int depth = tbl->depth, min_char = tbl->min_char;
  int i = CHARTAB_IDX (c, depth, min_char);

  if (depth == 3)
    set_sub_char_table_contents (table, i, val);
  else
    {
      sub = tbl->contents[i];
      if (! SUB_CHAR_TABLE_P (sub))
	{
	  if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (sub))
	    sub = uniprop_table_uncompress (table, i);
	  else
	    {
	      sub = make_sub_char_table (depth + 1,
					 min_char + i * chartab_chars[depth],
					 sub);
	      set_sub_char_table_contents (table, i, sub);
	    }
	}
      sub_char_table_set (sub, c, val, is_uniprop);
    }
  EXIT_LISP_FRAME_VOID ();
}

void
char_table_set (Lisp_Object table, int c, Lisp_Object val)
{
  ENTER_LISP_FRAME (table, val);
  LISP_LOCALS (sub);
  struct Lisp_Char_Table *tbl = XCHAR_TABLE (table);

  if (ASCII_CHAR_P (c)
      && SUB_CHAR_TABLE_P (tbl->ascii))
    set_sub_char_table_contents (tbl->ascii, c, val);
  else
    {
      int i = CHARTAB_IDX (c, 0, 0);

      sub = tbl->contents[i];
      if (! SUB_CHAR_TABLE_P (sub))
	{
	  sub = make_sub_char_table (1, i * chartab_chars[0], sub);
	  set_char_table_contents (table, i, sub);
	}
      sub_char_table_set (sub, c, val, UNIPROP_TABLE_P (table));
      if (ASCII_CHAR_P (c))
	set_char_table_ascii (table, char_table_ascii (table));
    }
  EXIT_LISP_FRAME_VOID ();
}

static void
sub_char_table_set_range (Lisp_Object table, int from, int to, Lisp_Object val,
			  bool is_uniprop)
{
  ENTER_LISP_FRAME (table, val);
  LISP_LOCALS (sub);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int depth = tbl->depth, min_char = tbl->min_char;
  int chars_in_block = chartab_chars[depth];
  int i, c, lim = chartab_size[depth];

  if (from < min_char)
    from = min_char;
  i = CHARTAB_IDX (from, depth, min_char);
  c = min_char + chars_in_block * i;
  for (; i < lim; i++, c += chars_in_block)
    {
      if (c > to)
	break;
      if (from <= c && c + chars_in_block - 1 <= to)
	set_sub_char_table_contents (table, i, val);
      else
	{
	  sub = tbl->contents[i];

	  if (! SUB_CHAR_TABLE_P (sub))
	    {
	      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (sub))
		sub = uniprop_table_uncompress (table, i);
	      else
		{
		  sub = make_sub_char_table (depth + 1, c, sub);
		  set_sub_char_table_contents (table, i, sub);
		}
	    }
	  sub_char_table_set_range (sub, from, to, val, is_uniprop);
	}
    }
  EXIT_LISP_FRAME_VOID ();
}


void
char_table_set_range (Lisp_Object table, int from, int to, Lisp_Object val)
{
  ENTER_LISP_FRAME (table, val);
  LISP_LOCALS (sub);
  struct Lisp_Char_Table *tbl = XCHAR_TABLE (table);

  if (from == to)
    char_table_set (table, from, val);
  else
    {
      bool is_uniprop = UNIPROP_TABLE_P (table);
      int lim = CHARTAB_IDX (to, 0, 0);
      int i, c;

      for (i = CHARTAB_IDX (from, 0, 0), c = i * chartab_chars[0]; i <= lim;
	   i++, c += chartab_chars[0])
	{
	  if (c > to)
	    break;
	  if (from <= c && c + chartab_chars[0] - 1 <= to)
	    set_char_table_contents (table, i, val);
	  else
	    {
	      sub = tbl->contents[i];

	      if (! SUB_CHAR_TABLE_P (sub))
		{
		  sub = make_sub_char_table (1, i * chartab_chars[0], sub);
		  set_char_table_contents (table, i, sub);
		}
	      sub_char_table_set_range (sub, from, to, val, is_uniprop);
	    }
	}
      if (ASCII_CHAR_P (from))
	set_char_table_ascii (table, char_table_ascii (table));
    }
  EXIT_LISP_FRAME_VOID ();
}


DEFUN ("char-table-subtype", Fchar_table_subtype, Schar_table_subtype,
       1, 1, 0,
       doc: /*
Return the subtype of char-table CHAR-TABLE.  The value is a symbol.  */)
  (Lisp_Object char_table)
{
  ENTER_LISP_FRAME (char_table);
  CHECK_CHAR_TABLE (char_table);

  EXIT_LISP_FRAME (XCHAR_TABLE (char_table)->purpose);
}

DEFUN ("char-table-parent", Fchar_table_parent, Schar_table_parent,
       1, 1, 0,
       doc: /* Return the parent char-table of CHAR-TABLE.
The value is either nil or another char-table.
If CHAR-TABLE holds nil for a given character,
then the actual applicable value is inherited from the parent char-table
\(or from its parents, if necessary).  */)
  (Lisp_Object char_table)
{
  ENTER_LISP_FRAME (char_table);
  CHECK_CHAR_TABLE (char_table);

  EXIT_LISP_FRAME (XCHAR_TABLE (char_table)->parent);
}

DEFUN ("set-char-table-parent", Fset_char_table_parent, Sset_char_table_parent,
       2, 2, 0,
       doc: /* Set the parent char-table of CHAR-TABLE to PARENT.
Return PARENT.  PARENT must be either nil or another char-table.  */)
  (Lisp_Object char_table, Lisp_Object parent)
{
  ENTER_LISP_FRAME (char_table, parent);
  LISP_LOCALS (temp);

  CHECK_CHAR_TABLE (char_table);

  if (!NILP (parent))
    {
      CHECK_CHAR_TABLE (parent);

      for (temp = parent; !NILP (temp); temp = XCHAR_TABLE (temp)->parent)
	if (EQ (temp, char_table))
	  error ("Attempt to make a chartable be its own parent");
    }

  set_char_table_parent (char_table, parent);

  EXIT_LISP_FRAME (parent);
}

DEFUN ("char-table-extra-slot", Fchar_table_extra_slot, Schar_table_extra_slot,
       2, 2, 0,
       doc: /* Return the value of CHAR-TABLE's extra-slot number N.  */)
  (Lisp_Object char_table, Lisp_Object n)
{
  ENTER_LISP_FRAME (char_table, n);
  CHECK_CHAR_TABLE (char_table);
  CHECK_NUMBER (n);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  EXIT_LISP_FRAME (XCHAR_TABLE (char_table)->extras[XINT (n)]);
}

DEFUN ("set-char-table-extra-slot", Fset_char_table_extra_slot,
       Sset_char_table_extra_slot,
       3, 3, 0,
       doc: /* Set CHAR-TABLE's extra-slot number N to VALUE.  */)
  (Lisp_Object char_table, Lisp_Object n, Lisp_Object value)
{
  ENTER_LISP_FRAME (char_table, n, value);
  CHECK_CHAR_TABLE (char_table);
  CHECK_NUMBER (n);
  if (XINT (n) < 0
      || XINT (n) >= CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (char_table)))
    args_out_of_range (char_table, n);

  set_char_table_extras (char_table, XINT (n), value);
  EXIT_LISP_FRAME (value);
}

DEFUN ("char-table-range", Fchar_table_range, Schar_table_range,
       2, 2, 0,
       doc: /* Return the value in CHAR-TABLE for a range of characters RANGE.
RANGE should be nil (for the default value),
a cons of character codes (for characters in the range), or a character code.  */)
  (Lisp_Object char_table, Lisp_Object range)
{
  ENTER_LISP_FRAME (char_table, range);
  LISP_LOCALS (val);
  CHECK_CHAR_TABLE (char_table);

  if (EQ (range, Qnil))
    val = XCHAR_TABLE (char_table)->defalt;
  else if (CHARACTERP (range))
    val = CHAR_TABLE_REF (char_table, XFASTINT (range));
  else if (CONSP (range))
    {
      int from, to;

      CHECK_CHARACTER_CAR (range);
      CHECK_CHARACTER_CDR (range);
      from = XFASTINT (XCAR (range));
      to = XFASTINT (XCDR (range));
      val = char_table_ref_and_range (char_table, from, &from, &to);
      /* Not yet implemented. */
    }
  else
    error ("Invalid RANGE argument to `char-table-range'");
  EXIT_LISP_FRAME (val);
}

DEFUN ("set-char-table-range", Fset_char_table_range, Sset_char_table_range,
       3, 3, 0,
       doc: /* Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
RANGE should be t (for all characters), nil (for the default value),
a cons of character codes (for characters in the range),
or a character code.  Return VALUE.  */)
  (Lisp_Object char_table, Lisp_Object range, Lisp_Object value)
{
  ENTER_LISP_FRAME (char_table, range, value);
  CHECK_CHAR_TABLE (char_table);
  if (EQ (range, Qt))
    {
      int i;

      set_char_table_ascii (char_table, value);
      for (i = 0; i < chartab_size[0]; i++)
	set_char_table_contents (char_table, i, value);
    }
  else if (EQ (range, Qnil))
    set_char_table_defalt (char_table, value);
  else if (CHARACTERP (range))
    char_table_set (char_table, XINT (range), value);
  else if (CONSP (range))
    {
      CHECK_CHARACTER_CAR (range);
      CHECK_CHARACTER_CDR (range);
      char_table_set_range (char_table,
			    XINT (XCAR (range)), XINT (XCDR (range)), value);
    }
  else
    error ("Invalid RANGE argument to `set-char-table-range'");

  EXIT_LISP_FRAME (value);
}

static Lisp_Object
optimize_sub_char_table (Lisp_Object table, Lisp_Object test)
{
  ENTER_LISP_FRAME (table, test);
  LISP_LOCALS (elt, this);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int i, depth = tbl->depth;
  bool optimizable;

  elt = XSUB_CHAR_TABLE (table)->contents[0];
  if (SUB_CHAR_TABLE_P (elt))
    {
      elt = optimize_sub_char_table (elt, test);
      set_sub_char_table_contents (table, 0, elt);
    }
  optimizable = SUB_CHAR_TABLE_P (elt) ? 0 : 1;
  for (i = 1; i < chartab_size[depth]; i++)
    {
      this = XSUB_CHAR_TABLE (table)->contents[i];
      if (SUB_CHAR_TABLE_P (this))
	{
	  this = optimize_sub_char_table (this, test);
	  set_sub_char_table_contents (table, i, this);
	}
      if (optimizable
	  && (NILP (test) ? NILP (Fequal (this, elt)) /* defaults to `equal'. */
	      : EQ (test, Qeq) ? !EQ (this, elt)      /* Optimize `eq' case.  */
	      : NILP (call2 (test, this, elt))))
	optimizable = 0;
    }

  EXIT_LISP_FRAME ((optimizable ? elt : table));
}

DEFUN ("optimize-char-table", Foptimize_char_table, Soptimize_char_table,
       1, 2, 0,
       doc: /* Optimize CHAR-TABLE.
TEST is the comparison function used to decide whether two entries are
equivalent and can be merged.  It defaults to `equal'.  */)
  (Lisp_Object char_table, Lisp_Object test)
{
  ENTER_LISP_FRAME (char_table, test);
  LISP_LOCALS (elt);
  int i;

  CHECK_CHAR_TABLE (char_table);

  for (i = 0; i < chartab_size[0]; i++)
    {
      elt = XCHAR_TABLE (char_table)->contents[i];
      if (SUB_CHAR_TABLE_P (elt))
	set_char_table_contents
	  (char_table, i, optimize_sub_char_table (elt, test));
    }
  /* Reset the `ascii' cache, in case it got optimized away.  */
  set_char_table_ascii (char_table, char_table_ascii (char_table));

  EXIT_LISP_FRAME (Qnil);
}


/* Map C_FUNCTION or FUNCTION over TABLE (top or sub char-table),
   calling it for each character or group of characters that share a
   value.  RANGE is a cons (FROM . TO) specifying the range of target
   characters, VAL is a value of FROM in TABLE, TOP is the top
   char-table.

   ARG is passed to C_FUNCTION when that is called.

   It returns the value of last character covered by TABLE (not the
   value inherited from the parent), and by side-effect, the car part
   of RANGE is updated to the minimum character C where C and all the
   following characters in TABLE have the same value.  */

static Lisp_Object
map_sub_char_table (void (*c_function) (Lisp_Object, Lisp_Object, Lisp_Object),
		    Lisp_Object function, Lisp_Object table, Lisp_Object arg, Lisp_Object val,
		    Lisp_Object range, Lisp_Object top)
{
  ENTER_LISP_FRAME (function, table, arg, val, range, top);
  LISP_LOCALS (this, parent, temp);
  /* Depth of TABLE.  */
  int depth;
  /* Minimum and maximum characters covered by TABLE. */
  int min_char, max_char;
  /* Number of characters covered by one element of TABLE.  */
  int chars_in_block;
  int from = XINT (XCAR (range)), to = XINT (XCDR (range));
  int i, c;
  bool is_uniprop = UNIPROP_TABLE_P (top);
  uniprop_decoder_t decoder = UNIPROP_GET_DECODER (top);

  if (SUB_CHAR_TABLE_P (table))
    {
      struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);

      depth = tbl->depth;
      min_char = tbl->min_char;
      max_char = min_char + chartab_chars[depth - 1] - 1;
    }
  else
    {
      depth = 0;
      min_char = 0;
      max_char = MAX_CHAR;
    }
  chars_in_block = chartab_chars[depth];

  if (to < max_char)
    max_char = to;
  /* Set I to the index of the first element to check.  */
  if (from <= min_char)
    i = 0;
  else
    i = (from - min_char) / chars_in_block;
  for (c = min_char + chars_in_block * i; c <= max_char;
       i++, c += chars_in_block)
    {
      this = (SUB_CHAR_TABLE_P (table)
			  ? XSUB_CHAR_TABLE (table)->contents[i]
			  : XCHAR_TABLE (table)->contents[i]);

      int nextc = c + chars_in_block;

      if (is_uniprop && UNIPROP_COMPRESSED_FORM_P (this))
	this = uniprop_table_uncompress (table, i);
      if (SUB_CHAR_TABLE_P (this))
	{
	  if (to >= nextc)
	    XSETCDR (range, make_number (nextc - 1));
	  val = map_sub_char_table (c_function, function, this, arg,
				    val, range, top);
	}
      else
	{
	  if (NILP (this))
	    this = XCHAR_TABLE (top)->defalt;
	  if (!EQ (val, this))
	    {
	      bool different_value = 1;

	      if (NILP (val))
		{
		  if (! NILP (XCHAR_TABLE (top)->parent))
		    {
		      parent = XCHAR_TABLE (top)->parent;

		      temp = XCHAR_TABLE (parent)->parent;


		      /* This is to get a value of FROM in PARENT
			 without checking the parent of PARENT.  */
		      set_char_table_parent (parent, Qnil);
		      val = CHAR_TABLE_REF (parent, from);
		      set_char_table_parent (parent, temp);
		      XSETCDR (range, make_number (c - 1));
		      val = map_sub_char_table (c_function, function,
						parent, arg, val, range,
						parent);
		      if (EQ (val, this))
			different_value = 0;
		    }
		}
	      if (! NILP (val) && different_value)
		{
		  XSETCDR (range, make_number (c - 1));
		  if (EQ (XCAR (range), XCDR (range)))
		    {
		      if (c_function)
			(*c_function) (arg, XCAR (range), val);
		      else
			{
			  if (decoder)
			    val = decoder (top, val);
			  call2 (function, XCAR (range), val);
			}
		    }
		  else
		    {
		      if (c_function)
			(*c_function) (arg, range, val);
		      else
			{
			  if (decoder)
			    val = decoder (top, val);
			  call2 (function, range, val);
			}
		    }
		}
	      val = this;
	      from = c;
	      XSETCAR (range, make_number (c));
	    }
	}
      XSETCDR (range, make_number (to));
    }
  EXIT_LISP_FRAME (val);
}


/* Map C_FUNCTION or FUNCTION over TABLE, calling it for each
   character or group of characters that share a value.

   ARG is passed to C_FUNCTION when that is called.  */

void
map_char_table (void (*c_function) (Lisp_Object, Lisp_Object, Lisp_Object),
		Lisp_Object function, Lisp_Object table, Lisp_Object arg)
{
  ENTER_LISP_FRAME (function, table, arg);
  LISP_LOCALS (range, val, parent, temp);
  uniprop_decoder_t decoder = UNIPROP_GET_DECODER (table);

  range = Fcons (make_number (0), make_number (MAX_CHAR));
  parent = XCHAR_TABLE (table)->parent;

  val = XCHAR_TABLE (table)->ascii;
  if (SUB_CHAR_TABLE_P (val))
    val = XSUB_CHAR_TABLE (val)->contents[0];
  val = map_sub_char_table (c_function, function, table, arg, val, range,
			    table);

  /* If VAL is nil and TABLE has a parent, we must consult the parent
     recursively.  */
  while (NILP (val) && ! NILP (XCHAR_TABLE (table)->parent))
    {
      int from = XINT (XCAR (range));

      parent = XCHAR_TABLE (table)->parent;
      temp = XCHAR_TABLE (parent)->parent;
      /* This is to get a value of FROM in PARENT without checking the
	 parent of PARENT.  */
      set_char_table_parent (parent, Qnil);
      val = CHAR_TABLE_REF (parent, from);
      set_char_table_parent (parent, temp);
      val = map_sub_char_table (c_function, function, parent, arg, val, range,
				parent);
      table = parent;
    }

  if (! NILP (val))
    {
      if (EQ (XCAR (range), XCDR (range)))
	{
	  if (c_function)
	    (*c_function) (arg, XCAR (range), val);
	  else
	    {
	      if (decoder)
		val = decoder (table, val);
	      call2 (function, XCAR (range), val);
	    }
	}
      else
	{
	  if (c_function)
	    (*c_function) (arg, range, val);
	  else
	    {
	      if (decoder)
		val = decoder (table, val);
	      call2 (function, range, val);
	    }
	}
    }
  EXIT_LISP_FRAME_VOID ();
}

DEFUN ("map-char-table", Fmap_char_table, Smap_char_table,
  2, 2, 0,
       doc: /* Call FUNCTION for each character in CHAR-TABLE that has non-nil value.
FUNCTION is called with two arguments, KEY and VALUE.
KEY is a character code or a cons of character codes specifying a
range of characters that have the same value.
VALUE is what (char-table-range CHAR-TABLE KEY) returns.  */)
  (Lisp_Object function, Lisp_Object char_table)
{
  ENTER_LISP_FRAME (function, char_table);
  CHECK_CHAR_TABLE (char_table);

  map_char_table (NULL, function, char_table, char_table);
  EXIT_LISP_FRAME (Qnil);
}


static void
map_sub_char_table_for_charset (void (*c_function) (Lisp_Object, Lisp_Object),
				Lisp_Object function, Lisp_Object table, Lisp_Object arg,
				Lisp_Object range, struct charset *charset,
				unsigned from, unsigned to)
{
  ENTER_LISP_FRAME (function, table, arg, range);
  LISP_LOCALS (this);
  struct Lisp_Sub_Char_Table *tbl = XSUB_CHAR_TABLE (table);
  int i, c = tbl->min_char, depth = tbl->depth;

  if (depth < 3)
    for (i = 0; i < chartab_size[depth]; i++, c += chartab_chars[depth])
      {

	this = tbl->contents[i];
	if (SUB_CHAR_TABLE_P (this))
	  map_sub_char_table_for_charset (c_function, function, this, arg,
					  range, charset, from, to);
	else
	  {
	    if (! NILP (XCAR (range)))
	      {
		XSETCDR (range, make_number (c - 1));
		if (c_function)
		  (*c_function) (arg, range);
		else
		  call2 (function, range, arg);
	      }
	    XSETCAR (range, Qnil);
	  }
      }
  else
    for (i = 0; i < chartab_size[depth]; i++, c++)
      {
	unsigned code;

	this = tbl->contents[i];
	if (NILP (this)
	    || (charset
		&& (code = ENCODE_CHAR (charset, c),
		    (code < from || code > to))))
	  {
	    if (! NILP (XCAR (range)))
	      {
		XSETCDR (range, make_number (c - 1));
		if (c_function)
		  (*c_function) (arg, range);
		else
		  call2 (function, range, arg);
		XSETCAR (range, Qnil);
	      }
	  }
	else
	  {
	    if (NILP (XCAR (range)))
	      XSETCAR (range, make_number (c));
	  }
      }
  EXIT_LISP_FRAME_VOID ();
}


/* Support function for `map-charset-chars'.  Map C_FUNCTION or
   FUNCTION over TABLE, calling it for each character or a group of
   succeeding characters that have non-nil value in TABLE.  TABLE is a
   "mapping table" or a "deunifier table" of a certain charset.

   If CHARSET is not NULL (this is the case that `map-charset-chars'
   is called with non-nil FROM-CODE and TO-CODE), it is a charset who
   owns TABLE, and the function is called only on a character in the
   range FROM and TO.  FROM and TO are not character codes, but code
   points of a character in CHARSET.

   This function is called in these two cases:

   (1) A charset has a mapping file name in :map property.

   (2) A charset has an upper code space in :offset property and a
   mapping file name in :unify-map property.  In this case, this
   function is called only for characters in the Unicode code space.
   Characters in upper code space are handled directly in
   map_charset_chars.  */

void
map_char_table_for_charset (void (*c_function) (Lisp_Object, Lisp_Object),
			    Lisp_Object function, Lisp_Object table, Lisp_Object arg,
			    struct charset *charset,
			    unsigned from, unsigned to)
{
  ENTER_LISP_FRAME (function, table, arg);
  LISP_LOCALS (range, this);
  int c, i;

  range = Fcons (Qnil, Qnil);

  for (i = 0, c = 0; i < chartab_size[0]; i++, c += chartab_chars[0])
    {

      this = XCHAR_TABLE (table)->contents[i];
      if (SUB_CHAR_TABLE_P (this))
	map_sub_char_table_for_charset (c_function, function, this, arg,
					range, charset, from, to);
      else
	{
	  if (! NILP (XCAR (range)))
	    {
	      XSETCDR (range, make_number (c - 1));
	      if (c_function)
		(*c_function) (arg, range);
	      else
		call2 (function, range, arg);
	    }
	  XSETCAR (range, Qnil);
	}
    }
  if (! NILP (XCAR (range)))
    {
      XSETCDR (range, make_number (c - 1));
      if (c_function)
	(*c_function) (arg, range);
      else
	call2 (function, range, arg);
    }
  EXIT_LISP_FRAME_VOID ();
}


/* Unicode character property tables.

   This section provides a convenient and efficient way to get Unicode
   character properties of characters from C code (from Lisp, you must
   use get-char-code-property).

   The typical usage is to get a char-table object for a specific
   property like this (use of the "bidi-class" property below is just
   an example):

	Lisp_Object bidi_class_table = uniprop_table (intern ("bidi-class"));

   (uniprop_table can return nil if it fails to find data for the
   named property, or if it fails to load the appropriate Lisp support
   file, so the return value should be tested to be non-nil, before it
   is used.)

   To get a property value for character CH use CHAR_TABLE_REF:

	Lisp_Object bidi_class = CHAR_TABLE_REF (bidi_class_table, CH);

   In this case, what you actually get is an index number to the
   vector of property values (symbols nil, L, R, etc).

   The full list of Unicode character properties supported by Emacs is
   documented in the ELisp manual, in the node "Character Properties".

   A table for Unicode character property has these characteristics:

   o The purpose is `char-code-property-table', which implies that the
   table has 5 extra slots.

   o The second extra slot is a Lisp function, an index (integer) to
   the array uniprop_decoder[], or nil.  If it is a Lisp function, we
   can't use such a table from C (at the moment).  If it is nil, it
   means that we don't have to decode values.

   o The third extra slot is a Lisp function, an index (integer) to
   the array uniprop_encoder[], or nil.  If it is a Lisp function, we
   can't use such a table from C (at the moment).  If it is nil, it
   means that we don't have to encode values.  */


/* Uncompress the IDXth element of sub-char-table TABLE.  */

static Lisp_Object
uniprop_table_uncompress (Lisp_Object table, int idx)
{
  ENTER_LISP_FRAME (table);
  LISP_LOCALS (val, sub);
  val = XSUB_CHAR_TABLE (table)->contents[idx];

  int min_char = XSUB_CHAR_TABLE (table)->min_char + chartab_chars[2] * idx;
  sub = make_sub_char_table (3, min_char, Qnil);

  const unsigned char *p, *pend;

  set_sub_char_table_contents (table, idx, sub);
  p = SDATA (val), pend = p + SBYTES (val);
  if (*p == 1)
    {
      /* SIMPLE TABLE */
      p++;
      idx = STRING_CHAR_ADVANCE (p);
      while (p < pend && idx < chartab_chars[2])
	{
	  int v = STRING_CHAR_ADVANCE (p);
	  set_sub_char_table_contents
	    (sub, idx++, v > 0 ? make_number (v) : Qnil);
	}
    }
  else if (*p == 2)
    {
      /* RUN-LENGTH TABLE */
      p++;
      for (idx = 0; p < pend; )
	{
	  int v = STRING_CHAR_ADVANCE (p);
	  int count = 1;
	  int len;

	  if (p < pend)
	    {
	      count = STRING_CHAR_AND_LENGTH (p, len);
	      if (count < 128)
		count = 1;
	      else
		{
		  count -= 128;
		  p += len;
		}
	    }
	  while (count-- > 0)
	    set_sub_char_table_contents (sub, idx++, make_number (v));
	}
    }
/* It seems that we don't need this function because C code won't need
   to get a property that is compressed in this form.  */
#if 0
  else if (*p == 0)
    {
      /* WORD-LIST TABLE */
    }
#endif
  EXIT_LISP_FRAME (sub);
}


/* Decode VALUE as an element of char-table TABLE.  */

static Lisp_Object
uniprop_decode_value_run_length (Lisp_Object table, Lisp_Object value)
{
  ENTER_LISP_FRAME (table, value);
  LISP_LOCALS (valvec);
  if (VECTORP (XCHAR_TABLE (table)->extras[4]))
    {
      valvec = XCHAR_TABLE (table)->extras[4];


      if (XINT (value) >= 0 && XINT (value) < ASIZE (valvec))
	value = AREF (valvec, XINT (value));
    }
  EXIT_LISP_FRAME (value);
}

static uniprop_decoder_t uniprop_decoder [] =
  { uniprop_decode_value_run_length };

static const int uniprop_decoder_count = ARRAYELTS (uniprop_decoder);

/* Return the decoder of char-table TABLE or nil if none.  */

static uniprop_decoder_t
uniprop_get_decoder (Lisp_Object table)
{
  ENTER_LISP_FRAME_T (uniprop_decoder_t, table);
  EMACS_INT i;

  if (! INTEGERP (XCHAR_TABLE (table)->extras[1]))
    EXIT_LISP_FRAME (NULL);
  i = XINT (XCHAR_TABLE (table)->extras[1]);
  if (i < 0 || i >= uniprop_decoder_count)
    EXIT_LISP_FRAME (NULL);
  EXIT_LISP_FRAME (uniprop_decoder[i]);
}


/* Encode VALUE as an element of char-table TABLE which contains
   characters as elements.  */

static Lisp_Object
uniprop_encode_value_character (Lisp_Object table, Lisp_Object value)
{
  ENTER_LISP_FRAME (table, value);
  if (! NILP (value) && ! CHARACTERP (value))
    wrong_type_argument (Qintegerp, value);
  EXIT_LISP_FRAME (value);
}


/* Encode VALUE as an element of char-table TABLE which adopts RUN-LENGTH
   compression.  */

static Lisp_Object
uniprop_encode_value_run_length (Lisp_Object table, Lisp_Object value)
{
  ENTER_LISP_FRAME (table, value);
  struct Lisp_Vector *value_table = XVECTOR (XCHAR_TABLE (table)->extras[4]);
  int i, size = ASIZE (XCHAR_TABLE (table)->extras[4]);

  for (i = 0; i < size; i++)
    if (EQ (value, xv_ref (value_table, i)))
      break;
  if (i == size)
    wrong_type_argument (build_string ("Unicode property value"), value);
  EXIT_LISP_FRAME (make_number (i));
}


/* Encode VALUE as an element of char-table TABLE which adopts RUN-LENGTH
   compression and contains numbers as elements.  */

static Lisp_Object
uniprop_encode_value_numeric (Lisp_Object table, Lisp_Object value)
{
  ENTER_LISP_FRAME (table, value);
  struct Lisp_Vector *value_table = XVECTOR (XCHAR_TABLE (table)->extras[4]);
  int i, size = ASIZE (XCHAR_TABLE (table)->extras[4]);

  CHECK_NUMBER (value);
  for (i = 0; i < size; i++)
    if (EQ (value, xv_ref(value_table, i)))
      break;
  value = make_number (i);
  if (i == size)
    set_char_table_extras (table, 4,
			   CALLN (Fvconcat,
				  XCHAR_TABLE (table)->extras[4],
				  Fmake_vector (make_number (1), value)));
  EXIT_LISP_FRAME (make_number (i));
}

static uniprop_encoder_t uniprop_encoder[] =
  { uniprop_encode_value_character,
    uniprop_encode_value_run_length,
    uniprop_encode_value_numeric };

static const int uniprop_encoder_count = ARRAYELTS (uniprop_encoder);

/* Return the encoder of char-table TABLE or nil if none.  */

static uniprop_decoder_t
uniprop_get_encoder (Lisp_Object table)
{
  ENTER_LISP_FRAME_T (uniprop_decoder_t, table);
  EMACS_INT i;

  if (! INTEGERP (XCHAR_TABLE (table)->extras[2]))
    EXIT_LISP_FRAME (NULL);
  i = XINT (XCHAR_TABLE (table)->extras[2]);
  if (i < 0 || i >= uniprop_encoder_count)
    EXIT_LISP_FRAME (NULL);
  EXIT_LISP_FRAME (uniprop_encoder[i]);
}

/* Return a char-table for Unicode character property PROP.  This
   function may load a Lisp file and thus may cause
   garbage-collection.  */

Lisp_Object
uniprop_table (Lisp_Object prop)
{
  ENTER_LISP_FRAME (prop);
  LISP_LOCALS (val, table, result);

  val = Fassq (prop, Vchar_code_property_alist);
  if (! CONSP (val))
    EXIT_LISP_FRAME (Qnil);
  table = XCDR (val);
  if (STRINGP (table))
    {
      AUTO_STRING (intl, "international/");
      result = Fload (concat2 (intl, table), Qt, Qt, Qt, Qt);
      if (NILP (result))
	EXIT_LISP_FRAME (Qnil);
      table = XCDR (val);
    }
  if (! CHAR_TABLE_P (table)
      || ! UNIPROP_TABLE_P (table))
    EXIT_LISP_FRAME (Qnil);
  val = XCHAR_TABLE (table)->extras[1];
  if (INTEGERP (val)
      ? (XINT (val) < 0 || XINT (val) >= uniprop_decoder_count)
      : ! NILP (val))
    EXIT_LISP_FRAME (Qnil);
  /* Prepare ASCII values in advance for CHAR_TABLE_REF.  */
  set_char_table_ascii (table, char_table_ascii (table));
  EXIT_LISP_FRAME (table);
}

DEFUN ("unicode-property-table-internal", Funicode_property_table_internal,
       Sunicode_property_table_internal, 1, 1, 0,
       doc: /* Return a char-table for Unicode character property PROP.
Use `get-unicode-property-internal' and
`put-unicode-property-internal' instead of `aref' and `aset' to get
and put an element value.  */)
  (Lisp_Object prop)
{
  ENTER_LISP_FRAME (prop);
  LISP_LOCALS (table);
  table = uniprop_table (prop);


  if (CHAR_TABLE_P (table))
    EXIT_LISP_FRAME (table);
  EXIT_LISP_FRAME (Fcdr (Fassq (prop, Vchar_code_property_alist)));
}

DEFUN ("get-unicode-property-internal", Fget_unicode_property_internal,
       Sget_unicode_property_internal, 2, 2, 0,
       doc: /* Return an element of CHAR-TABLE for character CH.
CHAR-TABLE must be what returned by `unicode-property-table-internal'. */)
  (Lisp_Object char_table, Lisp_Object ch)
{
  ENTER_LISP_FRAME (char_table, ch);
  LISP_LOCALS (val);
  uniprop_decoder_t decoder;

  CHECK_CHAR_TABLE (char_table);
  CHECK_CHARACTER (ch);
  if (! UNIPROP_TABLE_P (char_table))
    error ("Invalid Unicode property table");
  val = CHAR_TABLE_REF (char_table, XINT (ch));
  decoder = uniprop_get_decoder (char_table);
  EXIT_LISP_FRAME ((decoder ? decoder (char_table, val) : val));
}

DEFUN ("put-unicode-property-internal", Fput_unicode_property_internal,
       Sput_unicode_property_internal, 3, 3, 0,
       doc: /* Set an element of CHAR-TABLE for character CH to VALUE.
CHAR-TABLE must be what returned by `unicode-property-table-internal'. */)
  (Lisp_Object char_table, Lisp_Object ch, Lisp_Object value)
{
  ENTER_LISP_FRAME (char_table, ch, value);
  uniprop_encoder_t encoder;

  CHECK_CHAR_TABLE (char_table);
  CHECK_CHARACTER (ch);
  if (! UNIPROP_TABLE_P (char_table))
    error ("Invalid Unicode property table");
  encoder = uniprop_get_encoder (char_table);
  if (encoder)
    value = encoder (char_table, value);
  CHAR_TABLE_SET (char_table, XINT (ch), value);
  EXIT_LISP_FRAME (Qnil);
}


void
syms_of_chartab (void)
{
  /* Purpose of uniprop tables. */
  DEFSYM (Qchar_code_property_table, "char-code-property-table");

  defsubr (&Smake_char_table);
  defsubr (&Schar_table_parent);
  defsubr (&Schar_table_subtype);
  defsubr (&Sset_char_table_parent);
  defsubr (&Schar_table_extra_slot);
  defsubr (&Sset_char_table_extra_slot);
  defsubr (&Schar_table_range);
  defsubr (&Sset_char_table_range);
  defsubr (&Soptimize_char_table);
  defsubr (&Smap_char_table);
  defsubr (&Sunicode_property_table_internal);
  defsubr (&Sget_unicode_property_internal);
  defsubr (&Sput_unicode_property_internal);

  /* Each element has the form (PROP . TABLE).
     PROP is a symbol representing a character property.
     TABLE is a char-table containing the property value for each character.
     TABLE may be a name of file to load to build a char-table.
     This variable should be modified only through
     `define-char-code-property'. */

  DEFVAR_LISP ("char-code-property-alist", Vchar_code_property_alist,
	       doc: /* Alist of character property name vs char-table containing property values.
Internal use only.  */);
  Vchar_code_property_alist = Qnil;
}
