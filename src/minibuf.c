/* Minibuffer input and completion.

Copyright (C) 1985-1986, 1993-2019 Free Software Foundation, Inc.

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
#include <errno.h>

#include <binary-io.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "keymap.h"
#include "sysstdio.h"
#include "systty.h"
#include "pdumper.h"

/* List of buffers for use as minibuffers.
   The first element of the list is used for the outermost minibuffer
   invocation, the next element is used for a recursive minibuffer
   invocation, etc.  The list is extended at the end as deeper
   minibuffer recursions are encountered.  */

Lisp_Object Vminibuffer_list;

/* Data to remember during recursive minibuffer invocations.  */

static Lisp_Object minibuf_save_list;

/* Depth in minibuffer invocations.  */

EMACS_INT minibuf_level;

/* Fread_minibuffer leaves the input here as a string.  */

Lisp_Object last_minibuf_string;

/* Prompt to display in front of the mini-buffer contents.  */

static Lisp_Object minibuf_prompt;

/* Width of current mini-buffer prompt.  Only set after display_line
   of the line that contains the prompt.  */

static ptrdiff_t minibuf_prompt_width;


/* Put minibuf on currently selected frame's minibuffer.
   We do this whenever the user starts a new minibuffer
   or when a minibuffer exits.  */

static void
choose_minibuf_frame (void)
{
  if (FRAMEP (selected_frame)
      && FRAME_LIVE_P (XFRAME (selected_frame))
      && !EQ (minibuf_window, XFRAME (selected_frame)->minibuffer_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Lisp_Object buffer;

      /* I don't think that any frames may validly have a null minibuffer
	 window anymore.  */
      if (NILP (sf->minibuffer_window))
	emacs_abort ();

      /* Under X, we come here with minibuf_window being the
	 minibuffer window of the unused termcap window created in
	 init_window_once.  That window doesn't have a buffer.  */
      buffer = XWINDOW (minibuf_window)->contents;
      if (BUFFERP (buffer))
	/* Use set_window_buffer instead of Fset_window_buffer (see
	   discussion of bug#11984, bug#12025, bug#12026).  */
	set_window_buffer (sf->minibuffer_window, buffer, 0, 0);
      minibuf_window = sf->minibuffer_window;
    }

  /* Make sure no other frame has a minibuffer as its selected window,
     because the text would not be displayed in it, and that would be
     confusing.  Only allow the selected frame to do this,
     and that only if the minibuffer is active.  */
  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      if (MINI_WINDOW_P (XWINDOW (FRAME_SELECTED_WINDOW (XFRAME (frame))))
	  && !(EQ (frame, selected_frame)
	       && minibuf_level > 0))
	Fset_frame_selected_window (frame, Fframe_first_window (frame), Qnil);
  }
}

DEFUN ("active-minibuffer-window", Factive_minibuffer_window,
       Sactive_minibuffer_window, 0, 0, 0,
       doc: /* Return the currently active minibuffer window, or nil if none.  */)
     (void)
{
  return minibuf_level ? minibuf_window : Qnil;
}

DEFUN ("set-minibuffer-window", Fset_minibuffer_window,
       Sset_minibuffer_window, 1, 1, 0,
       doc: /* Specify which minibuffer window to use for the minibuffer.
This affects where the minibuffer is displayed if you put text in it
without invoking the usual minibuffer commands.  */)
  (Lisp_Object window)
{
  CHECK_WINDOW (window);
  if (! MINI_WINDOW_P (XWINDOW (window)))
    error ("Window is not a minibuffer window");

  minibuf_window = window;

  return window;
}


/* Actual minibuffer invocation.  */

static void read_minibuf_unwind (void);
static void run_exit_minibuf_hook (void);


/* Read a Lisp object from VAL and return it.  If VAL is an empty
   string, and DEFALT is a string, read from DEFALT instead of VAL.  */

static Lisp_Object
string_to_object (Lisp_Object val, Lisp_Object defalt)
{
  Lisp_Object expr_and_pos;
  ptrdiff_t pos;

  if (STRINGP (val) && SCHARS (val) == 0)
    {
      if (STRINGP (defalt))
	val = defalt;
      else if (CONSP (defalt) && STRINGP (XCAR (defalt)))
	val = XCAR (defalt);
    }

  expr_and_pos = Fread_from_string (val, Qnil, Qnil);
  pos = XFIXNUM (Fcdr (expr_and_pos));
  if (pos != SCHARS (val))
    {
      /* Ignore trailing whitespace; any other trailing junk
	 is an error.  */
      ptrdiff_t i;
      pos = string_char_to_byte (val, pos);
      for (i = pos; i < SBYTES (val); i++)
	{
	  int c = SREF (val, i);
	  if (c != ' ' && c != '\t' && c != '\n')
	    error ("Trailing garbage following expression");
	}
    }

  val = Fcar (expr_and_pos);
  return val;
}


/* Like read_minibuf but reading from stdin.  This function is called
   from read_minibuf to do the job if noninteractive.  */

static Lisp_Object
read_minibuf_noninteractive (Lisp_Object prompt, bool expflag,
			     Lisp_Object defalt)
{
  ptrdiff_t size, len;
  char *line;
  Lisp_Object val;
  int c;
  unsigned char hide_char = 0;
  struct emacs_tty etty;
  bool etty_valid UNINIT;

  /* Check, whether we need to suppress echoing.  */
  if (CHARACTERP (Vread_hide_char))
    hide_char = XFIXNAT (Vread_hide_char);

  /* Manipulate tty.  */
  if (hide_char)
    {
      etty_valid = emacs_get_tty (STDIN_FILENO, &etty) == 0;
      if (etty_valid)
	set_binary_mode (STDIN_FILENO, O_BINARY);
      suppress_echo_on_tty (STDIN_FILENO);
    }

  fwrite_unlocked (SDATA (prompt), 1, SBYTES (prompt), stdout);
  fflush_unlocked (stdout);

  val = Qnil;
  size = 100;
  len = 0;
  line = xmalloc (size);

  while ((c = getchar_unlocked ()) != '\n' && c != '\r')
    {
      if (c == EOF)
	{
	  if (errno != EINTR)
	    break;
	}
      else
	{
	  if (hide_char)
	    fprintf (stdout, "%c", hide_char);
	  if (len == size)
	    line = xpalloc (line, &size, 1, -1, sizeof *line);
	  line[len++] = c;
	}
    }

  /* Reset tty.  */
  if (hide_char)
    {
      fprintf (stdout, "\n");
      if (etty_valid)
	{
	  emacs_set_tty (STDIN_FILENO, &etty, 0);
	  set_binary_mode (STDIN_FILENO, O_TEXT);
	}
    }

  if (len || c == '\n' || c == '\r')
    {
      val = make_string (line, len);
      xfree (line);
    }
  else
    {
      xfree (line);
      error ("Error reading from stdin");
    }

  /* If Lisp form desired instead of string, parse it.  */
  if (expflag)
    val = string_to_object (val, CONSP (defalt) ? XCAR (defalt) : defalt);

  return val;
}

DEFUN ("minibufferp", Fminibufferp,
       Sminibufferp, 0, 1, 0,
       doc: /* Return t if BUFFER is a minibuffer.
No argument or nil as argument means use current buffer as BUFFER.
BUFFER can be a buffer or a buffer name.  */)
  (Lisp_Object buffer)
{
  Lisp_Object tem;

  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  else if (STRINGP (buffer))
    buffer = Fget_buffer (buffer);
  else
    CHECK_BUFFER (buffer);

  tem = Fmemq (buffer, Vminibuffer_list);
  return ! NILP (tem) ? Qt : Qnil;
}

DEFUN ("minibuffer-prompt-end", Fminibuffer_prompt_end,
       Sminibuffer_prompt_end, 0, 0, 0,
       doc: /* Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a minibuffer.  */)
  (void)
{
  /* This function is written to be most efficient when there's a prompt.  */
  Lisp_Object beg, end, tem;
  beg = make_fixnum (BEGV);

  tem = Fmemq (Fcurrent_buffer (), Vminibuffer_list);
  if (NILP (tem))
    return beg;

  end = Ffield_end (beg, Qnil, Qnil);

  if (XFIXNUM (end) == ZV && NILP (Fget_char_property (beg, Qfield, Qnil)))
    return beg;
  else
    return end;
}

DEFUN ("minibuffer-contents", Fminibuffer_contents,
       Sminibuffer_contents, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string.
If the current buffer is not a minibuffer, return its entire contents.  */)
  (void)
{
  ptrdiff_t prompt_end = XFIXNUM (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 1);
}

DEFUN ("minibuffer-contents-no-properties", Fminibuffer_contents_no_properties,
       Sminibuffer_contents_no_properties, 0, 0, 0,
       doc: /* Return the user input in a minibuffer as a string, without text-properties.
If the current buffer is not a minibuffer, return its entire contents.  */)
  (void)
{
  ptrdiff_t prompt_end = XFIXNUM (Fminibuffer_prompt_end ());
  return make_buffer_string (prompt_end, ZV, 0);
}


/* Read from the minibuffer using keymap MAP and initial contents INITIAL,
   putting point minus BACKUP_N bytes from the end of INITIAL,
   prompting with PROMPT (a string), using history list HISTVAR
   with initial position HISTPOS.  INITIAL should be a string or a
   cons of a string and an integer.  BACKUP_N should be <= 0, or
   Qnil, which is equivalent to 0.  If INITIAL is a cons, BACKUP_N is
   ignored and replaced with an integer that puts point at one-indexed
   position N in INITIAL, where N is the CDR of INITIAL, or at the
   beginning of INITIAL if N <= 0.

   Normally return the result as a string (the text that was read),
   but if EXPFLAG, read it and return the object read.
   If HISTVAR is given, save the value read on that history only if it doesn't
   match the front of that history list exactly.  The value is pushed onto
   the list as the string that was read.

   DEFALT specifies the default value for the sake of history commands.

   If ALLOW_PROPS, do not throw away text properties.

   if INHERIT_INPUT_METHOD, the minibuffer inherits the
   current input method.  */

static Lisp_Object
read_minibuf (Lisp_Object map, Lisp_Object initial, Lisp_Object prompt,
	      bool expflag,
	      Lisp_Object histvar, Lisp_Object histpos, Lisp_Object defalt,
	      bool allow_props, bool inherit_input_method)
{
  Lisp_Object val;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object mini_frame, ambient_dir, minibuffer, input_method;
  Lisp_Object enable_multibyte;
  EMACS_INT pos = 0;
  /* String to add to the history.  */
  Lisp_Object histstring;
  Lisp_Object histval;

  Lisp_Object empty_minibuf;
  Lisp_Object dummy, frame;

  specbind (Qminibuffer_default, defalt);
  specbind (Qinhibit_read_only, Qnil);

  /* If Vminibuffer_completing_file_name is `lambda' on entry, it was t
     in previous recursive minibuffer, but was not set explicitly
     to t for this invocation, so set it to nil in this minibuffer.
     Save the old value now, before we change it.  */
  specbind (intern ("minibuffer-completing-file-name"),
	    Vminibuffer_completing_file_name);
  if (EQ (Vminibuffer_completing_file_name, Qlambda))
    Vminibuffer_completing_file_name = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  if (!NILP (initial))
    {
      if (CONSP (initial))
	{
	  Lisp_Object backup_n = XCDR (initial);
	  initial = XCAR (initial);
	  CHECK_STRING (initial);
	  if (!NILP (backup_n))
	    {
	      CHECK_FIXNUM (backup_n);
	      /* Convert to distance from end of input.  */
	      if (XFIXNUM (backup_n) < 1)
		/* A number too small means the beginning of the string.  */
		pos =  - SCHARS (initial);
	      else
		pos = XFIXNUM (backup_n) - 1 - SCHARS (initial);
	    }
	}
      else
	CHECK_STRING (initial);
    }
  val = Qnil;
  ambient_dir = BVAR (current_buffer, directory);
  input_method = Qnil;
  enable_multibyte = Qnil;

  if (!STRINGP (prompt))
    prompt = empty_unibyte_string;

  if (!enable_recursive_minibuffers
      && minibuf_level > 0)
    {
      if (EQ (selected_window, minibuf_window))
	error ("Command attempted to use minibuffer while in minibuffer");
      else
	/* If we're in another window, cancel the minibuffer that's active.  */
	Fthrow (Qexit,
		build_string ("Command attempted to use minibuffer while in minibuffer"));
    }

  if ((noninteractive
       /* In case we are running as a daemon, only do this before
	  detaching from the terminal.  */
       || (IS_DAEMON && DAEMON_RUNNING))
      && NILP (Vexecuting_kbd_macro))
    {
      val = read_minibuf_noninteractive (prompt, expflag, defalt);
      return unbind_to (count, val);
    }

  /* Choose the minibuffer window and frame, and take action on them.  */

  /* Prepare for restoring the current buffer since choose_minibuf_frame
     calling Fset_frame_selected_window may change it (Bug#12766).  */
  record_unwind_protect (restore_buffer, Fcurrent_buffer ());

  choose_minibuf_frame ();

  record_unwind_protect_void (choose_minibuf_frame);

  record_unwind_protect (restore_window_configuration,
			 Fcurrent_window_configuration (Qnil));

  /* If the minibuffer window is on a different frame, save that
     frame's configuration too.  */
  mini_frame = WINDOW_FRAME (XWINDOW (minibuf_window));
  if (!EQ (mini_frame, selected_frame))
    record_unwind_protect (restore_window_configuration,
			   Fcurrent_window_configuration (mini_frame));

  /* If the minibuffer is on an iconified or invisible frame,
     make it visible now.  */
  Fmake_frame_visible (mini_frame);

  if (minibuffer_auto_raise)
    Fraise_frame (mini_frame);

  temporarily_switch_to_single_kboard (XFRAME (mini_frame));

  /* We have to do this after saving the window configuration
     since that is what restores the current buffer.  */

  /* Arrange to restore a number of minibuffer-related variables.
     We could bind each variable separately, but that would use lots of
     specpdl slots.  */
  minibuf_save_list
    = Fcons (Voverriding_local_map,
	     Fcons (minibuf_window,
		    minibuf_save_list));
  minibuf_save_list
    = Fcons (minibuf_prompt,
	     Fcons (make_fixnum (minibuf_prompt_width),
		    Fcons (Vhelp_form,
			   Fcons (Vcurrent_prefix_arg,
				  Fcons (Vminibuffer_history_position,
					 Fcons (Vminibuffer_history_variable,
						minibuf_save_list))))));
  minibuf_save_list
    = Fcons (Fthis_command_keys_vector (), minibuf_save_list);

  record_unwind_protect_void (read_minibuf_unwind);
  minibuf_level++;
  /* We are exiting the minibuffer one way or the other, so run the hook.
     It should be run before unwinding the minibuf settings.  Do it
     separately from read_minibuf_unwind because we need to make sure that
     read_minibuf_unwind is fully executed even if exit-minibuffer-hook
     signals an error.  --Stef  */
  record_unwind_protect_void (run_exit_minibuf_hook);

  /* Now that we can restore all those variables, start changing them.  */

  minibuf_prompt_width = 0;
  minibuf_prompt = Fcopy_sequence (prompt);
  Vminibuffer_history_position = histpos;
  Vminibuffer_history_variable = histvar;
  Vhelp_form = Vminibuffer_help_form;
  /* If this minibuffer is reading a file name, that doesn't mean
     recursive ones are.  But we cannot set it to nil, because
     completion code still need to know the minibuffer is completing a
     file name.  So use `lambda' as intermediate value meaning
     "t" in this minibuffer, but "nil" in next minibuffer.  */
  if (!NILP (Vminibuffer_completing_file_name))
    Vminibuffer_completing_file_name = Qlambda;

  /* If variable is unbound, make it nil.  */
  histval = find_symbol_value (Vminibuffer_history_variable);
  if (EQ (histval, Qunbound))
    {
      Fset (Vminibuffer_history_variable, Qnil);
      histval = Qnil;
    }

  if (inherit_input_method)
    {
      /* `current-input-method' is buffer local.  So, remember it in
	 INPUT_METHOD before changing the current buffer.  */
      input_method = Fsymbol_value (Qcurrent_input_method);
      enable_multibyte = BVAR (current_buffer, enable_multibyte_characters);
    }

  /* Switch to the minibuffer.  */

  minibuffer = get_minibuffer (minibuf_level);
  Fset_buffer (minibuffer);

  /* Defeat (setq-default truncate-lines t), since truncated lines do
     not work correctly in minibuffers.  (Bug#5715, etc)  */
  bset_truncate_lines (current_buffer, Qnil);

  /* If appropriate, copy enable-multibyte-characters into the minibuffer.  */
  if (inherit_input_method)
    bset_enable_multibyte_characters (current_buffer, enable_multibyte);

  /* The current buffer's default directory is usually the right thing
     for our minibuffer here.  However, if you're typing a command at
     a minibuffer-only frame when minibuf_level is zero, then buf IS
     the current_buffer, so reset_buffer leaves buf's default
     directory unchanged.  This is a bummer when you've just started
     up Emacs and buf's default directory is Qnil.  Here's a hack; can
     you think of something better to do?  Find another buffer with a
     better directory, and use that one instead.  */
  if (STRINGP (ambient_dir))
    bset_directory (current_buffer, ambient_dir);
  else
    {
      Lisp_Object tail, buf;

      FOR_EACH_LIVE_BUFFER (tail, buf)
	if (STRINGP (BVAR (XBUFFER (buf), directory)))
	  {
	    bset_directory (current_buffer,
			    BVAR (XBUFFER (buf), directory));
	    break;
	  }
    }

  if (!EQ (mini_frame, selected_frame))
    Fredirect_frame_focus (selected_frame, mini_frame);

  Vminibuf_scroll_window = selected_window;
  if (minibuf_level == 1 || !EQ (minibuf_window, selected_window))
    minibuf_selected_window = selected_window;

  /* Empty out the minibuffers of all frames other than the one
     where we are going to display one now.
     Set them to point to ` *Minibuf-0*', which is always empty.  */
  empty_minibuf = get_minibuffer (0);

  FOR_EACH_FRAME (dummy, frame)
    {
      Lisp_Object root_window = Fframe_root_window (frame);
      Lisp_Object mini_window = XWINDOW (root_window)->next;

      if (! NILP (mini_window) && ! EQ (mini_window, minibuf_window)
	  && !NILP (Fwindow_minibuffer_p (mini_window)))
	/* Use set_window_buffer instead of Fset_window_buffer (see
	   discussion of bug#11984, bug#12025, bug#12026).  */
	set_window_buffer (mini_window, empty_minibuf, 0, 0);
    }

  /* Display this minibuffer in the proper window.  */
  /* Use set_window_buffer instead of Fset_window_buffer (see
     discussion of bug#11984, bug#12025, bug#12026).  */
  set_window_buffer (minibuf_window, Fcurrent_buffer (), 0, 0);
  Fselect_window (minibuf_window, Qnil);
  XWINDOW (minibuf_window)->hscroll = 0;
  XWINDOW (minibuf_window)->suspend_auto_hscroll = 0;

  /* Erase the buffer.  */
  {
    ptrdiff_t count1 = SPECPDL_INDEX ();
    specbind (Qinhibit_read_only, Qt);
    specbind (Qinhibit_modification_hooks, Qt);
    Ferase_buffer ();

    if (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	&& ! STRING_MULTIBYTE (minibuf_prompt))
      minibuf_prompt = Fstring_make_multibyte (minibuf_prompt);

    /* Insert the prompt, record where it ends.  */
    Finsert (1, &minibuf_prompt);
    if (PT > BEG)
      {
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qfront_sticky, Qt, Qnil);
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qrear_nonsticky, Qt, Qnil);
	Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
			    Qfield, Qt, Qnil);
	if (CONSP (Vminibuffer_prompt_properties))
	  {
	    /* We want to apply all properties from
	       `minibuffer-prompt-properties' to the region normally,
	       but if the `face' property is present, add that
	       property to the end of the face properties to avoid
	       overwriting faces. */
	    Lisp_Object list = Vminibuffer_prompt_properties;
	    while (CONSP (list))
	      {
		Lisp_Object key = XCAR (list);
		list = XCDR (list);
		if (CONSP (list))
		  {
		    Lisp_Object val = XCAR (list);
		    list = XCDR (list);
		    if (EQ (key, Qface))
		      Fadd_face_text_property (make_fixnum (BEG),
					       make_fixnum (PT), val, Qt, Qnil);
		    else
		      Fput_text_property (make_fixnum (BEG), make_fixnum (PT),
					  key, val, Qnil);
		  }
	      }
	  }
      }
    unbind_to (count1, Qnil);
  }

  minibuf_prompt_width = current_column ();

  /* Put in the initial input.  */
  if (!NILP (initial))
    {
      Finsert (1, &initial);
      Fforward_char (make_fixnum (pos));
    }

  clear_message (1, 1);
  bset_keymap (current_buffer, map);

  /* Turn on an input method stored in INPUT_METHOD if any.  */
  if (STRINGP (input_method) && !NILP (Ffboundp (Qactivate_input_method)))
    call1 (Qactivate_input_method, input_method);

  run_hook (Qminibuffer_setup_hook);

  /* Don't allow the user to undo past this point.  */
  bset_undo_list (current_buffer, Qnil);

  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (XWINDOW (minibuf_window)->cursor.vpos >= 0
      && !noninteractive)
    {
      XWINDOW (minibuf_window)->cursor.hpos = 0;
      XWINDOW (minibuf_window)->cursor.x = 0;
      XWINDOW (minibuf_window)->must_be_updated_p = true;
      update_frame (XFRAME (selected_frame), true, true);
      flush_frame (XFRAME (XWINDOW (minibuf_window)->frame));
    }

  /* Make minibuffer contents into a string.  */
  Fset_buffer (minibuffer);
  if (allow_props)
    val = Fminibuffer_contents ();
  else
    val = Fminibuffer_contents_no_properties ();

  /* VAL is the string of minibuffer text.  */

  last_minibuf_string = val;

  /* Choose the string to add to the history.  */
  if (SCHARS (val) != 0)
    histstring = val;
  else if (STRINGP (defalt))
    histstring = defalt;
  else if (CONSP (defalt) && STRINGP (XCAR (defalt)))
    histstring = XCAR (defalt);
  else
    histstring = Qnil;

  /* Add the value to the appropriate history list, if any.  */
  if (! (NILP (Vhistory_add_new_input) || NILP (histstring)))
    call2 (intern ("add-to-history"), Vminibuffer_history_variable, histstring);

  /* If Lisp form desired instead of string, parse it.  */
  if (expflag)
    val = string_to_object (val, defalt);

  /* The appropriate frame will get selected
     in set-window-configuration.  */
  return unbind_to (count, val);
}

/* Return a buffer to be used as the minibuffer at depth `depth'.
   depth = 0 is the lowest allowed argument, and that is the value
   used for nonrecursive minibuffer invocations.  */

Lisp_Object
get_minibuffer (EMACS_INT depth)
{
  Lisp_Object tail = Fnthcdr (make_fixnum (depth), Vminibuffer_list);
  if (NILP (tail))
    {
      tail = list1 (Qnil);
      Vminibuffer_list = nconc2 (Vminibuffer_list, tail);
    }
  Lisp_Object buf = Fcar (tail);
  if (NILP (buf) || !BUFFER_LIVE_P (XBUFFER (buf)))
    {
      static char const name_fmt[] = " *Minibuf-%"pI"d*";
      char name[sizeof name_fmt + INT_STRLEN_BOUND (EMACS_INT)];
      AUTO_STRING_WITH_LEN (lname, name, sprintf (name, name_fmt, depth));
      buf = Fget_buffer_create (lname);

      /* Although the buffer's name starts with a space, undo should be
	 enabled in it.  */
      Fbuffer_enable_undo (buf);

      XSETCAR (tail, buf);
    }
  else
    {
      ptrdiff_t count = SPECPDL_INDEX ();
      /* We have to empty both overlay lists.  Otherwise we end
	 up with overlays that think they belong to this buffer
	 while the buffer doesn't know about them any more.  */
      delete_all_overlays (XBUFFER (buf));
      reset_buffer (XBUFFER (buf));
      record_unwind_current_buffer ();
      Fset_buffer (buf);
      if (!NILP (Ffboundp (intern ("minibuffer-inactive-mode"))))
	call0 (intern ("minibuffer-inactive-mode"));
      else
        Fkill_all_local_variables ();
      buf = unbind_to (count, buf);
    }

  return buf;
}

static void
run_exit_minibuf_hook (void)
{
  safe_run_hooks (Qminibuffer_exit_hook);
}

/* This function is called on exiting minibuffer, whether normally or
   not, and it restores the current window, buffer, etc.  */

static void
read_minibuf_unwind (void)
{
  Lisp_Object old_deactivate_mark;
  Lisp_Object window;

  /* If this was a recursive minibuffer,
     tie the minibuffer window back to the outer level minibuffer buffer.  */
  minibuf_level--;

  window = minibuf_window;
  /* To keep things predictable, in case it matters, let's be in the
     minibuffer when we reset the relevant variables.  */
  Fset_buffer (XWINDOW (window)->contents);

  /* Restore prompt, etc, from outer minibuffer level.  */
  Lisp_Object key_vec = Fcar (minibuf_save_list);
  this_command_key_count = ASIZE (key_vec);
  this_command_keys = key_vec;
  minibuf_save_list = Fcdr (minibuf_save_list);
  minibuf_prompt = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  minibuf_prompt_width = XFIXNAT (Fcar (minibuf_save_list));
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vhelp_form = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vcurrent_prefix_arg = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vminibuffer_history_position = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Vminibuffer_history_variable = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
  Voverriding_local_map = Fcar (minibuf_save_list);
  minibuf_save_list = Fcdr (minibuf_save_list);
#if 0
  temp = Fcar (minibuf_save_list);
  if (FRAME_LIVE_P (XFRAME (WINDOW_FRAME (XWINDOW (temp)))))
    minibuf_window = temp;
#endif
  minibuf_save_list = Fcdr (minibuf_save_list);

  /* Erase the minibuffer we were using at this level.  */
  {
    ptrdiff_t count = SPECPDL_INDEX ();
    /* Prevent error in erase-buffer.  */
    specbind (Qinhibit_read_only, Qt);
    specbind (Qinhibit_modification_hooks, Qt);
    old_deactivate_mark = Vdeactivate_mark;
    Ferase_buffer ();
    Vdeactivate_mark = old_deactivate_mark;
    unbind_to (count, Qnil);
  }

  /* When we get to the outmost level, make sure we resize the
     mini-window back to its normal size.  */
  if (minibuf_level == 0)
    resize_mini_window (XWINDOW (window), 0);

  /* Deal with frames that should be removed when exiting the
     minibuffer.  */
  {
    Lisp_Object frames, frame1, val;
    struct frame *f1;

    FOR_EACH_FRAME (frames, frame1)
      {
	f1 = XFRAME (frame1);

	if ((FRAME_PARENT_FRAME (f1)
	     || !NILP (get_frame_param (f1, Qdelete_before)))
	    && !NILP (val = (get_frame_param (f1, Qminibuffer_exit))))
	  {
	    if (EQ (val, Qiconify_frame))
	      Ficonify_frame (frame1);
	    else if (EQ (val, Qdelete_frame))
	      Fdelete_frame (frame1, Qnil);
	    else
	      Fmake_frame_invisible (frame1, Qnil);
	  }
      }
  }

  /* In case the previous minibuffer displayed in this miniwindow is
     dead, we may keep displaying this buffer (tho it's inactive), so reset it,
     to make sure we don't leave around bindings and stuff which only
     made sense during the read_minibuf invocation.  */
  call0 (intern ("minibuffer-inactive-mode"));
}


DEFUN ("read-from-minibuffer", Fread_from_minibuffer,
       Sread_from_minibuffer, 1, 7, 0,
       doc: /* Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an obsolete alternative to
  DEFAULT-VALUE.  It normally should be nil in new code, except when
  HIST is a cons.  It is discussed in more detail below.

Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.

If fourth arg READ is non-nil, interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'

Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use, and
  HISTPOS is the initial position for use by the minibuffer history
  commands.  For consistency, you should also specify that element of
  the history as the value of INITIAL-CONTENTS.  Positions are counted
  starting from 1 at the beginning of the list.  If HIST is the symbol
  `t', history is not recorded.

Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
  as the default to `read' if READ is non-nil and the user enters
  empty input.  But if READ is nil, this function does _not_ return
  DEFAULT-VALUE for empty input!  Instead, it returns the empty string.

  Whatever the value of READ, DEFAULT-VALUE is made available via the
  minibuffer history commands.  DEFAULT-VALUE can also be a list of
  strings, in which case all the strings are available in the history,
  and the first string is the default to `read' if READ is non-nil.

Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.

If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  It is only relevant when
studying existing code, or when HIST is a cons.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
input is STRING, but point is placed at _one-indexed_ position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  *Note* that this
behavior differs from the way such arguments are used in `completing-read'
and some related functions, which use zero-indexing for POSITION.  */)
  (Lisp_Object prompt, Lisp_Object initial_contents, Lisp_Object keymap, Lisp_Object read, Lisp_Object hist, Lisp_Object default_value, Lisp_Object inherit_input_method)
{
  Lisp_Object histvar, histpos, val;

  CHECK_STRING (prompt);
  if (NILP (keymap))
    keymap = Vminibuffer_local_map;
  else
    keymap = get_keymap (keymap, 1, 0);

  if (SYMBOLP (hist))
    {
      histvar = hist;
      histpos = Qnil;
    }
  else
    {
      histvar = Fcar_safe (hist);
      histpos = Fcdr_safe (hist);
    }
  if (NILP (histvar))
    histvar = Qminibuffer_history;
  if (NILP (histpos))
    XSETFASTINT (histpos, 0);

  val = read_minibuf (keymap, initial_contents, prompt,
		      !NILP (read),
		      histvar, histpos, default_value,
		      minibuffer_allow_text_properties,
		      !NILP (inherit_input_method));
  return val;
}

/* Functions that use the minibuffer to read various things.  */

DEFUN ("read-string", Fread_string, Sread_string, 1, 5, 0,
       doc: /* Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  This argument has been superseded by DEFAULT-VALUE and should normally be nil
  in new code.  It behaves as INITIAL-CONTENTS in `read-from-minibuffer' (which
  see).
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value or the list of default values.
 If non-nil, it is used for history commands, and as the value (or the first
 element of the list of default values) to return if the user enters the
 empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.  */)
  (Lisp_Object prompt, Lisp_Object initial_input, Lisp_Object history, Lisp_Object default_value, Lisp_Object inherit_input_method)
{
  Lisp_Object val;
  ptrdiff_t count = SPECPDL_INDEX ();

  /* Just in case we're in a recursive minibuffer, make it clear that the
     previous minibuffer's completion table does not apply to the new
     minibuffer.
     FIXME: `minibuffer-completion-table' should be buffer-local instead.  */
  specbind (Qminibuffer_completion_table, Qnil);

  val = Fread_from_minibuffer (prompt, initial_input, Qnil,
			       Qnil, history, default_value,
			       inherit_input_method);
  if (STRINGP (val) && SCHARS (val) == 0 && ! NILP (default_value))
    val = CONSP (default_value) ? XCAR (default_value) : default_value;
  return unbind_to (count, val);
}

DEFUN ("read-no-blanks-input", Fread_no_blanks_input, Sread_no_blanks_input, 1, 3, 0,
       doc: /* Read a string from the terminal, not allowing blanks.
Prompt with PROMPT.  Whitespace terminates the input.  If INITIAL is
non-nil, it should be a string, which is used as initial input, with
point positioned at the end, so that SPACE will accept the input.
\(Actually, INITIAL can also be a cons of a string and an integer.
Such values are treated as in `read-from-minibuffer', but are normally
not useful in this function.)
Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
the current input method and the setting of`enable-multibyte-characters'.  */)
  (Lisp_Object prompt, Lisp_Object initial, Lisp_Object inherit_input_method)
{
  CHECK_STRING (prompt);
  return read_minibuf (Vminibuffer_local_ns_map, initial, prompt,
		       0, Qminibuffer_history, make_fixnum (0), Qnil, 0,
		       !NILP (inherit_input_method));
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 2, 0,
       doc: /* Read the name of a command and return as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list.  If DEFAULT-VALUE is omitted or nil, and the user enters
null input, return a symbol whose name is an empty string.  */)
  (Lisp_Object prompt, Lisp_Object default_value)
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    default_string = SYMBOL_NAME (default_value);
  else
    default_string = default_value;

  name = Fcompleting_read (prompt, Vobarray, Qcommandp, Qt,
			   Qnil, Qnil, default_string, Qnil);
  if (NILP (name))
    return name;
  return Fintern (name, Qnil);
}

#ifdef NOTDEF
DEFUN ("read-function", Fread_function, Sread_function, 1, 1, 0,
       doc: /* One arg PROMPT, a string.  Read the name of a function and return as a symbol.
Prompt with PROMPT.  */)
  (Lisp_Object prompt)
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil, Qnil, Qnil, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 2, 0,
       doc: /* Read the name of a user option and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
if it is a list.
A user option, or customizable variable, is one for which
`custom-variable-p' returns non-nil.  */)
  (Lisp_Object prompt, Lisp_Object default_value)
{
  Lisp_Object name, default_string;

  if (NILP (default_value))
    default_string = Qnil;
  else if (SYMBOLP (default_value))
    default_string = SYMBOL_NAME (default_value);
  else
    default_string = default_value;

  name = Fcompleting_read (prompt, Vobarray,
			   Qcustom_variable_p, Qt,
			   Qnil, Qcustom_variable_history,
			   default_string, Qnil);
  if (NILP (name))
    return name;
  return Fintern (name, Qnil);
}

DEFUN ("read-buffer", Fread_buffer, Sread_buffer, 1, 4, 0,
       doc: /* Read the name of a buffer and return it as a string.
Prompt with PROMPT, which should be a string ending with a colon and a space.
Provides completion on buffer names the user types.
Optional second arg DEF is value to return if user enters an empty line,
 instead of that empty string.
 If DEF is a list of default values, return its first element.
Optional third arg REQUIRE-MATCH has the same meaning as the
 REQUIRE-MATCH argument of `completing-read'.
Optional arg PREDICATE, if non-nil, is a function limiting the buffers that
can be considered.  It will be called with each potential candidate, in
the form of either a string or a cons cell whose `car' is a string, and
should return non-nil to accept the candidate for completion, nil otherwise.
If `read-buffer-completion-ignore-case' is non-nil, completion ignores
case while reading the buffer name.
If `read-buffer-function' is non-nil, this works by calling it as a
function, instead of the usual behavior.  */)
  (Lisp_Object prompt, Lisp_Object def, Lisp_Object require_match,
   Lisp_Object predicate)
{
  Lisp_Object result;
  char *s;
  ptrdiff_t len;
  ptrdiff_t count = SPECPDL_INDEX ();

  if (BUFFERP (def))
    def = BVAR (XBUFFER (def), name);

  specbind (Qcompletion_ignore_case,
	    read_buffer_completion_ignore_case ? Qt : Qnil);

  if (NILP (Vread_buffer_function))
    {
      if (!NILP (def))
	{
	  /* A default value was provided: we must change PROMPT,
	     editing the default value in before the colon.  To achieve
	     this, we replace PROMPT with a substring that doesn't
	     contain the terminal space and colon (if present).  They
	     are then added back using Fformat.  */

	  if (STRINGP (prompt))
	    {
	      s = SSDATA (prompt);
	      len = SBYTES (prompt);
	      if (len >= 2 && s[len - 2] == ':' && s[len - 1] == ' ')
		len = len - 2;
	      else if (len >= 1 && (s[len - 1] == ':' || s[len - 1] == ' '))
		len--;

	      prompt = make_specified_string (s, -1, len,
					      STRING_MULTIBYTE (prompt));
	    }

	  AUTO_STRING (format, "%s (default %s): ");
	  prompt = CALLN (Fformat, format, prompt,
			  CONSP (def) ? XCAR (def) : def);
	}

      result = Fcompleting_read (prompt, intern ("internal-complete-buffer"),
				 predicate, require_match, Qnil,
				 Qbuffer_name_history, def, Qnil);
    }
  else
    result = (NILP (predicate)
	      /* Partial backward compatibility for older read_buffer_functions
		 which don't expect a `predicate' argument.  */
	      ? call3 (Vread_buffer_function, prompt, def, require_match)
	      : call4 (Vread_buffer_function, prompt, def, require_match,
		       predicate));
  return unbind_to (count, result);
}

static Lisp_Object
minibuf_conform_representation (Lisp_Object string, Lisp_Object basis)
{
  if (STRING_MULTIBYTE (string) == STRING_MULTIBYTE (basis))
    return string;

  if (STRING_MULTIBYTE (string))
    return Fstring_make_unibyte (string);
  else
    return Fstring_make_multibyte (string);
}

DEFUN ("try-completion", Ftry_completion, Stry_completion, 2, 3, 0,
       doc: /* Return common substring of all completions of STRING in COLLECTION.
Test each possible completion specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
see `symbol-name'.
All that match STRING are compared together; the longest initial sequence
common to all these matches is the return value.
If there is no match at all, the return value is nil.
For a unique match which is exact, the return value is t.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is the possible completion.
If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: the values STRING, PREDICATE and nil.
Whatever it returns becomes the value of `try-completion'.

If optional third argument PREDICATE is non-nil,
it is used to test each possible match.
The match is a candidate only if PREDICATE returns non-nil.
The argument given to PREDICATE is the alist element
or the symbol from the obarray.  If COLLECTION is a hash-table,
predicate is called with two arguments: the key and the value.
Additionally to this predicate, `completion-regexp-list'
is used to further constrain the set of candidates.  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate)
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  /* Size in bytes of BESTMATCH.  */
  ptrdiff_t bestmatchsize = 0;
  /* These are in bytes, too.  */
  ptrdiff_t compare, matchsize;
  enum { function_table, list_table, obarray_table, hash_table}
    type = (HASH_TABLE_P (collection) ? hash_table
	    : VECTORP (collection) ? obarray_table
	    : ((NILP (collection)
		|| (CONSP (collection) && !FUNCTIONP (collection)))
	       ? list_table : function_table));
  ptrdiff_t idx = 0, obsize = 0;
  int matchcount = 0;
  ptrdiff_t bindcount = -1;
  Lisp_Object bucket, zero, end, tem;

  CHECK_STRING (string);
  if (type == function_table)
    return call3 (collection, string, predicate, Qnil);

  bestmatch = bucket = Qnil;
  zero = make_fixnum (0);

  /* If COLLECTION is not a list, set TAIL just for gc pro.  */
  tail = collection;
  if (type == obarray_table)
    {
      collection = check_obarray (collection);
      obsize = ASIZE (collection);
      bucket = AREF (collection, idx);
    }

  if (HASH_TABLE_P (collection))
    hash_rehash_if_needed (XHASH_TABLE (collection));

  while (1)
    {
      /* Get the next element of the alist, obarray, or hash-table.  */
      /* Exit the loop if the elements are all used up.  */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion.  */

      if (type == list_table)
	{
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
	}
      else if (type == obarray_table)
	{
	  if (!EQ (bucket, zero))
	    {
	      if (!SYMBOLP (bucket))
		error ("Bad data in guts of obarray");
	      elt = bucket;
	      eltstring = elt;
	      if (XSYMBOL (bucket)->u.s.next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->u.s.next);
	      else
		XSETFASTINT (bucket, 0);
	    }
	  else if (++idx >= obsize)
	    break;
	  else
	    {
	      bucket = AREF (collection, idx);
	      continue;
	    }
	}
      else /* if (type == hash_table) */
	{
	  while (idx < HASH_TABLE_SIZE (XHASH_TABLE (collection))
		 && NILP (HASH_HASH (XHASH_TABLE (collection), idx)))
	    idx++;
	  if (idx >= HASH_TABLE_SIZE (XHASH_TABLE (collection)))
	    break;
	  else
	    elt = eltstring = HASH_KEY (XHASH_TABLE (collection), idx++);
	}

      /* Is this element a possible completion?  */

      if (SYMBOLP (eltstring))
	eltstring = Fsymbol_name (eltstring);

      if (STRINGP (eltstring)
	  && SCHARS (string) <= SCHARS (eltstring)
	  && (tem = Fcompare_strings (eltstring, zero,
				      make_fixnum (SCHARS (string)),
				      string, zero, Qnil,
				      completion_ignore_case ? Qt : Qnil),
	      EQ (Qt, tem)))
	{
	  /* Yes.  */
	  Lisp_Object regexps;

	  /* Ignore this element if it fails to match all the regexps.  */
	  {
	    for (regexps = Vcompletion_regexp_list; CONSP (regexps);
		 regexps = XCDR (regexps))
	      {
		if (bindcount < 0)
		  {
		    bindcount = SPECPDL_INDEX ();
		    specbind (Qcase_fold_search,
			      completion_ignore_case ? Qt : Qnil);
		  }
		tem = Fstring_match (XCAR (regexps), eltstring, zero);
		if (NILP (tem))
		  break;
	      }
	    if (CONSP (regexps))
	      continue;
	  }

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it.  */

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt, Qnil);
	      else
		{
		  if (bindcount >= 0)
		    {
		      unbind_to (bindcount, Qnil);
		      bindcount = -1;
		    }
		  tem = (type == hash_table
			 ? call2 (predicate, elt,
				  HASH_VALUE (XHASH_TABLE (collection),
					      idx - 1))
			 : call1 (predicate, elt));
		}
	      if (NILP (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  if (NILP (bestmatch))
	    {
	      matchcount = 1;
	      bestmatch = eltstring;
	      bestmatchsize = SCHARS (eltstring);
	    }
	  else
	    {
	      compare = min (bestmatchsize, SCHARS (eltstring));
	      tem = Fcompare_strings (bestmatch, zero,
				      make_fixnum (compare),
				      eltstring, zero,
				      make_fixnum (compare),
				      completion_ignore_case ? Qt : Qnil);
	      matchsize = EQ (tem, Qt) ? compare : eabs (XFIXNUM (tem)) - 1;

	      if (completion_ignore_case)
		{
		  /* If this is an exact match except for case,
		     use it as the best match rather than one that is not an
		     exact match.  This way, we get the case pattern
		     of the actual match.  */
		  if ((matchsize == SCHARS (eltstring)
		       && matchsize < SCHARS (bestmatch))
		      ||
		      /* If there is more than one exact match ignoring case,
			 and one of them is exact including case,
			 prefer that one.  */
		      /* If there is no exact match ignoring case,
			 prefer a match that does not change the case
			 of the input.  */
		      ((matchsize == SCHARS (eltstring))
		       ==
		       (matchsize == SCHARS (bestmatch))
		       && (tem = Fcompare_strings (eltstring, zero,
						   make_fixnum (SCHARS (string)),
						   string, zero,
						   Qnil,
						   Qnil),
			   EQ (Qt, tem))
		       && (tem = Fcompare_strings (bestmatch, zero,
						   make_fixnum (SCHARS (string)),
						   string, zero,
						   Qnil,
						   Qnil),
			   ! EQ (Qt, tem))))
		    bestmatch = eltstring;
		}
	      if (bestmatchsize != SCHARS (eltstring)
		  || bestmatchsize != matchsize)
		/* Don't count the same string multiple times.  */
		matchcount += matchcount <= 1;
	      bestmatchsize = matchsize;
	      if (matchsize <= SCHARS (string)
		  /* If completion-ignore-case is non-nil, don't
		     short-circuit because we want to find the best
		     possible match *including* case differences.  */
		  && !completion_ignore_case
		  && matchcount > 1)
		/* No need to look any further.  */
		break;
	    }
	}
    }

  if (bindcount >= 0)
    unbind_to (bindcount, Qnil);

  if (NILP (bestmatch))
    return Qnil;		/* No completions found.  */
  /* If we are ignoring case, and there is no exact match,
     and no additional text was supplied,
     don't change the case of what the user typed.  */
  if (completion_ignore_case && bestmatchsize == SCHARS (string)
      && SCHARS (bestmatch) > bestmatchsize)
    return minibuf_conform_representation (string, bestmatch);

  /* Return t if the supplied string is an exact match (counting case);
     it does not require any change to be made.  */
  if (matchcount == 1 && !NILP (Fequal (bestmatch, string)))
    return Qt;

  XSETFASTINT (zero, 0);		/* Else extract the part in which */
  XSETFASTINT (end, bestmatchsize);	/* all completions agree.  */
  return Fsubstring (bestmatch, zero, end);
}

DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 4, 0,
       doc: /* Search for partial matches to STRING in COLLECTION.
Test each of the possible completions specified by COLLECTION
to see if it begins with STRING.  The possible completions may be
strings or symbols.  Symbols are converted to strings before testing,
see `symbol-name'.
The value is a list of all the possible completions that match STRING.

If COLLECTION is an alist, the keys (cars of elements) are the
possible completions.  If an element is not a cons cell, then the
element itself is the possible completion.
If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.
If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

COLLECTION can also be a function to do the completion itself.
It receives three arguments: the values STRING, PREDICATE and t.
Whatever it returns becomes the value of `all-completions'.

If optional third argument PREDICATE is non-nil,
it is used to test each possible match.
The match is a candidate only if PREDICATE returns non-nil.
The argument given to PREDICATE is the alist element
or the symbol from the obarray.  If COLLECTION is a hash-table,
predicate is called with two arguments: the key and the value.
Additionally to this predicate, `completion-regexp-list'
is used to further constrain the set of candidates.

An obsolete optional fourth argument HIDE-SPACES is still accepted for
backward compatibility.  If non-nil, strings in COLLECTION that start
with a space are ignored unless STRING itself starts with a space.  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate, Lisp_Object hide_spaces)
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  int type = HASH_TABLE_P (collection) ? 3
    : VECTORP (collection) ? 2
    : NILP (collection) || (CONSP (collection) && !FUNCTIONP (collection));
  ptrdiff_t idx = 0, obsize = 0;
  ptrdiff_t bindcount = -1;
  Lisp_Object bucket, tem, zero;

  CHECK_STRING (string);
  if (type == 0)
    return call3 (collection, string, predicate, Qt);
  allmatches = bucket = Qnil;
  zero = make_fixnum (0);

  /* If COLLECTION is not a list, set TAIL just for gc pro.  */
  tail = collection;
  if (type == 2)
    {
      collection = check_obarray (collection);
      obsize = ASIZE (collection);
      bucket = AREF (collection, idx);
    }

  while (1)
    {
      /* Get the next element of the alist, obarray, or hash-table.  */
      /* Exit the loop if the elements are all used up.  */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion.  */

      if (type == 1)
	{
	  if (!CONSP (tail))
	    break;
	  elt = XCAR (tail);
	  eltstring = CONSP (elt) ? XCAR (elt) : elt;
	  tail = XCDR (tail);
	}
      else if (type == 2)
	{
	  if (!EQ (bucket, zero))
	    {
	      if (!SYMBOLP (bucket))
		error ("Bad data in guts of obarray");
	      elt = bucket;
	      eltstring = elt;
	      if (XSYMBOL (bucket)->u.s.next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->u.s.next);
	      else
		XSETFASTINT (bucket, 0);
	    }
	  else if (++idx >= obsize)
	    break;
	  else
	    {
	      bucket = AREF (collection, idx);
	      continue;
	    }
	}
      else /* if (type == 3) */
	{
	  while (idx < HASH_TABLE_SIZE (XHASH_TABLE (collection))
		 && NILP (HASH_HASH (XHASH_TABLE (collection), idx)))
	    idx++;
	  if (idx >= HASH_TABLE_SIZE (XHASH_TABLE (collection)))
	    break;
	  else
	    elt = eltstring = HASH_KEY (XHASH_TABLE (collection), idx++);
	}

      /* Is this element a possible completion?  */

      if (SYMBOLP (eltstring))
	eltstring = Fsymbol_name (eltstring);

      if (STRINGP (eltstring)
	  && SCHARS (string) <= SCHARS (eltstring)
	  /* If HIDE_SPACES, reject alternatives that start with space
	     unless the input starts with space.  */
	  && (NILP (hide_spaces)
	      || (SBYTES (string) > 0
		  && SREF (string, 0) == ' ')
	      || SREF (eltstring, 0) != ' ')
	  && (tem = Fcompare_strings (eltstring, zero,
				      make_fixnum (SCHARS (string)),
				      string, zero,
				      make_fixnum (SCHARS (string)),
				      completion_ignore_case ? Qt : Qnil),
	      EQ (Qt, tem)))
	{
	  /* Yes.  */
	  Lisp_Object regexps;

	  /* Ignore this element if it fails to match all the regexps.  */
	  {
	    for (regexps = Vcompletion_regexp_list; CONSP (regexps);
		 regexps = XCDR (regexps))
	      {
		if (bindcount < 0)
		  {
		    bindcount = SPECPDL_INDEX ();
		    specbind (Qcase_fold_search,
			      completion_ignore_case ? Qt : Qnil);
		  }
		tem = Fstring_match (XCAR (regexps), eltstring, zero);
		if (NILP (tem))
		  break;
	      }
	    if (CONSP (regexps))
	      continue;
	  }

	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it.  */

	  if (!NILP (predicate))
	    {
	      if (EQ (predicate, Qcommandp))
		tem = Fcommandp (elt, Qnil);
	      else
		{
		  if (bindcount >= 0)
		    {
		      unbind_to (bindcount, Qnil);
		      bindcount = -1;
		    }
		  tem = type == 3
		    ? call2 (predicate, elt,
			     HASH_VALUE (XHASH_TABLE (collection), idx - 1))
		    : call1 (predicate, elt);
		}
	      if (NILP (tem)) continue;
	    }
	  /* Ok => put it on the list.  */
	  allmatches = Fcons (eltstring, allmatches);
	}
    }

  if (bindcount >= 0)
    unbind_to (bindcount, Qnil);

  return Fnreverse (allmatches);
}

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 8, 0,
       doc: /* Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
COLLECTION can also be a function to do the completion itself.
PREDICATE limits completion to a subset of COLLECTION.
See `try-completion', `all-completions', `test-completion',
and `completion-boundaries', for more details on completion,
COLLECTION, and PREDICATE.  See also Info node `(elisp)Basic Completion'
for the details about completion, and Info node `(elisp)Programmed
Completion' for expectations from COLLECTION when it's a function.

REQUIRE-MATCH can take the following values:
- t means that the user is not allowed to exit unless the input is (or
  completes to) an element of COLLECTION or is null.
- nil means that the user can exit with any input.
- `confirm' means that the user can exit with any input, but she needs
  to confirm her choice if the input is not an element of COLLECTION.
- `confirm-after-completion' means that the user can exit with any
  input, but she needs to confirm her choice if she called
  `minibuffer-complete' right before `minibuffer-complete-and-exit'
  and the input is not an element of COLLECTION.
- anything else behaves like t except that typing RET does not exit if it
  does non-null completion.

If the input is null, `completing-read' returns DEF, or the first
element of the list of default values, or an empty string if DEF is
nil, regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
  with point positioned at the end.  If it is (STRING . POSITION), the
  initial input is STRING, but point is placed at _zero-indexed_
  position POSITION in STRING.  (*Note* that this is different from
  `read-from-minibuffer' and related functions, which use one-indexing
  for POSITION.)  This feature is deprecated--it is best to pass nil
  for INITIAL-INPUT and supply the default value DEF instead.  The
  user can yank the default value into the minibuffer easily using
  \\<minibuffer-local-map>\\[next-history-element].

HIST, if non-nil, specifies a history list and optionally the initial
  position in the list.  It can be a symbol, which is the history list
  variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
  that case, HISTVAR is the history list variable to use, and HISTPOS
  is the initial position (the position in the list used by the
  minibuffer history commands).  For consistency, you should also
  specify that element of the history as the value of INITIAL-INPUT.
  (This is the only case in which you should use INITIAL-INPUT instead
  of DEF.)  Positions are counted starting from 1 at the beginning of
  the list.  The variable `history-length' controls the maximum length
  of a history list.

DEF, if non-nil, is the default value or the list of default values.

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
  current input method and the setting of `enable-multibyte-characters'.

Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil.

See also `completing-read-function'.  */)
  (Lisp_Object prompt, Lisp_Object collection, Lisp_Object predicate, Lisp_Object require_match, Lisp_Object initial_input, Lisp_Object hist, Lisp_Object def, Lisp_Object inherit_input_method)
{
  return CALLN (Ffuncall,
		Fsymbol_value (intern ("completing-read-function")),
		prompt, collection, predicate, require_match, initial_input,
		hist, def, inherit_input_method);
}

/* Test whether TXT is an exact completion.  */
DEFUN ("test-completion", Ftest_completion, Stest_completion, 2, 3, 0,
       doc: /* Return non-nil if STRING is a valid completion.
Takes the same arguments as `all-completions' and `try-completion'.
If COLLECTION is a function, it is called with three arguments:
the values STRING, PREDICATE and `lambda'.  */)
  (Lisp_Object string, Lisp_Object collection, Lisp_Object predicate)
{
  Lisp_Object regexps, tail, tem = Qnil;
  ptrdiff_t i = 0;

  CHECK_STRING (string);

  if (NILP (collection) || (CONSP (collection) && !FUNCTIONP (collection)))
    {
      tem = Fassoc_string (string, collection, completion_ignore_case ? Qt : Qnil);
      if (NILP (tem))
	return Qnil;
    }
  else if (VECTORP (collection))
    {
      /* Bypass intern-soft as that loses for nil.  */
      tem = oblookup (collection,
		      SSDATA (string),
		      SCHARS (string),
		      SBYTES (string));
      if (!SYMBOLP (tem))
	{
	  if (STRING_MULTIBYTE (string))
	    string = Fstring_make_unibyte (string);
	  else
	    string = Fstring_make_multibyte (string);

	  tem = oblookup (collection,
			  SSDATA (string),
			  SCHARS (string),
			  SBYTES (string));
	}

      if (completion_ignore_case && !SYMBOLP (tem))
	{
	  for (i = ASIZE (collection) - 1; i >= 0; i--)
	    {
	      tail = AREF (collection, i);
	      if (SYMBOLP (tail))
		while (1)
		  {
		    if (EQ (Fcompare_strings (string, make_fixnum (0), Qnil,
					      Fsymbol_name (tail),
					      make_fixnum (0) , Qnil, Qt),
			   Qt))
		      {
			tem = tail;
			break;
		      }
		    if (XSYMBOL (tail)->u.s.next == 0)
		      break;
		    XSETSYMBOL (tail, XSYMBOL (tail)->u.s.next);
		  }
	    }
	}

      if (!SYMBOLP (tem))
	return Qnil;
    }
  else if (HASH_TABLE_P (collection))
    {
      struct Lisp_Hash_Table *h = XHASH_TABLE (collection);
      i = hash_lookup (h, string, NULL);
      if (i >= 0)
        {
          tem = HASH_KEY (h, i);
          goto found_matching_key;
        }
      else
	for (i = 0; i < HASH_TABLE_SIZE (h); ++i)
          {
            if (NILP (HASH_HASH (h, i))) continue;
            tem = HASH_KEY (h, i);
            Lisp_Object strkey = (SYMBOLP (tem) ? Fsymbol_name (tem) : tem);
            if (!STRINGP (strkey)) continue;
            if (EQ (Fcompare_strings (string, Qnil, Qnil,
                                      strkey, Qnil, Qnil,
                                      completion_ignore_case ? Qt : Qnil),
                    Qt))
              goto found_matching_key;
          }
      return Qnil;
    found_matching_key: ;
    }
  else
    return call3 (collection, string, predicate, Qlambda);

  /* Reject this element if it fails to match all the regexps.  */
  if (CONSP (Vcompletion_regexp_list))
    {
      ptrdiff_t count = SPECPDL_INDEX ();
      specbind (Qcase_fold_search, completion_ignore_case ? Qt : Qnil);
      for (regexps = Vcompletion_regexp_list; CONSP (regexps);
	   regexps = XCDR (regexps))
	{
          /* We can test against STRING, because if we got here, then
             the element is equivalent to it.  */
          if (NILP (Fstring_match (XCAR (regexps), string, Qnil)))
	    return unbind_to (count, Qnil);
	}
      unbind_to (count, Qnil);
    }

  /* Finally, check the predicate.  */
  if (!NILP (predicate))
    {
      return HASH_TABLE_P (collection)
	? call2 (predicate, tem, HASH_VALUE (XHASH_TABLE (collection), i))
	: call1 (predicate, tem);
    }
  else
    return Qt;
}

DEFUN ("internal-complete-buffer", Finternal_complete_buffer, Sinternal_complete_buffer, 3, 3, 0,
       doc: /* Perform completion on buffer names.
STRING and PREDICATE have the same meanings as in `try-completion',
`all-completions', and `test-completion'.

If FLAG is nil, invoke `try-completion'; if it is t, invoke
`all-completions'; otherwise invoke `test-completion'.  */)
  (Lisp_Object string, Lisp_Object predicate, Lisp_Object flag)
{
  if (NILP (flag))
    return Ftry_completion (string, Vbuffer_alist, predicate);
  else if (EQ (flag, Qt))
    {
      Lisp_Object res = Fall_completions (string, Vbuffer_alist, predicate, Qnil);
      if (SCHARS (string) > 0)
	return res;
      else
	{ /* Strip out internal buffers.  */
	  Lisp_Object bufs = res;
	  /* First, look for a non-internal buffer in `res'.  */
	  while (CONSP (bufs) && SREF (XCAR (bufs), 0) == ' ')
	    bufs = XCDR (bufs);
	  if (NILP (bufs))
	    return (list_length (res) == list_length (Vbuffer_alist)
		    /* If all bufs are internal don't strip them out.  */
		    ? res : bufs);
	  res = bufs;
	  while (CONSP (XCDR (bufs)))
	    if (SREF (XCAR (XCDR (bufs)), 0) == ' ')
	      XSETCDR (bufs, XCDR (XCDR (bufs)));
	    else
	      bufs = XCDR (bufs);
	  return res;
	}
    }
  else if (EQ (flag, Qlambda))
    return Ftest_completion (string, Vbuffer_alist, predicate);
  else if (EQ (flag, Qmetadata))
    return list3 (Qmetadata,
                  Fcons (Qcategory, Qbuffer),
                  Fcons (Qcycle_sort_function, Qidentity));
  else
    return Qnil;
}

/* Like assoc but assumes KEY is a string, and ignores case if appropriate.  */

DEFUN ("assoc-string", Fassoc_string, Sassoc_string, 2, 3, 0,
       doc: /* Like `assoc' but specifically for strings (and symbols).

This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, both
KEY and the elements of LIST are upcased for comparison.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.  */)
  (register Lisp_Object key, Lisp_Object list, Lisp_Object case_fold)
{
  register Lisp_Object tail;

  if (SYMBOLP (key))
    key = Fsymbol_name (key);

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object elt, tem, thiscar;
      elt = XCAR (tail);
      thiscar = CONSP (elt) ? XCAR (elt) : elt;
      if (SYMBOLP (thiscar))
	thiscar = Fsymbol_name (thiscar);
      else if (!STRINGP (thiscar))
	continue;
      tem = Fcompare_strings (thiscar, make_fixnum (0), Qnil,
			      key, make_fixnum (0), Qnil,
			      case_fold);
      if (EQ (tem, Qt))
	return elt;
      maybe_quit ();
    }
  return Qnil;
}


DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
       doc: /* Return current depth of activations of minibuffer, a nonnegative integer.  */)
  (void)
{
  return make_fixnum (minibuf_level);
}

DEFUN ("minibuffer-prompt", Fminibuffer_prompt, Sminibuffer_prompt, 0, 0, 0,
       doc: /* Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.  */)
  (void)
{
  return Fcopy_sequence (minibuf_prompt);
}



static void init_minibuf_once_for_pdumper (void);

void
init_minibuf_once (void)
{
  staticpro (&Vminibuffer_list);
  pdumper_do_now_and_after_load (init_minibuf_once_for_pdumper);
}

static void
init_minibuf_once_for_pdumper (void)
{
  PDUMPER_IGNORE (minibuf_level);
  PDUMPER_IGNORE (minibuf_prompt_width);

  /* We run this function on first initialization and whenever we
     restore from a pdumper image.  pdumper doesn't try to preserve
     frames, windows, and so on, so reset everything related here.  */
  Vminibuffer_list = Qnil;
  minibuf_level = 0;
  minibuf_prompt = Qnil;
  minibuf_save_list = Qnil;
  last_minibuf_string = Qnil;
}

void
syms_of_minibuf (void)
{
  staticpro (&minibuf_prompt);
  staticpro (&minibuf_save_list);

  DEFSYM (Qcompletion_ignore_case, "completion-ignore-case");
  DEFSYM (Qminibuffer_default, "minibuffer-default");
  Fset (Qminibuffer_default, Qnil);

  DEFSYM (Qminibuffer_completion_table, "minibuffer-completion-table");

  staticpro (&last_minibuf_string);

  DEFSYM (Qcustom_variable_history, "custom-variable-history");
  Fset (Qcustom_variable_history, Qnil);

  DEFSYM (Qminibuffer_history, "minibuffer-history");
  DEFSYM (Qbuffer_name_history, "buffer-name-history");
  Fset (Qbuffer_name_history, Qnil);

  DEFSYM (Qcustom_variable_p, "custom-variable-p");

  /* Normal hooks for entry to and exit from minibuffer.  */
  DEFSYM (Qminibuffer_setup_hook, "minibuffer-setup-hook");
  DEFSYM (Qminibuffer_exit_hook, "minibuffer-exit-hook");

  /* The maximum length of a minibuffer history.  */
  DEFSYM (Qhistory_length, "history-length");

  DEFSYM (Qcurrent_input_method, "current-input-method");
  DEFSYM (Qactivate_input_method, "activate-input-method");
  DEFSYM (Qcase_fold_search, "case-fold-search");
  DEFSYM (Qmetadata, "metadata");
  DEFSYM (Qcycle_sort_function, "cycle-sort-function");

  /* A frame parameter.  */
  DEFSYM (Qminibuffer_exit, "minibuffer-exit");

  DEFVAR_LISP ("read-expression-history", Vread_expression_history,
	       doc: /* A history list for arguments that are Lisp expressions to evaluate.
For example, `eval-expression' uses this.  */);
  Vread_expression_history = Qnil;

  DEFVAR_LISP ("read-buffer-function", Vread_buffer_function,
	       doc: /* If this is non-nil, `read-buffer' does its work by calling this function.
The function is called with the arguments passed to `read-buffer'.  */);
  Vread_buffer_function = Qnil;

  DEFVAR_BOOL ("read-buffer-completion-ignore-case",
	       read_buffer_completion_ignore_case,
	       doc: /* Non-nil means completion ignores case when reading a buffer name.  */);
  read_buffer_completion_ignore_case = 0;

  DEFVAR_LISP ("minibuffer-setup-hook", Vminibuffer_setup_hook,
	       doc: /* Normal hook run just after entry to minibuffer.  */);
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_LISP ("minibuffer-exit-hook", Vminibuffer_exit_hook,
	       doc: /* Normal hook run just after exit from minibuffer.  */);
  Vminibuffer_exit_hook = Qnil;

  DEFVAR_LISP ("history-length", Vhistory_length,
	       doc: /* Maximum length of history lists before truncation takes place.
A number means truncate to that length; truncation deletes old
elements, and is done just after inserting a new element.
A value of t means no truncation.

This variable only affects history lists that don't specify their own
maximum lengths.  Setting the `history-length' property of a history
variable overrides this default.  */);
  XSETFASTINT (Vhistory_length, 100);

  DEFVAR_BOOL ("history-delete-duplicates", history_delete_duplicates,
	       doc: /* Non-nil means to delete duplicates in history.
If set to t when adding a new history element, all previous identical
elements are deleted from the history list.  */);
  history_delete_duplicates = 0;

  DEFVAR_LISP ("history-add-new-input", Vhistory_add_new_input,
	       doc: /* Non-nil means to add new elements in history.
If set to nil, minibuffer reading functions don't add new elements to the
history list, so it is possible to do this afterwards by calling
`add-to-history' explicitly.  */);
  Vhistory_add_new_input = Qt;

  DEFVAR_BOOL ("completion-ignore-case", completion_ignore_case,
	       doc: /* Non-nil means don't consider case significant in completion.
For file-name completion, `read-file-name-completion-ignore-case'
controls the behavior, rather than this variable.
For buffer name completion, `read-buffer-completion-ignore-case'
controls the behavior, rather than this variable.  */);
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", enable_recursive_minibuffers,
	       doc: /* Non-nil means to allow minibuffer commands while in the minibuffer.
This variable makes a difference whenever the minibuffer window is active.
Also see `minibuffer-depth-indicate-mode', which may be handy if this
variable is non-nil. */);
  enable_recursive_minibuffers = 0;

  DEFVAR_LISP ("minibuffer-completion-table", Vminibuffer_completion_table,
	       doc: /* Alist or obarray used for completion in the minibuffer.
This becomes the ALIST argument to `try-completion' and `all-completions'.
The value can also be a list of strings or a hash table.

The value may alternatively be a function, which is given three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda':
  nil    -- return the best completion of STRING, or nil if there is none.
  t      -- return a list of all possible completions of STRING.
  lambda -- return t if STRING is a valid completion as it stands.  */);
  Vminibuffer_completion_table = Qnil;

  DEFVAR_LISP ("minibuffer-completion-predicate", Vminibuffer_completion_predicate,
	       doc: /* Within call to `completing-read', this holds the PREDICATE argument.  */);
  Vminibuffer_completion_predicate = Qnil;

  DEFVAR_LISP ("minibuffer-completion-confirm", Vminibuffer_completion_confirm,
	       doc: /* Whether to demand confirmation of completion before exiting minibuffer.
If nil, confirmation is not required.
If the value is `confirm', the user may exit with an input that is not
 a valid completion alternative, but Emacs asks for confirmation.
If the value is `confirm-after-completion', the user may exit with an
 input that is not a valid completion alternative, but Emacs asks for
 confirmation if the user submitted the input right after any of the
 completion commands listed in `minibuffer-confirm-exit-commands'.  */);
  Vminibuffer_completion_confirm = Qnil;

  DEFVAR_LISP ("minibuffer-completing-file-name",
	       Vminibuffer_completing_file_name,
	       doc: /* Non-nil means completing file names.  */);
  Vminibuffer_completing_file_name = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", Vminibuffer_help_form,
	       doc: /* Value that `help-form' takes on inside the minibuffer.  */);
  Vminibuffer_help_form = Qnil;

  DEFVAR_LISP ("minibuffer-history-variable", Vminibuffer_history_variable,
	       doc: /* History list symbol to add minibuffer values to.
Each string of minibuffer input, as it appears on exit from the minibuffer,
is added with

  (set minibuffer-history-variable
       (cons STRING (symbol-value minibuffer-history-variable)))

 If the variable is the symbol `t', no history is recorded.  */);
  XSETFASTINT (Vminibuffer_history_variable, 0);

  DEFVAR_LISP ("minibuffer-history-position", Vminibuffer_history_position,
	       doc: /* Current position of redoing in the history list.  */);
  Vminibuffer_history_position = Qnil;

  DEFVAR_BOOL ("minibuffer-auto-raise", minibuffer_auto_raise,
	       doc: /* Non-nil means entering the minibuffer raises the minibuffer's frame.
Some uses of the echo area also raise that frame (since they use it too).  */);
  minibuffer_auto_raise = 0;

  DEFVAR_LISP ("completion-regexp-list", Vcompletion_regexp_list,
	       doc: /* List of regexps that should restrict possible completions.
The basic completion functions only consider a completion acceptable
if it matches all regular expressions in this list, with
`case-fold-search' bound to the value of `completion-ignore-case'.
See Info node `(elisp)Basic Completion', for a description of these
functions.  */);
  Vcompletion_regexp_list = Qnil;

  DEFVAR_BOOL ("minibuffer-allow-text-properties",
	       minibuffer_allow_text_properties,
	       doc: /* Non-nil means `read-from-minibuffer' should not discard text properties.
This also affects `read-string', but it does not affect `read-minibuffer',
`read-no-blanks-input', or any of the functions that do minibuffer input
with completion; they always discard text properties.  */);
  minibuffer_allow_text_properties = 0;

  DEFVAR_LISP ("minibuffer-prompt-properties", Vminibuffer_prompt_properties,
	       doc: /* Text properties that are added to minibuffer prompts.
These are in addition to the basic `field' property, and stickiness
properties.  */);
  Vminibuffer_prompt_properties = list2 (Qread_only, Qt);

  DEFVAR_LISP ("read-hide-char", Vread_hide_char,
	       doc: /* Whether to hide input characters in noninteractive mode.
If non-nil, it must be a character, which will be used to mask the
input characters.  This variable should never be set globally.

This variable also overrides the default character that `read-passwd'
uses to hide passwords.  */);
  Vread_hide_char = Qnil;

  defsubr (&Sactive_minibuffer_window);
  defsubr (&Sset_minibuffer_window);
  defsubr (&Sread_from_minibuffer);
  defsubr (&Sread_string);
  defsubr (&Sread_command);
  defsubr (&Sread_variable);
  defsubr (&Sinternal_complete_buffer);
  defsubr (&Sread_buffer);
  defsubr (&Sread_no_blanks_input);
  defsubr (&Sminibuffer_depth);
  defsubr (&Sminibuffer_prompt);

  defsubr (&Sminibufferp);
  defsubr (&Sminibuffer_prompt_end);
  defsubr (&Sminibuffer_contents);
  defsubr (&Sminibuffer_contents_no_properties);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Stest_completion);
  defsubr (&Sassoc_string);
  defsubr (&Scompleting_read);
}
