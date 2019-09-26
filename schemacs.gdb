# -*- mode: gdb-script -*-

set history save on
set breakpoint pending on
set pagination off
set height 0
#tui enable

#set prompt \033[1;31m(gdb) \033[m


set print thread-events off
set print inferior-events off
set print frame-arguments none
#set print address off
set trace-commands off

handle SIGSEGV nostop
handle SIGSEGV noprint

#handle SIGINT nopass

set confirm off

define qq
  set confirm off
  quit
end

del

define bsave
  save breakpoints .breakpoints.gdb
end

# define hookpost-break
#   bsave
# end

# define hookpost-delete
#   bsave
# end

# define hookpost-commands
#   bsave
# end

# break wrong_type_argument
# break _exit

# break terminate_due_to_signal
# command
# up
# up
# end

break main
commands
  silent
  set $n = 0
  continue
end

break gdb_break
commands
  up
end

# break die
# commands
#   up
# end

catch signal SIGSEGV
commands
  p scheme_fptr_call_info
  echo fptr_run?\n
end

define fptr_run
  tbreak scheme_fptr_call_fun
  eval "ignore %d %d", $bpnum, scheme_fptr_call_info.count - 1
  run
end

define hook-run
  eval "shell %s/run-schemacs --just-make", $top_dir
end

define ref_break
  break gdb_found_ref
end

# break gdb_found_ref
# disable
# command
# bt 5
# up
# up
# end

# break before_scheme_gc
# command
# set record full memory-query off
# record full
# #cont
# end

#b __strlen_avx2
#dis

# break Scall0
# command
# record stop
# cont
# end

#source .breakpoints.gdb

define ps
  p $arg0
  printf "%s\n", gdb_write(gdb_print_scheme, (void *)$arg0)
end

define pl
  p $arg0
  printf "%s\n", gdb_write(gdb_print_lisp, (void *)$arg0)
end

define pp
  info symbol $arg0
end

define mg
  set $mg_tmp = memgrep1($arg1, $arg0)
  pp $mg_tmp
end
