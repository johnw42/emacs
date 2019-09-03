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

break wrong_type_argument
break _exit

break terminate_due_to_signal
command
up
up
end

break gdb_break
command
up
end

#source .breakpoints.gdb

define ps
  printf "%s\n", gdb_print_scheme((void *)$arg0)
end

define pl
  printf "%s\n", gdb_print((void *)$arg0)
end
