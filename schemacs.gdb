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

# needed for memgrep
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

break wrong_type_argument
break _exit

break gdb_break
commands
  up
end

break terminate_due_to_signal
commands
  up
  up
  bt
end

break exit
break abort

catch signal SIGSEGV
commands
  silent
  if $_any_caller_is("try_memgrep1")
    cont
  else
    p scheme_fptr_call_info
    echo segv; fptr_run?\n
  end
end

define fptr_run
  tbreak scheme_fptr_call_fun
  eval "ignore %d %d", $bpnum, scheme_fptr_call_info.count - 1
  run
end

define ref_break
  break gdb_found_ref
end

define watch_for
  printf "watch_for %p\n", $arg0
  # if !$_is_void($watch_num)
  #   delete $watch_num
  # end
  set $watching_addr = $arg0
  set $watching_for = $arg1  
  watch *$arg0
  set $watch_num = $bpnum
  commands
    #printf "set $tmp = *(uint64_t*)%lu == %lu\n", $watching_addr, $watching_for
    if *(uint64_t*)$watching_addr != $watching_for
      printf "wrong value: 0x%lx at %p\n", *(uint64_t)$watching_addr, $watching_addr
      continue
    else
      printf "OK value: 0x%lx at %p\n", *(uint64_t)$watching_addr, $watching_addr
      #on_watch
    end
  end
end

define on_watch
  echo on_watch\n
  if $_any_caller_matches(".*_scheme_gc$|container_|.*memgrep|msort_with_tmp$", 20)
    echo on_watch match\n
    cont
  else
    bt 5
  end
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

set $first_run = 1

break main
commands
  silent
  set $n = 0
  if $first_run
    #watch_for (size_t*)0x14e2b80 0x7fff00000001
  end
  set $first_run = 0
  continue
end

run


# define hook-run
#   eval "shell %s/run-schemacs --just-make", $top_dir
# end
