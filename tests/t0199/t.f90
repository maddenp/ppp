program t
  print '(a)',f(1)
contains
  character(7) function f(i)
    integer,optional::i
    if (present(i)) then
      f='present'
    else
      f='absent'
    endif
  end function f
end program t
