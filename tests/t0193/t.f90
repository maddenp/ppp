program p
  implicit none
  integer::i
  i=f(-1)
  print '(i0)',i
contains
  integer function f(i)
    integer::i
    print '(i0)',i
    if (i.lt.0) then
      f=i
      return
    endif
    f=i+1
  end function f
end program p
