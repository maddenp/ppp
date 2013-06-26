module m
  integer::i=3
contains
  subroutine s
    print '(a)','hello'
  end subroutine s
end module m

program t
  use m,only:s
  integer::i=77
  print '(i0)',i
  call s
end program t
