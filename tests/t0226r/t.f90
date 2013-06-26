module m
  integer::i=3
  integer::j=4
end module m

program t
  use m,k=>j
  print '(i0)',i,k
end program t
