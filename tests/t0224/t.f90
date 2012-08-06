module m
  integer::i=3
  integer::j=4
end module m

program t
  use m,only:i,j
  print '(i0)',i,j
end program t
