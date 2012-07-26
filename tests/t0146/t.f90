program t
  implicit none
  integer,dimension(3)::x
  integer::i
  data (x(i),i=1,3) /1,2,3/
  print '(i0)',x
end program t
