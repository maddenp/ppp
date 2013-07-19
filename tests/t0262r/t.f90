program t
  implicit none
  integer::a,b=2
  integer,pointer::c
  target::a(1),b
  print '(i0)',size(a,1)
  c=>b
  print '(i0)',c
end program t
