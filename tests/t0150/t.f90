program t
  implicit none
  type a
    integer::ai
  end type a
  type(a)::x
  integer::y
  x%ai=77
  y=x%ai
  print '(i0)',y
end program t
