integer function f()
  f=77
end function f

program t
  implicit none
  integer::f
  write (*,'(i0)') f()
end program t

