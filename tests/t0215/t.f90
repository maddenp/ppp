include 'factorial.f90'

program p
  implicit none
  integer::f
  call factorial(5,f)
  print '(i0)',f
end program p
