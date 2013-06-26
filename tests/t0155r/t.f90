program t
  integer :: i ( : )
  integer :: j
  allocatable :: i , j ( : , : )
  print *,allocated(j)
end program t
