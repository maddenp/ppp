program t
  integer :: i ( : )
  integer :: j
  pointer :: i , j(: , :)
  nullify(i)
  print *,associated(i)
end program t
