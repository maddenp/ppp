program p
  integer,allocatable::i(:),j(:)
  integer::ierr
  allocate(i(1:3),j(2+1),stat=ierr)
  i=1
  j=2
  print *,i
  print *,j
  deallocate(i,j,stat=ierr)
end program p
