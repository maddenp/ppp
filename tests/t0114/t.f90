program p
  integer,allocatable::i(:),j(:)
  integer::ierr
  allocate(i(1:3),j(2+1),stat=ierr)
  deallocate(i,j,stat=ierr)
end program p
