program t
  integer::ierr
100 format (i0)
  type x
    integer,pointer::p(:)
    integer::q(3)
  end type x
  type(x)::y
  y%q(1)=1
  y%q(2)=2
  y%q(3)=3
  print 100,y%q
  allocate(y%p(1),stat=ierr)
  deallocate(y%p,stat=ierr)
end program t
