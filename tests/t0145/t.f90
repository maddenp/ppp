program t
100 format (i0)
  type x
    integer,pointer::a(:)
  end type x
  type y
    type(x),pointer::b(:)
  end type y
  type(y)::z
  integer::ierr
  allocate(z%b(3))
  allocate(z%b(2)%a(1))
  z%b(2)%a(1)=7
  print 100,z%b(2)%a(1)
  deallocate(z%b(2)%a,stat=ierr)
  deallocate(z%b,stat=ierr)
end program t
