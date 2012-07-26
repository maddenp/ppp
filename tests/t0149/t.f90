program t
  implicit none
  type a
    integer,pointer::pa
  end type a
  type b
    integer,pointer::pb(:)
  end type b
  integer,target::p=77
  integer,target::q=88
  integer::ierr
  type(a)::x
  type(b)::y
  x%pa=>p
  allocate(y%pb(1),stat=ierr)
  deallocate(y%pb)
  nullify(y%pb)
end program t
