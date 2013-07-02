      program t
      implicit none
      type a
      integer,pointer::pa=>null()
      end type a
      type b
      integer,pointer::pb(:)
      end type b
      integer,target::p=77
      integer,target::q=88
      integer::ierr
      type(a)::x
      type(b)::y
      print *,associated(x%pa)
      x%pa=>p
      print *,x%pa.eq.p
      print *,associated(y%pb)
      allocate(y%pb(1),stat=ierr)
      print *,associated(y%pb)
      deallocate(y%pb)
      print *,associated(y%pb)
      nullify(y%pb)
      print *,associated(y%pb)
      end program t
