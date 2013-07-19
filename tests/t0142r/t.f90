program t
  type x
    integer,pointer::p(:)
    integer::q(3)
  end type x
  print *,kind(q(3))
end program t
