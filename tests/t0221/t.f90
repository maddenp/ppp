module m
  implicit none
  type vector
    integer::x
    integer::y
  end type vector
  interface operator (.plus.)
     module procedure addw
  end interface operator (.plus.)
contains
  function addw(a,b)
    type(vector),intent(in)::a,b
    type(vector)::addw
    addw%x=a%x+b%x
    addw%y=a%y+b%y
  end function addw
end module m

program t
  use m
  implicit none
  type(vector)::f,g
  integer::h,i
  f%x=7
  f%y=8
  g%x=9
  g%y=10
  h=7
  i=8
  print *,f.plus.g
  print *,h+i
end program t
