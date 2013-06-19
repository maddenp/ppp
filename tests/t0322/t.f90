module m
  implicit none
  type wrapper
     integer::i
  end type wrapper
  interface operator (+)
     module procedure addw
  end interface
contains
  function addw(a,b)
    type(wrapper),intent(in)::a,b
    type(wrapper)::addw
    addw%i=a%i+b%i
  end function addw
end module m

program t
  use m
  implicit none
  type(wrapper)::x,y
  x%i=2
  y%i=3
  print *,x+y
end program t
