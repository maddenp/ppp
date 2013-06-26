function addx(p1,p2) result (r)
  use m
  implicit none
  type(x),intent(in)::p1,p2
  type(x)::r
  r%i=p1%i+p2%i
end function addx
