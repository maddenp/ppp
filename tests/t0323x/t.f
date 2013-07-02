      program t
      use m
      implicit none
      interface operator (+)
      function addx(p1,p2) result (r)
      use m
      type(x),intent(in)::p1,p2
      type(x)::r
      end function addx
      end interface operator (+)
      type(x)::a,b
      a%i=1
      b%i=2
      print *,a+b
      end program t
