      module m
      implicit none
      type vector
      integer::x
      integer::y
      end type vector
      interface operator (.plus.)
      module procedure addvec
      end interface operator (.plus.)
      interface assignment (=)
      module procedure assignvec
      endinterface assignment (=)
      contains
      function addvec(a,b)
      type(vector),intent(in)::a,b
      type(vector)::addvec
      addvec%x=a%x+b%x
      addvec%y=a%y+b%y
      end function addvec
      subroutine assignvec (b,a)
      type(vector),intent(in)::a
      real,intent(out)::b
      b = sqrt((a%x**2.0) + (a%y**2.0))
      endsubroutine assignvec
    
      end module m

      program t
      use m
      implicit none
      type(vector)::f,g
      integer::h,i
      real::magnitude
      f%x=7
      f%y=8
      g%x=9
      g%y=10
      h=7
      i=8
      print *,f.plus.g
      print *,h+i
      magnitude =f
      print *,magnitude
      end program t
