      program t
      implicit none
      type x
      integer::a
      end type x
      type y
      type(x)::b(3)
      end type y
      type(y)::z
      integer::i
      data (z%b(i)%a,i=1,3) /1,2,3/
      print '(i0)',z%b
      end program t
