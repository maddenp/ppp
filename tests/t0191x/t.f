      program t
      implicit none
      integer::i
      i=f(0)
      print '(i0)',i
      contains
      integer function f(i)
      integer::i
      print '(i0)',i
      f=i+1
      end function f
      end program t
