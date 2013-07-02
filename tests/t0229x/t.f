      program t
      implicit none
      integer::i(3)=(/1,2,3/)
      integer::f=1
      real c,temp
      c(f)=5.0*(f-32.0)/9.0
      i(f)=7
      print '(f5.1)',c(32)
      print '(i0)',i(1)
      end program t
