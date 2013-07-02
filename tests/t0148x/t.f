      program t
      implicit none
      type x
      character(len=5)::s
      end type x
      type(x)::y
      y%s='hello'
      print '(a)',y%s(2:4)
      end program t
