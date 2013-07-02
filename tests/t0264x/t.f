      program t
      implicit none
      print '(i0)',f()
      contains
      elemental integer function f()
      f=88
      end function f
      end program t
