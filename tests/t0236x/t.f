      module m
      contains
      subroutine s
      end subroutine s
      logical function f()
      f=.true.
      end function f
      end module m
      
      program t
      use m,only:f
      print '(l)',f()
      end program t
