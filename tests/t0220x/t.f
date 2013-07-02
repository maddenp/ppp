      program t
      print *,'ok'
      end program t
      
      module m
      contains
      subroutine s
      print *,'bad'
      end subroutine s
      end module m
      
