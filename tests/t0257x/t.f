      module m
      implicit none
      public s
      contains
      subroutine s
      print *,'bad'
      end subroutine s
      end module m
      
      program t
      use m,only:s
      print *,'ok'
      end program t
