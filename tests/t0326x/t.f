      program t
      real c(1,2)
      call s(1,c)
      contains
      subroutine s(a,b)
      integer a
c     b is an assumed-size array
      real b(a,*)
      print *,size(b,1)
      end subroutine s
      end program t
