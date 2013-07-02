      program t
      call s(1,2,3)
      end program t
      
      subroutine s(a,
     cb,
!     comment
     !c)
      integer,intent(in)::a,b,c
      print '(a)','hello
     1world'
      end subroutine s
