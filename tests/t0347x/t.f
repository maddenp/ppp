      subroutine s(y)
      complex*16 y(*)
      print *,kind(y)
      end subroutine s
  
      program p
      call s((1.,1.))
      end program p
