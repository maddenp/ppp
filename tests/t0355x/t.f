      program p
      implicit none
      integer::x6h =1
      integer::x_6h=1
      integer::x16h=1
c don't misinterpret variables as holleriths
      call s(a=x6h,  b=2)
      call s(a=x_6h, b=2)
      call s(a=x16h, b=2)
      contains
      subroutine s(a,b)
      integer::a,b
      print *,a,b
      end subroutine s
      end program p
