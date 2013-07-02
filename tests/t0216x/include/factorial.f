      recursive subroutine factorial(i,f)
      implicit none
      integer f,i,j
      intent(in) i
      intent(out) f
      if (i.le.0) then
         stop 'argument must be > 0'
      else if (i.eq.1) then
      f=i
      else
         call factorial(i-1,j)
         f=i*j
      end if
      end subroutine factorial
