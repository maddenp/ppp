      program t
      implicit none
C open stmt read/write from scratch w/rewind
      character*18::answer
      integer::a
      integer::k
      
      open (unit=95, status='scratch')
      do k=1,3
         write (95, '(i2)') k
      end do
      
      rewind (95)
      
      do k=1,3
         read (95, '(i2)') a
         print *,a
      enddo 
      
      endprogram t
 
