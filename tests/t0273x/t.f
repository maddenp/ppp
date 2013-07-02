      program t
      implicit none
C open stmt (no error) iostat&ierr
      character*16::out
      integer::ierr
      
      open (unit=95, status='old', file='infile', err=100, iostat=ierr)
      read (95, '(a)') out
      print *,out
      
 100  print *,'yes'
      if (ierr .eq. 0) then
         print *,'and yes'
      endif
      
      endprogram t
 
