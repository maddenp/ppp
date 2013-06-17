program t
  implicit none
!open stmt error (file nonexistence) iostat & err label 
  integer::ierr
  
  open (unit=95, status='old', file='test', err=100, iostat=ierr)

  print *,'no'

100 print *,'yes'

  if (ierr .ne. 0) then
    print *,'and yes'
  endif
  
  
endprogram t
 
