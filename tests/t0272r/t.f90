program t
  implicit none
!open stmt error (file nonexistence) iostat & err label 
!and number label with letter label in if-construct
  integer::ierr
  
  open (unit=95, status='old', file='test', err=100, iostat=ierr)

  print *,'no'

100 print *,'yes'

200 x:  if (ierr .ne. 0) then
    print *,'and yes'
  endif x
  
  
endprogram t
 
