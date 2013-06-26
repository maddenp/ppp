program t
  implicit none
! io-control-spec write stmt err label (with error)
  
  open (95, status='new', file='tmpfile', access='direct', recl=3)
  write (95, err=100) 'hello'
print *,'no'
 100 print*,'test successful'
close (95,status='delete')
endprogram t
