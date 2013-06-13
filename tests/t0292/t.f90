program t
  implicit none
! io-control-spec write stmt iostat (with error)
  integer::ierr=0
  
  open (95, status='new', file='tmpfile', access='direct', recl=3)
  write (95, iostat=ierr) 'hello'

  if (ierr .ne. 0) then
    print *,'test successful'
  endif
endprogram t
