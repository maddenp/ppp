program t
  implicit none
!write command exceeding open stmt record length 
  character*10::output
  integer::ierr
  integer::ierra

  open (95, file='tmpfile', status='new', recl=5)
  write (95, '(a)', iostat=ierr) 'zyxwvutsrqponm'
 
  if (ierr .ne. 0) then
    print *,'backwards alphabet'
  endif

  close (95,status='delete')

endprogram t
