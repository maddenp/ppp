program t
  implicit none
  character*16::answer
! open stmt new status
  open (95, file='tmpfile', status='new')
  write (95, '(a)') 'test 268 success'
  close (95)
  open (95, file='tmpfile', status='old')
  read (95, '(a)') answer
  print *,answer
  close (95,status='delete')
end program t
