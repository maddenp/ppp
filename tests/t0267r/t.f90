        !First Line Comment
program t
  implicit none
! open stmt old status
  character*14::answer
  open (unit=95, file='infile', status='old')
  read (95, '(a)')answer
  print *,answer
end program t
