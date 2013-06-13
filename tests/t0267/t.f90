program t1
  implicit none
! open stmt old status
  character*14::answer
  open (unit=95, file='infile', status='old')
  read (95, '(a)')answer
  print *,answer
endprogram t1
