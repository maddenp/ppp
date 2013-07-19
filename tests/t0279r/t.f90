program t
  implicit none
! open stmt record length specifier
  character*10::output

  open (95, file='infile', status='old', recl=5)
  read (95, '(a)') output
  print *,output
endprogram t
