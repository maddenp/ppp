program t
  implicit none
!open stmt sequential formatted 
  character*5::output

  open (95, status='old', file='infile', access='sequential', form='formatted')

  read (95, '(a5)') output
  
  print *,output
  
  print *,'goodbye'
  
  read (95, '(a)') output
  
  print *,output
  endprogram t
