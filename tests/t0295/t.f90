program t1
  implicit none
! io-control-spec read stmt end label
  character*4::out
  integer::ierr
  open (95, file='infile', status='old')
  read (95, '(a)') out
  read (95, '(a)',end=100) out
  print *, out
  print *,'i wish this'
  
100 print *,'test was successful'
endprogram t1

