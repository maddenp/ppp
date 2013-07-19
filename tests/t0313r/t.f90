program t
  implicit none
! inquire stmt all 'action' specifiers (read)
! note: results differ by complier interpretation 
  character*9::a='hello'
  character*3::b,c,d
  integer::ierr
  open (56, status='old', file='infile', action='read')
  inquire (56, action=a, read=b, write=c, readwrite=d)
  print *,a
  print *,b
  print *,c
  print *,d
 endprogram t
