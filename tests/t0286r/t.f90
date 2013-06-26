program t
  implicit none
! open stmt 'zero' in the blank specifier
integer::output
  open (87, status='old', file='infile', blank='zero')
  read (87, '(i10)') output
  print *,output

  read (87, '(i10)') output
  print *,output
endprogram t
