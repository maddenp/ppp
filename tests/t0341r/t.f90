program t
! test for bn and bz in format-item-list
  integer::a,b
1234 format (bz,i5,bn,i5)
  open(95, file='infile',status='old')
  read(95,1234) a,b
  write (*,1234) a,b
endprogram t
