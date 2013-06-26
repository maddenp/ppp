program t
  integer::i,j
  open(unit=77,file='infile',status='old')
  read (77,'(i2,i2)') i,j
  write (*,'(i0,a,i0)') i,' ',j
  close(77)
end program t
