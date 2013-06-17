module m
  implicit none
  integer::a=11,b=22,c=33
  namelist /n/ a,b,c
end module m

program p
  use m,only:n,a,b,c
  implicit none
  open (88,file='infile',status='old',err=100)
  read (88,nml=n)
  print *,a
  print *,b
  print *,c
  close (88)
  stop
100 print *,'error'
end program p
