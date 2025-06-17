program t
  implicit none
! io-control-spec namelist specifier
  integer::i
  integer::w=1000
  real::x=3.141592654
  integer::y(3)
  character*10::z='blast off!'
  namelist /a/ w,x,y,z
  integer::u=3
  character*4::v='test'
  namelist /b/ u,v
  y= (/1,2,3/)
  open (95, file='tmpfile', status='new')
  write (95, nml=a)
  write (95, nml=b)
  rewind(95)
  y = (/-1,-1,-1/)
  w=-1
  z='failure'
  x=-1.00000
  read (95, nml=a)
  do i=1,3
    print *,y(4-i)
  enddo
  print *,z
  close (95,status='delete')
endprogram t

