program t1
  implicit none
  integer::i
  namelist /a/ w,x,y,z
  integer::w=1000
  real::x=3.141592654
  integer::y(3)
  character*10::z='blast off!'
  namelist /b/ s,t
  integer::s=3
  character*4::t='test'
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
endprogram t1

