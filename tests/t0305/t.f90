program t1
  implicit none
  character*11::a,b,c,x,y,z
  integer::r
  character*6::end='direct'
  open (56, status='new', file='tmpfile', access=end, form='unformatted', recl=8)
  inquire (56, access=a, sequential=b, direct=c, form=x, formatted=y, unformatted=z, recl=r)

  print *,a
  print *,b
  print *,c
  print *,x
  print *,y
  print *,z
  print *,r
 endprogram t1

