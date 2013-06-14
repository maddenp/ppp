program t1
  implicit none
  logical::I1=.true.
character*7::a
  inquire (95, named=I1)
  print *,I1

  open (95, file='tmpfile')
  inquire (95, named=I1, name=a)
  print *,I1
  print *,a
 endprogram t1

