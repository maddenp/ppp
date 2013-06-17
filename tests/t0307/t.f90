program t1
  implicit none
! inquire stmt 'nextrec' specifier
  integer::a
  open (56, status='new', file='tmpfile', access='direct', recl=8)
  write (56, rec=4) 'record d'
  inquire (56, nextrec=a)
  print *,a
 endprogram t1

