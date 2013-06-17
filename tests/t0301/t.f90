program t
  implicit none
!inquire stmt 'number' specifier
  integer::a

  inquire (file='infile', number=a)
  print *,a

  open (95, file='infile')
  inquire (95, number=a)
  print *,a
 endprogram t

