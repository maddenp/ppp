program t1
  implicit none
! inquire stmt 'pad' specifier (no)
! and character variable in open stmt
  character*3::a, yes='no'
  open (56, status='new', file='tmpfile', pad=yes)
  inquire (56, pad=a)
  print *,a
   endprogram t1
