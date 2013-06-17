program t
  implicit none
! inquire stmt 'delim' specifier (quote)
! and character variable in open stmt
  character*10::a
  character*5::write='quote'
  open (56, status='new', file='tmpfile', delim=write)
  inquire (56, delim=a)
  print *,a
  close (56,status='delete')
   endprogram t
