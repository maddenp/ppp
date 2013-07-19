      program t
      implicit none
C inquire stmt 'delim' specifier (quote)
c and character variable in open stmt
      character*10::a
      character*5::write='quote'
      open (56, status='new', file='tmpfile', delim=write)
      inquire (56, delim=a)
      print *,a
      close (56,status='delete')
      endprogram t
