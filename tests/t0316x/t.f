      program t
      implicit none
c inquire stmt 'delim' specifier (none)
c and character-variable in open stmt
      character*10::a
      character*4::inquire='none'
      open (56, status='new', file='tmpfile', delim=inquire)
      inquire (56, delim=a)
      print *,a
      close (56,status='delete')
      endprogram t
