      program t
      implicit none
! inquire stmt 'blank' specifier (zero)
! and character variable in open stmt
      character*4::a, endprogram='zero'
      open (56, status='new', file='tmpfile', blank=end
     @program)
      inquire (56, blank=a)
      print *,a
      close (56,status='delete')
      endprogram t

