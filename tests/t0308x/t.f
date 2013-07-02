      program t
      implicit none
          ! inquire stmt 'blank specifier' (null)
! and character variable in open stmt
      character*4::a, open='null'
      open (56, status='new', file='tmpfile', blank=open)
      inquire (56, blank=a)
      print *,a
      close (56,status='delete')
      endprogram t

