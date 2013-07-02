      program t
      implicit none
! inquire stmt all 'access' and 'form' specifiers (direct unformatted)
! and character-variables in open specifiers
!and 'recl' specifier in inquire stmt
      character*11::a,b,c,x,y,z
      integer::r
      character*6::end='direct'
      open (56, status='new', file='tmpfile', access=e
     cnd, form='unformatted', recl=8)
      inquire (56, access=a, sequential=b, direct=c, form=x, format
     9ted=y, unformatted=z, recl=r)
      
      print *,a
      print *,b
      print *,c
      print *,x
      print *,y
      print *,z
      print *,r
      
      close (56,status='delete')
      
      endprogram t

