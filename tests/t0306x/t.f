      program t
      implicit none
* inquire stmt all 'access' and 'form' specifiers (direct formatted)
C and character-variable in open statement
c and 'recl' specifier in inquire stmt
      character*11::a,b,c,x,y,z
      integer::r
      character*9::new='formatted'
      open (56, status='new', file=
     ''tmpfile', access='direct', form=new, recl=1)
      inquire (56, access=a, sequential=b
     x, direct=c, form=x, fo
     !rmatted=y, unformatted=z, recl=r)
      
      print *,a
      print *,b
      print *,c
      print *,x
      print *,y
      print *,z
      print *,r
      
      close (56,status='delete')
      
      endprogram t

