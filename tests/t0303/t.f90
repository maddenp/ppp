program t1
  implicit none
! inquire stmt all 'access' and 'form' specifiers (sequential formatted)
! and character-variables in open specifiers
  character*11::a,b,c,x,y,z
  character*10::access='sequential'
  open (56, status='new', file='tmpfile', access=access, form='formatted')
  inquire (56, access=a, sequential=b, direct=c, form=x, formatted=y, unformatted=z)

  print *,a
  print *,b
  print *,c
  print *,x
  print *,y
  print *,z
 endprogram t1

