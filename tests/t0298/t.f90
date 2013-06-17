program t1
  implicit none
! io-control-spec format specifier
  integer::a
  real::b
  character*4::c
  logical::d
  open (90, file='tmpfile', status='new')
  write (90, '(i5)') 1043
  write (90, '(f9.6)') 3.141592654
  write (90, '(a7)') 'hello'
  write (90, '(l)') .true.
  rewind (90)
  read (90, '(i6)') a
  print *,a
  read (90, '(f5.1)') b
  print *,b
  read (90, '(a4)') c
  print *,c
  read (90, '(l)') d
  print *,d
endprogram t1

