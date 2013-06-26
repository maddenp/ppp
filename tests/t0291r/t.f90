program t
  implicit none
!open stmt 'no' in pad specifier
  character*10::output
  integer::ierr
  character*8::outputa
  character*2::a
  open (97, status='old', file='infile', pad='no')
  
  read(97, '(a)', iostat=ierr) output
  
  if (ierr .ne. 0) then
    read (97, '(a)') outputa
    print *,outputa
  endif

endprogram t


