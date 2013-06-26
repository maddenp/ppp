program t
  implicit none
!open stmt scratch status, check for existence then nonexistence 
  logical::I1=.false.
  integer::ierr
character::out
  
  open (unit=345, status='scratch')
  write (345, '(a)') 'bad'
  inquire (345, exist=I1)
  close (345)

  read (345, '(a)', iostat=ierr) out
  
  if ((ierr .ne. 0) .and. (I1 .eqv. .true.) .and. (out .ne. 'bad')) then
    print *,'we have'
    print *,'a winner'
  endif
  
  open (345, status='old')
  close (345, status='delete')
  
endprogram t

