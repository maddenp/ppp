program t
  implicit none
!open stmt read & write action, inquire action 
  character*10::output,a,b
  integer::ierr
  integer::ierra
  
  open (95, status='new', file='tmpfile', action='write')
  write (95, '(a)') 'successful'
  read (95, '(a)', iostat=ierr) output
  if (ierr .ne. 0) then
    print *, 'test'
  endif
  inquire (95, action=a)
  close (95)

  open (95, status='old', file ='tmpfile', action='read')
  read (95, '(a)') output
  write (95, '(a)', iostat=ierra) 'evil words'
  if (ierra .ne. 0) then
    print *,output
  endif
  inquire (95, action=b)
  print *,a
  print*,b
  
  
endprogram t
