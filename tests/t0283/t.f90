program t
  implicit none
! open stmt sequential unformatted
  character*6::output
  integer::k
  integer::i
  integer::a
  open (82, file='tmpfile', status='new', access='sequential', form='unformatted')

  do k=1,5
    write (82) 'record',k
  end do
do i=1,3
  backspace (82)
enddo

read (82) output,a
print *,output,a

endprogram t
