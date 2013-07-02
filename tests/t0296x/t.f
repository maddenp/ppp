      program t
      implicit none
! io-control-spec advance specifier, size specifier,
! and end of record branch input and output
      character*7::a,b,c,d
      integer::u,v
      
      open (99, status='new', file='tmpfile')
      write (99, '(a)', advance='no') 'hello'
      write (99, '(a)', advance='yes') 'world'
      write (99, '(a)') 'goodbyeworld'
      
      rewind (99)
  
      read (99, '(a5)', advance ='no', size=u) a
      read (99, '(a)', advance='yes') b
      read (99, '(a)', advance='no', size=v,eor=100) c
      read (99, '(a)', eor=100,advance='no') d
      
      print *,'no'
      
 100  if ((u .eq. 5) .and. (v .eq. 7)) then
         print *,a
         print *,b
         print*,c
         print*,d
      endif
      
      close (99,status='delete')
      
      endprogram t
      
