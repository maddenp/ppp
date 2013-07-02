      program t
      implicit none
! open stmt 'yes' (default) in pad specifier
      character*10::output
      open (97, status='old', file='infile', pad='yes')
      
      read(97, '(a)') output
      
      print *,output
      endprogram t

  
