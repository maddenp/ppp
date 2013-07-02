      program t
      implicit none  
    !     open stmt append position 
      character*8::output
      open (95, status='old', file='infile', position='append')
      backspace 95
      read (95, '(a)') output
      print *,output
      endprogram t
      
