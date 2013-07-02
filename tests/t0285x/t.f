      program t
      implicit none
C     open stmt null (default) in the blank specifier
      integer::output
      open (87, status='old', file='infile', blank='null')
      read (87, '(i10)') output
      print *,output
      
      read (87, '(i10)') output
      print *,output
      endprogram t
