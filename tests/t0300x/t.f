      program t
      implicit none
! inquire stmt 'opened' specifier
           !and character variable in open specifier
      logical::I1=.true.
      character*6::name='infile'
      inquire (file='infile', opened=I1)
      print *,I1
      
      open (95, file=name)
      inquire (95, opened =I1)
      print *,I1
      endprogram t

