      program t
      character*5::a='hello'
      character*5::b='world'
 1234 format(2(3X,a3))
      write (*,1234) a,b
      endprogram t
