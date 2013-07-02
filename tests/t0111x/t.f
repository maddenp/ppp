      program t
      character(len=5)::a='hello'
      character(5)::b
      character*5::c
      character*(1+2+3)::d='seven'
      character*(*),parameter::e='eight'
      b='there'
      c='world'
      print '(a)',a
      print '(a)',b
      print '(a)',c
      print '(a)',d
      print '(a)',e
      end program t
