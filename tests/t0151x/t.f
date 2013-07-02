      program t
      TYPE PERSON
      INTEGER AGE
      CHARACTER (LEN = 50) NAME
      END TYPE PERSON
      TYPE(PERSON)::P
      P=PERSON (21, 'JOHN SMITH')
      print '(a,a,i0,a)',p%name(1:10),' is ',p%age,' years old'
      end program t
