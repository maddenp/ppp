      program t
      character(len=4)::a,b
      character(len=5)::c(2)=(/'hello','world'/)
      equivalence (a,c(1)),(b,c(2))
      print *,a
      print *,b
      end program t
