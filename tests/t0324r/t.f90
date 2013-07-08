! test F90:R830/R831 nonblock-do-construct
program t
  integer::i,j
  do 1 i=1,2
    do 1 j=1,2
      print *,i
1 continue
end program t
