c test F90:R830/R831 nonblock-do-construct
      program t
      integer::i,j,k
      do 88 i=1,2
      do 88 j=1,2
      do 88 k=1,2
      print *,i
 88   continue
      end
