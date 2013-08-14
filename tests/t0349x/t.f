      program t
c test for nested do loops
      integer::i,j,k,l
      do 77 i=1,2
      do 88 j=1,2
      do 88 k=1,2
      do 88 l=1,2
      print *,1
 88   continue
 77   continue
      end program t
