      program t
      integer::i,j,k
      target::i(1)
      target j(:,:),k(4,5)
      allocatable::j
      allocate(j(2,3))
      print '(i0)',size(i,1)
      print '(i0)',size(j,1)
      print '(i0)',size(j,2)
      print '(i0)',size(k,1)
      print '(i0)',size(k,2)
      end program t
