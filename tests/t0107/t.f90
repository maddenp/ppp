program t
  do 100 i=1,2
    print '(i0)',0
    do 200 j=1,2
      print '(i0)',1
200 end do
    do 100 k=1,2
      print '(i0)',2
100   print '(i0)',3
    end program t
