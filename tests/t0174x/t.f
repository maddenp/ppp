      program t
      integer::to=6
      print '(i0)',to
      call mvbits(topos=0,frompos=2,from=7,to=to,len=2)
      print '(i0)',to
      end program t
