      program t
      integer a
      integer :: b
      integer c,d
      integer::e,f
      integer(kind=4) g
      integer(kind=8)::h,i
      integer::j=0
      integer::k=1,l=2+3
      integer,pointer::m
      integer,target::n
      integer,external::o
      integer(kind=4),parameter::q=4
      integer(kind=8),allocatable,dimension(:,:)::r
      a=-1
      b=-2
      c=-3
      d=-4*1
      e=a*b
      f=c**e
      g=a-b*c
      print '(i0)',a
      print '(i0)',b
      print '(i0)',c
      print '(i0)',d
      print '(i0)',e
      print '(i0)',f
      print '(i0)',g
      print '(i0)',j
      print '(i0)',k
      print '(i0)',l
      print '(i0)',q
      end program t
