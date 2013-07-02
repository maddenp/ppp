      program t
      implicit none
      integer::i,j
      j=-1
      i=f(j)
      print '(i0)',i
      print '(i0)',j
      contains
      integer function f(i)
      integer,intent(inout)::i
      print '(i0)',i
      if (i.lt.0) then
         i=999
         f=i
         return
      endif
      f=i+1
      end function f
      end program t
