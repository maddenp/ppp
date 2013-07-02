      program t
      integer::i=1
      select case (i)
      case (1)
         print '(a)','one'
         print '(i0)',i
      case default
         print '(a)','default'
         print '(i0)',i
      end select
      end program t
