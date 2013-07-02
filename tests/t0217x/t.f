      program t
      call a
      end program t
      
      subroutine s
      print '(a)','s'
      return
      entry a
      print '(a)','a'
      end subroutine s
