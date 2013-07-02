      program t
      
! from Tom Henderson
      
      implicit none
      character (len=100) :: lawyers
      integer :: x, y, zzz
      
      x = 2
      y = 1
      zzz = x + 
     ay
      print *, 'zzz = ',zzz
      zz
     az = x * 
     ay
       print *, 'zzz = ',zzz
      zzz = x -
     ay
      print *, 'zzz = ',zzz
      zzz = x -
     ay
      print *, 'zzz = ',zzz
      
      lawyers = 'Jones & Clay & 
     aDavis'
      print *,'LAWYERS_1 = <',trim(lawyers),'>'
      
      lawyers = 'Jones! &! Clay! &! 
     aDavis!'
      print *,'LAWYERS_2 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & Clay & 
      
     aDavis'
      print *,'LAWYERS_4 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & Clay & 
     aDavis
     a'
      print *,'LAWYERS_5 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & ''Clay'' & 
     aDavis'
      print *,'LAWYERS_6 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & ""Clay"" & 
     aDavis'
      print *,'LAWYERS_7 = <',trim(lawyers),'>'
      
      lawyers = "Jones & ""Clay"" & 
     aDavis"
      print *,'LAWYERS_8 = <',trim(lawyers),'>'
      
      lawyers = "Jones & ''Clay'' & 
     aDavis"
      print *,'LAWYERS_9 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & Clay & 
     a 
     aDavis'
      print *,'LAWYERS_10 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & Clay & 
     a
     aDavis'
      print *,'LAWYERS_11 = <',trim(lawyers),'>'
      
      lawyers =  ! a comment
     a'Jones & Clay & Da
     avis'
      print *,'LAWYERS_12 = <',trim(lawyers),'>'
      
      lawyers = 'Jones & Clay & Davis'
      print *,'LAWYERS_13 = <<',trim(lawyers),">
     a>"
      
      end program t
      
