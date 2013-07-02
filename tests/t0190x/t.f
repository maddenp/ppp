      module a
      end
      
      module b
      end module
      
      module c
      end module c
      
      module d
      integer::i=3
      logical::l
      real::r
      end module d
      
      program t
      use d
      print *,i
      end program t
