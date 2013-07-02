      program t
      type x
      integer,pointer::i=>null()
      end type x
      type(x)::y
      print '(l)',associated(y%i)
      end program t
      
