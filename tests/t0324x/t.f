c test F90:R830/R831 nonblock-do-construct
      program p                                                         
      integer::i,j                                                      
      do 1 j=1,2                                                        
      do 1 i=1,2                                                        
      print *,1                                                         
    1 continue                                                          
      end                                                               