      program t
      integer::a=1234
      real::b=3.141592654
      logical::c=.true.
 9876 format(T17,I5,tl14,F6.3,tr25,L4)
      write (*,9876) a,b,c
      endprogram t
      
