      program t
* test for s, sp, and ss in format-item-list
      integer::a=1234
 1234 format (i5,sp,i5,ss,i5,s,i5)
      write (*,1234) a,a,a,a
      endprogram t
