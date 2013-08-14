program t
! test for i, f, t, tl, tr, and l in format-item-list
  integer::a=1234
  real::b=3.141592654
  logical::c=.true.
9876 format(T17,I5,I2.1,tl14,F6.3,tr25,L4)
  write (*,9876) a,a,b,c
endprogram t
