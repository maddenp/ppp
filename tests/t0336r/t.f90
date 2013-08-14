program t
! test for b in format-item-list
  real::a=3.1415
  integer::b=127
1234 format (b32,b9.3)
  write (*,1234) a,b
endprogram t
