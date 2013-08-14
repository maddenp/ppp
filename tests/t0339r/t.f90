program t
! test for : in format-item-list
  integer::a=12345
1234 format (1x, i5/)
  5678 format (1x, i5,:/)
  write(*,1234) a
  write(*,5678) a
endprogram t
