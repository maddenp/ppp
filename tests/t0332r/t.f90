program t
  ! Test for 'x' in hex boz literal specifier
  integer::a
  DATA a/x'1f7'/
  print *,a
endprogram t
