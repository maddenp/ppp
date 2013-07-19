program t
  integer::i(2)
  integer,parameter::j=1
  i(1)=1
  i(j+1)=2
  print '(i0)',i(1)
  print '(i0)',i(2)
end program t
