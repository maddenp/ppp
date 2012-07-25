program t
  integer,dimension(3)::i=(/1,2,3/)
  where (i>1) i=i*2
  print *,i
end program t
