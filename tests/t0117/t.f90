program p
  integer,dimension(3)::i=(/1,2,3/)
  where (i>1)
    i=i*2
  elsewhere
    i=i*3
  endwhere
  print *,i
end program p
