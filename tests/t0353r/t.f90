program p
  character(len=6)::c(1)
  c(1)='123456'
  print *,c(1)(1:3)
end program p
