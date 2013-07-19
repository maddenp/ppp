program t
  integer::c(6)
  character*6::d(6)
  DATA c/1h1,6HhElLo ,1HW,4horld,2h !,1h /
  DATA d/1h1,6HhElLo ,1HW,4horld,2h !,1h /
  print *,c(6),c(5),c(4),c(3),c(2),c(1)
  print *,d(6),d(5),d(4),d(3),d(2),d(1)
end program t
