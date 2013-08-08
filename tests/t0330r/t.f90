program t
  character(len=3)::c(5)
  DATA c/'He',3hllo,' Wo',3Hrld,'!'/
  write (*,'(a,a,a,a,a)') trim(c(1)),c(2),c(3),c(4),c(5)
end program t
