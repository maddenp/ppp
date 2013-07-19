program t
  call s(1,2,3)
end program t

subroutine s(a,&
  b,&
! comment
  c)
  integer,intent(in)::a,b,c
  print '(a)','hello &
world'
end subroutine s
