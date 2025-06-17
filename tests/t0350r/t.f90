program t
! test for continued holleriths in call, data, and format statements
  character(len=6)::a_b(3)
  character(len=3)::c
  data a_b/'Hello ', 5 HW&
    &orld,1h!/
  data c/5H!!!!!/
  print *,a_b ! comment
100 format (" Hello ", 5 HW&
      &orld, 2 h !)
  print 100
  call s( 1 &
    & h;)
  print *,c
end program t

subroutine s(c)
  integer,intent(in)::c
  print *,c
end subroutine s
