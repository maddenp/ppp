program t
  character(len=6)::a_b(3)
  data a_b/'hello ', 5 Hw&
    &orld,1h!/
  print *,a_b ! comment
100 format (" hello ", 5 Hw&
      &orld, 2 h !)
  print 100
  call s( 1 &
    & h;)
contains
  subroutine s(c)
    character,intent(in)::c
    print *,c
  end subroutine s
end program t
