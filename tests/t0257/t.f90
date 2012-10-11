module m
  implicit none
  public s
contains
  subroutine s
  end subroutine s
end module m

program t
  use m,only:s
end program t
