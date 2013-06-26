program t
  implicit none
  integer::i=1
  call double(i)
  call triple(i)
  call quad(i)
  print '(i0)',i
contains
  pure subroutine double(i)
    integer,intent(out)::i
    i=i*2
  end subroutine double
  elemental subroutine triple(i)
    integer,intent(out)::i
    i=i*3
  end subroutine triple
  pure elemental subroutine quad(i)
    integer,intent(out)::i
    i=i*4
  end subroutine quad
end program t
