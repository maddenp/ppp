program p
  call s(2)
contains
  subroutine s(j)
    integer,intent(in)::j
    integer::i=1
    print '(i0)',i+j
  end subroutine s
end program p
