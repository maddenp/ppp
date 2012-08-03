program p
  integer::k=-1
  call s(2,k)
  print '(i0)',k
contains
  subroutine s(j,k)
    integer,intent(in)::j
    integer,intent(out)::k
    integer::i=1
    print '(i0)',i+j
    k=77
  end subroutine s
end program p
