program t
  integer::i=2
100 x: select case (i)
200 case default
    print '(a)','default'
    print '(i0)',i
  end select x
end program t
