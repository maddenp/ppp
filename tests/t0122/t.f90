program p
  integer::i
  do i=0,5
    print '(i0)',i
    select case (i)
    case (:1)
      print '(a)','less than two'
    case (2:3)
      print '(a)','two or three'
    case (4:)
      print '(a)','more than three'
    end select
  end do
end program p
