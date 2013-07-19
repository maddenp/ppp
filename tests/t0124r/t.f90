program t
  character(len=5)::a='lemon'
  character(len=5),parameter::b='melon'
  select case (a)
  case (:b)
    print '(a,a,a)',a,' comes before ',b
  case default
    print '(a,a,a)',a,' comes after ',b
  end select
end program t
