program p
  logical::x=.true.
  select case (x)
  case (.true.)
    print '(a)','true!'
  case default
    print '(a)','false!'
  end select
end program p
