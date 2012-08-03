program p
  call s(*100)
  print '(a)','bad'
  goto 200
100 print '(a)','good'
200 continue
contains
  subroutine s(*)
    return 1
  end subroutine s
end program p
