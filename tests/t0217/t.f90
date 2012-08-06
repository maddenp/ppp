program p
  call a
end program p

subroutine s
  print '(a)','s'
  return
  entry a
  print '(a)','a'
end subroutine s
