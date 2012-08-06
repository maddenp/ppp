program p
  implicit none
  integer::s,a
  print '(i0)',s()
  print '(i0)',a()
end program p

integer function s()
  implicit none
  s=0
  return
  entry a() result(s)
  s=1
end function s
