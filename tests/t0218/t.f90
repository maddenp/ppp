program p
  implicit none
  integer::s,a
  print '(i0)',s()
  print '(i0)',a()
end program p

integer function s()
  implicit none
  integer::a
  s=0
  return
  entry a
  s=1
end function s
