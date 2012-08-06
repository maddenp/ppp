program p
  implicit none
  integer::s,a
  print '(i0)',s()
  print '(i0)',a()
end program p

integer function s() result(r)
  implicit none
  integer::r
  r=0
  return
  entry a() result(r)
  r=1
end function s
