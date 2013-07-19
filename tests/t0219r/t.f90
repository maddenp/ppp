program t
  implicit none
  integer::s,a
  print '(i0)',s()
  print '(i0)',a()
end program t

integer function s() result(r)
  implicit none
  r=0
  return
  entry a() result(r)
  r=1
end function s
