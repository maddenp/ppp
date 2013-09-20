program t
  call s
end program t

subroutine s
!sms$ignore begin
  print '(a)','hello'
!sms$ignore end
end subroutine s
