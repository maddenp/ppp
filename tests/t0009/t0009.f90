program t0009
  print *,1234
!sms$remove begin
  print *,5678
!sms$remove end
  print *,1234
!sms$remove begin
  print *,5678
  print *,5678
!sms$remove end
  print *,1234
end program t0009
