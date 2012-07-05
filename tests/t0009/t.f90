program t
  print *,1
!sms$remove begin
  print *,2
!sms$remove end
  print *,3
!sms$remove begin
  print *,4
  print *,5
!sms$remove end
  print *,6
end program t
