program t
  write (*,'(i0)') 1
!sms$remove begin
  write (*,'(i0)') 2
!sms$remove end
  write (*,'(i0)') 3
!sms$remove begin
  write (*,'(i0)') 4
  write (*,'(i0)') 5
!sms$remove end
  write (*,'(i0)') 6
end program t
