program p
  x: do i=1,2
    print *,'i'
    cycle
    y: do j=1,2
      print *,'j1'
      print *,'j2'
    enddo y
  end do x
end program p
