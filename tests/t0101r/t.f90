program t
  x: do i=1,2
    print *,'i1'
    y: do j=1,2
      print *,'j1'
      cycle x
      print *,'j2'
    enddo y
    print *,'i2'
  end do x
end program t
