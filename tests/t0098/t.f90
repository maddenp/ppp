program t
  x: do i=1,2
    print *,'i'
    y: do j=1,2
      exit y
      print *,'j'
    enddo y
  end do x
end program t
