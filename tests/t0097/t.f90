program p
  x: do i=1,2
    print *,'i'
    do j=1,2
      print *,'j'
      exit x
    enddo
    exit
  end do x
end program p
