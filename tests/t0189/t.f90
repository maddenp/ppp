program p
  common /x/ i,j,k(3)
  do i=i,j
    print '(i0)',k(i)
  enddo
end program p

block data work
  common /x/ i,j,k(3)
  data i/1/,j/3/,k/7,8,9/
end block data
