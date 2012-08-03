program p
  implicit none
  real,dimension(8)::a=(/1,2,3,4,5,6,7,8/)
  print '(f3.0)',cumm_sum(a)
contains
  recursive function cumm_sum(array) result (c_sum)
    real array(:),c_sum(size(array))
    integer n
    n=size(array)
    if (n.le.1) then
      c_sum=array
    else
      n=n/2
      c_sum(:n)=cumm_sum(array(:n))
      c_sum(n+1:)=c_sum(n)+cumm_sum(array(n+1:))
    endif
  end function cumm_sum
end program p
