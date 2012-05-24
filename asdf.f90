subroutine q
  implicit none
  integer::i=5
  print *,i
end

PROGRAM asdf
  !SMS$ignore begin "CAPS"
  ! comment 1
  print *, & ! comment 2
'hello world 1'
  print *,'heLLo "x" worLd 2'     
!sMs$ignore end
  print *,'heLLo ''x'' worLd 3' ! comment 3
  PRINT *,"heLLo 'x' worLd 4"
  PRINT *,"heLLo ""x"" worLd 5"
  PRINT *,"heLLo ""x"" &
worLd 6"
  PRINT *,"heLLo ""x"" &

! comment 4
   worLd 7"
  PRINT *,"heLLo ""x"" &
    & worLd 8"
  print *,'hello' ;;;;; print *,'world 9'
  print *,'hello' ; &
    print *,'world 0'
  call Q
end prOGRam ASDF
