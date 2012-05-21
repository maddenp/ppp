PROGRAM asdf
  !SMS$ignore begin "CAPS"
  ! comment 1
  print *, & ! comment 2
'hello world'
  print *,'heLLo "x" worLd'     
!sMs$ignore end
  print *,'heLLo ''x'' worLd' ! comment 3
  PRINT *,"heLLo 'x' worLd"
  PRINT *,"heLLo ""x"" worLd"
  PRINT *,"heLLo ""x"" &
worLd"
  PRINT *,"heLLo ""x"" &
   worLd"
  PRINT *,"heLLo ""x"" &
    & worLd"
end prOGRam ASDF
