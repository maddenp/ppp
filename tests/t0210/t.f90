program t
  ! comment 1
  print '(a)', & ! comment 2
'hello world 1'
  print '(a)','heLLo "x" worLd 2'     
  print '(a)','heLLo ''x'' worLd 3' ! comment 3
7 PRINT '(a)',"heLLo 'x' worLd 4" ! don't continue &
  PRINT '(a)',"heLLo ""x"" worLd 5"
  8 PRINT '(a)',"heLLo ""x"" &
worLd 6"
  PRINT '(a)',"heLLo ""x"" &

! comment 4
   worLd 7"
  PRINT '(a)',"heLLo ""x"" &
    &     worLd 8"
  print '(a)','hello' ;;

  continue ;;; print '(a)','world 9'
  print '(a)','hello' ; &
    print '(a)','world 0'
end program t
