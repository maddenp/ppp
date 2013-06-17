program t
  TYPE LINE
    REAL, DIMENSION (2, 2) :: COORD   ! X1, Y1, X2, Y2
    REAL                   :: WIDTH   ! Line width in centimeters
    INTEGER                :: PATTERN ! 1 for solid, 2 for dash, 3 for dot
  END TYPE LINE
end program t
