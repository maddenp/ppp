program t
INTERFACE
SUBROUTINE EXT1 (X, Y, Z)
REAL, DIMENSION (100, 100) :: X, Y, Z
END SUBROUTINE EXT1
SUBROUTINE EXT2 (X, Z)
REAL X
COMPLEX (KIND = 4) Z (2000)
END SUBROUTINE EXT2
FUNCTION EXT3 (P, Q)
LOGICAL EXT3
INTEGER P (1000)
LOGICAL Q (1000)
END FUNCTION EXT3
END INTERFACE
PRINT *,'ok'
end program t
