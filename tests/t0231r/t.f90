program t
INTERFACE SWITCH
SUBROUTINE INT_SWITCH (X, Y)
INTEGER, INTENT (INOUT) :: X, Y
END SUBROUTINE INT_SWITCH
SUBROUTINE REAL_SWITCH (X, Y)
REAL, INTENT (INOUT) :: X, Y
END SUBROUTINE REAL_SWITCH
SUBROUTINE COMPLEX_SWITCH (X, Y)
COMPLEX, INTENT (INOUT) :: X, Y
END SUBROUTINE COMPLEX_SWITCH
END INTERFACE SWITCH
PRINT *,'ok'
end program t
