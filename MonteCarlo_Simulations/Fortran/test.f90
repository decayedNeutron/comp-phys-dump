PROGRAM test


IMPLICIT NONE

INTEGER:: S(10,10,10), n(3), L

L = 11

CALL r(S,n)

print*, n(1)

CONTAINS
    SUBROUTINE r(S,n)

    IMPLICIT none

    INTEGER, DIMENSION(:,:,:), INTENT(IN):: S
    INTEGER, DIMENSION(:), INTENT(OUT):: n

    n = shape(S)

    END SUBROUTINE r


END PROGRAM test


