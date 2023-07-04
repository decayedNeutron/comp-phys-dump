PROGRAM pde

IMPLICIT NONE

REAL*8, PARAMETER :: cnst = 0.00001d0, A = -70d0, B = -40d0, C = 20d0, D = -10d0
REAL*8, DIMENSION(34,34):: T, T2
INTEGER :: i,j,ni,k, cexit
REAL*8 :: dx, dy, corr

! initializze the temp
T = 0d0
! T(1,1) = 2000d0
dx = 1.0d0
dy = 1.0d0
! OPEN(UNIT = 11, FILE = 'TempProf2NewMul.dat', STATUS = 'new')
!
! OPEN(UNIT = 11, FILE = 'InitialProf2.dat', STATUS = 'new')
! do j = 1, 34
!     WRITE(11,*) (T(j,k), k = 1,34)
!
! end do
! CLOSE(11)
k = 0
ni = 0
T2 = T
DO
    ni = ni + 1
    ! corner
    T2(1,1) = 0.5d0*(T(1,2) - dx*C + T(2,1) - dx*A)
    T2(1,34) = 0.5d0*(T(1,33) - dy*D + T(2,34) + dx*A)
    T2(34,1) = 0.5d0*(T(33,1) + dx*B + T(34,2) - dy*C)
    T2(34,34) = 0.5d0*(T(33,34) + dx*B + T(34,33) + dy*D)
    !boundary
    DO j = 2,33
        T2(1,j) = 0.25d0*(2*T(2,j) - 2*dx*A + T(1,j+1) + T(1,j-1))
        T2(34,j) = 0.25d0*(2*T(33,j) + 2*dx*B + T(34,j+1) + T(34,j-1))
        T2(j,1) = 0.25d0*(T(j+1,1) + T(j-1,1) + 2*T(j,2) - 2*dy*C)
        T2(j,34) = 0.25d0*(T(j+1,34) + T(j-1,34) + 2*T(j,33) + 2*dy*D)
    END DO





    ! interior region
    DO j = 2,33
        DO k = 2,33

        T2(j,k) = (T(j+1,k+1) + T(j+1,k-1) + T(j-1,k+1) + T(j-1,k-1))/4.0d0

        END DO
    END DO

    corr = 2000d0 - T(1,1)
    T2 = T2 + corr
!     IF (MOD(ni,10) .eq. 0) THEN
! do j = 1, 34
!     WRITE(11,*) (T(j,k), k = 1,34)
!
! end do
! END IF
    ! exit condition
    cexit = 1
    DO j = 1, 34
        DO k = 1,34

        IF (ABS(T(j,k)-T2(j,k)) .ge. cnst) THEN
            cexit = 0


        END IF
        END DO
    END DO


    IF (cexit == 1 .or. ni >= 900000) THEN
        PRINT*,cexit, ni
        EXIT
    ELSE
        CONTINUE
    ENDIF

    T = T2

END DO


PRINT*,"Temperature at lattice site (10,10)", T(10,10)+dx, T(1,1)


CLOSE(11)





END PROGRAM pde
