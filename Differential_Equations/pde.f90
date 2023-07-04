PROGRAM pde

IMPLICIT NONE

REAL*8 :: constraint = 0.0001d0
REAL*8, DIMENSION(34,34):: T, T2
INTEGER :: i,j,ni,k, c
! initializze the temp

DO i = 1,34
    DO j = 1,34
        T(i,j) = 0.0d0
    END DO
END DO

DO i = 1,34

    T(1,i) = 3.7d0
    T(34,i) = 0.4d0
    T(i,1) = 3.7d0 - ((3.7d0 - 0.4d0)/(33))*(i-1)
    T(i,34) = 3.7d0 - ((3.7d0 - 0.4d0)/(33))*(i-1)

END DO

! OPEN(UNIT = 11, FILE = 'InitialProf.dat', STATUS = 'new')
! do j = 1, 34
!     WRITE(11,*) (T(j,k), k = 1,34)
!
! end do
! CLOSE(11)

ni = 0
T2 = T
DO
    ni = ni + 1



    DO j = 2,33
        DO k = 2,33

        T2(j,k) = (T(j+1,k+1) + T(j+1,k-1) + T(j-1,k+1) + T(j-1,k-1))/4.0d0

        END DO
    END DO

    ! exit condition
    c = 1
    DO j = 1, 34
        DO k = 1,34

        IF (ABS(T(j,k)-T2(j,k)) .ge. constraint) THEN
            c = 0


        END IF
        END DO
    END DO

    IF (c == 1 .or. ni >= 5000000) THEN
        PRINT*,c, ni
        EXIT
    ELSE
        CONTINUE
    ENDIF

    T = T2

END DO

PRINT*,"Temperature at lattice site (20,20)", T2(20,20)
!
! OPEN(UNIT = 11, FILE = 'TempProf.dat', STATUS = 'new')
! do j = 1, 34
!     WRITE(11,*) (T(j,k), k = 1,34)
!
! end do
! CLOSE(11)





END PROGRAM pde
