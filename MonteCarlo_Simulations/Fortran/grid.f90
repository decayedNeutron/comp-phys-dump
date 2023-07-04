PROGRAM grid

IMPLICIT NONE

INTEGER :: i,j,k, t
REAL*8 :: r(10,10,10)
CHARACTER(LEN=50) :: filen,fn2,tt


! DO t = 1,1000
WRITE(filen, '(A7,I1)') 'R5/GRIS_', 0
WRITE(fn2, '(A8,I1)') 'R5/GRID3_', 0
WRITE(tt,*) t
CALL RANDOM_NUMBER(r)

OPEN(UNIT = 10, file = trim(filen)//trim(adjustl(tt))//".dat", Status = 'new')

OPEN(unit=11, file = trim(fn2)//trim(adjustl(tt))//".dat", Status = 'new')

DO i = 1,10
DO j = 1,10
DO k = 1,10
IF (r(i,j,k) >= 0.0d0) THEN
WRITE(10,'(3I4)') i,j,k
ELSE
WRITE(11,'(3I4)') i,j,k
END IF
END DO
END DO
END DO
CLOSE(10)
CLOSE(11)
! END DO


END PROGRAM grid
