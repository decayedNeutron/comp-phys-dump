PROGRAM euler_im

IMPLICIT NONE

INTEGER :: n,i,k
REAL*8 :: func, dx = 0.1d0, y, x, y_t , a, b
CHARACTER(LEN=50) :: filen
REAL*8, PARAMETER :: val = TAN(1.5500d0)
! y == y_i
! y_t == temporary calucultion point (not same as Modified one)


! PRINT*, 'What is the no. steps?'
! READ*, n

!initial value of x
a = 0.0d0
!final value of x
b = 1.55d0


do k = 1,6
WRITE(filen, '(A8,I1)') 'LEuler_im', k
y  = 0d0
dx = 0.1d0/(10**k)
n = NINT(b/dx)

OPEN(UNIT = 9, file = trim(filen)//".dat", Status = 'new')

do i = 1,n
    y_t = y + dx*func(y)
    y = y + dx*(func(y_t)+func(y))/2d0

IF (MOD(i,(n/155)) .eq. 0) WRITE(9,*) a + i*dx, y
end do

CLOSE(9)

WRITE(*,'(A30,f10.9,A4,f25.20)') 'The value of integral for dx = ', dx, "is", y
PRINT*, 'With an error of',(val-y)
end do

END PROGRAM euler_im

REAL*8 FUNCTION func(y)

    IMPLICIT NONE
    REAL*8 :: y

    func = 1 + y**2
END FUNCTION
