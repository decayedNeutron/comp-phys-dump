PROGRAM euler_mod

IMPLICIT NONE

INTEGER :: n,i
REAL*8 :: func, dx = 0.001d0, y, x, y_t , a, b, val2 = TAN(1.55d0)

! y == y_i
! y_t == temporary calucultion point


! PRINT*, 'What is the no. steps?'
! READ*, n

!initial value of x
a = 0.0d0
!final value of x
b = 1.55d0
n = NINT(b/dx)

y  = 0d0
OPEN(UNIT = 9, file = "Euler_mod.dat", Status = 'new')

do i = 1,n
    y_t = y + 0.05*dx*func(y)
    y = y + dx*(func(y_t))

    WRITE(9,*) a + i*dx, y
end do

CLOSE(9)


PRINT*, 'The value using Modified Euler is ',y
PRINT*, 'The error is', y-val2

END PROGRAM euler_mod

REAL*8 FUNCTION func(y)

    IMPLICIT NONE
    REAL*8 :: y

    func = 1 + y**2
END FUNCTION
