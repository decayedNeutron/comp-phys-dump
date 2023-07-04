PROGRAM RK4

IMPLICIT NONE

! Initial and Final Values
REAL*8, PARAMETER :: A = 0.0d0 , B = 1.550d0, val = TAN(1.55d0)

! Variables
REAL*8 :: x, dx, y, k1, k2, k3, k4, f
INTEGER :: n

! Iterators
INTEGER :: i, j

! Variable Initializations
dx = 0.001d0
n = NINT((B-A)/dx)
y = 0d0
x = 0d0

! Open the file to write data

OPEN(UNIT = 10, FILE = 'R.dat', STATUS = 'new')
DO i = 1,n

    k1 = f(A,y)
    k2 = f(A,y + 0.5d0*dx*k1)
    k3 = f(A,y + 0.5d0*dx*k2)
    k4 = f(A,y + dx*k3)

    y = y + dx*(k1 + 2*k2 + 2*k3 + k4)/real(6)
    x = x + dx
    WRITE(10,*) x, y
END DO
CLOSE(10)

PRINT*, 'The value using RK4 is ',y
PRINT*, 'The error is', y-val

END PROGRAM RK4



REAL*8 FUNCTION f(x,y)

    IMPLICIT NONE
    REAL*8 :: x, y

    f = 1 + y**2

END FUNCTION
