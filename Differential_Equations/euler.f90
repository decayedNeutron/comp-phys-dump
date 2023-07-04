PROGRAM euler

IMPLICIT NONE

INTEGER :: n,i
REAL*8 :: func, dx = 0.001d0, vals, val, a, b,val2 = TAN(1.55d0)


! PRINT*, 'What is the no. steps?'
! READ*, n
a = 0.0d0
b = 1.55d0
n = NINT(b/dx)
vals = 0d0
val  =0d0
OPEN(UNIT = 9, file = "Euler.dat", Status = 'new')

do i = 1,n
    vals = vals + dx*func(val)
    val = vals
    WRITE(9,*) a + i*dx, vals
end do

CLOSE(9)

PRINT*, 'The value using Euler is ',vals
PRINT*, 'The error is', vals-val2

END PROGRAM euler

REAL*8 FUNCTION func(y)

    IMPLICIT NONE
    REAL*8 :: y

    func = 1 + y**2
END FUNCTION
