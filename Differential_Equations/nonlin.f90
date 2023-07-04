PROGRAM nonlin

IMPLICIT NONE

! Initial Values
REAL*8, PARAMETER :: x0 = 0.1d0, v0 = 1.9d0

! Variables
REAL*8 :: dt, t, x, v, f
INTEGER :: n

! Iterators
INTEGER:: i, j

! Variable Initialisations
n = 5000
dt = 0.01
x = x0
v = v0
t = 0d0


OPEN(UNIT = 11, FILE = 'sp.dat', STATUS = 'new')
DO i = 1,n+1
    WRITE(11,*) x,v,t
    v = v + dt*f(x,0d0)
    x = x + dt*v
    t = t + dt


END DO

CLOSE(11)
END PROGRAM nonlin

REAL*8 FUNCTION f(x,t)

    IMPLICIT NONE
    REAL*8 :: x, t

    f = -1d0*SIN(x)
END FUNCTION
