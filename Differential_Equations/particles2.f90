PROGRAM particles

IMPLICIT NONE

!Iterators
INTEGER :: i,j

!Constants
REAL*8, PARAMETER :: k = 1.0d0
! Variable Declarations
REAL*8, DIMENSION(50) :: x, v
REAL*8 :: dt, t
INTEGER :: n, h, g
! Initial Values
INTEGER, DIMENSION(50) :: x_0i = (/ (i .eq. 1 .or. i .eq. 26, i = 1, 50) /)
REAL*8, DIMENSION(50) ::v_0 = (/(0 , i = 1,50)/), m = (/(1.0d0, i = 1,50)/), x_0
                        x_0 = x_0i*0.8
x = x_0
v = v_0
n = 200000
dt = 0.0002
t = 0

! OPEN(UNIT = 11, FILE = 'Particle.dat', STATUS = 'new')
DO j = 1, n
! !
    DO i = 1,50
        g = MOD(i,50)+1
        h = MOD(i-1,50)
        if (h==0) h = 50

v(i) = v(i) + dt*(x(g)+x(h)-2*x(i))

    END DO
        x = x + dt*v
        t = t + dt
WRITE(11,*) t, x(1), x(2), x(3), x(50), x(49), x(48)
END DO
PRINT*, 'The position of 1st Particle at t = 40s is:', x(1)
! CLOSE(11)
END PROGRAM particles








