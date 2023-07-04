PROGRAM particles

IMPLICIT NONE

!Iterators
INTEGER :: i,j

!Constants
REAL*8, PARAMETER :: k = 1.0d0, PI = 4*ATAN(1.0d0)
! Variable Declarations
REAL*8, DIMENSION(50) :: x, v, x_t, v_t, x_rk, v_rk
REAL*8 :: dt, t
INTEGER :: n, g, h
! Initial Values
INTEGER, DIMENSION(50) :: x_0i = (/ (i .eq. 1 .or. i .eq. 26, i = 1, 50) /)
REAL*8, DIMENSION(50) ::v_0 = (/(0 , i = 1,50)/), m = (/(1.0d0, i = 1,50)/), x_0
                        x_0 = x_0i*0.8
x = x_0
v = v_0
n = 200000
dt = 0.0002
t = 0

! OPEN(UNIT = 11, FILE = 'Particles22.dat', STATUS = 'new')


DO j = 1, n
! WRITE(11,*) 50
! WRITE(11,*) ""
 WRITE(*,*) t, x(1), x(2), x(26), x(30)
    DO i = 1,50
        !WRITE(11,*) 'N', 5*sin(2*PI*i/50), x(i), 5*cos(2*PI*i/50)

        g = MOD(i,50)+1
        h = MOD(i-1,50)
        if (h==0) h = 50
        ! place values of k11,k21
        x_rk(i) = v(i)
        v_rk(i) = (x(g) + x(h) - 2*x(i))

        ! calculate k12, k22 using above
        x_t(i) = x(i) + v(i)*dt*0.5d0
        v_t(i) = v(i) + (x_t(g) + x_t(h) - 2*x_t(i))*0.5d0*dt

        !placing k*2's with factor of 2 into rk slopes
        x_rk(i) =  x_rk(i) + 2*v_t(i)
        v_rk(i) = v_rk(i) + 2*(x_t(g) + x_t(h) - 2*x_t(i))

        ! calculate k13,k23 using above
        x_t(i) = x(i) + v(i)*dt*0.5d0
        v_t(i) = v(i) + (x_t(g) + x_t(h) -  2*x_t(i))*0.5d0*dt

        !placing k*3's with factor of 2 into rk slopes
        x_rk(i) =  x_rk(i) + 2*v_t(i)
        v_rk(i) = v_rk(i) + 2*(x_t(g) + x_t(h) - 2*x_t(i))

        ! calculate k14, k24 and place values into RK slopes
        x_t(i) = x(i) + v(i)*dt
        v_t(i) = v(i) + (x_t(g) + x_t(h) - 2*x_t(i))*dt
        x_rk(i) =  x_rk(i) + v_t(i)
        v_rk(i) = v_rk(i) + (x_t(g) + x_t(h) - 2*x_t(i))

        ! update the values
        v(i) = v(i) + v_rk(i)*dt/real(6)
        x(i) = x(i) + x_rk(i)*dt/real(6)
    END DO

    t = t + dt

END DO
! CLOSE(11)
END PROGRAM particles
