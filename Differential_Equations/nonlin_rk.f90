PROGRAM nonlin_rk

IMPLICIT NONE

! Initial Values
REAL*8, PARAMETER :: x0 = 2d0, v0 = 1.9d0

! Variables
REAL*8 :: dt, t, x, v, x_rk, v_rk, x_t, v_t, f
INTEGER :: n

! Iterators
INTEGER:: i, j

! Variable Initialisations
n = 5000
dt = 0.01
x = x0
v = v0
t = 0d0

OPEN(UNIT = 11, FILE = 'Pendulum_fast2.dat', STATUS = 'new')


DO i = 1,n+1
    ! method choosen WRITE then UPDATE (gives initial value into data)
    WRITE(11,*) x,v,t

    ! place values of k11,k21
    x_rk = v
    v_rk = f(x,t)

    ! calculate k12, k22 using above
    x_t = x + v*dt*0.5d0
    v_t = v + f(x,t)*0.5d0*dt

    !placing k*2's with factor of 2 into rk slopes
    x_rk = x_rk + 2*v_t
    v_rk = v_rk + 2*f(x_t,t)

    ! calculate k13,k23 using above
    x_t = x + v_t*dt*0.5d0
    v_t = v + f(x_t,t)*0.50*dt

    !placing k*3's with factor of 2 into rk slopes
    x_rk = x_rk + 2*v_t
    v_rk = v_rk + 2*f(x_t,t)

    ! calculate k14, k24 and place values into RK slopes
    x_t = x + v_t*dt
    v_t = v + f(x_t,t)*dt
    x_rk = x_rk + v_t
    v_rk = v_rk + f(x_t,t)

    ! update the values
    v = v + v_rk*dt/real(6)
    x = x + x_rk*dt/real(6)
    t = t + dt

END DO

CLOSE(11)

END PROGRAM nonlin_rk


REAL*8 FUNCTION f(x,t)

    IMPLICIT NONE
    REAL*8 :: x, t

    f = -1d0*SIN(x)
END FUNCTION
