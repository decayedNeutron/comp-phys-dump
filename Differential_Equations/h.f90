PROGRAM HeatFlow

IMPLICIT NONE

REAL*8 :: T(1:100,1:100), ti, dt, lapT, dx, dy, alpha
INTEGER :: i, j, k ,l, n

T = 0d0

DO i = 20,30
    DO j = 45,55
        T(i,j) = 2000
        T(i+50,j) = 2000
    END DO
END DO


ti = 0d0
dt = 0.01d0
n = 10000
dx = 1.0d0
dy = 1.0d0
alpha = 20

OPEN(unit = 11, file = "Heat8.dat", status = "new")

DO l = 0,n
IF (MOD(n,10) == 0) THEN
    DO k = 1,100
        WRITE(11,*) (T(k,j) , j = 1,100)
    END DO
    END IF
    DO i = 2,99

        DO j = 2,99

            lapT = alpha*((T(i+1,j) - 2*T(i,j) + T(i-1,j))/(dx**2) + (T(i,j+1) - 2*T(i,j) + T(i,j-1))/(dy**2))

            T(i,j) = T(i,j) + lapT*dt
        END DO

    END DO

    t = t + dt
    DO i = 48,52
    DO j = 48,52
        T(i,j) = 500

    END DO
END DO

END DO




END PROGRAM HeatFlow

