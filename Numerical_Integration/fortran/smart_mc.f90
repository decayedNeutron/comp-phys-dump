PROGRAM smart_mc

IMPLICIT NONE

INTEGER :: n, k, l, m = 1
REAL*8 :: x(6), mc_int, func, var, err, r(2), volm, i, gau_distro
REAL*8, PARAMETER :: lenth = 5.0d0, PI = 4.0d0*ATAN(1d0), rt2 = 1.0d0/SQRT(2.0d0)

volm = PI**3

print*,PI
PRINT*, 'What is the value of n?'
READ*, n

9 n = n*10
mc_int = 0d0
err = 0d0



DO k = 1,n
    DO l = 1,6
        call RANDOM_NUMBER(r)
        x(l) = gau_distro(r)*rt2
    END DO
    i = func(x)
    mc_int = mc_int + i
    err = err + i**2
END DO

mc_int = mc_int/real(n)
err = err/real(n)
err = err - mc_int**2

mc_int = volm*mc_int
err = volm*SQRT(err/real(n))
print*, 'The value of integral is for',n,' = ',mc_int
print*, 'The value of error (st dev) is = ' , err


if (n <100000001) goto 9


END PROGRAM smart_mc




REAL*8 FUNCTION func(x)

    REAL*8 :: x(6), x_y

    x_y = (x(1)-x(4))**2 + (x(2)-x(5))**2 + (x(3)-x(6))**2

    func = EXP((-0.50d0)*x_y)

END FUNCTION

REAL*8 FUNCTION gau_distro(x)

    REAL*8 :: r2 , x(2), r(2), f

 2   call random_number(r)
    x = 2.0d0*r - 1.0d0

    r2 = x(1)**2 + x(2)**2
    if (r2 >= 1.0d0 .or. r2 == 0d0) goto 2

    f = sqrt(-2.0d0*log(r2)/r2)
    gau_distro = x(2)*f

END FUNCTION
