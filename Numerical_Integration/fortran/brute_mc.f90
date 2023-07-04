PROGRAM brute_mc

IMPLICIT none

INTEGER :: i,n, k
REAL*8, DIMENSION(6) :: x,r,a,b,h
REAL*8 :: mc_int, err, volm, func, f

a = (/-5,-5,-5,-5,-5,-5/)/1
b = (/ 5, 5, 5, 5, 5, 5/)/1
h = b-a
k = 1

volm = 1d0
do i = 1,6
    volm= volm*h(i)
end do

9 PRINT*, 'No. of iterations?'
READ*, n
k = k+1
mc_int = 0d0

do i = 1,n

    call random_number(r)
    x = a + h*r
    f = func(x)
    mc_int = mc_int + f
    err = err + f**2
end do

mc_int = mc_int/real(n)
err = err/real(n)
err = err - mc_int**2

mc_int = volm*mc_int
err = volm*SQRT(err/real(n))
print*, 'The value of integral is = ',mc_int
print*, 'The value of error (st dev) is = ' , err

if (k < 10) goto 9




END PROGRAM brute_mc

real*8 function func(x)

    implicit none
    REAL*8 :: x(6), x2,y2,x_y

    x2 = x(1)**2 + x(2)**2 + x(3)**2
    y2 = x(4)**2 + x(5)**2 + x(6)**2
    x_y = (x(1)-x(4))**2 + (x(2)-x(5))**2 + (x(3)-x(6))**2

    func = EXP(-1d0*(x2 + y2 + x_y/2))
end function func
