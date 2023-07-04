PROGRAM smart_mc

IMPLICIT NONE
REAL*8 :: nums(10)
INTEGER :: i,n, k
REAL*8, DIMENSION(6) :: x,r,a,b,h
REAL*8 :: mc_int, err, volm, func, f

a = (/-5,-5,-5,-5,-5,-5/)/5
b = (/ 5, 5, 5, 5, 5, 5/)/5
h = b-a

k = k+1
volm = 1d0
do i = 1,6
    volm= volm*h(i)
end do

9 PRINT*, 'No. of iterations?'
READ*, n

mc_int = 0d0

do i = 1,n

    call normal_rand(r,6)

    x =  r
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





END PROGRAM smart_mc

real*8 function func(x)

    implicit none
    REAL*8 :: x(6), x2,y2,x_y

    x2 = x(1)**2 + x(2)**2 + x(3)**2
    y2 = x(4)**2 + x(5)**2 + x(6)**2
    x_y = (x(1)-x(4))**2 + (x(2)-x(5))**2 + (x(3)-x(6))**2

    func = EXP(-1d0*(x2 + y2 + x_y/2))
end function func

SUBROUTINE normal_rand(array,n)

IMPLICIT NONE
REAL*8, DIMENSION(n), INTENT(OUT):: array
REAL*8, DIMENSION(n) :: u_1,u_2,N1,N2
REAL*8:: seed(8), dt_seed(8)
REAL*8, PARAMETER :: PI = 4*ATAN(1d0)
INTEGER, INTENT(IN) :: n


call random_number(seed)

call random_seed(put = NINT(1000000*(seed-0.5d0)))

call random_number(u_1)
call random_number(u_2)

!Generate Normal Random no. using
! Box-Muller Transformation


!N1 = SQRT(-LOG(u_1))*COS(2*PI*u_2)
N2 = SQRT(-LOG(u_1))*SIN(2*PI*u_2)
array = 2*N2

END SUBROUTINE normal_rand
