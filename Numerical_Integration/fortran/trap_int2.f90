PROGRAM trap_int2
implicit none


REAL*8 :: func, integral
REAL*8, PARAMETER :: PI = 4*ATAN(1d0)


REAL*8:: i, a, b, h
INTEGER :: n

integral = 0d00
a = 0d0
b = PI

! WRITE(*,*) "What is the lower limit of integration"
! READ*, a
! WRITE(*,*) "What is the upper limit of the integration"
! READ*, b
WRITE(*,*) "What is number of divisions"
READ*, n

h = (b-a)/n

do i=1,n
    integral = integral + h*( func(a + h*i) + func(a + h*(i-1)) )/2

end do

print*, 'The integral of sinx from 0 to pi is :' , integral

end program trap_int2

real*8 function func(x)
    implicit none
    real*8, intent(in):: x

    func = SIN(x)
end function func
