program trap_int

implicit none
REAL*8:: i, func, a, b, h, integral = 0d0
INTEGER :: n


WRITE(*,*) "What is the lower limit of integration"
READ*, a
WRITE(*,*) "What is the upper limit of the integration"
READ*, b
WRITE(*,*) "What is number of divisions"
READ*, n

h = (b-a)/n

do i=1,n
    integral = integral + h*( func(a + h*i) + func(a + h*(i-1)) )/2

end do

print*, "The value of integral 4/(1+x^2) from 0 to 1 is : ", integral


end program trap_int

real*8 function func(x)
    implicit none
    real*8, intent(in):: x

    func = 4/(1+x**2)
end function func
