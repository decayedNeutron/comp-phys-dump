program trap_int

implicit none

REAL*16 :: func, integral
REAL*16, PARAMETER :: PI = 4*ATAN(1d0)
INTEGER :: u = 7 , ios, i
INTEGER*8 :: p
print*,PI
! print*, "The value of integral 4/(1+x^2) from 0 to 1 with dx = 0.01 is : ", integral(100), "with error : ", PI - integral(100)
! print*, "The value of integral 4/(1+x^2) from 0 to 1 with dx = 0.001 is : ", integral(1000),&
! "with error : ", PI - integral(1000)
! print*, "The value of integral 4/(1+x^2) from 0 to 1 with dx = 0.0001 is : ", integral(10000),&
! "with error : ", PI - integral(10000)
! print*, "The value of integral 4/(1+x^2) from 0 to 1 with dx = 0.00001 is : ", integral(100000),&
! "with error : ", PI - integral(100000)

OPEN(UNIT = u, IOSTAT = ios, FILE = 'errors_new_prec.dat', STATUS = 'new')
IF (ios .eq. 0) THEN

do i = 1,4
p = 10**i
WRITE(u,*) real(1)/real(p), PI - integral(p)
end do
CLOSE(u)

ELSE
PRINT *, 'Some error in opening the file'

END IF
end program trap_int

real*16 function integral(n)

REAL*16:: i, a, b, h, func
INTEGER*8:: n
a = 0d0
b = 1d0
integral = 0d00

! WRITE(*,*) "What is the lower limit of integration"
! READ*, a
! WRITE(*,*) "What is the upper limit of the integration"
! READ*, b
! WRITE(*,*) "What is number of divisions"
! READ*, n

h = (b-a)/n

do i=1,n
    integral = integral + h*( func(a + h*i) + func(a + h*(i-1)) )/2

end do



end function integral

real*16 function func(x)
    implicit none
    real*16, intent(in):: x

    func = 4/(1+x**2)
end function func
