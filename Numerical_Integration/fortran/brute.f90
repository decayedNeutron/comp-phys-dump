program brute

implicit none

INTEGER :: i,j,k,l,m,n,s,ih
REAL*8 :: xi,xj,xk,yl,ym,yn,func, integral = 0d0, h6
REAL*8, DIMENSION(6) :: a,b,h

a = (/-5,-5,-5,-5,-5,-5/)
b = (/5,5,5,5,5,5/)
PRINT*, "No. of grid points"
READ*, s
h = (b-a)/s
print*, h
h6 = 1d0

do ih = 1,6
    h6 = h6*h(ih)
end do

do i = 1,s
    xi = a(1)+i*h(1)
    do j = 1,s
        xj = a(2) + j*h(2)
        do k = 1,s
            xk = a(3) + k*h(3)
            do l = 1,s
                yl = a(4) + l*h(4)
                do m = 1,s
                    ym = a(5) + m*h(5)
                    do n = 1,s
                        yn = a(6) + n*h(6)
                        integral = integral + h6*())

end program brute

real*8 function func(xi,xj,xk,yl,ym,yn)

    implicit none
    REAL*8 :: xi,xj,xk,yl,ym,yn,x2,y2,x_y

    x2 = xi*xi + xj*xj + xk*xk
    y2 = yl*yl + ym*ym + yn*ym
    x_y = (xi-yl)**2 + (xj-ym)**2 + (xk-yn)**2

    func = EXP(-1d0*(x2 + y2 + x_y/2))
end function func

