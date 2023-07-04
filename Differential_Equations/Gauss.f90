PROGRAM GaussS

IMPLICIT NONE
REAL*8 :: constraint = 0.001d0
CHARACTER(LEN=50) :: filen
REAL*8, ALLOCATABLE:: y(:), x(:), yi(:)
INTEGER :: n,i,j, g,h, c,k,ii
REAL*8 :: dx, a,b,d1,d2,d3, d4
a = 0.0d0
b = 1.0d0
dx = 0.01
n = NINT((b-a)/dx)
Print*, 'No. of points:', n
allocate(y(n+1),x(n+1))
k=0
d1 = (1d0-5d0*dx/2d0)
d2 = (1d0 + 5d0*dx/2d0)
d3 = (10.0d0*dx**2d0)
d4 = 1d0/(2d0-10d0*dx**2)

y(1) = 0.00d0
y(n+1) = 2.0d0
! do j = 2,n
! y(j) = j*2.0d0/real(n)
! end do
x = (/ (dx*i, i = 0,n)/)
i = 0
k = 1

DO ii = 1,9
constraint = 0.1d0**(ii)
DO
    i = i + 1
    yi = y
    DO j = 2, n
        g = j+ 1
        h = j - 1
        y(j) =  d4*(d1*yi(g) + d2*yi(h) - d3*x(j))

    END DO

!     IF (MOD(i,1000) .eq. 0) THEN
!
!         WRITE(filen, '(A9,I1)') 'Gaus', k
!         OPEN(UNIT = 9, file = trim(filen)//".dat", Status = 'new')
!         do j = 1,n+1
!             WRITE(9,*) x(j), y(j)
!         end do
!         CLOSE(9)
!         k = k+1
!     END IF


    c = 1
    do j = 2, n

        IF (ABS(y(j)-yi(j)) .ge. constraint) THEN

        c = 0
        END IF
    end do
if (c == 1 .or. i >= 100000000) then
    print*,c
    exit
else

    continue

end if
end do

print*,'No. of iterations',i
WRITE(filen, '(A5,I1)') 'NewGs', k
        OPEN(UNIT = 9, file = trim(filen)//".dat", Status = 'new')
        do j = 1,n+1
            WRITE(9,*) x(j), y(j)
        end do
        CLOSE(9)
 k = k+1

CLOSE(11)
Print*, 'Difference in values constrainted by', constraint
Print*, 'The Value at x = 0.80 is' , y(NINT(0.80d0/dx))
end do
END PROGRAM GaussS
