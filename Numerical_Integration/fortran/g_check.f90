PROGRAM gauss_check

implicit none
REAL*8 :: sede, avg, sq_avg, stdev, c_k, summ, summ2
INTEGER, ALLOCATABLE :: seedd(:)
INTEGER :: n, i, j, k
REAL*8 :: rand_no(10000000)



call random_seed(size = n)
allocate(seedd(n))

do i=1,n
call random_number(sede)
seedd(i) = NINT((sede-0.5)*100000000)
end do

call random_seed(put = seedd)
call random_number(rand_no)

avg = sum(rand_no)/(10000000d0)
sq_avg = sum(rand_no*rand_no)/(10000000d0)
stdev = SQRT(sq_avg - avg**2)



!!generating exponential rv. from uniform

rand_no = (-LOG(1-rand_no))/2

OPEN(UNIT = 4, FILE='exponent_check.dat', STATUS='new')

do i = 1,10000000

if (rand_no(i) .gt. 1.0d0) WRITE(4,*) rand_no(i)-1.0d0


end do



CLOSE(4)
! CLOSE(4)

END PROGRAM
