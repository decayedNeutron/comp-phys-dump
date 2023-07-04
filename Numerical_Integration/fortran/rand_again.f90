PROGRAM rand_again

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

! PRINT*, 'The mean of random numbers is', avg
! PRINT*, 'The standard deviation of the random no. is', stdev
OPEN(UNIT = 5, FILE = 'c_k10M.dat', STATUS = 'new')
! do j = 1,1000
!     summ = 0d0
!     k = j-1
!     do i = 1,1000-k
!         summ = summ + rand_no(i)*rand_no(i+k)
!     end do
!     summ = summ/real(n-k)
!     c_k = (summ - avg**2)/(stdev**2)
!     WRITE(5,*) k,c_k
! end do

do j = 1,1000
    summ = 0d0
    summ2 = 0d0
    k = j-1
    do i = 1,10000000-k
        summ = summ + (rand_no(i)-avg)*(rand_no(i+k)-avg)
        summ2 = summ2 + (rand_no(i) - avg)**2
    end do
    c_k = summ/summ2
    WRITE(5,*) k,c_k
end do
CLOSE(5)


! OPEN(UNIT = 3, FILE="scatter.dat", STATUS = 'new')
! do i = 1,9999
! WRITE(3,*) rand_no(i), rand_no(i+1)
! end do

!generating exponential rv. from uniform
!
! rand_no = (-LOG(1-rand_no))/2
!
! OPEN(UNIT = 4, FILE='uniform1M.dat', STATUS='new')
!
! WRITE(4,'(f10.8)') rand_no
!
! PRINT*, rand_no(1:10)

CLOSE(4)
! CLOSE(4)

END PROGRAM
