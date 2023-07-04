PROGRAM normal_rand

IMPLICIT NONE

REAL*8, DIMENSION(100000) :: u_1,u_2,N1,N2, rand_no, c_k
INTEGER*8 :: seed(8), dt_seed(8), j, k, i
REAL*8, PARAMETER :: PI = 4*ATAN(1d0)
REAL*8 :: summ, summ2, avg

call DATE_AND_TIME(values = dt_seed)

call random_number(u_1)
call random_number(u_2)

!Generate Normal Random no. using
! Box-Muller Transformation


!N1 = SQRT(-LOG(u_1))*COS(2*PI*u_2)
rand_no = SQRT(-LOG(u_1))*SIN(2*PI*u_2)
avg = sum(rand_no)/(100000d0)
OPEN(UNIT = 5, FILE = 'c_knormal.dat', STATUS = 'new')
do j = 1,1000
    summ = 0d0
    summ2 = 0d0
    k = j-1
    do i = 1,100000-k
        summ = summ + (rand_no(i)-avg)*(rand_no(i+k)-avg)
        summ2 = summ2 + (rand_no(i) - avg)**2
    end do
    c_k = summ/summ2
    WRITE(5,*) k,c_k
end do
CLOSE(5)

OPEN(UNIT = 1, FILE = 'normal2_.dat', STATUS = 'new')
WRITE(1,'(f20.14)') 2*rand_no
CLOSE(1)
print*, N2(1:10)

END PROGRAM normal_rand


