PROGRAM ising

IMPLICIT none

INTEGER, ALLOCATABLE:: S(:,:,:)
REAL*8, ALLOCATABLE :: S_rand(:,:,:)
INTEGER :: n, q, ix(3), L, ni, T, cnt

REAL*8 :: e_tot, e, e_i, e_f, en, r_ix(3), rn, de, prob, m_abs
REAL*8 ::  mn, m_tot, m_av, e_av
INTEGER :: ii,jj,kk,time, seed(8), ll, n_equil, n_stat
REAL*8 :: e2_av, m2_av, m4_av, bincu, cv, chi
REAL*8 :: kT = 4.9d0
CHARACTER(LEN=50) :: filen,fn2,tt

!
! PRINT*, 'Enter the no. lattice point in each direction:'
! READ*, L
! PRINT*, "Enter the no. of MC iterations:"
! READ*, ni

L = 10
ni = 60000
allocate(S(L,L,L), S_rand(L,L,L))
n_equil = 10000
n_stat = 1
en = 0
mn = 0

n = L*L*L

! S = -1
! DO ii = 1,L
!     DO jj = 1, L
!         DO kk = 1, L
!             S(ii,jj,kk) = 2*NINT(real(kk)/real(10))-1
!         END DO
!     END DO
!
! END DO
! print*,S

! seed = (/ 10223, -21344, 20311, -32311, 21332, -10, 10100101, 23 /)
!call random_seed(put = seed)

!initialise
! OPEN(unit = 10, file = "Ising3D_rand_initvals.dat", status = 'new')
OPEN (unit = 11, file = 'IsingMore9ABS.dat', status = 'new')

call random_number(S_rand)
S = 2*NINT(S_rand)-1
deallocate(S_rand)

! S = -1

DO ii = 1,L
    DO jj = 1, L
        DO kk = 1, L
            mn = mn + S(ii,jj,kk)
            en = en + e(S,ii,jj,kk,L)
!              WRITE(10,'(I5,I3,I3,I3,I3)') t,ii,jj,kk,L(ii,jj,kk)
        END DO
    END DO

END DO

en = en*0.5d0
print*, "For Random Initial state of 9 x 9 x 9 lattice"
print*,"Initial State, E:", en,"M:",mn

cnt = 1
!---------------------------------------------------------------------------
!--------- Temp Loop Starting -------------------


! DO T = 0,45

!     en = 0
!     mn = 0
    m_av = 0
    e_av = 0
    e2_av = 0
    m2_av = 0
    m4_av = 0
    m_abs = 0
!     kT = 4.7d0 - T*0.02d0
    print*, 'Temperature :',kT

    DO time = 1, ni

!     WRITE(filen, '(A8,I1)') 'A5/GRID9', 0
!     WRITE(fn2, '(A9,I1)') 'A5/GRID29', 0
!
!     IF (time < 100) THEN
!         IF(MOD(time-1,10) .eq. 0) THEN
!         WRITE(tt,*) cnt
!         cnt = cnt + 1
!         OPEN(UNIT = 20, file = trim(filen)//trim(adjustl(tt))//".dat", Status = 'new')
!         OPEN(unit=21, file = trim(fn2)//trim(adjustl(tt))//".dat", Status = 'new')
!         END IF
!     END IF
!     IF (time <= 10000 .and. time >= 101) THEN
!         IF(MOD(time-1,2000) .eq. 0) THEN
!         WRITE(tt,*) cnt
!         cnt = cnt + 1
!         OPEN(UNIT = 20, file = trim(filen)//trim(adjustl(tt))//".dat", Status = 'new')
!         OPEN(unit=21, file = trim(fn2)//trim(adjustl(tt))//".dat", Status = 'new')
!         END IF
!     END IF

        DO kk = 1, L
            DO jj = 1, L
                DO ii = 1, L
!
!                     IF (time < 100) THEN
!                         IF(MOD(time-1,5) .eq. 0) THEN
!                             IF (S(ii,jj,kk) == 1) THEN
!                                 WRITE(20,'(3I4)') ii,jj,kk
!                             ELSE
!                                 WRITE(21,'(3I4)') ii,jj,kk
!                             END IF
!                         END IF
!                     END IF
!                     IF (time <= 10000 .and. time >= 101) THEN
!                         IF(MOD(time-1,1000) .eq. 0) THEN
!                             IF (S(ii,jj,kk) == 1) THEN
!                                 WRITE(20,'(3I4)') ii,jj,kk
!                             ELSE
!                                 WRITE(21,'(3I4)') ii,jj,kk
!                             END IF
!                         END IF
!                     END IF
!


                    call random_number(r_ix)
                    ix = INT(float(L)*r_ix)+1 ! random triplet for a lattice point

                    e_i = e(S,ix(1),ix(2),ix(3),L) ! ENERGY before flip
                    S(ix(1),ix(2),ix(3)) = -S(ix(1),ix(2),ix(3))
                    e_f = e(S,ix(1),ix(2),ix(3),L) ! ENERGY after flip

                    de = e_f - e_i      ! Delta E for a single flip

                    IF( de <= 0.0) THEN
                        en =  en + de
                        mn = mn + 2.0*float(S(ix(1),ix(2),ix(3)))
                    ELSE

                        call random_number(rn)
                    ! print*,rn
                        prob = exp(-de/kT)

                        IF (rn .gt. prob) THEN
                            S(ix(1),ix(2),ix(3)) = -S(ix(1),ix(2),ix(3))
                        ELSE
                            en =  en + de
                            mn = mn + 2.0*float(S(ix(1),ix(2),ix(3)))
                        END IF

                    END IF

                END DO
            END DO


        END DO

                CLOSE(20)
                CLOSE(21)

        WRITE(11,*) time,en/real(1000), mn/real(1000)
        if (time .gt. n_equil) then
            if (mod(time, n_stat) .eq. 0) then
                m_av = m_av + mn
                e_av = e_av + en
                e2_av = e2_av + en**2
                m2_av = m2_av + mn**2
                m4_av = m4_av + mn**4
                m_abs = m_abs + abs(mn)
            end if
        end if



    END DO

    m_av  = dfloat(n_stat)*m_av/dfloat(ni - n_equil)
    e_av  = dfloat(n_stat)*e_av/dfloat(ni - n_equil)
    e2_av  = dfloat(n_stat)*e2_av/dfloat(ni - n_equil)
    m2_av = dfloat(n_stat)*m2_av/dfloat(ni - n_equil)
    m4_av  = dfloat(n_stat)*m4_av/dfloat(ni - n_equil)
    m_abs = dfloat(n_stat)*m_abs/dfloat(ni - n_equil)

    bincu = 1.0d0 - m4_av/(3.0d0 * (m2_av**2))

    cv = (e2_av - e_av**2)/(kT**2)
    chi = (m2_av - m_abs**2)/(kT)

    print*, "Final State, <E/spin>:", e_av/(real(n)),"<M/spin>:",m_av/real(n), "<|M|/spin>:",m_abs/real(n)
    print*,''

!     write(11,*) kT,m_av/real(n),e_av/(real(n)),m_abs/real(n),cv,chi,bincu

! END DO


CLOSE(11)
deallocate(S)
END PROGRAM

REAL*8 FUNCTION e_tot(S,L)

IMPLICIT none

INTEGER:: S(9,9,9)

REAL*8 :: J_is, e
INTEGER :: i,j, k,L ! iterators

e_tot = 0

DO i = 1,L
    DO j = 1,L
        DO k = 1,L
            e_tot = e_tot + e(S,i,j,k,L)
        END DO
    END DO
END DO
e_tot = e_tot/2


END FUNCTION e_tot

REAL*8 FUNCTION m_tot(S,L)

IMPLICIT none

INTEGER:: S(9,9,9)
REAL*8 :: J1, e
INTEGER :: i,j, k, L! iterators
! allocate(S(L,L,L))
m_tot = 0
DO i = 1,L
    DO j = 1,L
        DO k = 1,L
            m_tot = m_tot + S(i,j,k)
        END DO
    END DO
END DO



END FUNCTION m_tot

REAL*8 FUNCTION e(S,i,j,k,L)

IMPLICIT none

INTEGER :: S(9,9,9)
INTEGER :: L
REAL*8 :: J_is
INTEGER :: i,j, k! indexe at which energy is required
J_is = 1.0d0 ! ISING model constant

! allocate(S(L,L,L))

e = 0
IF (i .gt. 1) THEN
    e = e + S(i-1,j,k)
ELSE
    e = e + S(L,j,k)
END IF
IF (i .lt. L) THEN
    e = e + S(i+1,j,k)
ELSE
    e = e + S(1,j,k)
END IF
IF (j .gt. 1) THEN
    e = e + S(i,j-1,k)
ELSE
    e = e + S(i,L,k)
END IF
IF (j .lt. L) THEN
    e = e + S(i,j+1,k)
ELSE
    e = e + S(i,1,k)
END IF
IF (k .gt. 1) THEN
    e = e + S(i,j,k-1)
ELSE
    e = e + S(i,j,L)
END IF
IF (k .lt. L) THEN
    e = e + S(i,j,k+1)
ELSE
    e = e + S(i,j,1)
END IF



e = -e*J_is*S(i,j,k)

END FUNCTION e

