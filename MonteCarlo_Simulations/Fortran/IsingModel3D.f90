PROGRAM ising

IMPLICIT none

INTEGER, ALLOCATABLE:: S(:,:,:) ! the spin holder
REAL*8, ALLOCATABLE :: S_rand(:,:,:) ! TO GENERATE Random Initial State
INTEGER :: n, q, ix(3), L, ni

REAL*8 :: e_tot, e_i, e_f, en, r_ix(3), rn, de, prob
REAL*8 ::  mn, m_tot, m_av, e_av
INTEGER :: ii,jj,kk,time, seed(8), t = 0, ll

REAL*8 :: kT = 4.9d0 ! the temperature factor

!
! PRINT*, 'Enter the no. lattice point in each direction:'
! READ*, L
! PRINT*, "Enter the no. of MC iterations:"
! READ*, ni

L = 11
ni = 50000
allocate(S(L,L,L), S_rand(L,L,L))

en = 0
mn = 0
m_av = 0
e_av = 0
n = L*L*L




! seed = (/ 10223, -21344, 20311, -32311, 21332, -10, 10100101, 23 /)
!call random_seed(put = seed)

!initialise
! OPEN(unit = 10, file = "Ising3D_rand_initvals.dat", status = 'new')
! OPEN (unit = 11, file = 'Is_ran_49_new.dat', status = 'new')

call random_number(S_rand)
S = 2*NINT(S_rand)-1


DO ii = 1,L
    DO jj = 1, L
        DO kk = 1, L
            mn = mn + S(ii,jj,kk)
            CALL e_r(S,ix(1),ix(2),ix(3),L,e_i)
            en = en + e_i
!              WRITE(10,'(I5,I3,I3,I3,I3)') t,ii,jj,kk,L(ii,jj,kk)
        END DO
    END DO

END DO

en = en*0.5d0

print*,"Initial State, E:", en,"M:",mn


DO time = 1, ni
    DO kk = 1, L
        DO jj = 1, L
            DO ii = 1, L
                call random_number(r_ix)
                ix = INT(float(L)*r_ix)+1 ! random triplet for a lattice point
                CALL e_r(S,ix(1),ix(2),ix(3),L,e_i) ! ENERGY before flip
                S(ix(1),ix(2),ix(3)) = -S(ix(1),ix(2),ix(3))
                CALL e_r(S,ix(1),ix(2),ix(3),L,e_f) ! ENERGY after flip

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
    WRITE(11,*) time,en/real(1000), mn/real(1000)
            m_av = m_av + mn
        e_av = e_av + en
END DO
print*, "Final State, E:", en,"M:",mn
! print*, e_tot(S,10), m_tot(S,10)
print*, m_av/real(ni*n), e_av/(real(ni*n))

CONTAINS
                SUBROUTINE e_r(S,i,j,k,L,e)

                IMPLICIT none

                INTEGER, DIMENSION(:,:,:), INTENT(IN) :: S
                REAL*8, INTENT(OUT) :: e

                INTEGER, INTENT(IN) :: L
                REAL*8 :: J_is
                INTEGER :: i,j, k! indexe at which energy is required
                J_is = 1.0d0 ! ISING model constant

                ! allocate(S(L,L,L))


                e = 0d0
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

                END SUBROUTINE e_r


END PROGRAM

! REAL*8 FUNCTION e_tot(S,L)
!
! IMPLICIT none
!
! INTEGER:: S(10,10,10)
!
! REAL*8 :: J_is, e
! INTEGER :: i,j, k,L ! iterators
!
! e_tot = 0
!
! DO i = 1,L
!     DO j = 1,L
!         DO k = 1,L
!             CALL e_r(S,i,j,k,e)
!             e_tot = e_tot + e
!         END DO
!     END DO
! END DO
! e_tot = e_tot/2
!
!
! END FUNCTION e_tot
!
! REAL*8 FUNCTION m_tot(S,L)
!
! IMPLICIT none
!
! INTEGER:: S(10,10,10)
! REAL*8 :: J1, e
! INTEGER :: i,j, k, L! iterators
! ! allocate(S(L,L,L))
! m_tot = 0
! DO i = 1,L
!     DO j = 1,L
!         DO k = 1,L
!             m_tot = m_tot + S(i,j,k)
!         END DO
!     END DO
! END DO
!
!
!
! END FUNCTION m_tot
!
! SUBROUTINE e_r(S,i,j,k,e)
!
! IMPLICIT none
!
! INTEGER, DIMENSION(:,:,:), INTENT(IN) :: S
! REAL*8, INTENT(OUT) :: e
! INTEGER :: L
! REAL*8 :: J_is
! INTEGER :: i,j, k! indexe at which energy is required
! J_is = 1.0d0 ! ISING model constant
!
! ! allocate(S(L,L,L))
! shp = shape(S)
! L = shp(1)
! e = 0
! IF (i .gt. 1) THEN
!     e = e + S(i-1,j,k)
! ELSE
!     e = e + S(L,j,k)
! END IF
! IF (i .lt. L) THEN
!     e = e + S(i+1,j,k)
! ELSE
!     e = e + S(1,j,k)
! END IF
! IF (j .gt. 1) THEN
!     e = e + S(i,j-1,k)
! ELSE
!     e = e + S(i,L,k)
! END IF
! IF (j .lt. L) THEN
!     e = e + S(i,j+1,k)
! ELSE
!     e = e + S(i,1,k)
! END IF
! IF (k .gt. 1) THEN
!     e = e + S(i,j,k-1)
! ELSE
!     e = e + S(i,j,L)
! END IF
! IF (k .lt. L) THEN
!     e = e + S(i,j,k+1)
! ELSE
!     e = e + S(i,j,1)
! END IF
!
!
!
! e = -e*J_is*S(i,j,k)
!
! END SUBROUTINE e_r
!
