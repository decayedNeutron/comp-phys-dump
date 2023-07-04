PROGRAM ising

IMPLICIT none

INTEGER :: L(1:10,1:10,1:10), n, q, ix(3), L1, ni

REAL*8 :: e_tot, e,e_temp, en, r_ix(3), rn, de,  prob
REAL*8 ::  L_rand(1:10,1:10,1:10), mn, m_tot, m_av, e_av
INTEGER :: ii,jj,kk,time, seed(8), t = 0, ll
REAL*8 :: kT = 1d0
ni = 50000
en = 0
mn = 0
m_av = 0
e_av = 0
L1 = 10

seed = (/ 10223, -21344, 20311, -32311, 21332, -10, 10100101, 23 /)
!call random_seed(put = seed)

!initialise
! OPEN(unit = 10, file = "Ising3D_rand_initvals.dat", status = 'new')
! OPEN (unit = 11, file = 'Ising3D_all_1_initt1.dat', status = 'new')

! call random_number(L_rand)
! L = 2*NINT(L_rand)-1

L  = +1
ni = 50000
en = 0
mn = 0
m_av = 0
e_av = 0
L1 = 10
DO ii = 1,L1
    DO jj = 1, L1
        DO kk = 1, L1
            mn = mn + L(ii,jj,kk)
            en = en + e(L,ii,jj,kk,L1)
!              WRITE(10,'(I5,I3,I3,I3,I3)') t,ii,jj,kk,L(ii,jj,kk)
        END DO
    END DO

END DO

en = en*0.5d0



print*,"Initial State, E:", en,"M:",mn

DO ll = 1,20
L  = +1
ni = 50000
en = 0
mn = 0
m_av = 0
e_av = 0
L1 = 10
DO ii = 1,L1
    DO jj = 1, L1
        DO kk = 1, L1
            mn = mn + L(ii,jj,kk)
            en = en + e(L,ii,jj,kk,L1)
!              WRITE(10,'(I5,I3,I3,I3,I3)') t,ii,jj,kk,L(ii,jj,kk)
        END DO
    END DO

END DO

en = en*0.5d0
print*, 'kT = ', kT
DO time = 1, ni
    DO ii = 1, L1
        DO jj = 1,L1
            DO kk = 1,L1
                call random_number(r_ix)
                ix = INT(float(L1)*r_ix)+1

                e_temp = e(L,ix(1),ix(2),ix(3),L1)
                L(ix(1),ix(2),ix(3)) = -L(ix(1),ix(2),ix(3))
                de = e(L,ix(1),ix(2),ix(3),L1) - e_temp

                IF( de <= 0.0) THEN
                    en =  en + de
                    mn = mn + 2.0*float(L(ix(1),ix(2),ix(3)))
                ELSE

                    call random_number(rn)
                   ! print*,rn
                    prob = exp(-de/kT)

                    IF (rn .gt. prob) THEN
                        L(ix(1),ix(2),ix(3)) = -L(ix(1),ix(2),ix(3))
                    ELSE
                        en =  en + de
                        mn = mn + 2.0*float(L(ix(1),ix(2),ix(3)))
                    END IF

                END IF
            END DO
        END DO
        m_av = m_av + mn
        e_av = e_av + en

    END DO
!    WRITE(11,*) time,en/real(1000), mn/real(1000)

END DO
print*, "Final State, E:", en,"M:",mn
print*, e_tot(L,10), m_tot(L,10)
print*, m_av/real(ni*L1*L1*L1), e_av/(real(ni*L1*L1*L1))

kT = kT + 0.25d0

END DO


END PROGRAM

REAL*8 FUNCTION e_tot(L,n)

IMPLICIT none

INTEGER :: L(1:10,1:10,1:10),n
REAL*8 :: J1, e
INTEGER :: i,j, k, m, o, q ! iterators
e_tot = 0
DO i = 1,n
    DO j = 1,n
        DO k = 1,n
            e_tot = e_tot + e(L,i,j,k,n)
        END DO
    END DO
END DO
e_tot = e_tot/2


END FUNCTION e_tot

REAL*8 FUNCTION m_tot(L,n)

IMPLICIT none

INTEGER :: L(1:10,1:10,1:10),n
REAL*8 :: J1, e
INTEGER :: i,j, k, m, o, q ! iterators
m_tot = 0
DO i = 1,n
    DO j = 1,n
        DO k = 1,n
            m_tot = m_tot + L(i,j,k)
        END DO
    END DO
END DO



END FUNCTION m_tot

REAL*8 FUNCTION e(L,i,j,k,n)

IMPLICIT none

INTEGER :: L(1:10,1:10,1:10),n
REAL*8 :: J1
INTEGER :: i,j, k, m, o, q ! iterators
J1 = 1.0d0
e = 0
IF (i .gt. 1) THEN
    e = e + L(i-1,j,k)
ELSE
    e = e + L(n,j,k)
END IF
IF (i .lt. n) THEN
    e = e + L(i+1,j,k)
ELSE
    e = e + L(1,j,k)
END IF
IF (j .gt. 1) THEN
    e = e + L(i,j-1,k)
ELSE
    e = e + L(i,n,k)
END IF
IF (j .lt. n) THEN
    e = e + L(i,j+1,k)
ELSE
    e = e + L(i,1,k)
END IF
IF (k .gt. 1) THEN
    e = e + L(i,j,k-1)
ELSE
    e = e + L(i,j,n)
END IF
IF (k .lt. n) THEN
    e = e + L(i,j,k+1)
ELSE
    e = e + L(i,j,1)
END IF



e = -e*J1*L(i,j,k)

END FUNCTION e

