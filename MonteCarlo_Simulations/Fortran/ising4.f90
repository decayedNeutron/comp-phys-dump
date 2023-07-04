PROGRAM ising_copy

IMPLICIT none

INTEGER, allocatable :: S(:,:,:)

REAL*8 :: e_tot, e,e_temp, en, r_ix(3), rn, de,  prob, mn, m_tot, m_av, e_av
REAL*8 , allocatable::  S_rand(:,:,:)
INTEGER :: ii,jj,kk,time,ix(3), seed(8), t = 0, ll, n, q,  L, ni
REAL*8 :: kT = 3.9d0, J_is = 1.0d0
INTEGER:: a1,a2,b1,b2,c1,c2

! PRINT*, 'Enter the no. lattice point in each direction:'
! READ*, L
! PRINT*, "Enter the no. of MC iterations:"
! READ*, ni

L = 10
ni = 50000

allocate(S(L,L,L),S_rand(L,L,L))
en = 0.0d0
mn = 0.0d0
n = L*L*L

call random_number(S_rand)

!  S = NINT(S_rand)*2 - 1
S = 1
DO ii = 1,L
    DO jj = 1, L
        DO kk = 1, L
        a1 = ii + 1
        a2 = ii - 1
        b1 = jj + 1
        b2 = jj - 1
        c1 = kk + 1
        c2 = kk - 1
        if(ii == 1)a2 = L
        if(ii == L)a1 = 1
        if(jj == 1)b2 = L
        if(jj == L)b1 = 1
        if(kk == 1)c2 = L
        if(kk == L)c1 = 1

            mn = mn + S(ii,jj,kk)
            en = en - J_is*S(ii,jj,kk)*(S(a1,jj,kk) + S(a2,jj,kk))
            en = en - J_is*S(ii,jj,kk)*(S(ii,b1,kk) + S(ii,b2,kk))
            en = en - J_is*S(ii,jj,kk)*(S(ii,jj,c1) + S(ii,jj,c2))
!              WRITE(10,'(I5,I3,I3,I3,I3)') t,ii,jj,kk,L(ii,jj,kk)
        END DO
    END DO

END DO
en = en*0.5d0
print*, mn, en

DO time = 1, ni
    DO ii = 1, L
        DO jj = 1,L
            DO kk = 1,L

            e_temp = 0
            e = 0
                call random_number(r_ix)
                ix = INT(float(L)*r_ix)+1

                a1 = ix(1) + 1
        a2 = ix(1) - 1
        b1 = ix(2) + 1
        b2 = ix(2) - 1
        c1 = ix(3) + 1
        c2 = ix(3) - 1
        if(ix(1) == 1)a2 = L
        if(ix(1) == L)a1 = 1
        if(ix(2) == 1)b2 = L
        if(ix(2) == L)b1 = 1
        if(ix(3) == 1)c2 = L
        if(ix(3) == L)c1 = 1

            e_temp = e_temp - J_is*S(ix(1),ix(2),ix(3))*(S(a1,ix(2),ix(3)) + S(a2,ix(2),ix(3)))
            e_temp = e_temp - J_is*S(ix(1),ix(2),ix(3))*(S(ix(1),b1,ix(3)) + S(ix(1),b2,ix(3)))
            e_temp = e_temp - J_is*S(ix(1),ix(2),ix(3))*(S(ix(1),ix(2),ix(3)) + S(ix(1),ix(2),ix(3)))

                S(ix(1),ix(2),ix(3)) = -S(ix(1),ix(2),ix(3))
            e = e - J_is*S(ix(1),ix(2),ix(3))*(S(a1,ix(2),ix(3)) + S(a2,ix(2),ix(3)))
            e = e - J_is*S(ix(1),ix(2),ix(3))*(S(ix(1),b1,ix(3)) + S(ix(1),b2,ix(3)))
            e = e - J_is*S(ix(1),ix(2),ix(3))*(S(ix(1),ix(2),ix(3)) + S(ix(1),ix(2),ix(3)))

            de = e - e_temp

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
        m_av = m_av + mn
        e_av = e_av + en

    END DO
END DO
print*, "Final State, E:", en,"M:",mn
! print*, e_tot(L,10), m_tot(L,10)
print*, m_av/real(ni*n), e_av/(real(ni*n))

END PROGRAM ising_copy

