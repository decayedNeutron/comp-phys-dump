PROGRAM ising

IMPLICIT none

INTEGER :: L(1:20,1:20,1:20), n, q

REAL*8 :: e_tot, e, L_rand(1:20,1:20,1:20),en, m_tot
! initial condition
en = 0


L = -1

print*,"For all spins point at -1 in a  20 x 20 x 20 lattice -"
print*,'The total energy of the lattice:', e_tot(L,20)
print*,'The total Magnetic moment of lattice:', m_tot(L,20)





END PROGRAM

REAL*8 FUNCTION e_tot(L,n)

IMPLICIT none

INTEGER :: L(1:20,1:20,1:20),n
REAL*8 :: e
INTEGER :: i,j, k ! iterators
e_tot = 0
DO i = 1,n
    DO j = 1,n
        DO k = 1,n
            e_tot = e_tot + e(L,i,j,k,n)
        END DO
    END DO
END DO


END FUNCTION e_tot

REAL*8 FUNCTION m_tot(L,n)

IMPLICIT none

INTEGER:: L(1:20,1:20,1:20)
REAL*8 :: J1, e
INTEGER :: i,j, k, n! iterators
! allocate(S(L,L,L))
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

INTEGER :: L(1:20,1:20,1:20),n
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
    e = e + L(i,j,k)*L(i,j,k+1)
ELSE
    e = e + L(i,j,1)
END IF



e = e*J1*L(i,j,k)

END FUNCTION e

