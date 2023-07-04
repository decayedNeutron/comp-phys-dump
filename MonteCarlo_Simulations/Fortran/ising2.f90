PROGRAM ising

IMPLICIT none

INTEGER :: L(1:10,1:10,1:10), n, q

REAL*8 :: e_tot, e, L_rand(1:20,1:20,1:20),en
! initial condition
en = 0
L = +1

! call random_number(L_rand)
! L = NINT(2*L_rand - 1)


print*,"For all spins point at +1 in a  10 x 10 x 10 lattice -"
print*,'The total energy of the lattice:', e_tot(L,10)




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

e_tot = e_tot*0.5d0


END FUNCTION e_tot


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
    e = e + L(i,j,k)*L(i,j,k+1)
ELSE
    e = e + L(i,j,1)
END IF



e = e*J1*L(i,j,k)

END FUNCTION e
