MODULE params
    IMPLICIT NONE

    INTEGER, PARAMETER :: natoms = 1200! number of particles
    INTEGER, PARAMETER :: max_neigh = 120 ! maximum no. neighbours
    INTEGER, PARAMETER :: t_tstat = 40 ! frequency of THERMOSTAT
    INTEGER, PARAMETER :: t_neigh = 40 ! frequency of Neighbour List UPDATION
    INTEGER, PARAMETER :: nat3 = natoms*3 ! degrees of freedom
    INTEGER, PARAMETER :: n_list_size  = natoms*max_neigh
    REAL*8, PARAMETER :: eps = 1.00d0 ! epsilon (unit of energy)
    REAL*8, PARAMETER :: sig = 1.0d0 ! sigma = radius of particle (unit of length)
    REAL*8, PARAMETER :: boxlen = 20.0d0 ! box length
    REAL*8, PARAMETER :: rc = 2.5d0 ! cut off radius
    REAL*8, PARAMETER :: rs = rc + 2.0d0 ! skin radius for neighbourlist
    REAL*8, PARAMETER :: dt = 0.0025d0 ! time step
    REAL*8, PARAMETER :: m = 1.0d0 ! mass
    REAL*8, PARAMETER :: kbT = 1.0
    REAL*8, PARAMETER :: kbTbm  = 3*kBT/2.0/m
    REAL*8, PARAMETER :: sig12 = sig**12, box2 = boxlen/2.0
    REAL*8, PARAMETER :: sig6b2 = 0.50d0*sig**6
    REAL*8, PARAMETER :: sig6 = sig**6, sigma6_6 = sig6*6.0d0, sigma12_12 = sig12*12.0
    REAL*8, PARAMETER :: f_const = 48.00d0*eps
    REAL*8, PARAMETER :: v_const = 4.00d0*eps
    REAL*8, PARAMETER :: dtby2m = 0.5d0*dt/m, dt2hf = dt*dt*0.5/m, pbc = 10.0*boxlen
    CHARACTER(LEN=30), PARAMETER :: filen_root = "20K", filen_ext = ".dat"


END MODULE params

MODULE initials

USE OMP_LIB
USE params

IMPLICIT NONE

    CONTAINS

        SUBROUTINE initialize_positions(r)

        INTEGER :: i, rndm
        REAL*8 :: pert, r_rand(3)
        REAL*8, DIMENSION(nat3), INTENT(OUT) :: r

        PRINT*, "What position initialisation do you want?"
        PRINT*, '1. Completlely Random (at your own risk)'
        PRINT*, '2. A predefined Lattice with random perturbations'
        PRINT*, '3. A predifined Lattice'

        READ*, rndm

        SELECT CASE (rndm)
            CASE (1)
                DO i = 1, natoms
                    CALL random_number(r_rand)
                    r(i*3-2:i*3) = r_rand(3)*boxlen
                END DO
                PRINT*, "Randomly alloted positions --- Divergence incoming *^%#$#&@*@"

            CASE (2)
                WRITE(*,"('Size of the perturbations (0 - 0.5) :-  ')", advance = 'no')
                READ*,pert
                DO i = 1, natoms
                    CALL random_number(r_rand)
                    r(i*3-2:i*3) =(/real(mod(i,13)+1),real(mod(INT(i/13),13)+1),real(INT(i/13/13))/)*1.5 + r_rand*pert
                END DO
                PRINT*, "Perturbed Lattice INITIALISED "

            CASE (3)
                DO i = 1, natoms
                    r(i*3-2:i*3) =(/real(mod(i,13)+1),real(mod(INT(i/13),13)+1),real(INT(i/13/13))/)*1.5
                END DO
                PRINT*, "Lattice INITIALISED"
            CASE DEFAULT
                DO i = 1, natoms
                    r(i*3-2:i*3) =(/real(mod(i,13)+1),real(mod(INT(i/13),13)+1),real(INT(i/13/13))/)*1.5
                END DO
                PRINT*, "Lattice INITIALISED"
        END SELECT
    END SUBROUTINE

    SUBROUTINE initialize_velocities(v)
        REAL*8, DIMENSION(nat3), INTENT(OUT) :: v
        REAL*8 :: v_avg(3), v_amp
        INTEGER:: i, j, k

        v_amp = dsqrt(12*kbT/m)
        DO i = 1, natoms
            CALL random_number(v_avg)
            v(i*3-2:i*3) =  v_amp*(v_avg-0.5)
        END DO
        v_avg = (/ sum(v(1:nat3:3))/natoms, sum(v(2:nat3:3))/natoms, sum(v(3:nat3:3))/natoms /)

        DO i = 1, natoms
            v(i*3-2:i*3) = v(i*3-2:i*3) - v_avg
        END DO

        PRINT*, 'Velocities Initialised at KE per particle =', sum(v*v)/2.0/m/natoms
    END SUBROUTINE

END MODULE




MODULE dynamics
    USE OMP_LIB
    USE params
    IMPLICIT NONE
    ! Modifiers for Lennard-Jones Potential
    REAL*8, PARAMETER :: fc =  f_const*(sig12/rc**13 - sig6b2/rc**7)! Original Force at rc
    REAL*8, PARAMETER :: vc = v_const*(sig12/rc**12 - sig6/rc**6) + fc*rc !! Original potential at r_c
    REAL*8 ,PARAMETER :: rc2 = rc**2, rs2 = rs**2
    REAL*8, PARAMETER :: rc12 = rc**12, rc6 = rc**6
    REAL*8, DIMENSION(nat3) :: r, v, f

    INTEGER, DIMENSION(n_list_size) :: n_list
    REAL*8 :: pe

    CONTAINS
       SUBROUTINE lj_force
            REAL*8 :: ffac(1:3), r_cap(1:3), r_mod
            INTEGER :: i, j, k


            f = 0.0
            pe = 0.0
            k = 1
            i = 0
            OUTTER : DO
                INNER : DO

                        j = n_list(k)-1
                        IF (j == -2) EXIT INNER ! Seperator for seperate particles
                        IF (j == -3) EXIT OUTTER! END of the list
                        r_cap = r(i*3+1:i*3+3) - r(j*3+1:j*3+3) ! Calculates the r_ij vector

                        !----------------------------------------- Image Convention --------------------------------------!

                        if(abs(r_cap(1)) >= box2) r_cap(1) = (boxlen - abs(r_cap(1)))*((-r_cap(1))/abs(r_cap(1)))
                        if(abs(r_cap(2)) >= box2) r_cap(2) = (boxlen - abs(r_cap(2)))*((-r_cap(2))/abs(r_cap(2)))
                        if(abs(r_cap(3)) >= box2) r_cap(3) = (boxlen - abs(r_cap(3)))*((-r_cap(3))/abs(r_cap(3)))

                        r_mod = DSQRT(r_cap(1)**2 + r_cap(2)**2 + r_cap(3)**2)

                        r_cap = r_cap/r_mod
                            IF (r_mod <= rc) THEN
                            ! ----- Force Calculatiosn
                            ffac = (f_const*(sig12/(r_mod**13) - (sig6b2)/(r_mod**7))-fc) * r_cap
                            f(i*3+1:i*3+3) = f(i*3+1:i*3+3) + ffac
                            f(j*3+1:j*3+3) = f(j*3+1:j*3+3) - ffac
                            ! ----- Energy Calculations
                            pe = pe + v_const*(sig12/r_mod**12 - sig6/r_mod**6) + fc*r_mod - vc
                        END IF
                            k = k + 1
                        END DO INNER
                        k = k + 1
                        i = i + 1
                        IF (i > 2196) EXIT OUTTER

                END DO OUTTER


        END SUBROUTINE lj_force

        SUBROUTINE update_neighbour_list
            IMPLICIT NONE
            REAL*8 :: r_cap(1:3), r_mod
            INTEGER :: i, j, k, l
                k = 1
                DO i = 0, natoms-2
                    l = 0
                    INSIDE : DO j = i+1, natoms-1

                    r_cap = r(i*3+1:i*3+3) - r(j*3+1:j*3+3)

                    ! ------------ Image Convention -----------------------------------------!

                    if(abs(r_cap(1)) .ge. boxlen/2.0d0) r_cap(1) = (boxlen - abs(r_cap(1)))*((-r_cap(1))/abs(r_cap(1)))
                    if(abs(r_cap(2)) .ge. boxlen/2.0d0) r_cap(2) = (boxlen - abs(r_cap(2)))*((-r_cap(2))/abs(r_cap(2)))
                    if(abs(r_cap(3)) .ge. boxlen/2.0d0) r_cap(3) = (boxlen - abs(r_cap(3)))*((-r_cap(3))/abs(r_cap(3)))

                    IF(sum(r_cap*r_cap) <= rs2 ) THEN

                        n_list(k) = j+1
                        k = k+1
                        l = l+1
                    END IF

                    IF (l >= max_neigh) EXIT INSIDE

                    END DO INSIDE

                    n_list(k) = -1
                    k = k + 1
                END DO
                n_list(k) = -2
        END SUBROUTINE

END MODULE dynamics



MODULE thermodynamics
    USE params
    USE dynamics

    CONTAINS
        SUBROUTINE thermostat(v,ke, nn)
            REAL*8, DIMENSION(nat3), INTENT(INOUT) :: v
            REAL*8, INTENT(IN) :: ke
            INTEGER, INTENT(IN) :: nn

            IF (MOD(nn,t_tstat) == t_tstat-1 ) THEN
            v = v*sqrt(kbTbm*natoms/(ke))
            END IF
        END SUBROUTINE


END MODULE



PROGRAM MolDyn
    USE params
    USE dynamics
    USE initials
    USE thermodynamics
    IMPLICIT NONE
    ! ---------------------------- required arrays ---------------!

    ! --------------- requireed variables -------------------------!
    REAL*8 :: r_rand(3), st, fin, t, tmax , ke, v_amp, v_avg(3)
    INTEGER :: i, j, k, n, nn, kkk
   ! --------- for file handling ---------------------------!
    CHARACTER(LEN=5), DIMENSION(3) :: modifs
    CHARACTER(LEN=50) :: filen, filen1
    CHARACTER(LEN=50) :: modif, tt
    CHARACTER :: ret = ACHAR(13)


   ! -------------------------- building file_name
    modif = '_'
    modifs = [CHARACTER(LEN = 5) :: "Th", "Nlist", "3Q3"]

    DO i = 1,3
        modif = trim(modif)//trim(modifs(i))
    END DO

    filen = trim(filen_root)//trim(modif)//trim(filen_ext)
    WRITE(filen1, '(A7)') 'Mol/Pos'

!     OPEN(UNIT= 10, file = '20K1200_Th.dat', STATUS = "UNKNOWN")

    ! -----------------------initialize positions and velocities

    call initialize_positions(r)
    CALL update_neighbour_list()
    call initialize_velocities(v)

    OPEN(UNIT = 20, FILE = 'NList.dat', STATUS='UNKNOWN')
!     WRITE(20,"(300I4)") n_list

    ! ------------------------ initialize forces and energies
    call lj_force


    ! ------------------------ set maximum simulation time
    tmax = 10.0d0
    n = NINT(tmax/dt)
    n = 1000
    ke = 0
    t = 0
    kkk = 0
    CALL CPU_TIME(st)

    ! ------------------------ MAIN LOOP ----------------------------------------------!

    do nn = 0, n


        ! -------------------------------------------update positions

        r = r + v*dt + f*dt2hf

        ! ------------------------------------------ apply periodic boundary conditions

        do i = 1, nat3
        r(i) = mod(r(i) + pbc, boxlen)
        end do

        ! ------------------------------------------ update velocities
        v = v + f*dtby2m

        ! ------------------------------------------ update forces
        call lj_force

        ! ------------------------------------------ update velocities again
        v = v + f*dtby2m

        ! ----------------------------------------- calc energies and TODO Thermodynamics
        ke = sum(v**2)/2
        ! ------------------------------------------- write data to file
        WRITE(10,*) t, pe/natoms, ke/natoms, sum(v(1:nat3:3))/natoms, sum(v(2:nat3:3))/natoms, sum(v(3:nat3:3))/natoms

        ! ------------------------------------------- update veolcities according to thermostat every T_TSTAT iterations
        CALL thermostat(v,ke,nn)

        ! ------------------------------------------- update neighbourlist every T_NEIGH iterations
        IF (MOD(nn,T_NEIGH) == 0) THEN

            CALL update_neighbour_list

        END IF
        ! ------------------------------------------- update TIME
        t = t + dt
        ! ------------------------------------------- Store Position data for plotting purposes
        IF (MOD(nn,10) == 0) THEN

            WRITE(tt,'(I6)') kkk
            OPEN(UNIT = 22, file = trim(adjustl(filen1))//trim(adjustl(tt))//".dat", status = 'UNKNOWN')
            WRITE(22,'(3F20.10)') r
            kkk = kkk + 1
            CLOSE(22)
        END IF

        ! -------------------------------------------------------- Check if KE Goes Haywire
        IF (ke > 100000.0d0)  THEN
        OPEN(UNIT  = 29, FILE = 'R_chaos.dat',STATUS = 'UNKNOWN')
        OPEN(UNIT  = 30, FILE = 'V_chaos.dat',STATUS = 'UNKNOWN')
        OPEN(UNIT  = 31, FILE = 'F_chaos.dat',STATUS = 'UNKNOWN')

        WRITE(29,"(3F20.10)") r
        WRITE(30,"(3F20.10)") v
        WRITE(31,"(3F20.10)") f

        CLOSE(29);CLOSE(30);CLOSE(31)
        EXIT
        END IF

        ! Visually Aesthetic Time Counter
        WRITE(*,"('                  Time : 'I10,A1)", ADVANCE = 'no') nn, ret
    END DO

    CALL CPU_TIME(fin)
!      OPEN(UNIT = 20, FILE = 'NList2.dat', STATUS='UNKNOWN')
!     WRITE(20,"(300I4)") n_list
! OPEN(UNIT  = 29, FILE = 'R_chaos.dat',STATUS = 'UNKNOWN')
! OPEN(UNIT  = 30, FILE = 'V_chaos.dat',STATUS = 'UNKNOWN')
! OPEN(UNIT  = 31, FILE = 'F_chaos.dat',STATUS = 'UNKNOWN')
!
! WRITE(29,"(3F20.10)") r
! WRITE(30,"(3F20.10)") v
! WRITE(31,"(3F20.10)") f
PRINT*, n, ' iterations done................'
PRINT*, "Elapsed time: ", (fin - st),' s or ',(fin-st)/60,'min  or', (fin-st)/nn, ' per iteration'
PRINT*, filen
CLOSE(20)
CLOSE(10)

END PROGRAM MolDyn



