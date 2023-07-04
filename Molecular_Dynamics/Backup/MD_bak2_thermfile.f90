MODULE params
    IMPLICIT NONE

    INTEGER, PARAMETER :: natoms = 13**3! number of particles
    INTEGER, PARAMETER :: max_neigh = 60 ! maximum no. neighbours
    INTEGER, PARAMETER :: t_tstat = 2 ! frequency of THERMOSTAT
    INTEGER, PARAMETER :: nat3 = natoms*3 ! degrees of freedom
    INTEGER, PARAMETER :: Paritosh  = 1
    REAL*8, PARAMETER :: eps = 1.00d0 ! epsilon (unit of energy)
    REAL*8, PARAMETER :: sig = 1.0d0 ! sigma = radius of particle (unit of length)
    REAL*8, PARAMETER :: boxlen = 20.0d0 ! box length
    REAL*8, PARAMETER :: rc = 2.5d0 ! cut off radius
    REAL*8, PARAMETER :: rs = rc + 2.0d0 ! skin radius for neighbourlist
    REAL*8, PARAMETER :: dt = 0.005d0 ! time step
    REAL*8, PARAMETER :: m = 1.0d0 ! mass
    REAL*8, PARAMETER :: kbT = 1.0
    REAL*8, PARAMETER :: kbTbm  = 3*kBT/2.0/m
    REAL*8, PARAMETER :: sig12 = sig**12
    REAL*8, PARAMETER :: sig6b2 = 0.50d0*sig**6
    REAL*8, PARAMETER :: sig6 = sig**6, sigma6_6 = sig6*6.0d0, sigma12_12 = sig12*12.0
    REAL*8, PARAMETER :: f_const = 48.00d0*eps
    REAL*8, PARAMETER :: v_const = 4.00d0*eps
    REAL*8, PARAMETER :: dtby2m = 0.5d0*dt/m, dt2hf = dt*dt*0.5/m, pbc = 10.0*boxlen
    CHARACTER(LEN=30), PARAMETER :: filen_root = "20K_Therm", filen_ext = ".dat"


END MODULE params

MODULE initials

USE OMP_LIB
USE params

IMPLICIT NONE

    CONTAINS

        SUBROUTINE initialize_positions(r)

        INTEGER :: i, rndm
        REAL*8 :: pert
        REAL*8, DIMENSION(nat3), INTENT(OUT) :: r

        PRINT*, "What position initialisation do you want?"
        PRINT*, '1. Completlely Random (at your own risk)'
        PRINT*, '2. A predefined Lattice with random perturbations'
        PRINT*, '3. A predifined Lattice'

        READ*, rndm

        SELECT CASE (rndm)
            CASE (1)
                DO i = 1, natoms
                    r(i*3-2:i*3) = rand(3)*boxlen
                END DO
                PRINT*, "Randomly alloted positions --- Divergence incoming *^%#$#&@*@"

            CASE (2)
                WRITE(*,"('Size of the perturbations (0 - 0.5) :-  ')", advance = 'no')
                READ*,pert
                DO i = 1, natoms
                    r(i*3-2:i*3) =(/real(mod(i,13)+1),real(mod(INT(i/13),13)+1),real(INT(i/13/13))/)*1.5 + rand(3)*pert
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
        v = v_amp*(rand(nat3)-0.5)

        v_avg = (/ sum(v(1:nat3:3))/natoms, sum(v(2:nat3:3))/natoms, sum(v(3:nat3:3))/natoms /)
        DO i = 1, natoms
            v(i*3-2:i*3) = v(i*3-2:i*3) - v_avg
        END DO
    END SUBROUTINE

END MODULE




MODULE dynamics
    USE OMP_LIB
    USE params
    IMPLICIT NONE
    ! Modifiers for Lennard-Jones Potential
    REAL*8, PARAMETER :: fc =  f_const*(sig12/rc**13 - sig6b2/rc**7)! Original Force at rc
    REAL*8, PARAMETER :: vc = v_const*(sig12/rc**12 - sig6/rc**6) + fc*rc !! Original potential at r_c
    REAL*8 ,PARAMETER :: rc2 = rc**2
    REAL*8, PARAMETER :: rc12 = rc**12, rc6 = rc**6
    CONTAINS
        SUBROUTINE lj_force(r, f, pe)
            REAL*8, DIMENSION(nat3), INTENT(IN) :: r ! positions of Particles (x_1,y_1,z_1,x_2,y_2.......)
            REAL*8, DIMENSION(nat3), INTENT(OUT) :: f ! force on particles (Fx_1, Fy_1, Fz_1, Fx_2 ...... )
            REAL*8, INTENT(OUT) :: pe
            REAL*8 :: ffac(1:3), r_cap(1:3), r_mod
            INTEGER :: i, j, k


            f = 0.0
            pe = 0.0

            DO i = 0, natoms-2
                DO j = i+1, natoms-1

                    r_cap = r(i*3+1:i*3+3) - r(j*3+1:j*3+3)

                    ! ------------ Image Convention -----------------------------------------!

                    if(abs(r_cap(1)) .ge. boxlen/2.0d0) r_cap(1) = (boxlen - abs(r_cap(1)))*((-r_cap(1))/abs(r_cap(1)))
                    if(abs(r_cap(2)) .ge. boxlen/2.0d0) r_cap(2) = (boxlen - abs(r_cap(2)))*((-r_cap(2))/abs(r_cap(2)))
                    if(abs(r_cap(3)) .ge. boxlen/2.0d0) r_cap(3) = (boxlen - abs(r_cap(3)))*((-r_cap(3))/abs(r_cap(3)))

                    r_mod = SQRT(r_cap(1)**2 + r_cap(2)**2 + r_cap(3)**2)
                    r_cap = r_cap/r_mod

                    IF (r_mod <= rc) THEN
                        ! ----- Force Calculatiosn
                        ffac = (f_const*(sig12/(r_mod**13) - (sig6b2)/(r_mod**7))-fc) * r_cap
                        f(i*3+1:i*3+3) = f(i*3+1:i*3+3) + ffac
                        f(j*3+1:j*3+3) = f(j*3+1:j*3+3) - ffac
                        ! ----- Energy Calculations
                        pe = pe + v_const*(sig12/r_mod**12 - sig6/r_mod**6) + fc*r_mod - vc
                    END IF

                END DO
            END DO


        END SUBROUTINE lj_force

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
            v = v*sqrt(kbTbm*natoms/(2*ke))
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
    REAL*8, DIMENSION(nat3) :: r, v, f
    ! --------------- requireed variables -------------------------!
    REAL*8 :: r_rand(3), st, fin, t, tmax ,pe, ke, v_amp, v_avg(3)
    INTEGER :: i, j, k, n, nn
   ! --------- for file handling ---------------------------!
    CHARACTER(LEN=5), DIMENSION(3) :: modifs
    CHARACTER(LEN=50) :: filen
    CHARACTER(LEN=50) :: modif
   ! -------------------------- building file_name

    modif = '_'
    modifs = [CHARACTER(LEN = 5) :: "Q2", "Fast", "QQ"]
    DO i = 1,3
        modif = trim(modif)//trim(modifs(i))
    END DO
    filen = trim(filen_root)//trim(modif)//trim(filen_ext)


!     OPEN(UNIT= 10, file = filen, STATUS = "NEW")
    ! -----------------------initialize positions and velocities
    call initialize_positions(r)
    call initialize_velocities(v)
    ! ------------------------ initialize forces and energies
    call lj_force(r, f, pe)


    ! ------------------------ set maximum simulation time
    tmax = 10.0d0
    n = NINT(tmax/dt)
    n = 900
    ke = 0
    t = 0

    CALL CPU_TIME(st)

    ! ------------------------ MAIN LOOP ----------------------------------------------!
    do nn = 0, n

        print*, 'Time: ', nn
        ! -------------------------------------------update positions

        r = r + v*dt + f*dt2hf

        ! ------------------------------------------ apply periodic boundary conditions

        do i = 1, nat3
        r(i) = mod(r(i) + pbc, boxlen)
        end do

        ! ------------------------------------------ update velocities
        v = v + f*dtby2m

        ! ------------------------------------------ update forces
        call lj_force(r, f, pe)

        ! ------------------------------------------ update velocities again
        v = v + f*dtby2m

        ! ----------------------------------------- calc energies and TODO Thermodynamics
        ke = sum(v**2)/2
        ! ------------------------------------------- write data to file
        WRITE(10,*) t, pe/natoms, ke/natoms, sum(v(1:nat3:3))/natoms, sum(v(2:nat3:3))/natoms, sum(v(3:nat3:3))/natoms


        ! ------------------------------------------- update veolcities according to thermostat every 1 second

        CALL thermostat(v,ke,nn)

        ! ------------------------------------------- update TIME
        t = t + dt

    end do

    CALL CPU_TIME(fin)
!     OPEN(UNIT = 20, FILE = 'VelDis1.dat', STATUS='NEW')
    WRITE(20,*) v

PRINT*, "Elapsed time: ", (fin - st),(fin-st)/60,'min', ' or', (fin-st)/n, ' per iteration'
PRINT*, filen


END PROGRAM MolDyn



