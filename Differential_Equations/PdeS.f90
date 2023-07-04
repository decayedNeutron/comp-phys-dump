! Program to solve the Laplace equation using Dirichlet boundary conditions
program laplace
      implicit none

      integer, parameter:: nx = 34, ny = 34 ! BOUNDARIES AT x,y=0 and x,y=34
      real*8 :: old_temp(1:nx, 1:ny), temp(1:nx, 1:ny)
      integer :: i,j,jj,ii,kk
      real*8 :: bound_temp, increment_temp, dx, dy, prefactor
      character(len=30) :: filename
      integer :: iunit, ci,cii,test, counter

      bound_temp=0.0d0; increment_temp=0.1d0
      old_temp=0.0d0 ! INITIAL CONDITION FOR WHOLE LATTICE

     ! BOUNDARY CONDITIONS
      do i=1,ny
        old_temp(1,i) = 3.7d0 !LEFT BOUNDARY at x=1
        old_temp(nx,i) = 0.4d0 !RIGHT BOUNDARY t x=34
      end do

      do i=2, nx-1 ! BOUNDARY CONDITIONS ALONG Y
        old_temp(i,1) = 3.7d0 - dfloat(i-1)*increment_temp ! LOWER BOUNDARY
        old_temp(i,ny) = 3.7d0 - dfloat(i-1)*increment_temp ! TOP BOUNDARY
      end do

      temp=old_temp
      iunit=71; ci=0
      write(filename, '("initialize_",i0,".dat")') ci
      open(newunit=iunit, file=filename) !WRITE DOWN THE BC AT ZEROTH ITERATION

      do ii=1,nx
        do jj=1,ny
            write(iunit,*) ii,jj,old_temp(ii,jj)
        end do
      end do
     close(iunit)

      dx=0.05d0; dy=0.05d0 ! we will not be using this
      test=0; counter=0; prefactor=(0.5d0*dx*dx*dy*dy)/(dx*dx + dy*dy)

      do ! LOOP OVER ITERATIONS
        counter= counter+1
        test=0
        do jj=2,ny-1 ! UPDATE STEP
            do ii=2,nx-1
                !temp(ii,jj) = 0.25*(old_temp(ii-1,jj) +old_temp(ii+1,jj) + old_temp(ii,jj-1) + old_temp(ii,jj+1))
                temp(ii,jj) = 0.25*(temp(ii-1,jj) +old_temp(ii+1,jj) + temp(ii,jj-1) + old_temp(ii,jj+1))
            end do
        end do

        ! CHECK FOR CONVERGENCE AT EACH LATTICE SITE
        do jj=2,ny-1
            do ii=2,nx-1
                if((abs(temp(jj,ii) - old_temp(jj,ii))).gt.0.000125d0) test=1
            end do
        end do

        if (test.eq.0) exit ! EXIT CONDITION
        old_temp = temp ! AFTER TEST CONDITION
      end do
      write(*,*) 'counter', counter

      ! WRITE THE CONVERGED RESULT: SOLN TO THE EQN WITH BC

      iunit=71; ci=10000
      write(filename,'("initialize_",i0,".dat")') ci

      open(newunit=iunit, file=filename)
      do ii=1,nx
        do jj=1,ny
            write(iunit,*) ii,jj,temp(ii,jj)
        end do
      end do
      close(iunit)

end program laplace
