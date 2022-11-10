!
!  Conway Game of Life
!
!  COMPILE: gfortran -o life life_serial.f90
!  RUN: ./life
! 

program life
  
  implicit none
  integer, parameter :: ni=200, nj=200, nsteps = 500
  integer :: i, j, n, im, ip, jm, jp, nsum, isum, seed_size
  integer, allocatable :: old(:,:), new(:,:), rnd_seed(:)
  real, allocatable :: arand(:)

  ! allocate arrays, including room for ghost cells
  allocate(old(0:ni+1,0:nj+1), new(0:ni+1,0:nj+1))

  call random_seed(SIZE=seed_size)
  allocate(rnd_seed(seed_size),arand(ni))
  call random_seed(GET=rnd_seed)

  ! initialize elements of old to 0 or 1
  do j = 1, nj
    rnd_seed(1) = rnd_seed(1) - 1
    call random_seed(PUT=rnd_seed)
    call random_number(arand)
    old(1:ni,j) = nint(arand)
  end do




  isum = sum(old(1:ni,1:nj))
  ! Print initial number of live cells.
  write(*,"(/'Initial number of live cells = ', i6/)") isum


  !  iterate
  time_iteration: do n = 1, nsteps

    ! corner boundary conditions
    old(0,0) = old(ni,nj)
    old(0,nj+1) = old(ni,1)
    old(ni+1,nj+1) = old(1,1)
    old(ni+1,0) = old(1,nj)

    ! left-right boundary conditions
    old(1:ni,0) = old(1:ni,nj)
    old(1:ni,nj+1) = old(1:ni,1)

    ! top-bottom boundary conditions
    old(0,1:nj) = old(ni,1:nj)
    old(ni+1,1:nj) = old(1,1:nj)


    do j = 1, nj
      do i = 1, ni

        im = i - 1
        ip = i + 1
        jm = j - 1
        jp = j + 1
        nsum = old(im,jp) + old(i,jp) + old(ip,jp) &
             + old(im,j )             + old(ip,j ) &
             + old(im,jm) + old(i,jm) + old(ip,jm)

        select case (nsum)
          case (3)
            new(i,j) = 1
          case (2)
            new(i,j) = old(i,j)
          case default
            new(i,j) = 0
          end select

      enddo
    enddo

    ! copy new state into old state
    old = new


  enddo time_iteration

  ! Iterations are done; sum the number of live cells
  isum = sum(new(1:ni,1:nj))
  
  ! Print final number of live cells.
  write(*,"(/'Number of live cells = ', i6/)") isum

  deallocate(old, new, arand, rnd_seed)

end program life
