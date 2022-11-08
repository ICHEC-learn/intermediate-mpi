!------------------------------------
!  Conway Game of Life

! 4 processors, domain decomposition
! in both i and j directions, virtual
! topology

!------------------------------------
program life

  implicit none
  include 'mpif.h'

  integer, parameter :: ni = 200, nj = 200, nsteps = 500
  integer :: i, j, n, im, ip, jm, jp, nsum, isum, isum1, &
       ierr, myid, nprocs, i1, i2, i1p, i2m, j1, j2, j1p, &
       j2m, i1n, i2n, j1n, j2n, ninom, njnom, &
       nitot, isumloc, iproc_north, iproc_south, &
       iproc_east, iproc_west, iproc_ne, iproc_nw, iproc_se, &
       iproc_sw, istart, iend
  integer :: status(mpi_status_size), row_type, comm_cart, &
       ndims, reorder, mycartid, ewdir, nsdir
  integer, dimension(2) :: dim_size, periods
  integer, dimension(16) :: irequest
  integer, allocatable, dimension(:,:) :: old, new
  real :: arand

  ! initialize MPI

  call mpi_init(ierr)
  call mpi_comm_rank(mpi_comm_world, myid, ierr)
  call mpi_comm_size(mpi_comm_world, nprocs, ierr)

!TODO: Domain decomposition */
  ! nominal number of points per proc. (without ghost cells,
  ! assume numbers divide evenly); dim_size(1) and dim_size(2)
  ! are the numbers of procs in the i and j directions.
  if(nprocs == 1) then
     ndims = 1
  else
     ndims = ...
  endif

  dim_size(1) = ...
  dim_size(2) = ...

  ninom = ...
  njnom = ...

!TODO: Create virtual topology
  ! presently set up for only 4 procs
  ! periodic in all directions

  ! coordinate directions for north, south, east, and west
  nsdir = 0
  if(nprocs == 1) then
    ewdir = 0
  else
    ewdir = 1
  endif

  periods(1) = ...
  periods(2) = ...
  reorder = 0
  call mpi_cart_create( ... )
  call mpi_cart_shift( ... )
  call mpi_cart_shift( ... )
  call mpi_cart_shift( ... )
  call mpi_cart_shift( ... )

!TODO: Define processors on corners
  ! Virtual topology has been used to define adjacent processors
  ! to the north, south, east, and west; still have to define
  ! processors on corners

  if(nprocs > 1) then
     select case(myid)
     case(0)
        iproc_nw    = ...
        iproc_ne    = ...
        iproc_se    = ...
        iproc_sw    = ...
     case(1)
        iproc_nw    = ...
        iproc_ne    = ...
        iproc_se    = ...
        iproc_sw    = ...
     case(2)
        iproc_nw    = ...
        iproc_ne    = ...
        iproc_se    = ...
        iproc_sw    = ...
     case(3)
        iproc_nw    = ...
        iproc_ne    = ...
        iproc_se    = ...
        iproc_sw    = ...
     end select
  endif

  ! nominal starting and ending indices (without ghost cells)

  i1n = (myid/2)*ninom + 1
  i2n = i1n + ninom - 1
  j1n = mod(myid,2)*njnom + 1
  j2n = j1n + njnom - 1

  ! local starting and ending indices, including 2 ghost cells

  i1  = i1n - 1
  i2  = i2n + 1
  i1p = i1  + 1
  i2m = i2  - 1
  j1  = j1n - 1
  j2  = j2n + 1
  j1p = j1  + 1
  j2m = j2  - 1
  nitot = i2 - i1 + 1
  
  ! allocate arrays

  allocate( old(i1:i2,j1:j2), new(i1:i2,j1:j2) )

  ! Initialize elements of old to 0 or 1.  We're doing some
  ! sleight of hand here to make sure we initialize to the
  ! same values as in the serial case. The random_number
  ! function is called for every i and j, even if they are
  ! not on the current processor, to get the same random
  ! distribution as the serial case, but they are only used
  ! if this i and j reside on the current procesor.

  do j = 1, nj
     do i = 1, ni
        call random_number(arand)
        if(i > i1 .and. i < i2 .and. j > j1 .and.j < j2 ) then
           old(i,j) = nint(arand)
        endif
     enddo
  enddo

! TODO: Create derived type for single row of array.
  ! There are nj "blocks," each containing 1 element,
  ! with a stride of nitot between the blocks

  call mpi_type_vector( ... )
  call mpi_type_commit( ... )

  !  iterate

  time_iteration: do n = 1, nsteps

     ! transfer data to ghost cells

     if(nprocs == 1) then

        ! top and bottom
        old(i1,j1p:j2m) = old(i2m,j1p:j2m)
        old(i2,j1p:j2m) = old(i1p,j1p:j2m)

        ! left and right
        old(i1p:i2m,j1) = old(i1p:i2m,j2m)
        old(i1p:i2m,j2) = old(i1p:i2m,j1p)

        ! corners
        old(i1,j1) = old(i2m,j2m)
        old(i1,j2) = old(i2m,j1p)
        old(i2,j1) = old(i1p,j2m)
        old(i2,j2) = old(i1p,j1p)

     else

!TODO: send and receive top and bottom rows 
        call mpi_isend( ... )
        call mpi_irecv( ... )
        call mpi_isend( ... )
        call mpi_irecv( ... )

!TODO: send and receive left and right columns 
        call mpi_isend( ... )
        call mpi_irecv( ... )
        call mpi_isend( ... )
        call mpi_irecv( ... )

!TODO: send and receive corners 
        call mpi_isend( ... )
        call mpi_irecv( ... )
        call mpi_isend( ... )
        call mpi_irecv( ... )
        call mpi_isend( ... )
        call mpi_irecv( ... )
        call mpi_isend( ... )
        call mpi_irecv( ... )

!TODO: make sure all non-blocking messages have arrived
        do i = 1, 16
           call mpi_wait( ... )
        enddo

     endif  ! nprocs

     ! update states of cells

     do j = j1p, j2m
        do i = i1p, i2m

           ip = i + 1
           im = i - 1
           jp = j + 1
           jm = j - 1
           nsum =  old(im,jp) + old(i,jp) + old(ip,jp) &
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
     old(i1p:i2m,j1p:j2m) = new(i1p:i2m,j1p:j2m)

  enddo time_iteration

  ! Iterations are done; sum the number of live cells
  
  isum = sum(new(i1p:i2m,j1p:j2m))
  
!TODO: Reduce partial sums 
  ! Print final number of live cells

  if(nprocs > 1) then
     isumloc = isum
     call mpi_reduce( ... )
  endif

  if(myid == 0) then
     write(*,"(/'Number of live cells = ', i6/)") isum
  endif

  deallocate(old, new)

  call mpi_finalize(ierr)

end program life
