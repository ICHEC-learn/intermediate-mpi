!------------------------------------
!  Conway Game of Life

! 4 processors, domain decomposition
! in both i and j directions, virtual
! topology

!------------------------------------
program life
  use mpi_f08
  implicit none

  integer (kind=4), parameter :: ni=200, nj=200, nsteps=500, rank=1
  integer :: ndims,dim_size(2),coords(2),corner(2),uniSize,nsdir,ewdir
  integer :: centre,north,south,east,west,iproc_nw,iproc_ne,iproc_sw,iproc_se
  integer :: seed_size
  integer (kind=4) :: isum,isumloc,ninom,njnom
  integer (kind=4) :: i,j,ip,jp,im,jm,n,nsum
  integer, allocatable :: rnd_seed(:), row(:)
  integer (kind=4), allocatable, target, dimension(:,:) :: old, new
  real, allocatable, dimension(:) :: arand
  logical :: reorder, periods(2)
  type(MPI_Comm) :: comm_cart
  type(MPI_Datatype) :: row_hdle, col_hdle
  type(MPI_Request) :: req(16)


  call MPI_Init()
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize)
  if (uniSize .NE. 4) then
    write(6,*) ' Must have 4 MPI processes exactly '
    call MPI_Abort(MPI_COMM_WORLD,10)
  endif


 
  ! nominal number of points per proc. (without ghost cells,
  ! assume numbers divide evenly); dim_size(1) and dim_size(2)
  ! are the numbers of procs in the i and j directions.


  dim_size(1) = 2
  dim_size(2) = 2

  ninom = ni/dim_size(1)
  njnom = nj/dim_size(2)


!TODO: Create virtual topology
  ! presently set up for only 4 procs
  ! periodic in all directions

  ! coordinate directions for north, south, east, and west
  nsdir = 0
  ewdir = 1

  ndims = ...
  periods(1) = ....
  periods(2) = ...
  reorder = ...
  call mpi_cart_create(...)
  call MPI_Comm_rank(...)
  call mpi_cart_shift(...)
  call mpi_cart_shift(...)



!TODO: Define processors on corners
  ! Virtual topology has been used to define adjacent processors
  ! to the north, south, east, and west; still have to define
  ! processors on corners

  call MPI_cart_coords(...)
! North-west
  corner(1) = modulo(coords(1)+1,2); corner(2) = modulo(coords(2)-1,2); 
  call mpi_cart_rank(...)

! In this case corners are all the same rank
  iproc_ne = iproc_nw
  iproc_sw = iproc_nw
  iproc_se = iproc_nw


  if (centre .EQ. 0) write(6,*) ' Neighbours ',north,south,east,west  
  ! allocate arrays

  allocate(old(0:ninom+1,0:njnom+1),new(0:ninom+1,0:njnom+1))
  allocate(arand(ni),row(njnom))

  old = 0; new = 0;


  ! Initialize elements of old to 0 or 1.  We're doing some
  ! sleight of hand here to make sure we initialize to the
  ! same values as in the serial case. The random_number
  ! function is called for every i and j, even if they are
  ! not on the current processor, to get the same random
  ! distribution as the serial case, but they are only used
  ! if this i and j reside on the current procesor.


  call random_seed(SIZE=seed_size)
  allocate(rnd_seed(seed_size))
  call random_seed(GET=rnd_seed)

  rnd_seed(1) = rnd_seed(1) - coords(2)*ninom
  do j = 1, njnom
    rnd_seed(1) = rnd_seed(1) - 1
    call random_seed(PUT=rnd_seed)
    call random_number(arand)
    do i = 1,ninom 
      old(i,j) = nint(arand(coords(1)*ninom+i))
    end do
  enddo


  isum = sum(old(1:ninom,1:njnom))
  write(6,*) ' Initial sum ',centre,isum
  isumloc = isum
  isum = 0
  call mpi_reduce(isumloc,isum,1,MPI_INTEGER4,MPI_SUM,0,comm_cart)
  if (centre .EQ. 0) write(6,*) ' Init total ',isum


! TODO: Create derived type for single row of array.
  ! There are nj "blocks," each containing 1 element,
  ! with a stride of nitot between the blocks

  call mpi_type_vector( ... )
  call mpi_type_commit(...)

  call mpi_type_vector(...)
  call mpi_type_commit(...)


  !  iterate

  time_iteration: do n = 1, nsteps

     ! transfer data to ghost cells



!TODO: send and receive top and bottom rows 
        call mpi_isend(...)          ! row=1, col=1
        call mpi_irecv(...)          ! row=0, col=1
        call mpi_isend(...)      ! row=ninom, col=1
        call mpi_irecv(...)    ! row=ninom+1, col=1

!TODO: send and receive left and right columns 
        call mpi_isend(...)           ! row=1,col=1
        call mpi_irecv(...)           ! row=0, col=1
        call mpi_isend(...)       ! row=1, col=njnom
        call mpi_irecv(...)     ! row=1, col=njnom+1

!TODO: send and receive corners 
        call mpi_isend(...)
        call mpi_irecv(...)
        call mpi_isend(...)
        call mpi_irecv(...)
        call mpi_isend(...)
        call mpi_irecv(...)
        call mpi_isend(...)
        call mpi_irecv(...)

!TODO: make sure all non-blocking messages have arrived
        call mpi_waitall(16,req,MPI_STATUSES_IGNORE)


     ! update states of cells

     do j = 1, njnom
        do i = 1, ninom

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
     old = new


  enddo time_iteration


  ! Iterations are done; sum the number of live cells
  
  isum = sum(new(1:ninom,1:njnom))
  
  ! Print final number of live cells

  isumloc = isum
  isum = 0
  call mpi_reduce(isumloc,isum,1,MPI_INTEGER4,MPI_SUM,0,comm_cart)

  if(centre == 0) then
     write(*,"(/'Number of live cells = ', i6/)") isum
  endif

  call MPI_Type_free(row_hdle)
  call MPI_Type_free(col_hdle)

  deallocate(old, new, arand)


  call MPI_Comm_free(comm_cart)
  call mpi_finalize()

end program life
