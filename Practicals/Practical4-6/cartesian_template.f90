!
! Create a Cartesian virtual topology, map the matrix on it
! and calculate sum of its elemements
!
! COMPILE: mpifort -o cartesian cartesian.f90
! RUN: mpirun -np 20 ./cartesian
! (Run with 20 processors)
!
program cartesian
  use mpi_f08
  implicit none
  integer (kind=4), parameter  :: ndims=2, M=10, N=10
  integer (kind=4) :: i,j,k,ierror,matsize,localsum,globalsum
  integer (kind=4) :: myRank, cartRank
  integer (kind=4) :: uniSize, cartSize
  integer (kind=4) :: prevRank, nextRank
  Type(MPI_Comm) :: Comm_cart
  
  integer (kind=4), dimension(ndims) :: dims
  logical:: periods(2), reorder
  integer (kind=4) :: A(N,M), randvec(N*M)
  real (kind=4) :: rvec(N*M)

  call MPI_Init( ierror )
  if (ierror .NE. 0) then
    write(6,*) ' Cannot start MPI '
    call MPI_Abort(MPI_COMM_WORLD,100,ierror)
  endif
  ! Get the world size and rank
  call MPI_Comm_size( MPI_COMM_WORLD, uniSize, ierror )
  call MPI_Comm_rank( MPI_COMM_WORLD, myRank, ierror ) 

  !TODO: Create a division of processors in a cartesian grid
   

  ! TODO: Both dimensions will be periodic
 
  ! TODO: Create a new virtual topology
 
  ! TODO: Find the new size and rank


  ! Print some information about our rank
  write(6,'(a,i4,a,i4,a,i4,a,i4,a)') 'I am process ',myRank,' of ', uniSize,' on comm_world and ', &
           cartRank,' of ',cartSize,' on comm_cart.'

  ! TODO: Compute the neighbours for each process
  

  ! Allocate the submatrix for each process
  matsize = M*N
    
  ! Rank 0 creates the entire matrix
  if (myRank .EQ. 0) then
    call random_seed
    call random_number(rvec)
    forall (i=1:matsize) randvec(i) = nint(rvec(i))
  endif

  !TODO:
  call MPI_Bcast( ... )

 
  ! Initialise sub-matrix with same random vector
  k = 0;
  do j=1,M
    do i=1,N
      A(i,j) = randvec(k)
      k = k + 1
    end do
  end do

  ! Alternative approach
  ! ! Initialise sub-matrix with same random vector
  ! k = 0;
  ! do j=1,M
  !   do i=1,N
  !     A(i,j) = randvec(k)
  !     k = k + 1
  !   end do
  ! end do

  ! Sum the local part of the array
  localsum = sum(A)


  ! Print the local sum on each process
  write(6,'(a,i4,a,i4,a,i4)') 'I am process ',myRank,' of ',uniSize,' and sum is ',localsum

  ! TODO: Reduce the global sum on process 0
  call MPI_Reduce( ... )

  ! Print this global sum
  if ( myRank .eq. 0 ) then
    write(6,*)  'Global sum is ',globalSum
  endif

  call MPI_Finalize(ierror);

end program cartesian

