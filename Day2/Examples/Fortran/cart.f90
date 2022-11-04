!
! A 2d cylinder of 12 processes in a 4x3 cartesian grid
!
program cartcreate
  use mpi_f08
  implicit none
  integer :: myRank, uniSize, ierror
  integer :: ndims=2, dims(2)=(/4, 3/)
  logical :: periods(2)=(/1, 0/), reorder=1
  integer :: coords(2), coordRank
  Type(MPI_Comm) :: comm_cart

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank, ierror)

  if(uniSize .eq. 12) then
    call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, comm_cart, ierror)

    if(myRank .eq. 5) then
        call MPI_Cart_coords(comm_cart, myRank, ndims, coords, ierror)
        write(6, '(a, i2, a, i2, i2)'), 'Rank ', myRank, ' coordinates are ', coords(0), coords(1)     
    endif
    if(myRank .eq. 0) then
        coords(0)=3 
        coords(1)=1
        call MPI_Cart_rank(comm_cart, coords, coordRank, ierror)
        write(6, '(a, i2, i2, a, i2)'), 'The processor at position ', coords(0), coords(1), ' has rank ', coordRank    
    endif
    
  endif

  call MPI_Finalize(ierror)

end program cartcreate

