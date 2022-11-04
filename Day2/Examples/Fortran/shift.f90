!
! A 1d ring example with MPI_Cart_shift and MPI_Sendrecv
!
program mpishift
  use mpi_f08
  implicit none
  integer :: uniSize, ierror, i, commRank, prevRank, nextRank
  integer :: sbuf(2),rbuf(2)
  integer :: ndims, dims(1)
  logical :: periods(1), reorder
  TYPE(MPI_Comm) :: comm_cart
  TYPE(MPI_Status) :: status

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)

  ndims=1
  dims(1) = uniSize
  periods(1) = 1
  reorder=1
  CALL MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, comm_cart, ierror)
  CALL MPI_Comm_rank(comm_cart, commRank, ierror)

  CALL MPI_Cart_shift(comm_cart, 0, 1, prevRank, nextRank, ierror)
  print*, "Rank ", commRank, ": rank prev = ", prevRank, ", rank next = ", nextRank
 
  sBuf = (/commRank, 0/) 
  rBuf = (/-1, 0/)
  do while ( rBuf(1) .ne. commRank ) 
    CALL MPI_Sendrecv(sBuf, 2, MPI_INTEGER, nextRank, 100, rBuf, 2, MPI_INTEGER, prevRank, 100, comm_cart, status, ierror)
    sBuf(2) = sBuf(2) +  rBuf(1)
    sBuf(1) = rBuf(1)
  end do 
  
  print*, "Rank ", commRank, ", sum: ", sBuf(2)

  call MPI_Finalize(ierror)
end program mpishift
