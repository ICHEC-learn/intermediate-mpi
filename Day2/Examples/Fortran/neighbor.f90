!
! A 1d ring example with MPI_Cart_shift and MPI_Neighbor_alltoall
!
program neighbour
  use mpi_f08
  implicit none
  integer :: uniSize, ierror, i, sum=0, commRank, prevRank, nextRank
  integer :: ndims, dims(1)
  logical :: periods(1), reorder
  TYPE(MPI_Comm) :: comm_cart
  integer :: sendbuf(2), recvbuf(2)

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
  
  sendbuf(2) = commRank;
  DO i=1, uniSize
    !Issend-Recv-Wait replaced with  MPI_Neighbor_alltoall
    !recvbuf[0] as recvbuf and sendbuf[1] as sendbuf
    CALL MPI_Neighbor_alltoall(sendbuf, 1, MPI_INTEGER, recvbuf, 1, MPI_INTEGER, comm_cart, ierror)
    sendbuf(2) = recvbuf(1)
    sum = sum + recvbuf(1)
  END DO
  
  print*, "Rank ", commRank, ": ", sum

  call MPI_Finalize(ierror)
end program neighbour
