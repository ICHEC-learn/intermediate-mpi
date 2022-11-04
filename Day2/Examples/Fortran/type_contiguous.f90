!
! A 1d ring example with MPI_Type_contiguous
!
program typecontiguous
  use mpi_f08
  implicit none
  integer :: myRank, uniSize, ierror, i
  integer :: sbuf(2),rbuf(2), dest, src
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: newtype

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  sBuf = (/myRank, 0/) 
  rBuf = (/-1, 0/)

  dest = modulo(myRank+1,uniSize)
  src  = modulo(myRank-1,uniSize)

  call MPI_Type_contiguous(2, MPI_INTEGER, newtype, ierror)
  call MPI_Type_commit(newtype, ierror)

  do while ( rBuf(1) .ne. myRank ) 
    call MPI_Sendrecv(sBuf, 1, newtype, dest, 100, rBuf, 1, newtype, src, 100, MPI_COMM_WORLD, status, ierror)
    sBuf(2) = sBuf(2) +  rBuf(1)
    sBuf(1) = rBuf(1)
  end do 
  
  print*, "Rank ", myRank, ", sum: ", sBuf(2)
  call MPI_Type_free(newtype, ierror)

  call MPI_Finalize(ierror)
end program typecontiguous
