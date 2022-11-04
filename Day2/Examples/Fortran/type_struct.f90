!
! A 1d ring example with MPI_Type_struct
!
program typestruct
  use mpi_f08
  implicit none

  type buf
     sequence
     real (kind=4) :: dblrank
     integer :: intrank
  end type buf
  type(buf) :: sBuf, rBuf, sum

  integer :: myRank, uniSize, ierror, i
  integer :: dest, src
  TYPE(MPI_Status) :: status
  TYPE(MPI_Request) :: request
  integer :: count, array_of_blocklengths(2)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: array_of_displacements(2), intaddress, dbladdress
  TYPE(MPI_Datatype) :: array_of_types(2), newtype

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  count=2
  array_of_blocklengths(1) = 1
  array_of_blocklengths(2) = 1
  call MPI_Get_address(sBuf%dblrank, dbladdress, ierror)
  call MPI_Get_address(sBuf%intrank, intaddress, ierror)
  array_of_displacements(1) = 0
  array_of_displacements(2) = MPI_Aint_diff(intaddress, dbladdress);
  array_of_types(1) = MPI_REAL
  array_of_types(2) = MPI_INTEGER

  call MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierror)
  call MPI_Type_commit(newtype, ierror)

  dest = modulo(myRank+1,uniSize)
  src  = modulo(myRank-1,uniSize)

  sBuf%dblrank=real(myRank)
  sBuf%intrank=myRank
  sum%dblrank=0.0
  sum%intrank=0

  do i=1, uniSize
    call MPI_Isend(sBuf, 2, newtype, dest, 100, MPI_COMM_WORLD, request, ierror)
    call MPI_Recv(rBuf, 2, newtype, src, 100, MPI_COMM_WORLD, status, ierror)
    call MPI_Wait(request, status, ierror)
    sBuf=rBuf
    sum%dblrank = sum%dblrank+rBuf%dblrank
    sum%intrank = sum%intrank+rBuf%intrank
  end do 
  
  print*, "Rank ", myRank, ", double sum: ", sum%dblrank, ", int sum", sum%intrank
  call MPI_Type_free(newtype, ierror)

  call MPI_Finalize(ierror)
end program typestruct
