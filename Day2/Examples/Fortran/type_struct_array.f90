!
! A 1d ring example with MPI_Type_struct and array of structs
!
program typestructarray
  use mpi_f08
  implicit none

  type buf
     sequence
     real(kind=4) :: dblrank
     integer(kind=4) :: intrank
  end type buf
  type(buf)  :: sBuf(2), rBuf(2), sum(2)

  integer :: myRank, uniSize, ierror, i, j
  integer :: dest, src
  TYPE(MPI_Status) :: status
  TYPE(MPI_Request) :: request
  integer :: count, array_of_blocklengths(2)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: array_of_displacements(2), intaddress, dbladdress
  TYPE(MPI_Datatype) :: array_of_types(2), newtype
  integer :: bufsize
  INTEGER(KIND=MPI_ADDRESS_KIND) :: buflb, bufextent, buftrueextent

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  count=2
  array_of_blocklengths(1) = 1
  array_of_blocklengths(2) = 1
  call MPI_Get_address(sBuf(1)%dblrank, dbladdress, ierror)
  call MPI_Get_address(sBuf(1)%intrank, intaddress, ierror)
  array_of_displacements(1) = 0
  array_of_displacements(2) = MPI_Aint_diff(intaddress, dbladdress);
  array_of_types(1) = MPI_REAL
  array_of_types(2) = MPI_INTEGER

  call MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierror)
  call MPI_Type_commit(newtype, ierror)

  if(myRank .eq. 0) then
    call MPI_Type_size(newtype, bufsize, ierror);
    call MPI_Type_get_extent(newtype, buflb, bufextent, ierror);
    call MPI_Type_get_true_extent(newtype, buflb, buftrueextent, ierror);
    print*,'Size= ', bufsize, ' Extent= ', INT(bufextent), ' True extent= ', INT(buftrueextent)
  endif

  dest = modulo(myRank+1,uniSize)
  src  = modulo(myRank-1,uniSize)
  
  do i=1, 2
    sBuf(i)%dblrank=real((i)*myRank)
    sBuf(i)%intrank=(i)*myRank
    sum(i)%dblrank=0.0
    sum(i)%intrank=0
  end do

  do i=1, uniSize
    call MPI_Isend(sBuf, 2, newtype, dest, 100, MPI_COMM_WORLD, request, ierror)
    call MPI_Recv(rBuf, 2, newtype, src, 100, MPI_COMM_WORLD, status, ierror)
    call MPI_Wait(request, status, ierror)
    sBuf=rBuf
    do j=1, 2
      sum(j)%dblrank = sum(j)%dblrank+rBuf(j)%dblrank
      sum(j)%intrank = sum(j)%intrank+rBuf(j)%intrank
    end do
  end do 

  do i=1, 2  
    print*, "Rank ", myRank, ", double sum: ", sum(i)%dblrank, ", int sum", sum(i)%intrank
  end do

  call MPI_Type_free(newtype, ierror)

  call MPI_Finalize(ierror)
end program typestructarray
