!
! Split processes with odd and even ranks into 2 communicators  
!
program mpisplit
  use mpi_f08
  implicit none
  integer :: myRank, uniSize, ierror
  integer :: color, key, newRank, summ
  TYPE(MPI_Comm) :: newcomm

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)

  if(mod(myRank, 2) .eq. 0) then
    color=1
    key=myRank
  else
    color=2
    key=uniSize-myRank
  end if

  call MPI_Comm_split(MPI_COMM_WORLD, color, key, newcomm, ierror);
  call MPI_Comm_rank(newcomm, newRank, ierror);
  call MPI_Allreduce(newRank, summ, 1, MPI_INTEGER, MPI_SUM, newcomm, ierror);

    write(6, '(a,i2,a,i2, a)'), 'I am process ', myRank, ' in MPI_COMM_WORLD and ', newRank, ' in the new communicator.'
    write(6, '(a,i3,i3)'), 'Color and Sum of ranks in the new communicator: ', color, summ 

  call MPI_Finalize(ierror)
end program mpisplit
