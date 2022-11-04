!
! An example of iprobe with 2 processes.  
!
program iprobe
  use mpi_f08
  implicit none
  integer :: myRank, ierror, i, tag
  logical :: flag=0
  integer :: a(5)
  TYPE(MPI_Status) :: status
  TYPE(MPI_Request) :: request

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  if (myRank .eq. 0) then
    a=(/2345,654,96574,-12,7676/)
    tag=myRank
    write(*,*), 'Process', myRank, 'sending the message.'
    call MPI_Issend(a, 5, MPI_INTEGER, 1, 100, MPI_COMM_WORLD, request, ierror)
    call MPI_Wait(request,status,ierror)
  else if (myRank .eq. 1) then
    do while(flag .eq. 0)
      call MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, flag, status, ierror);
      write(*,*), 'After MPI_Iprobe, flag=', flag
    end do
    call MPI_Recv(a, 5, MPI_INTEGER, status%MPI_SOURCE, status%MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    write(*,*), 'Process', myRank, ': message received.'
    do i=1, 5
      write(*,*), 'a(', i, ')=', a(i)
    end do
  endif

  call MPI_Finalize(ierror)
end program iprobe
