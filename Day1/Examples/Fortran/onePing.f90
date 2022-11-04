!
! OnePing: Process 0 sends a message to process 1
!
program onePing
  use mpi_f08
  implicit none
  integer :: myRank, ierror, i
  integer :: arr(5)
  TYPE(MPI_Status) :: status

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  if (myRank .eq. 0) then
    arr=(/2345,654,96574,-12,7676/)
    write(*,*) 'I am ', myRank, ' before send ping'
    call MPI_Send(arr, 5, MPI_INTEGER, 1, 100, MPI_COMM_WORLD, ierror)
  else if (myRank .eq. 1) then
    call MPI_Recv(arr, 5, MPI_INTEGER, 0, 100, MPI_COMM_WORLD, status, ierror)
    write(*,*) 'I am ', myRank, ' after  recv ping'
    do i=1, 5
      write(*,*) 'arr(', i, ')=', arr(i)
    end do
  endif

  call MPI_Finalize(ierror)
end program onePing
