!
! OnePing: P0 sends a message to P1 with wrong dest rank
!
program onePing
  use mpi_f08
  implicit none
  integer :: myRank, ierror, i, ierror2, errclass, errbuflen
  integer :: arr(5)
  TYPE(MPI_Status) :: status
  character(len=MPI_MAX_ERROR_STRING) :: errbuffer

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)
  call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierror); 

  if (myRank .eq. 0) then
    arr=(/2345,654,96574,-12,7676/)
    write(*,*) 'I am ', myRank, ' before send ping'
    call MPI_Send(arr, 5, MPI_INTEGER, 10, 100, MPI_COMM_WORLD, ierror)
    if(ierror .ne. MPI_SUCCESS) then
      call MPI_Error_class(ierror, errclass, ierror2);
      if(errclass .eq. MPI_ERR_RANK) then
        write(6, *), 'Invalid rank used in MPI send call'
        call MPI_Error_string(ierror, errbuffer, errbuflen, ierror2);
        write(6, *), errbuffer
        call MPI_Abort(MPI_COMM_WORLD, 0, ierror2);
      end if
    end if
  else if (myRank .eq. 1) then
    call MPI_Recv(arr, 5, MPI_INTEGER, 0, 100, MPI_COMM_WORLD, status, ierror)
    write(*,*) 'I am ', myRank, ' after  recv ping'
    do i=1, 5
      write(*,*) 'arr(', i, ')=', arr(i)
    end do
  endif

  call MPI_Finalize(ierror)
end program onePing
