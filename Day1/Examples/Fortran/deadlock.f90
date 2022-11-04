!
! An example of deadlock with MPI_Ssend
!
program deadlock
  use mpi_f08
  implicit none
  integer :: myRank, ierror, i
  integer :: a(5), b(5)
  TYPE(MPI_Status) :: status

  call MPI_Init(ierror)
  call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierror)

  if (myRank .eq. 0) then
    a=(/2345,654,96574,-12,7676/)
    call MPI_Ssend(a, 5, MPI_INTEGER, 1, 100, MPI_COMM_WORLD, ierror)
    call MPI_Recv(b, 5, MPI_INTEGER, 1, 101, MPI_COMM_WORLD, status, ierror)
    do i=1, 5
      write(*,*) 'b(', i, ')=', b(i)
    end do
  else if (myRank .eq. 1) then
    b=(/-2345,-654,-96574,12,-7676/)
    call MPI_Ssend(b, 5, MPI_INTEGER, 0, 101, MPI_COMM_WORLD, ierror)
    call MPI_Recv(a, 5, MPI_INTEGER, 0, 100, MPI_COMM_WORLD, status, ierror)
    do i=1, 5
      write(*,*) 'a(', i, ')=', a(i)
    end do
  endif

  call MPI_Finalize(ierror)
end program deadlock
