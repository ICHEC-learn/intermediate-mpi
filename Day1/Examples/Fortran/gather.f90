!
! Gathering rank ids from all processes at root
!
program gather
    use mpi
    implicit none
    integer (kind=4) :: i,ierror, world_rank, world_size
    integer, allocatable :: buf(:)
  
    call MPI_Init( ierror )
    if (ierror .ne. 0) stop ' Cannot initialize MPI '
    call MPI_Comm_rank( MPI_COMM_WORLD, world_rank, ierror )
    call MPI_Comm_size( MPI_COMM_WORLD, world_size, ierror )

    allocate(buf(0:world_size-1))

    call MPI_Gather( world_rank, 1, MPI_INTEGER, buf, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror )
    if ( world_rank .eq. 0 )  then
            write(6,'(a,i0,a,i0)') 'I am process ',world_rank,' of ',world_size
        do i = 0,world_size-1
            write(6,'(i2)',advance='NO')  buf(i)
        end do
        write(6,*)
    endif

    call MPI_Finalize(ierror);
end program gather
