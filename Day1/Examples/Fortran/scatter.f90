!
! Scatter rows of the 4x4 matrix to 4 processors
!
program scatter
    use mpi
    implicit none
    integer (kind=4) :: ierror, world_rank, world_size,SIZE
    parameter(SIZE=4)
    integer (kind=4) :: sendbuf(SIZE, SIZE), recvbuf(SIZE)
    data sendbuf /1, 2, 3, 4, &
                  5, 6, 7, 8, &
                  9, 10, 11, 12, &
                  13, 14, 15, 16 /
  
    call MPI_Init( ierror )
    if (ierror .ne. 0) stop ' Cannot initialize MPI '
    call MPI_Comm_rank( MPI_COMM_WORLD, world_rank, ierror )
    call MPI_Comm_size( MPI_COMM_WORLD, world_size, ierror )

    if ( world_size .eq. SIZE )  then
      call MPI_Scatter(sendbuf, SIZE, MPI_INTEGER, recvbuf, SIZE, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror )
       write(6, *) 'rank ',world_rank,' Results ', recvbuf
    endif

    call MPI_Finalize(ierror);
end program scatter
