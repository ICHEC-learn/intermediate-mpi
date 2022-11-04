!
! Rank 0 reads in to a buffer and does the broadcast
!
program broadcast
    use mpi
    implicit none
    integer (kind=4) :: ierror, world_rank, world_size, buf

    call MPI_Init( ierror )
    if (ierror .ne. 0) stop ' Cannot initialize MPI '
    call MPI_Comm_rank( MPI_COMM_WORLD, world_rank, ierror )
    call MPI_Comm_size( MPI_COMM_WORLD, world_size, ierror )

    if ( world_rank .eq. 0 ) then
        write(6,*) ' Please insert a number'
        read(5,*) buf
    endif
    call MPI_Bcast( buf, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror )
    if (ierror .ne. 0) write(6,*) ' Problem with broadcast '
    write(6,'(a,i0,a,i0,a,i0)') 'I am process ',world_rank,' of ',world_size,' and buf is ',buf

    call MPI_Finalize(ierror);
end program broadcast
