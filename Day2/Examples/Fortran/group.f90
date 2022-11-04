!
! Create a group by including processes with odd ranks from another group
!
program mpigroup
  use mpi_f08
  implicit none
  integer :: uniSize, ierror
  integer :: odd_ranks(3)=(/ 1, 3, 5 /), newRank, oddRank
  TYPE(MPI_Group) :: new_group, odd_group
  TYPE(MPI_Comm) :: odd_comm

  call MPI_Init(ierror)
  call MPI_Comm_size(MPI_COMM_WORLD,uniSize,ierror)

  if (uniSize .eq. 6) then
    call MPI_Comm_group(MPI_COMM_WORLD, new_group, ierror);
    call MPI_Group_rank(new_group, newRank, ierror);
    call MPI_Group_incl(new_group, uniSize/2, odd_ranks, odd_group, ierror);
    call MPI_Comm_create(MPI_COMM_WORLD, odd_group, odd_comm, ierror);
    call MPI_Group_rank(odd_group, oddRank, ierror);

    if(oddRank .ne. MPI_UNDEFINED) then
      write(6, '(a,i2,a,i2, a)'), 'I am process ', newRank, ' in new group and ', oddRank, ' in the odd group.'
    else
      write(6, '(a,i2,a)'), 'I am process ', newRank, ' in new group but I am not part of the odd group.' 
    endif
  endif

  call MPI_Finalize(ierror)
end program mpigroup
