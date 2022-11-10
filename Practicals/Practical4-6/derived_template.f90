!
! Create derived datatypes from 2d matrix
!
! COMPILE: mpifort -o derived derived.f90
! RUN: mpirun -np 2 ./derived
! (Run with 2 processors)
!

program derived
  use mpi_f08
  implicit none
  integer (kind=4), parameter :: N=10,M=10
  integer (kind=4) :: i,j,ierror,uniSize,myRank
  integer (kind=4) :: src,dest,tag
  integer (kind=4), target :: A(N,M)
  Type(MPI_Datatype) :: matRow, matCol, subMat
  integer (kind=4), pointer :: ptr

  call MPI_Init( ierror )
  if (ierror .ne. 0) stop ' Cannot initialize MPI '
  ! Find our world rank & size to print it
  call MPI_Comm_rank( MPI_COMM_WORLD, myRank, ierror )
  call MPI_Comm_size( MPI_COMM_WORLD, uniSize, ierror )
  if ( uniSize .le. 1 ) then
    write(6,*) 'Error: must be run on more that 1 process'
    call MPI_Abort( MPI_COMM_WORLD, 10, ierror )
  endif

!TODO
  call MPI_Type_vector( ... )
  call MPI_Type_commit( ... )
  call MPI_Type_vector( ... )
  call MPI_Type_commit( ... )
  call MPI_Type_vector( ... )
  call MPI_Type_commit( ... )

  do j = 1,M
    do i = 1,N
      A(i,j) = myRank;
    end do
  end do

  ! Will only send messages from 0 to 1
  src = 0; dest = 1; tag = 5;

  ! Send the 0th row
  if ( myRank .eq. 0 ) then
    ptr => A(1,1)
    call MPI_Send( ... )
  else if ( myRank .eq. 1 ) then
    ptr => A(1,1)
    call MPI_Recv( ... );
  endif

  ! Let's print the matrix on rank 1 for illustration
  ! This also resets the matrix
  if ( myRank .eq. 1 ) then
    write(6,*) 'Should have received the first row:'
    do i = 1,N
      do j = 1,M
        write(6,'(i4)',advance='NO') A(i,j)
        A(i,j) = myRank
      end do
        write(6,*)
    end do
    write(6,*)
  endif

!TODO
  ! Send the 0th column
  if ( myRank .eq. 0 ) then
    ptr => A(1,1)
    call MPI_Send( ... )
  else if ( myRank .eq. 1 ) then
     ptr => A(1,1)
    call MPI_Recv( ... )
  endif

  ! Let's print the matrix on rank 1 for illustration
  ! This also resets the matrix
  if ( myRank .eq. 1 ) then
    write(6,*) 'Should have received the 1st column:'
    do  i = 1,N
      do j = 1,M
        write(6,'(i4)',advance='NO') A(i,j)
        A(i,j) = myRank
      end do
      write(6,*)
    end do
    write(6,*)
  endif

!TODO
  ! Send the 0th column, but receive into the 4th column
  if ( myRank .eq. 0 ) then
    ptr => A(1,1)
    call MPI_Send( ... )
  else if ( myRank .eq. 1 ) then
    ptr => A(1,5)
    call MPI_Recv( ... )
  endif

  ! Let's print the matrix on rank 1 for illustration
  ! This also resets the matrix
  if ( myRank .eq. 1 ) then
    write(6,*) 'Should have received into the 5th column:'
      do  i = 1,N
        do j = 1,M
          write(6,'(i4)',advance='NO')  A(i,j)
          A(i,j) = myRank
        end do
        write(6,*)
      end do
    write(6,*)
  endif

!TODO
  ! Send a submatrix
  if ( myRank .eq. 0 ) then
    ptr => A(1,1)
    call MPI_Send( ... )
  else if ( myRank .eq. 1 ) then
    ptr => A(1,1)
    call MPI_Recv( ... )
  endif

  ! Let's print the matrix on rank 1 for illustration
  ! This also resets the matrix
  if ( myRank .eq. 1 ) then
    write(6,*) 'Should have received into the top left corner:'
    do i = 1,N 
      do  j = 1,M 
        write(6,'(i4)',advance='NO') A(i,j)
        A(i,j) = myRank
      end do
      write(6,*)
    end do
    write(6,*)
  endif

!TODO
  MPI_Type_free( ... );
  MPI_Type_free( ... );
  MPI_Type_free( ... );

  call MPI_Finalize(ierror)
end program derived
