PROGRAM ring

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the High Performance           !
! Computing Centre Stuttgart (HLRS).                           !
! The examples are based on the examples in the MPI course of  !
! the Edinburgh Parallel Computing Centre (EPCC).              !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! HLRS and EPCC take no responsibility for the use of the      !
! enclosed teaching material.                                  !
!                                                              !
! Authors: Joel Malard, Alan Simpson,            (EPCC)        !
!          Rolf Rabenseifner, Traugott Streicher (HLRS)        !
!                                                              !                           !
!                                                              !
! Purpose: A program to try persistent comms.                  !
!                                                              !
! Contents: F-Source                                           !
!                                                              !
!==============================================================!

  USE mpi_f08

  IMPLICIT NONE

  INTEGER, PARAMETER :: tag=201

  INTEGER :: my_rank, uniSize

  INTEGER :: right, left

  INTEGER (kind=4) :: i, iter, total, n, answer

  INTEGER (kind=4), ASYNCHRONOUS :: snd_buf, rcv_buf

  DOUBLE PRECISION :: Tstart, Tend

  CHARACTER (LEN=20) :: argv

  TYPE(MPI_Status),  DIMENSION(2), ASYNCHRONOUS :: arr_status

  TYPE(MPI_Request), DIMENSION(2), ASYNCHRONOUS :: arr_request

  INTEGER(KIND=MPI_ADDRESS_KIND) :: iadummy


  CALL MPI_Init()

  CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
  CALL MPI_Comm_size(MPI_COMM_WORLD, uniSize)

  right = modulo(my_rank+1,uniSize)
  left  = modulo(my_rank-1,uniSize)

  if (command_argument_count() .EQ. 0) then
    write(6,*) ' Program needs the number of times the message is passed around the ring'
    write(6,*) ' E.g.   ./prog N '
    call MPI_ABORT(MPI_COMM_WORLD,10)
  endif

! Get number of times to sendmessage around the ring
  call get_command_argument(1,argv)
  read(argv,fmt='(i20)') n



! Check result is correct
  answer = uniSize*(uniSize-1)/2


  Tstart = MPI_Wtime()

! Setup persisten communications


  DO iter = 1,n

    total = 0
    snd_buf = my_rank

    DO i = 1, uniSize

! Send messages



! evaluate sum

    END DO

    IF (total .NE. answer) then
       write(6,*) ' Calculation incorrect '
       call MPI_Abort(MPI_COMM_WORLD,15)
    ENDIF

  END DO

! Destroy handles

  Tend = MPI_Wtime()

  if (my_rank .EQ. 0) write(6,*) ' Time taken is ',Tend-Tstart


  CALL MPI_Finalize()

END PROGRAM
