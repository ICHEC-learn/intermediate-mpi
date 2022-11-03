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


program ring_pack
  use mpi_f08 
  implicit none
  integer, parameter :: tag=201
  integer :: my_rank, uniSize, right, left, pos, buf_size
  integer (kind=4) :: i, iter, message, answer, n

  integer (kind=4), asynchronous, allocatable :: snd_buf(:), rcv_buf(:)

  double precision :: Tstart, Tend;

  character (len=20) :: argv

  TYPE(MPI_Status),  DIMENSION(2), ASYNCHRONOUS :: arr_status

  TYPE(MPI_Request), DIMENSION(2), ASYNCHRONOUS :: arr_request


  call MPI_Init()

  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
  call MPI_Comm_size(MPI_COMM_WORLD, uniSize)

  right = modulo(my_rank+1,uniSize)
  left  = modulo(my_rank-1,uniSize)


! Get number of times to send message around the ring
  if (command_argument_count() .EQ. 0) then
    write(6,*) ' Program needs the number of times the message is passed around the ring'
    write(6,*) ' E.g.   ./prog N '
    call MPI_ABORT(MPI_COMM_WORLD,10)
  endif

! Get number of times to sendmessage around the ring
  call get_command_argument(1,argv)
  read(argv,fmt='(i20)') n
  buf_size = n*4
  allocate(snd_buf(n))
  allocate(rcv_buf(n))

! Check result is correct
  answer = uniSize*(uniSize-1)/2


! Start timer
  Tstart = MPI_Wtime()


  message = 0

! Send the packed messages around the ring
  do i = 1,uniSize

! Pack messages into single snd_buf
    pos = 0
    do iter=1,n
      call MPI_Pack(message,1,MPI_INTEGER4,snd_buf,buf_size,pos,MPI_COMM_WORLD);
    end do
    call MPI_Irecv(rcv_buf, pos, MPI_PACKED, left, tag, MPI_COMM_WORLD, arr_request(1))

    call MPI_Issend(snd_buf, pos, MPI_PACKED, right, tag, MPI_COMM_WORLD, arr_request(2))

    call MPI_Waitall(2, arr_request, arr_status)

    IF (.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING) CALL MPI_F_sync_reg(rcv_buf)
    IF (.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING) CALL MPI_F_sync_reg(snd_buf)

! Unpack the messages and add them to total
    pos = 0
    do iter=1,n
      call MPI_Unpack(rcv_buf,buf_size,pos,message,1,MPI_INTEGER4,MPI_COMM_WORLD)  
    end do

! Update the message assume that all the packed messages are the same
    message = message + my_rank
  end do

! Get the total of all the elements in the final message
  if (message .NE. answer) then
       write(6,fmt='(a,i0,a,i0,a,i0)') ' message <> answer  message=',message, 'answer=',answer, 'rank=',my_rank
       call MPI_Abort(MPI_COMM_WORLD,15)
  endif




  Tend = MPI_Wtime()

  if (my_rank .EQ. 0) write(6,*) ' Time taken is ',Tend-Tstart

  deallocate(snd_buf)
  deallocate(rcv_buf)

  call MPI_Finalize()

end program ring_pack
