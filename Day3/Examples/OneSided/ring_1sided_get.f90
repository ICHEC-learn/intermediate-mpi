!*****************************************************************
! *                                                              *
! * This file has been written as a sample solution to an        *
! * exercise in a course given at the High Performance           *
! * Computing Centre Stuttgart (HLRS).                           *
! * The examples are based on the examples in the MPI course of  *
! * the Edinburgh Parallel Computing Centre (EPCC).              *
! * It is made freely available with the understanding that      *
! * every copy of this file must include this header and that    *
! * HLRS and EPCC take no responsibility for the use of the      *
! * enclosed teaching material.                                  *
! *                                                              *
! * Authors: Joel Malard, Alan Simpson,            (EPCC)        *
! *          Rolf Rabenseifner, Traugott Streicher (HLRS)        *
! *                                                              *
! *                                                              *  
! * Purpose: A program to try out one-sided communication        *
! *          with window=Tbuf and MPI_GET to get                 *
! *          remote window (Tbuf) value into local Obuf.         *
! *                                                              *
! * Contents: F-Source                                           *
! *                                                              *
! ****************************************************************


program ring_get
  use mpi_f08
  implicit none

  integer (kind=4), parameter :: kind_val=4
  integer :: my_rank, world, left, right, disp_unit
  integer (kind=kind_val), asynchronous :: Tbuf
  integer (kind=kind_val) :: Obuf
  integer (kind=MPI_ADDRESS_KIND) :: Tbuf_size, target_disp
  integer (kind=4) :: total, i

  type(MPI_Win) :: win


  call MPI_Init()

  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

  call MPI_Comm_size(MPI_COMM_WORLD, world)

! Modulo will alway return a +ve value
  right = modulo(my_rank+1,world)
  left  = modulo(my_rank-1,world)


! Create the window.
! Remember that buffer size is in bytes
! and that all arguments should be of the correct datatype
  Tbuf_size = kind(Tbuf)
  disp_unit = Tbuf_size
  call MPI_Win_create(Tbuf, Tbuf_size, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, win)

  total = 0
  Tbuf = my_rank
  target_disp = 0

  do i = 1,world 
! As FORTRAN compiler can reorder executable lines, we  must prevent this.
    if (.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING) CALL MPI_F_sync_reg(Tbuf)

    call MPI_Win_fence(MPI_MODE_NOPUT + MPI_MODE_NOPRECEDE, win)
! Getting info from left neighbour and storing
    call MPI_Get(Obuf, 1, MPI_INTEGER4, left, target_disp, 1, MPI_INTEGER4, win)
    call MPI_Win_fence(MPI_MODE_NOSTORE + MPI_MODE_NOPUT + MPI_MODE_NOSUCCEED, win)
    
! Must ensure that RMA ops are completed before using Obuf and updating Tbuf
    if (.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING) CALL MPI_F_sync_reg(Tbuf)
    Tbuf = Obuf
    total = total + Tbuf
  end do

  write(6,fmt='(a,i0,a,i0)') "PE",my_rank,":   Sum = ",total


  call MPI_Win_free(win)
  call MPI_Finalize()
end program ring_get
