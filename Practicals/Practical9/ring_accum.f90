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
! *          with window=Tbuf and to get sum of ranks in         *
! *          remote window.                                      *
! *                                                              *
! * Contents: F-Source                                           *
! *                                                              *
! ****************************************************************


program ring_accum
  use mpi_f08
  use, intrinsic :: ISO_C_BINDING
  implicit none
  integer (kind=4), parameter :: kind_val=4 
  INTEGER, PARAMETER :: max_length=8388608     ! ==> 2 x 32 MB per process
  integer  my_rank, world
  integer (kind=kind_val), asynchronous, pointer :: Tbuf(:)
  type (C_PTR) :: ptr_Tbuf
  type(MPI_Win) :: win


  call MPI_Init()

  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

  call MPI_Comm_size(MPI_COMM_WORLD, world)


! Cannot use Win_create_dynamic with Fortran
! For this routine the window address must be in the form af a C pointer
  call MPI_Win_allocate(...,ptr_Tbuf,win)
! Associate the fptr with cptr
  call C_F_POINTER(ptr_Tbuf, Tbuf, (/max_length/))





  call MPI_Accumulate(..., win)



! The total must be the sum of the ranks
  write(6,fmt='(a,i0,a,i0)') 'PE',my_rank,'  Sum = ',total

  call MPI_Win_free(win)
  call MPI_Finalize()
end program ring_accum 
