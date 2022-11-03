/*****************************************************************
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
! * Contents: C-Source                                           *
! *                                                              *
! ****************************************************************/

#include <stdio.h>
#include <mpi.h>

int main(int argc, char *argv[]) {
  int  my_rank, world;

  MPI_Win win;


  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &world);


// Create the window using
  MPI_Win_allocate(...);
// or
  MPI_Win_create_dynamic(...);


  MPI_Accumulate(...,win);


// Sum should be the sum of the ranks
  printf ("PE%i:\tSum = %i\n", my_rank, sum);

  MPI_Win_free(&win);
  MPI_Finalize();
}
