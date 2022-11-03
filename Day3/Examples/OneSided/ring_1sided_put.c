/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the High Performance           *
 * Computing Centre Stuttgart (HLRS).                           *
 * The examples are based on the examples in the MPI course of  *
 * the Edinburgh Parallel Computing Centre (EPCC).              *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * HLRS and EPCC take no responsibility for the use of the      *
 * enclosed teaching material.                                  *
 *                                                              *
 * Authors: Joel Malard, Alan Simpson,            (EPCC)        *
 *          Rolf Rabenseifner, Traugott Streicher (HLRS)        *
 *                                                              *
 *                                                              *  
 * Purpose: A program to try out one-sided communication        *
 *          with window=Tbuf and MPI_PUT to put                 *
 *          local Obuf value into remote window (Tbuf).         *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>


int main (int argc, char *argv[])
{
  int my_rank, size;
  int Tbuf, Obuf;
  int right, left;
  int sum, i;

  MPI_Win     win;


  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  MPI_Comm_size(MPI_COMM_WORLD, &size);

  /* Get left and right without bracnching */
  right = (my_rank+1)      % size;
  left  = (my_rank-1+size) % size;


  /* Create the window. */
  /* Remember that buffer size is in bytes */
  MPI_Win_create(&Tbuf, (MPI_Aint) sizeof(int), sizeof(int), MPI_INFO_NULL, MPI_COMM_WORLD, &win);

  sum = 0;
  Obuf = my_rank;

  for( i = 0; i < size; i++) 
  {
    MPI_Win_fence(MPI_MODE_NOSTORE | MPI_MODE_NOPRECEDE, win);
  /* Putting Obuf into target's Window */
    MPI_Put(&Obuf, 1, MPI_INT, right, (MPI_Aint) 0, 1, MPI_INT, win);
    MPI_Win_fence(MPI_MODE_NOSTORE | MPI_MODE_NOPUT | MPI_MODE_NOSUCCEED, win);
    
  /* Must ensure that RMA ops are completed before using Tbuf and updating Obuf */
    Obuf = Tbuf;
    sum += Obuf;
  }

  printf ("PE%i:\tSum = %i\n", my_rank, sum);

  MPI_Win_free(&win);
  MPI_Finalize();
}
