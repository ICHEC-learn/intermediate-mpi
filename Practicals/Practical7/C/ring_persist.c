/*==============================================================!
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
! Contents: C-Source                                           !
!                                                              !
!==============================================================*/

#include <stdio.h>
#include <mpi.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {

  int tag=201;

  int my_rank, uniSize;

  int right, left;

  int i, iter, total, n, answer;

  int snd_buf, rcv_buf;

  double Tstart, Tend;


  MPI_Status arr_status[2];

  MPI_Request arr_request[2];


  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &uniSize);

  right = (my_rank+1)%uniSize;
  left  = (my_rank-1+uniSize)%uniSize;

  if (argc < 2) {
    printf(" Program needs the number of times the message is passed around the ring\n");
    printf(" E.g.   ./prog N \n");
    MPI_ABORT(MPI_COMM_WORLD,10);
  }

// Get number of times to sendmessage around the ring
  n = atoi(argv[1]);

// Check result is correct
  answer = uniSize*(uniSize-1)/2;


// Start timer
  Tstart = MPI_Wtime();

// Setup persistent comminucations




  for (iter=0; iter<n; iter++) {


    for (i=0; i<uniSize; i++) {

// Send messages



// Create the sum

    }

    if (total != answer) MPI_Abort(MPI_COMM_WORLD,15);


  }

// Destroy persistent handles


  Tend = MPI_Wtime();

  if (my_rank == 0) printf(" Time taken is %f\n",Tend-Tstart);


  MPI_Finalize();

}
