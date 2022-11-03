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

  int i, iter, n, answer, position, message;

  int buf_size, *snd_buf, *rcv_buf;

  double Tstart, Tend;


  MPI_Status arr_status[2];

  MPI_Request arr_request[2];


  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &uniSize);

  right = (my_rank+1)%uniSize;
  left  = (my_rank-1+uniSize)%uniSize;

  if (argc < 2) {
    printf(" Program needs the size of the message is passed around the ring\n");
    printf(" E.g.   ./prog N \n");
    MPI_Abort(MPI_COMM_WORLD,10);
  }

// Get number of times to send message around the ring
  n = atoi(argv[1]);
  buf_size = n*sizeof(int);
  snd_buf = (int *) malloc(buf_size);
  rcv_buf = (int *) malloc(buf_size);

// Check result is correct
  answer = uniSize*(uniSize-1)/2;


// Start timer
  Tstart = MPI_Wtime();


  message = 0;

// Send the packed messages around the ring
  for (i=0; i<uniSize; i++) {

// Pack messages into single snd_buf
    position = 0;
    for (iter=0; iter<n; iter++) {
      MPI_Pack(&message,1,MPI_INT,snd_buf,buf_size,&position,MPI_COMM_WORLD);
    }
    MPI_Irecv(rcv_buf, position, MPI_PACKED, left, tag,
                        MPI_COMM_WORLD, &arr_request[0]);

    MPI_Issend(snd_buf, position, MPI_PACKED, right, tag,
                          MPI_COMM_WORLD, &arr_request[1]);

    MPI_Waitall(2, arr_request, arr_status);

// Unpack the messages and add them to total
    position = 0;
    for (iter=0; iter<n; iter++) {
      MPI_Unpack(rcv_buf,buf_size,&position,&message,1,MPI_INT,MPI_COMM_WORLD);   
    }

// Update the message assume that all the packed messages are the same
    message = message + my_rank;
  }

// Get the total of all the elements in the final message
  if (message != answer) {
       printf(" total <> answer  total=%d,answer=%d, rank=%d\n",message,answer,my_rank);
       MPI_Abort(MPI_COMM_WORLD,15);
  }




  Tend = MPI_Wtime();

  if (my_rank == 0) printf(" Time taken is %f\n",Tend-Tstart);

  free(snd_buf);
  free(rcv_buf);

  MPI_Finalize();

}
