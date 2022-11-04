/*
 *
 * Scatter rows of the 4x4 matrix to 4 processors
 *
*/
#include <stdio.h>
#include <mpi.h>
#define SIZE 4

int main( int argc, char **argv ) {
  int myRank, uniSize, ierror;
  int sendbuf[SIZE][SIZE]={
  {1, 2, 3, 4},
  {5, 6, 7, 8},
  {9, 10, 11, 12},
  {13, 14, 15, 16},
  };
  int recvbuf[SIZE];
  
  ierror=MPI_Init(&argc, &argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD, &uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

  if(uniSize==SIZE){
    ierror = MPI_Scatter(sendbuf, SIZE, MPI_INT, recvbuf, SIZE, MPI_INT, 0, MPI_COMM_WORLD );
    printf("rank= %d  Results: %d %d %d %d\n", myRank,recvbuf[0],recvbuf[1],recvbuf[2],recvbuf[3]);
  }

  ierror = MPI_Finalize();
  return ierror;
}

