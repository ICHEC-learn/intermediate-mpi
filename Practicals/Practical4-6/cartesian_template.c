/*
 * Create a Cartesian virtual topology, map the matrix on it
 * and calculate sum of its elemements
 *
 * COMPILE: mpicc -o cartesian cartesian.c
 * RUN: mpirun -np 20 ./cartesian
 * (Run with 20 processors)
*/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <time.h>
#include <math.h>
#include "alloc2d.h"

#define N 20
#define M 20

int main( int argc, char **argv ) {

  int i, j;
  int myRank, cartRank;
  int uniSize, cartSize;
  int prevRank, nextRank;
  MPI_Comm comm_cart;
    
  // Get the world size and rank
  MPI_Init( &argc, &argv );
  MPI_Comm_size( MPI_COMM_WORLD, &uniSize );
  MPI_Comm_rank( MPI_COMM_WORLD, &myRank );

  // TODO: Create a division of processors in a cartesian grid
   

  // TODO: Both dimensions will be periodic


  // TODO: Create a new virtual topology


  // TODO: Find the new size and rank

  // Print some information about our rank
  printf( "I am process %d of %d on comm_world and %d of %d on comm_cart.\n", myRank, uniSize, cartRank, cartSize );

  // TODO: Compute the neighbours for each process


  // Allocate the submatrix for each process
  int matsize = M*N;
  int **A = alloc2dInt( N, M );
  int *randvec = (int *) malloc(matsize*sizeof(int));
    
  // Rank 0 creates the entire matrix and broadcasts
  if (myRank == 0) {
    srand(time(NULL));
    for (i=0; i<matsize; i++) {
      randvec[i]  = (int) rint(((float)rand()/RAND_MAX));
    }
  }

//TODO
  MPI_Bcast( ... );

  // Initialise sub-matrix with same random vector
  int k = 0;
  for (i=0; i<N; i++) {
    for (j=0; j<M; j++) {
      A[i][j] = randvec[k];
      k++;
     }
  }

  // Sum the local part of the array
  int sum = 0;
  for ( i = 0; i < N; i++ ) {
    for ( j = 0; j < M; j++ ) {
      sum += A[i][j];
    }
  }



  // Print the local sum on each process
  printf( "I am process %d of %d and sum is %d\n", cartRank, cartSize, sum );

  // TODO: Reduce the global sum on process 0
  int globalSum;
  MPI_Reduce( ... );

  // Print this global sum
  if ( myRank == 0 ) {
    printf( "Global sum is %d\n", globalSum );
  }

  MPI_Finalize();
  return 0;
}
