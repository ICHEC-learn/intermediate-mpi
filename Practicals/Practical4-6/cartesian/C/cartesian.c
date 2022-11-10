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

  // Create a division of processors in a cartesian grid
  int ndims = 2;
  int dims[2] = { 0, 0 };
  MPI_Dims_create( uniSize, ndims, &dims[0] );
  // Print the dims array to show how it has been changed
  if ( myRank == 0 ) {
    printf("dims array: %d %d\n", dims[0], dims[1] );
  }

  // Both dimensions will be periodic
  int periods[2] = { 1, 1 };
  int reorder = 0;
  // Create a new virtual topology
  MPI_Cart_create( MPI_COMM_WORLD, ndims, dims, periods, reorder, &comm_cart );
  // Find the new size and rank
  MPI_Comm_size( comm_cart, &cartSize );
  MPI_Comm_rank( comm_cart, &cartRank );
  // Print some information about our rank
  printf( "I am process %d of %d on comm_world and %d of %d on comm_cart.\n", myRank, uniSize, cartRank, cartSize );

  // Compute the neighbours for each process
  for ( i = 0; i < ndims; i++ ) {
    MPI_Cart_shift( comm_cart, i, 1, &prevRank, &nextRank );
    printf( "Process %d on comm_cart. Direction %d, prev %d, next %d.\n", cartRank, i, prevRank, nextRank );
  }

  // Allocate the submatrix for each process
  int matsize = M*N;
  int **A = alloc2dInt( N, M );
  int *randvec = (int *) malloc(matsize*sizeof(int));
    
  // Rank 0 creates the entire matrix
  if (myRank == 0) {
    srand(time(NULL));
    for (i=0; i<matsize; i++) {
      randvec[i]  = (int) rint(((float)rand()/RAND_MAX));
    }
  }

  MPI_Bcast(randvec,matsize,MPI_INT,0,MPI_COMM_WORLD);

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

  // Alternative approach
  // Allocate the submatrix for each process on comm_cart
/*  int n=N/dims[0];
  int m=M/dims[1];
  int **A = alloc2dInt( n, m );
  //Initialise matrix
  srand(time(NULL));
  for (i=0; i<n; i++) {
    for (j=0; j<m; j++) {
      A[i][j] = (int) rint(((float)rand()/RAND_MAX));
    }
  }

  // Sum the local part of the array
  int sum = 0;
  for ( i = 0; i < n; i++ ) {
    for ( j = 0; j < m; j++ ) {
      sum += A[i][j];
    }
  }
*/

  // Print the local sum on each process
  printf( "I am process %d of %d and sum is %d\n", cartRank, cartSize, sum );

  // Reduce the global sum on process 0
  int globalSum;
  MPI_Reduce( &sum, &globalSum, 1, MPI_INT, MPI_SUM, 0,  comm_cart);

  // Print this global sum
  if ( myRank == 0 ) {
    printf( "Global sum is %d\n", globalSum );
  }

  MPI_Finalize();
  return 0;
}
