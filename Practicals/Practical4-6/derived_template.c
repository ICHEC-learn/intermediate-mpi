/*
 * Create derived datatypes from 2d matrix
 *
 * COMPILE: mpicc -o derived derived.c
 * RUN: mpirun -np 2 ./derived
 * (Run with 2 processors)
*/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#include "alloc2d.h"

#define N 10//# of rows
#define M 10//# of columns

int main( int argc, char **argv ) {
  int ierror;
  int i, j;
  int myRank, uniSize;

  ierror = MPI_Init( &argc, &argv );
  ierror = MPI_Comm_rank( MPI_COMM_WORLD, &myRank );
  ierror = MPI_Comm_size( MPI_COMM_WORLD, &uniSize );

  int **A = alloc2dInt( N, M );
  //Rank 0 has a matrix of all zeros and Rank 1 has a matrix of all 1s  
  for ( i = 0; i < N; i++ ) {
    for ( j = 0; j < M; j++ ) {
      A[i][j] = myRank;
    }
  }

  MPI_Datatype matRow, matCol, subMat;

//TODO
  MPI_Type_vector( ... );
  MPI_Type_commit( ... );
  MPI_Type_vector( ... );
  MPI_Type_commit( ... );
  MPI_Type_vector( ... );
  MPI_Type_commit( ... );

  // Will only send messages from 0 to 1
  int src = 0;
  int dest = 1;
  int tag = 5;

//TODO
  // Send the 0th row
  if ( myRank == 0 ) {
    MPI_Send( ... dest, tag, MPI_COMM_WORLD );
  }
  else if ( myRank == 1 ) {
    MPI_Recv( ... src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
  }

  // Let's print the matrix on rank 1 for illustration
  // This also resets the matrix
  if ( myRank == 1 ) {
    printf( "Should have received the first row:\n" );
    for ( i = 0; i < N; i++ ) {
      for ( j = 0; j < M; j++ ) {
        printf( "%d ", A[i][j] );
        A[i][j] = myRank;
      }
      printf( "\n" );
    }
    printf( "\n" );
  }

//TODO
  // Send the 0th column
  if ( myRank == 0 ) {
    MPI_Send( ..., dest, tag, MPI_COMM_WORLD );
  }
  else if ( myRank == 1 ) {
    MPI_Recv( ... src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
  }

  // Let's print the matrix on rank 1 for illustration
  // This also resets the matrix
  if ( myRank == 1 ) {
    printf( "Should have received the 0th column:\n" );
    for ( i = 0; i < N; i++ ) {
      for ( j = 0; j < M; j++ ) {
        printf( "%d ", A[i][j] );
        A[i][j] = myRank;
      }
      printf( "\n" );
    }
    printf( "\n" );
  }

//TODO
  // Send the 0th column, but receive into the 4th column
  if ( myRank == 0 ) {
    MPI_Send( ..., dest, tag, MPI_COMM_WORLD );
  }
  else if ( myRank == 1 ) {
    MPI_Recv( ... src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
  }

  // Let's print the matrix on rank 1 for illustration
  // This also resets the matrix
  if ( myRank == 1 ) {
    printf( "Should have received into the 4th column:\n" );
    for ( i = 0; i < N; i++ ) {
      for ( j = 0; j < M; j++ ) {
        printf( "%d ", A[i][j] );
        A[i][j] = myRank;
      }
      printf( "\n" );
    }
    printf( "\n" );
  }

//TODO
  // Send a submatrix
  if ( myRank == 0 ) {
    MPI_Send( ... dest, tag, MPI_COMM_WORLD );
  }
  else if ( myRank == 1 ) {
    MPI_Recv( ... src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE );
  }

  // Let's print the matrix on rank 1 for illustration
  // This also resets the matrix
  if ( myRank == 1 ) {
    printf( "Should have received into the top left corner:\n" );
      for ( i = 0; i < N; i++ ) {
        for ( j = 0; j < M; j++ ) {
          printf( "%d ", A[i][j] );
          A[i][j] = myRank;
        }
        printf( "\n" );
      }
      printf( "\n" );
  }

//TODO
  MPI_Type_free( ... );
  MPI_Type_free( ... );
  MPI_Type_free( ... );

  ierror = MPI_Finalize();
  return ierror;
}
