/*
 *
 * Rank 0 reads into a buffer and does the broadcast
 *
*/
#include <stdio.h>
#include <mpi.h>

int main( int argc, char **argv ) {
  int myRank, uniSize, ierror;
  int buf;

  ierror=MPI_Init(&argc, &argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD, &uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  
  if ( myRank == 0 ) {
      printf( "Please insert a number\n" );
      scanf( "%d", &buf );
  }
  ierror = MPI_Bcast( &buf, 1, MPI_INT, 0, MPI_COMM_WORLD );
  printf( "I am process %d of %d and buf is %d\n", myRank, uniSize, buf );

  ierror = MPI_Finalize();
  return ierror;
}

