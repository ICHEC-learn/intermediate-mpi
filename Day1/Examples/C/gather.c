/*
 *
 * Gathering rank ids from all processes at root
 *
*/
#include <stdio.h>
#include <mpi.h>

int main( int argc, char **argv ) {
  int myRank, uniSize, ierror;
  int i;

  ierror=MPI_Init(&argc, &argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD, &uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  
  int *buf= (int*)malloc( sizeof( int ) * uniSize );

  ierror = MPI_Gather( &myRank, 1, MPI_INT, buf, 1, MPI_INT, 0, MPI_COMM_WORLD );
  if(myRank==0){
    printf("Rank 0 printing ...\n");
    for(i=0; i<uniSize; i++){
      printf("From rank %d: %d\n", i, buf[i]);
    }
  }

  ierror = MPI_Finalize();
  return ierror;
}

