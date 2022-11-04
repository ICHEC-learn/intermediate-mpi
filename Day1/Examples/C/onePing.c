/*
 *
 * OnePing: Process 0 sends a message to process 1
 *
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
  int myRank, ierror, arr[5], i;    
  MPI_Status status;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  if (myRank == 0) {
    arr[0]=2345; arr[1]=654; arr[2]=96574; arr[3]=-12; arr[4]=7676;
    printf("I am %i before send ping \n", myRank);
    ierror=MPI_Send(arr, 5, MPI_INT, 1, 100, MPI_COMM_WORLD);
  } 
  else if (myRank == 1) {
    ierror=MPI_Recv(arr, 5, MPI_INT, 0, 100, MPI_COMM_WORLD, &status);
    printf("I am %i after  recv ping \n", myRank);
    for(i=0; i<5; i++) printf("arr[%d]=%d\n", i, arr[i]);
  }

  ierror=MPI_Finalize();
  return ierror;
}

