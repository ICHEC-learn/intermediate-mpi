/*
 *
 * An example of iprobe with 2 processes.  
 *
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char* argv[]){
  int myRank, ierror, a[5], i, flag=0;    
  MPI_Status status;
  MPI_Request request;

  ierror=MPI_Init(&argc, &argv);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

  if(myRank ==0){    
    a[0]=2345; a[1]=654; a[2]=96574; a[3]=-12; a[4]=7676;
    int tag=myRank;
    printf("Process %d: sending the message.\n", myRank);
    ierror=MPI_Issend(a, 5, MPI_INT, 1, tag, MPI_COMM_WORLD,&request);
    ierror=MPI_Wait(&request,&status);
    }
  else if(myRank == 1){
    while (flag == 0){
      ierror=MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
      printf("After MPI_Iprobe, flag = %d\n", flag);
    }
    ierror=MPI_Recv(a, 5, MPI_INT, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    printf("Process %d: message received.\n", myRank);
    for(i=0; i<5; i++) printf("a[%d]=%d\n", i, a[i]);
  }

    ierror=MPI_Finalize();
    return ierror;
}
