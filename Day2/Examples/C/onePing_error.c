/*
 *
 * OnePing: P0 sends a message to P1 with wrong dest rank
 *
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
  int myRank, ierror, arr[5], i, ierror2, errclass, errbuflen;    
  MPI_Status status;
  char errbuffer[MPI_MAX_ERROR_STRING];

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN); 

  if (myRank == 0) {
    arr[0]=2345; arr[1]=654; arr[2]=96574; arr[3]=-12; arr[4]=7676;
    printf("I am %i before send ping \n", myRank);
    ierror=MPI_Send(arr, 5, MPI_INT, 10, 100, MPI_COMM_WORLD);
    if(ierror != MPI_SUCCESS){
      ierror2=MPI_Error_class(ierror, &errclass);
      if(errclass == MPI_ERR_RANK){
        fprintf(stderr, "Invalid rank used in MPI send call\n");
        ierror2=MPI_Error_string(ierror, errbuffer, &errbuflen);
        fprintf(stderr, errbuffer);
        ierror2=MPI_Abort(MPI_COMM_WORLD, 0);
      }
    }
  } 
  else if (myRank == 1) {
    ierror=MPI_Recv(arr, 5, MPI_INT, 0, 100, MPI_COMM_WORLD, &status);
    printf("I am %i after  recv ping \n", myRank);
    for(i=0; i<5; i++) printf("arr[%d]=%d\n", i, arr[i]);
  }

  ierror=MPI_Finalize();
  return ierror;
}

