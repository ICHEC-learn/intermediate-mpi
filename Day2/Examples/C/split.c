/*
 *
 * Split processes with odd and even ranks into 2 communicators  
 *
*/

#include <stdio.h> 
#include <mpi.h> 

int main(int argc, char **argv){
  int myRank, uniSize, ierror; 
  int color, key, newRank, sum;
  MPI_Comm newcomm;   

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  
  if(myRank % 2 == 0){
      color=1;
      key=myRank;
  }
  else{
      color=2;
      key=uniSize-myRank;
  }
 
  ierror=MPI_Comm_split(MPI_COMM_WORLD, color, key, &newcomm);
  ierror=MPI_Comm_rank(newcomm, &newRank);
  ierror=MPI_Allreduce (&newRank, &sum, 1, MPI_INT, MPI_SUM, newcomm);

  printf("I am process %d in MPI_COMM_WORLD and %d in the new communicator.\n", myRank, newRank);
  if(newRank==0){
    printf("Color and Sum of ranks in the new communicator: %d, %d.\n", color, sum);
  }
  ierror=MPI_Finalize();
  return ierror;
}

