/*
 *
 * An example of deadlock with MPI_Ssend
 *
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
  int myRank, ierror, a[5], b[5], i;    
  MPI_Status status;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  if (myRank == 0) {
    a[0]=2345; a[1]=654; a[2]=96574; a[3]=-12; a[4]=7676;
    ierror=MPI_Ssend(a, 5, MPI_INT, 1, 100, MPI_COMM_WORLD);
    ierror=MPI_Recv(b, 5, MPI_INT, 1, 101, MPI_COMM_WORLD, &status);
    for(i=0; i<5; i++) printf("b[%d]=%d\n", i, b[i]);
  } 
  else if (myRank == 1) {
    b[0]=-2345; b[1]=-654; b[2]=-96574; b[3]=12; b[4]=-7676;
    ierror=MPI_Ssend(b, 5, MPI_INT, 0, 101, MPI_COMM_WORLD);
    ierror=MPI_Recv(a, 5, MPI_INT, 0, 100, MPI_COMM_WORLD, &status);
    for(i=0; i<5; i++) printf("a[%d]=%d\n", i, a[i]);
  }

  ierror=MPI_Finalize();
  return ierror;
}

