/*
 * 
 * A 1d ring example with MPI_Type_contiguous
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
  int i,ierror, myRank,uniSize;
  MPI_Status status;
  MPI_Datatype newtype;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  int sBuf[2]={myRank, 0};
  int rBuf[2]={-1, 0};    

  int dest=(myRank < uniSize - 1) ? myRank + 1 : 0;
  int src=(myRank > 0) ? myRank - 1 : uniSize - 1;

  ierror=MPI_Type_contiguous(2, MPI_INT, &newtype);
  ierror=MPI_Type_commit(&newtype);

  while ( rBuf[0] != myRank ) {
    ierror=MPI_Sendrecv(sBuf, 1, newtype, dest, 100, rBuf, 1, newtype, src, 100,MPI_COMM_WORLD, &status);
    sBuf[1] += rBuf[0];
    sBuf[0] = rBuf[0];
  }

  printf("Rank %d, sums %d\n", myRank, sBuf[1] );
  ierror=MPI_Type_free(&newtype);

  ierror = MPI_Finalize();
  return ierror;
}
