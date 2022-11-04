/*
 * 
 * A 1d ring example with MPI_Type_Struct
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){

  struct buf{
     double dblrank;
     int intrank;
  }sBuf, rBuf, sum;

  int i,ierror, myRank, uniSize;
  MPI_Status status;
  MPI_Request request;
  int count, array_of_blocklengths[2];
  MPI_Aint array_of_displacements[2], intaddress, dbladdress;
  MPI_Datatype array_of_types[2], newtype;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  count=2;
  array_of_blocklengths[0] = 1;
  array_of_blocklengths[1] = 1;
  ierror=MPI_Get_address(&sBuf.dblrank, &dbladdress);
  ierror=MPI_Get_address(&sBuf.intrank, &intaddress);
  array_of_displacements[0] = (MPI_Aint) 0;
  array_of_displacements[1] = MPI_Aint_diff(intaddress, dbladdress);
  array_of_types[0] = MPI_DOUBLE;
  array_of_types[1] = MPI_INT;

  ierror=MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, &newtype);
  ierror=MPI_Type_commit(&newtype);

  int dest=(myRank < uniSize - 1) ? myRank + 1 : 0;
  int src=(myRank > 0) ? myRank - 1 : uniSize - 1;

  sBuf.dblrank=(double)myRank;
  sBuf.intrank=myRank;
  sum.dblrank=0.0;
  sum.intrank=0;

  for( i = 0; i < uniSize; i++) {
    ierror=MPI_Isend(&sBuf, 1, newtype, dest, 100, MPI_COMM_WORLD, &request); 
    ierror=MPI_Recv(&rBuf, 1, newtype, src, 100, MPI_COMM_WORLD, &status);  
    ierror=MPI_Wait(&request, &status); 
    sBuf=rBuf;
    sum.dblrank += rBuf.dblrank;
    sum.intrank += rBuf.intrank;
  }

  printf("Rank %d, double sum %lf, int sum=%d\n", myRank, sum.dblrank, sum.intrank );
  
  ierror=MPI_Type_free(&newtype);
  ierror = MPI_Finalize();
  return ierror;
}
