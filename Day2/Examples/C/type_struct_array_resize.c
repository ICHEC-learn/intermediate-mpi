/*
 * 
 * A 1d ring example with array of structs and MPI_Type_create_resized
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){

  struct buf{
     double dblrank;
     int intrank;
  }sBuf[2], rBuf[2], sum[2];

  int i,j,ierror, myRank, uniSize;
  MPI_Status status;
  MPI_Request request;
  int count, array_of_blocklengths[2];
  MPI_Aint array_of_displacements[2], intaddress, dbladdress;
  MPI_Datatype array_of_types[2], newtype, newtype_resized;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  count=2;
  array_of_blocklengths[0] = 1;
  array_of_blocklengths[1] = 1;
  ierror=MPI_Get_address(&sBuf[0].dblrank, &dbladdress);
  ierror=MPI_Get_address(&sBuf[0].intrank, &intaddress);
  array_of_displacements[0] = (MPI_Aint) 0;
  array_of_displacements[1] = MPI_Aint_diff(intaddress, dbladdress);
  array_of_types[0] = MPI_DOUBLE;
  array_of_types[1] = MPI_INT;

  ierror=MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, &newtype);
  ierror=MPI_Type_create_resized(newtype, (MPI_Aint) 0, (MPI_Aint) sizeof(sBuf[0]), &newtype_resized);
  ierror=MPI_Type_commit(&newtype_resized);

  if(myRank==0){
    int bufsize;
    MPI_Aint buflb, bufextent, buftrueextent; 
    ierror=MPI_Type_size(newtype_resized, &bufsize);
    ierror=MPI_Type_get_extent(newtype_resized, &buflb, &bufextent);
    ierror=MPI_Type_get_true_extent(newtype_resized, &buflb, &buftrueextent);
    printf("Resized: Size= %d, Extent=%d, True extent=%d\n", bufsize, (int)bufextent, (int)buftrueextent);
  }

  int dest=(myRank < uniSize - 1) ? myRank + 1 : 0;
  int src=(myRank > 0) ? myRank - 1 : uniSize - 1;

  for(i=0; i<2; i++){
    sBuf[i].dblrank=(double)(i+1.0)*myRank;
    sBuf[i].intrank=(i+1)*myRank;
    sum[i].dblrank=0.0;
    sum[i].intrank=0;
  }

  for( i = 0; i < uniSize; i++) {
    ierror=MPI_Isend(&sBuf, 2, newtype_resized, dest, 100, MPI_COMM_WORLD, &request); 
    ierror=MPI_Recv(&rBuf, 2, newtype_resized, src, 100, MPI_COMM_WORLD, &status);  
    ierror=MPI_Wait(&request, &status); 
    for(j=0; j<2; j++){
      sBuf[j]=rBuf[j];
      sum[j].dblrank += rBuf[j].dblrank;
      sum[j].intrank += rBuf[j].intrank;
    }
  }

  for(i=0; i<2; i++){
    printf("Rank %d, double sum %lf, int sum=%d\n", myRank, sum[i].dblrank, sum[i].intrank );
  }
  
  ierror=MPI_Type_free(&newtype_resized);
  ierror = MPI_Finalize();
  return ierror;
}
