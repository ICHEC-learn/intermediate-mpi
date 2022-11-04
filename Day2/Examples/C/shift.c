/*
 * 
 * A 1d ring example with MPI_Cart_shift and MPI_Sendrecv
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
  int uniSize, ierror, i, commRank, prevRank, nextRank;
  int ndims, dims[1], periods[1], reorder;
  MPI_Comm comm_cart;
  MPI_Status status;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);

  ndims=1;
  dims[0]=uniSize; 
  periods[0]=1; 
  reorder=1;
  ierror=MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &comm_cart);
  ierror=MPI_Comm_rank(comm_cart, &commRank);

  ierror=MPI_Cart_shift(comm_cart, 0, 1, &prevRank, &nextRank);
  printf("Rank %d: rank prev = %d, rank next = %d\n", commRank, prevRank, nextRank);

  int sBuf[2] = {commRank, 0};
  int rBuf[2] = {-1, 0};    

  while ( rBuf[0] != commRank ) {
    ierror=MPI_Sendrecv(sBuf, 2, MPI_INT, nextRank, 100, rBuf, 2, MPI_INT, prevRank, 100, comm_cart, &status);
    sBuf[1] += rBuf[0];
    sBuf[0] = rBuf[0];
  }
  printf("Rank %d, sums %d\n", commRank, sBuf[1] );

  ierror = MPI_Finalize();
  return ierror;
}
