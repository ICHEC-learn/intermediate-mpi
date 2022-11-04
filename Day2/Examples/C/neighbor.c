/*
 * 
 * A 1d ring example with MPI_Cart_shift and MPI_Neighbor_alltoall
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){

  int uniSize, ierror, i, sum=0, commRank, prevRank, nextRank;
  int ndims, dims[1], periods[1], reorder;
  MPI_Comm comm_cart;
  int sendbuf[2], recvbuf[2];

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

  sendbuf[1] = commRank;
  for(i=0;i<uniSize;i++){
    //Issend-Recv-Wait replaced with  MPI_Neighbor_alltoall
    //recvbuf[0] as recvbuf and sendbuf[1] as sendbuf
    MPI_Neighbor_alltoall(sendbuf, 1, MPI_INT, recvbuf, 1, MPI_INT, comm_cart);
    sendbuf[1]=recvbuf[0];
    sum+=recvbuf[0];
  }
  
  printf("Rank %d: %d\n", commRank, sum);

  ierror=MPI_Finalize();
  return ierror;
}

