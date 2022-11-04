/*
 * 
 * A 2d cylinder of 12 processes in a 4x3 cartesian grid
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){

  int myRank, uniSize, ierror;
  int ndims, dims[2], periods[2], reorder;
  int coords[2], coordRank;
  MPI_Comm comm_cart;

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);

  if(uniSize==12){
    ndims=2;
    dims[0]=4; dims[1]=3;
    periods[0]=1; periods[1]=0;
    reorder=1;
    ierror=MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &comm_cart);

    if (myRank == 5){
        MPI_Cart_coords(comm_cart, myRank, ndims, coords);
        printf("Rank %d coordinates are %d %d\n", myRank, coords[0], coords[1]);
     }
     if(myRank==0){
        coords[0]=3; coords[1]=1;
        MPI_Cart_rank(comm_cart, coords, &coordRank);
        printf("The processor at position (%d, %d) has rank %d\n", coords[0], coords[1], coordRank);
     } 
  }

  ierror=MPI_Finalize();
  return ierror;
}

