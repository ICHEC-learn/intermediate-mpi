/*
*
*  Conway Game of Life
*
*  COMPILE: mpicc -o life life_parallel.c
*  RUN: mpirun -np 4 ./life
* 
*/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#define NI 200 /* array sizes */
#define NJ 200  
#define NSTEPS 500 /* number of time steps */

void main(int argc, char **argv)
{
  int i, j, n, im, ip, jm, jp, ni, nj, nsum, isum, isumloc;
  int uniSize, indx;
  int *old, *new;
  float x;
  FILE *outFile;

  int nsdir, ewdir, ndims, periods[2], reorder;
  int centre,north,south,east,west;
  int coords[2], corner[2], dim_size[2];
  int iproc_nw, iproc_ne, iproc_sw, iproc_se;
  MPI_Comm comm_cart;
  MPI_Datatype row_hdle, col_hdle;
  MPI_Request req[16];


  MPI_Init(&argc,&argv);


  MPI_Comm_size(MPI_COMM_WORLD,&uniSize);

  if (uniSize != 4) {
    printf(" This code must be run with 4 processes\n");
    MPI_Abort(MPI_COMM_WORLD,10);
  }

  /* allocate arrays */
  ni = NI/2 + 2; /* add 2 for left and right ghost cells */
  nj = NJ/2 + 2;



  old = malloc(ni*nj*sizeof(int));
  new = malloc(ni*nj*sizeof(int));


//TODO: Create virtual topology
  nsdir = 0;
  ewdir = 1;

  ndims = ...
  dim_size[0]=... dim_size[1]=...
  periods[0] = ...
  periods[1] = ...
  reorder = ...
  MPI_Cart_create(...);
  MPI_Comm_rank(...);
  MPI_Cart_shift(...);
  MPI_Cart_shift(...);


  MPI_Cart_coords(...);

  /*  initialize elements of old to 0 or 1    *
   *  Set the seed like this so that it can   *
   *  directly compared with the serial       *
   *  version                                 */
  for(i=1; i<ni-1; i++) {
    j = i + coords[0]*(nj-2);
    srand(1234+j);

    if (coords[1] == 1) {
       for (j=1; j<nj-1; j++) rand();
    }
    for(j=1; j<nj-1; j++) {
      indx = j + nj*i;
      x = rand()/((float)RAND_MAX + 1);
      if(x < 0.5) {
          old[indx] = 0;
      } else {
          old[indx] = 1;
      }
    }

    if (coords[0] == 0) {
       for (j=1; j<nj-1; j++) rand();
    }
  }



  isum = 0;
  for (i=1; i<ni-1; i++) {
    for (j=1; j<nj-1; j++) {
      indx = j + nj*i;
      isum = isum + old[indx];
    }
  }
  printf("Initial sum %d, %d\n",centre,isum);
  isumloc = isum;
  MPI_Reduce(&isumloc,&isum,1,MPI_INT,MPI_SUM,0,comm_cart);
  if (centre == 0) printf(" Init total %d\n",isum);

  printf(" Neighbours %d, %d, %d, %d, %d\n",centre,north,south,east,west);


  corner[0] = (coords[0]+1)%2; 
  corner[1] = (coords[1]+1)%2; 
  MPI_Cart_rank(comm_cart,corner,&iproc_ne);

/* In this case corners are all the smae rank */
  iproc_nw = iproc_ne;
  iproc_sw = iproc_ne;
  iproc_se = iproc_ne;


//TODO: Create derived type for single row and column 

  MPI_Type_vector(...);
  MPI_Type_commit(...);

  MPI_Type_vector(...);
  MPI_Type_commit(...);



  /*  time steps */
  for(n=0; n<NSTEPS; n++) {



/*TODO: send and receive north and south rows */
       MPI_Isend(...);          //  row=1, col=1
       MPI_Irecv(...);             //  row=0, col=1
       MPI_Isend(...);   //  row=ni-2, col=1
       MPI_Irecv(...);   //  row=ni-1, col=1

/*TODO: send and receive west and east columns */
       MPI_Isend(...);           //  row=1, col=1 
       MPI_Irecv(...);             //  row=1, col=0
       MPI_Isend(...);         //  row=1, col=nj-2
       MPI_Irecv(...);         //  row=1, col=nj-1

/*TODO: send and receive corners */
       MPI_Isend(...);          // row=1, col=1
       MPI_Irecv(...);             // row=0, col=0
       MPI_Isend(...);       // row=1, col=nj-2
       MPI_Irecv(...);         // row=0, col=nj-1
       MPI_Isend(...);  // row=ni-2, col=1
       MPI_Irecv(...);    // row=ni-1, col=0
       MPI_Isend(...); // row=ni-2, col=nj-2
       MPI_Irecv(...); // row=ni-1, col=nj-1 


       MPI_Waitall(...);





    for(i=1; i<ni-1; i++) {
      for(j=1; j<nj-1; j++) {

        im = i-1;
        ip = i+1;
        jm = j-1;
        jp = j+1;

        nsum =  old[im*nj+jp] + old[i*nj+jp] + old[ip*nj+jp]
              + old[im*nj+ j] + old[ip*nj+j]
              + old[im*nj+jm] + old[i*nj+jm] + old[ip*nj+jm];

        switch(nsum) {

          case 3:
            new[i*nj+j] = 1;
            break;

          case 2:
            new[i*nj+j] = old[i*nj+j];
            break;

          default:
            new[i*nj+j] = 0;
        }
      }
    }

    /* copy new state into old state */
    for(i=1; i<ni-1; i++) {
      for(j=1; j<nj-1; j++) {
        old[i*nj+j] = new[i*nj+j];
      }
    }
  }

  /*  Iterations are done; sum the number of live cells */
  isum = 0;
  for(i=1; i<ni-1; i++) {
    for(j=1; j<nj-1; j++) {
      isum = isum + new[i*nj+j];
    }
  }


  printf("\nNumber of live cells = %d\n", isum);

  isumloc = isum;
  MPI_Reduce(&isumloc,&isum,1,MPI_INT,MPI_SUM,0,comm_cart);
  if (centre == 0) printf(" Final total %d\n",isum);

  MPI_Type_free(&row_hdle);
  MPI_Type_free(&col_hdle);

  MPI_Comm_free(&comm_cart);
  MPI_Finalize();
}
