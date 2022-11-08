/**************************************
   Conway Game of Life
 
   2-processor domain decomposition;
   Domain decomposed with horizontal
   line, i.e., top half and bottom half
 ***************************************/


#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#define NI 200                                    /* array sizes */
#define NJ 200
#define NSTEPS 500                                /* number of time steps */

void main(int argc, char *argv[])
{
  int i, j, n, im, ip, jm, jp, ni, nj, nsum, isum, isum1, isumloc, nprocs, myid;
  int ig, jg, i1g, i2g, j1g, j2g, ninom, njnom, i1, i2, i2m, j1, j2, j2m;
  int niproc, njproc;                           /* no. procs in each direction */
  int **old, **new;
  MPI_Status status;
  float x;

  /* initialize MPI */
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&myid);

//TODO: Domain decomposition 
  /* nominal number of points per proc. in each direction
     (without ghost cells; assume numbers divide evenly) */ 
  niproc = ...                              /* divide domain in i direction only */
  njproc = ...
  ninom = ...
  njnom = ...

  /* global starting and ending indices (without ghost cells) */
  i1g = (myid*ninom) + 1;
  i2g = i1g + ninom - 1;
  j1g = 1;
  j2g = NJ;

  /* local starting and ending indices, including ghost cells */
  i1  = 0;
  i2  = ninom + 1;
  i2m = i2-1;
  j1  = 0;
  j2  = NJ+1;
  j2m = j2-1;

  /* allocate arrays; want elements to be contiguous,
     so allocate 1-D arrays, then set pointer to each row
     (old and new) to allow use of array notation for convenience */

  ni = i2-i1+1;
  nj = j2-j1+1;

  old = malloc(ni*sizeof(int*));
  new = malloc(ni*sizeof(int*));

  for (i=0; i<ni; i++) {
    old[i] = malloc(nj*sizeof(int));
    new[i] = malloc(nj*sizeof(int));
  }

  /*  initialize elements of old to 0 or 1
      We're doing some sleight of hand here to make sure we
      initialize to the same values as in the serial code.
      The rand() function is called for every i and j, even
      if they are not on the current processor, to get the same
      random distribution as the serial case, but they are
      only used if this (i,j) resides on the current procesor.
  */

  for (ig=1; ig<=NI; ig++){
    for (jg=1; jg<=NJ; jg++){
      x = rand()/((float)RAND_MAX + 1);

      /* if this i is on the current processor */
      if ( ig >= i1g && ig <= i2g ) {
        /* local i and j indices, accounting for lower ghost cell */
        i = ig - i1g + 1;
        j = jg;
        if (x<0.5){
          old[i][j] = 0;
        }
        else{
          old[i][j] = 1;
        }
      }
     }
  }

  /*  time steps */
  for (n=0; n<NSTEPS; n++) {
    /* transfer data to ghost cells */
    for (i=1; i<i2; i++){          /* left and right columns */
      old[i][0]  = old[i][j2m];
      old[i][j2] = old[i][1];
    }

    if (nprocs == 1){
      for (j=1; j<j2; j++){          /* top and bottom rows */
        old[0][j]  = old[i2m][j];
        old[i2][j] = old[1][j];
      }

      old[0][0]   = old[i2m][j2m];  /* corners */
      old[0][j2]  = old[i2m][1];
      old[i2][0]  = old[1][j2m];
      old[i2][j2] = old[1][1];
    }
    else {
      if (myid == 0){
//TODO: send and receive top and bottom rows
        MPI_Send( ... );
        MPI_Recv( ... );
        MPI_Send( ... );
        MPI_Recv( ... );

//TODO: send and receive corners 
        MPI_Send( ... );
        MPI_Recv( ... );
        MPI_Send( ... );
        MPI_Recv( ... );
      }
      else {
//TODO: receive and send top and bottom rows 
        MPI_Recv( ... );
        MPI_Send( ... );
        MPI_Recv( ... );
        MPI_Send( ... );

//TODO: recv and send corners 
        MPI_Recv( ... );
        MPI_Send( ... );
        MPI_Recv( ... );
        MPI_Send( ... );
      }
    }

    for (i=1; i<i2; i++) {
      for (j=1; j<j2; j++) {
        im = i-1;
        ip = i+1;
        jm = j-1;
        jp = j+1;

        nsum =  old[im][jp] + old[i][jp] + old[ip][jp]
              + old[im][j ]              + old[ip][j ]
              + old[im][jm] + old[i][jm] + old[ip][jm];

        switch(nsum) {
          case 3:
            new[i][j] = 1;
            break;
          case 2:
            new[i][j] = old[i][j];
            break;
          default:
            new[i][j] = 0;
        }
      }
    }

    /* copy new state into old state */
    for (i=1; i<ni; i++) {
      for (j=1; j<nj; j++) {
        old[i][j] = new[i][j];
      }
    }
  }

  /*  Iterations are done; sum the number of live cells */
  isum = 0;

  for (i=1; i<ni; i++) {
    for (j=1; j<nj; j++) {
      isum = isum + new[i][j];
    }
  }

  /* Print final number of live cells. */

//TODO: Find the total number of live cells 
  if (nprocs > 1){
    if (myid == 0){
      MPI_Recv( ... );
      isum = isum + isum1;
    }
    else {
      MPI_Send( ... );
    }
  }

  if (myid == 0) printf("Number of live cells = %d\n", isum);
  MPI_Finalize();
}
