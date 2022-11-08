/************************************
      Conway Game of Life

 4 processors, domain decomposition
 in both i and j directions, virtual
 topology

*************************************/

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

#define NI 200
#define NJ 200
#define NSTEPS 500

void main(int argc, char *argv[]){

  int i, j, n, im, ip, jm, jp, nsum, isum, isum1, nprocs ,myid;
  int ig, jg, i1g, i2g, j1g, j2g, ninom, njnom, ninj, 
      i1, i2, j1, j2, ni, nj, i2m, j2m;
  int niproc, njproc, isumloc;
  int proc_north, proc_south, proc_east, proc_west, 
      proc_ne, proc_nw, proc_se, proc_sw;
  int **old, **new, *old1d, *new1d;
  int ndims, reorder, mycartid, ewdir, nsdir;
  int dim_size[2], periods[2];

  float x;

  MPI_Status status;
  MPI_Datatype column_type;
  MPI_Request request[16];
  MPI_Comm comm_cart;

  /* initialize MPI */

  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&myid);

//TODO: Domain decomposition using Cartesian Topology
  /* nominal number of points per proc. (without
     ghost cells, assume numbers divide evenly);
     dim_size[0] and dim_size[1] are the number
     of procs in the i and j directions */

  if(nprocs == 1){
    ndims = 1;
  }else{
    ndims = ... 
  }

  dim_size[0] = ...
  dim_size[1] = ...

  ninom = ...
  njnom = ...

//TODO: Create virtual topology
  /* presently set up for only 1 or 4 procs */
  nsdir = 0;
  if(nprocs == 1){
    ewdir = 0;
  }else{
    ewdir = 1;
  }
  periods[0] = ...
  periods[1] = ...
  reorder = 0;
  MPI_Cart_create( ... );
  MPI_Cart_shift( ... );
  MPI_Cart_shift( ... );
  MPI_Cart_shift( ... );
  MPI_Cart_shift( ... );

//TODO: Define processors on corners
  /* Virtual topology has been used to define adjacent processors
     to the north, south, east, and west; still have to define
     processors on corners */

  if(nprocs > 1){
    switch(myid){
    case 0:
      proc_nw    = ... 
      proc_ne    = ...
      proc_se    = ... 
      proc_sw    = ...
      break;
    case 1:
      proc_nw    = ...
      proc_ne    = ...
      proc_se    = ...
      proc_sw    = ...
      break;
    case 2:
      proc_nw    = ...
      proc_ne    = ...
      proc_se    = ...
      proc_sw    = ...
      break;
    case 3:
      proc_nw    = ...
      proc_ne    = ...
      proc_se    = ...
      proc_sw    = ...
    }
  }

  /* local starting and ending index, including 2 ghost cells
     (one at bottom, one at top) */

  i1  = 0;
  i2  = ninom + 1;
  i2m = i2 - 1;
  j1  = 0;
  j2  = njnom + 1;
  j2m = j2 - 1;
  
  
  /* global starting and ending indices (without ghost cells) */

  i1g = (myid/2)*ninom + 1;
  j1g = (myid%2)*njnom + 1;
  i2g = i1g + ninom - 1;
  j2g = j1g + njnom - 1;

  /* allocate arrays; want elements to be contiguous,
     so allocate 1-D arrays, then set pointer to each row
     (old and new) to allow use of array notation for convenience */

  ni = i2-i1+1;
  nj = j2-j1+1;
  ninj = ni*nj;  /* number of points on current processor */

  old1d = malloc(ninj*sizeof(int));
  new1d = malloc(ninj*sizeof(int));
  old   = malloc(ni*sizeof(int*));  /* allocate pointers to rows */
  new   = malloc(ni*sizeof(int*));

  for(i=0; i<ni; i++){      /* set row pointers to appropriate */
    old[i] = &old1d[i*nj];  /* locations in 1-D arrays         */
    new[i] = &new1d[i*nj];
  }

  /*  Initialize elements of old to 0 or 1.
      We're doing some sleight of hand here to make sure we
      initialize to the same values as in the serial case.
      The rand() function is called for every i and j, even
      if they are not on the current processor, to get the same
      random distribution as the serial case, but they are
      only used if i and j reside on the current procesor. */

  for(ig=1; ig<=NI; ig++){
    for(jg=1; jg<=NJ; jg++){
      x = rand()/((float)RAND_MAX + 1);

      /* if this ig and jg are on the current processor */
      if( ig >= i1g && ig <= i2g && jg >= j1g && jg <= j2g ){

        /* local i and j indices, accounting for lower ghost cell */
        i = ig - i1g + 1;
        j = jg - j1g + 1;

        if(x<0.5){
          old[i][j] = 0;
        }else{
          old[i][j] = 1;
        }
      }

    }
  }

//TODO: Create derived type for single column of local array.
   /*There are ninom "blocks," each containing 1 element,
     with a stride of nj between the blocks */

  MPI_Type_vector( ... );
  MPI_Type_commit( ... );

  /* iterate */

  for(n=0; n<NSTEPS; n++){

    /* copy or transfer data to ghost cells */

    if(nprocs == 1){
      for(i=1; i<i2; i++){          /* left and right columns */
        old[i][0]  = old[i][j2m];
        old[i][j2] = old[i][1];
      }
      for(j=1; j<j2; j++){          /* top and bottom rows */
        old[0][j]  = old[i2m][j];
        old[i2][j] = old[1][j];
      }
      old[0][0]   = old[i2m][j2m];  /* corners */
      old[0][j2]  = old[i2m][1];
      old[i2][0]  = old[1][j2m];
      old[i2][j2] = old[1][1];
    }else{
//TODO: send and receive left and right columns using derived type 

      MPI_Isend( ... );
      MPI_Irecv( ... );
      MPI_Isend( ... );
      MPI_Irecv( ... );

//TODO: send and receive top and bottom rows 

      MPI_Isend( ... );
      MPI_Irecv( ... );
      MPI_Isend( ... );
      MPI_Irecv( ... );

//TODO: send and receive corners 

      MPI_Isend( ... );
      MPI_Irecv( ... );
      MPI_Isend( ... );
      MPI_Irecv( ... );
      MPI_Isend( ... );
      MPI_Irecv( ... );
      MPI_Isend( ... );
      MPI_Irecv( ... );

// TODO:  Make sure all non-blocking messages have arrived 

      for(i=0; i<16; i++){
        MPI_Wait( ... );
      }
    }

    /* update states of cells */

    for(i=1; i<i2; i++){
      for(j=1; j<j2; j++){
                
        /* Periodic boundary conditions are
           maintained through ghost cells. */

        im = i-1;
        ip = i+1;
        jm = j-1;
        jp = j+1;
        nsum =  old[im][jp] + old[i][jp] + old[ip][jp]
              + old[im][j ]              + old[ip][j ] 
              + old[im][jm] + old[i][jm] + old[ip][jm];

        switch(nsum){
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
    
    for(i=1; i<i2; i++){
      for(j=1; j<j2; j++){
        old[i][j] = new[i][j];
      }
    }
  }

  /*  Iterations are done; sum the number of live cells */

  isum = 0;
  for(i=1; i<i2; i++){
    for(j=1; j<j2; j++){
      isum = isum + new[i][j];
    }
  }

//TODO: Reduce partial sums 
  /* Print final number of live cells.  For multiple processors,
     must reduce partial sums */
  
  if(nprocs > 1){
    isumloc = isum;
    MPI_Reduce( ... );
  }
  if(myid == 0) printf("nNumber of live cells = %d\n", isum);
  
  MPI_Finalize();
}
