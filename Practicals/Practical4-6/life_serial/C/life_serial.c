/*
*
*  Conway Game of Life
*
*  COMPILE: gcc -o life life_serial.c
*  RUN: ./life
* 
*/

#include <stdio.h>
#include <stdlib.h>
#define NI 200 /* array sizes */
#define NJ 200  
#define NSTEPS 500 /* number of time steps */

void main()
{
  int i, j, n, im, ip, jm, jp, ni, nj, nsum, isum;
  int *old, *new;
  float x;

  /* allocate arrays */
  ni = NI + 2; /* add 2 for left and right ghost cells */
  nj = NJ + 2;

  old = malloc(ni*nj*sizeof(int));
  new = malloc(ni*nj*sizeof(int));


  /*  initialize elements of old to 0 or 1 *
   *  use this form so that it is directly *
   *  comparable with parallel version     */
  for(i=1; i<=NI; i++) {
    srand(1234+i);
    for(j=1; j<=NJ; j++) {


      x = rand()/((float)RAND_MAX + 1);
      if(x < 0.5) {
          old[i*nj+j] = 0;
      } else {
          old[i*nj+j] = 1;
      }
    }
  }




  isum = 0;

  for(i=1; i<=NI; i++) {
    for(j=1; j<=NJ; j++) {
      isum = isum + old[i*nj+j];
    }
  }

  printf("\nIntial Number of live cells = %d\n", isum);



  /*  time steps */
  for(n=0; n<NSTEPS; n++) {

    /* corner boundary conditions */
    old[0] = old[NI*nj+NJ];
    old[NJ+1] = old[NI*nj+1];

    old[(NI+1)*nj+NJ+1] = old[nj+1];
    old[(NI+1)*nj] = old[nj+NJ];

    /* left-right boundary conditions */
    for(i=1; i<=NI; i++) {
      old[i*nj] = old[i*nj+NJ];
      old[i*nj+NJ+1] = old[i*nj+1];
    }

    /* top-bottom boundary conditions */
    for(j=1; j<=NJ; j++) {
      old[j] = old[NI*nj+j];
      old[(NI+1)*nj+j] = old[nj+j];
    }



    for(i=1; i<=NI; i++) {
      for(j=1; j<=NJ; j++) {

        im = i-1;
        ip = i+1;
        jm = j-1;
        jp = j+1;

        nsum =  old[im*nj+jp] + old[i*nj+jp] + old[ip*nj+jp]
              + old[im*nj+j ]              + old[ip*nj+j ]
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
    for(i=1; i<=NI; i++) {
      for(j=1; j<=NJ; j++) {
        old[i*nj+j] = new[i*nj+j];
      }
    }
  }

  /*  Iterations are done; sum the number of live cells */
  isum = 0;

  for(i=1; i<=NI; i++) {
    for(j=1; j<=NJ; j++) {
      isum = isum + new[i*nj+j];
    }
  }

  printf("\nNumber of live cells = %d\n", isum);

  free(old); free(new);
}
