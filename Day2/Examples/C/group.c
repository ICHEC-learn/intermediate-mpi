/*
 *
 * Create a group by including processes with odd ranks from another group
 *
*/

#include <stdio.h> 
#include <mpi.h> 

int main(int argc, char **argv){
  int uniSize, ierror; 
  int odd_ranks[3]={1, 3, 5}, newRank, oddRank;
  MPI_Group new_group, odd_group;
  MPI_Comm odd_comm;   

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  
  if(uniSize==6){
    ierror=MPI_Comm_group(MPI_COMM_WORLD, &new_group);
    ierror=MPI_Group_rank(new_group, &newRank);
    ierror=MPI_Group_incl(new_group, uniSize/2, odd_ranks, &odd_group);
    ierror=MPI_Comm_create(MPI_COMM_WORLD, odd_group, &odd_comm);
    ierror=MPI_Group_rank(odd_group, &oddRank);

    if(oddRank != MPI_UNDEFINED){
      printf("I am process %d in new group and %d in the odd group.\n", newRank, oddRank);
    }
    else{
      printf("I am process %d in new group but I am not part of the odd group.\n", newRank);
    }
  }

  ierror=MPI_Finalize();
  return ierror;
}

