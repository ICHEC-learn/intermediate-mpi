/*
 * 
 * Hello World MPI Example
 * 
*/
#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){

  int ierror;
  int myRank,uniSize;
  int version, subversion;
  int iMyName;
  char myName[MPI_MAX_PROCESSOR_NAME];

  int flag = 0;
  ierror=MPI_Initialized(&flag);
  if (flag==0){
    printf("MPI_Initialized returned false before MPI_Init.\n");
  }

  ierror=MPI_Init(&argc,&argv);
  ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
  ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  ierror=MPI_Get_processor_name(myName,&iMyName);
  ierror=MPI_Get_version(&version,&subversion);
  printf("I am process %d out of %d running on %s with MPI version %d.%d\n",
      myRank,uniSize,myName,version,subversion);
  ierror=MPI_Finalize();

  return ierror;
}

