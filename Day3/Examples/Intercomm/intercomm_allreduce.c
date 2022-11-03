#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv){
    int ierror, color, msg, msg1, local_rcv, inter_rcv[2], dummy;
    int myRank,uniSize,local_rank,Root_group;
    int version, subversion;
    int iMyName;
    // We initialise a char array that is guaranteed to be big enough to store the returned processor name
    char myName[MPI_MAX_PROCESSOR_NAME];

    MPI_Comm SplitComm, InterComm0, InterComm1;


    // Must call MPI_Init before any other MPI calls
    ierror=MPI_Init(&argc,&argv);



    // We take a command line argument to decide the number of loops to do
    // Get the size of the communicator
    ierror=MPI_Comm_size(MPI_COMM_WORLD,&uniSize);
    // Get our rank in this communicator
    ierror=MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
    // Get the name of the processer we are running on
    ierror=MPI_Get_processor_name(myName,&iMyName);
    // Get what MPI version we are using
    ierror=MPI_Get_version(&version,&subversion);
    printf("I am process %d out of %d running on %s with MPI version %d.%d\n",
            myRank,uniSize,myName,version,subversion);

    if ( uniSize < 4) {
       printf(" Must be 4 or more ranks, terminalting\n");
       MPI_Abort( MPI_COMM_WORLD, 11);
    }
 
    // Set up groups: root group with 2 ranks and remaining two leaf groups
    if (myRank < 2) {
      color = 2;
      Root_group = 1;
    } else {
      color = myRank%2;
      Root_group = 0;
    }


    // Create the intracomms for each of the groups
    ierror = MPI_Comm_split(MPI_COMM_WORLD, color, 0, &SplitComm);
    if (ierror != 0) {
      printf("Cannot split communicator, terminating\n");
      MPI_Abort(MPI_COMM_WORLD,12);
    }
      

    MPI_Comm_rank(SplitComm, &local_rank);

    /*
     * Setup intercomm, rank 0 in root group -> even group, local leader is rank=2
     *                  rank 1 in root group ->  odd group, local leader is rank=3
     * local_leader is rank within the group
     * remote_leader is the rank in MPI_COMM_WORLD
     *
     * We must now split the intercommuncator because no rank is specified in Allreduce
     * Both InterComm0 and InterComm1 defined in the Root_group but only one defined in 
     * non-root group.
     */
 
    if (Root_group) {
    // Root_group 0 -> Even group
       ierror = MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 2, 0, &InterComm0);
    // Root_group 1 -> Odd group
       ierror = MPI_Intercomm_create(SplitComm, 1, MPI_COMM_WORLD, 3, 0, &InterComm1);
    // Non-root groups
    } else {
    // Even group -> Root_group 0 = myRank
       if (myRank%2 == 0) {
         ierror = MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 0, 0, &InterComm0);
       } else {
    // Odd group -> Root_group 1 = myRank
         ierror = MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 1, 0, &InterComm1);
       }
   }
    if (ierror != 0) {
       printf(" InterComm not created for rank=%d",myRank);
       MPI_Abort(MPI_COMM_WORLD,13);
    }


    // Allreduce to local group
    msg = 10 + myRank; 
    MPI_Allreduce(&msg,&local_rcv,1,MPI_INT,MPI_SUM,SplitComm);


    inter_rcv[0] = -1; inter_rcv[1] = -1;
    msg1 = 100 + myRank;

    /* 
     * We wish that, in the Root_group, the data is received by the local_leader.
     * Because of how Alreduce works each rank gets the data from both non-root groups.
     */
    if (Root_group) {
      MPI_Allreduce(&msg1,&inter_rcv[0],1,MPI_INT,MPI_SUM,InterComm0);
      MPI_Allreduce(&msg1,&inter_rcv[1],1,MPI_INT,MPI_SUM,InterComm1);
    } else {
      if (myRank%2 == 0) {
         MPI_Allreduce(&msg1,&inter_rcv[0],1,MPI_INT,MPI_SUM,InterComm0);
      } else {
         MPI_Allreduce(&msg1,&inter_rcv[0],1,MPI_INT,MPI_SUM,InterComm1);
      }
    }

    


    printf("MyRank=%d, locol_rank=%d, color=%d\n msg=%d, msg1=%d\n local_rcv=%d\n inter_rcv[0]=%d, \
            inter_rcv[1]=%d\n",myRank,local_rank,color,msg,msg1,local_rcv,inter_rcv[0],inter_rcv[1]);




    // Destroy comminicators
    if (Root_group) {
       MPI_Comm_free(&InterComm0);
       MPI_Comm_free(&InterComm1);
    } else {
      if (myRank%2 == 0) {
         MPI_Comm_free(&InterComm0);
      } else {
         MPI_Comm_free(&InterComm1);
      }
    }    
    MPI_Comm_free(&SplitComm);



    // Remember to always call MPI_Finalize()
    ierror=MPI_Finalize();
    return ierror;
}
