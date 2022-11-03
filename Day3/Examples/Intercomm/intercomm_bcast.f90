
program intercomm
    use mpi_f08
    implicit none
    integer :: ierror, color
    integer (kind=4) :: msg, msg1
    integer :: myRank,uniSize,local_rank,root,Root_group,remote_leader
    integer :: iMyName, version, subversion
! We initialise a char array that is guaranteed to be big enough to store the returned processor name
    character (len=MPI_MAX_PROCESSOR_NAME) :: myName

    type(MPI_Comm) :: SplitComm_handle, InterComm_handle


! Must call MPI_Init before any other MPI calls
    call MPI_Init()


! Get the size of the communicator
    call MPI_Comm_size(MPI_COMM_WORLD,uniSize)
! Get our rank in this communicator
    call MPI_Comm_rank(MPI_COMM_WORLD,myRank)
! Get the name of the processer we are running on
    call MPI_Get_processor_name(myName,iMyName)
! Get what MPI version we are using
    call MPI_Get_version(version,subversion);
    write(6,fmt='(a,i0,a,i0,a,a,a,i0,a,i0)') 'I am process ',myrank,' out of ',uniSize, &
&                       ' running on ',myName,' with MPI version ',version,'.',subversion


    if ( uniSize .LT. 4) then
       write(6,*) ' Must be 4 or more ranks, terminalting'
       call MPI_Abort( MPI_COMM_WORLD, 11);
    endif
 
! Set up groups: root group with 2 ranks and remaining two leaf groups
    if (myRank .lt. 2) then
      color = 2
      Root_group = 1
    else
      color = modulo(myRank,2)
      Root_group = 0
    endif


! Create the intracomms for each of the groups
    call MPI_Comm_split(MPI_COMM_WORLD, color, 0, SplitComm_handle, ierror)
    if (ierror .NE. 0) then
      write(6,*) 'Cannot split communicator, terminating'
      call MPI_Abort(MPI_COMM_WORLD,12)
    endif
      

    call MPI_Comm_rank(SplitComm_handle, local_rank);

! Setup intercomm, rank 0 in root group -> even group, local leader is rank=2
!                  rank 1 in root group ->  odd group, local leader is rank=3
! local_leader is rank within the group
! remote_leader is the rank in MPI_COMM_WORLD
    if (Root_group .EQ. 1) then
       remote_leader = 2 + modulo(myRank,2)
       call MPI_Intercomm_create(SplitComm_handle, local_rank, MPI_COMM_WORLD, remote_leader, 0, &
&           InterComm_handle, ierror)
    else
       remote_leader = mod(myRank,2)
       call MPI_Intercomm_create(SplitComm_handle, 0, MPI_COMM_WORLD, remote_leader, 0, InterComm_handle, ierror)
    endif
    if (ierror .NE. 0) then
       write(6,*) ' InterComm not created for rank=',myRank
       call MPI_Abort(MPI_COMM_WORLD,13)
    endif


! Broadcast to local group
    msg = 10 + myRank 
    root = 0
    call MPI_Bcast(msg,1,MPI_INTEGER4,root,SplitComm_handle)

! Broadcast over intercomm
    msg1 = 100 + myRank
    if (Root_group .EQ. 1) then
      root = MPI_ROOT;
    else 
      root = mod(myRank,2)
    endif
    call MPI_Bcast(msg1,1,MPI_INTEGER4,root,InterComm_handle);

    write(6,fmt='(a,i0,a,i0,a,i0)') 'MyRank=',myRank,' locol_rank=',local_rank,' color=',color
    write(6,fmt='(a,i0)') ' msg=',msg
    write(6,fmt='(a,i0)') ' msg1=',msg1


! Destroy comminicators
    call MPI_Comm_free(InterComm_handle)
    call MPI_Comm_free(SplitComm_handle)



! Remember to always call MPI_Finalize()
    call MPI_Finalize()
end program intercomm
