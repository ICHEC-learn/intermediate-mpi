program intercomm_allreduce
    use mpi_f08
    implicit none
    integer (kind=4) :: msg, msg1, local_rcv, inter_rcv(2)
    integer :: myRank,uniSize,local_rank,color,ierror
    logical (kind=1) :: Root_group
    integer :: iMyName, version, subversion
! We initialise a char array that is guaranteed to be big enough to store the returned processor name
    character (len=MPI_MAX_PROCESSOR_NAME) :: myName

    type(MPI_Comm) :: SplitComm, InterComm0, InterComm1


! Must call MPI_Init before any other MPI calls
    call MPI_Init()




! Get the size of the communicator
    call MPI_Comm_size(MPI_COMM_WORLD,uniSize)
! Get our rank in this communicator
    call MPI_Comm_rank(MPI_COMM_WORLD,myRank)
! Get the name of the processer we are running on
    call MPI_Get_processor_name(myName,iMyName)
! Get what MPI version we are using
    call MPI_Get_version(version,subversion)
    write(6,fmt='(a,i0,a,i0,a,a,a,i0,a,i0)')'I am process ',myRank,' out of ',uniSize,' running on ',myName, &
&      ' with MPI version ',version,'.',subversion

    if ( uniSize .LT. 4) then
       write(6,*) ' Must be 4 or more ranks, terminalting'
       call MPI_Abort( MPI_COMM_WORLD, 11);
    endif
 
! Set up groups: root group with 2 ranks and remaining two leaf groups
    if (myRank .LT. 2) then
      color = 2
      Root_group = .TRUE.
    else
      color = modulo(myRank,2)
      Root_group = .FALSE.
    endif


! Create the intracomms for each of the groups
    call MPI_Comm_split(MPI_COMM_WORLD, color, 0, SplitComm, ierror);
    if (ierror .NE. 0) then
      write(6,*) ' Cannot split communicator, terminating'
      call MPI_Abort(MPI_COMM_WORLD,12)
    endif
      

    call MPI_Comm_rank(SplitComm, local_rank)

    
!     * Setup intercomm, rank 0 in root group -> even group, local leader is rank=2
!     *                  rank 1 in root group ->  odd group, local leader is rank=3
!     * local_leader is rank within the group
!     * remote_leader is the rank in MPI_COMM_WORLD
!     *
!     * We must now split the intercommuncator because no rank is specified in Allreduce
!     * Both InterComm0 and InterComm1 defined in the Root_group but only one defined in 
!     * non-root group.

 
    if (Root_group) then
! Root_group 0 -> Even group
       call MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 2, 0, InterComm0, ierror)
! Root_group 1 -> Odd group
       call MPI_Intercomm_create(SplitComm, 1, MPI_COMM_WORLD, 3, 0, InterComm1, ierror)

! Non-root groups
    else
! Even group -> Root_group 0 = myRank
       if (mod(myRank,2) .EQ. 0) then
         call MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 0, 0, InterComm0, ierror)
       else
! Odd group -> Root_group 1 = myRank
         call MPI_Intercomm_create(SplitComm, 0, MPI_COMM_WORLD, 1, 0, InterComm1, ierror)
       endif
    endif

    if (ierror .NE. 0) then
       write(6,*) ' InterComm not created for rank=',myRank
       call MPI_Abort(MPI_COMM_WORLD,13);
    endif


! Allreduce to local group
    msg = 10 + myRank
    call MPI_Allreduce(msg,local_rcv,1,MPI_INTEGER4,MPI_SUM,SplitComm)


    inter_rcv = -1
    msg1 = 100 + myRank

! 
!     * The Root_group ranks will receive the sums from both non-root groups.
!     * The non-root groups again will receive data from the root_group.
!
    if (Root_group) then
      call MPI_Allreduce(msg1,inter_rcv(1),1,MPI_INTEGER4,MPI_SUM,InterComm0)
      call MPI_Allreduce(msg1,inter_rcv(2),1,MPI_INTEGER4,MPI_SUM,InterComm1)
    else 
      if (mod(myRank,2) .EQ. 0) then
         call MPI_Allreduce(msg1,inter_rcv(1),1,MPI_INTEGER4,MPI_SUM,InterComm0)
      else
         call MPI_Allreduce(msg1,inter_rcv(1),1,MPI_INTEGER4,MPI_SUM,InterComm1)
      endif
    endif

    


    write(6,fmt='(3(a,i0))') ' MyRank=',myRank,' locol_rank=',local_rank,' color=',color
    write(6,fmt='(2(a,i0))') ' msg=',msg,' msg1=',msg1
    write(6,fmt='(a,i0)')    'local_rcv=',local_rcv 
    write(6,fmt='(2(a,i0))') ' inter_rcv(1)=',inter_rcv(1),' inter_rcv(2)=',inter_rcv(2)




! Destroy comminicators
    if (Root_group) then
       call MPI_Comm_free(InterComm0)
       call MPI_Comm_free(InterComm1)
    else
      if (mod(myRank,2) .EQ. 0) then
         call MPI_Comm_free(InterComm0)
      else
         call MPI_Comm_free(InterComm1)
      endif
    endif    
    call MPI_Comm_free(SplitComm)



! Remember to always call MPI_Finalize()
    call MPI_Finalize()
end program intercomm_allreduce
