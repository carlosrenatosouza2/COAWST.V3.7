program main

   use iomod
   use mpi

   implicit none
   
   integer :: ierror, nprocs, myrank
   
   call MPI_Init(ierror)
!   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)
!   call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierror)
   
!   if (myrank == 0) then
!      print*, 'Rank: ', myrank, ' from ', nprocs, ' ranks.'
!   endif


   call IOmessage()


   call MPI_Finalize(ierror)
end program main
