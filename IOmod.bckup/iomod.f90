module iomod

   use mpi


   implicit none
   
   private
   public :: IOmessage

   contains

   subroutine IOmessage ()

      integer :: ierror, nprocs, myrank
      
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierror)
      call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierror)
      
      if (myrank == 0) then
         print*, 'Rank: ', myrank, ' from ', nprocs, ' ranks. Hello from IOmessage/IOmod.'
      endif
      
      

   end subroutine IOmessage

end module iomod
