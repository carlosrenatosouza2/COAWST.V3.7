module iomod

   use mpi


   implicit none
   
   private
   public :: IOmessage

   contains

   subroutine IOmessage (MyCOMM)

      integer, intent(in)  :: MyCOMM
      integer              :: nprocsCW, myrankCW, ierror
      integer              :: nprocs, myrank
      
      call MPI_Comm_size(MPI_COMM_WORLD, nprocsCW, ierror)
      call MPI_Comm_rank(MPI_COMM_WORLD, myrankCW, ierror)

      call MPI_Comm_size(MyCOMM, nprocs, ierror)
      call MPI_Comm_rank(MyCOMM, myrank, ierror)
      
      !print*, 'Rank: ', myrankCW, '/', nprocsCW, ' COMM_WORLD', MPI_COMM_WORLD
      !print*, 'Rank: ', myrank, '/', nprocs, ' MyCOMM', MyCOMM
      
      

   end subroutine IOmessage

end module iomod
