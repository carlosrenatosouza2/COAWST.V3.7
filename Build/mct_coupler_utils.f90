      MODULE mct_coupler_utils_mod
      implicit none
      PRIVATE
      PUBLIC  :: mct_getarg
      CONTAINS
!-----------------------------------------------------------------------
      SUBROUTINE mct_getarg (Iarg, Carg)
!-----------------------------------------------------------------------
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: Iarg
      character (len=*), intent(inout) :: Carg
!
!  Local variable declarations.
!
      integer :: Lstr, ierror
      CALL getarg (Iarg, Carg)
      RETURN
      END SUBROUTINE mct_getarg
      END MODULE mct_coupler_utils_mod
