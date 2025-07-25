      MODULE mct_coupler_params
      USE mod_coupler_kinds
      implicit none
       INTEGER, SAVE :: ATM_COMM_WORLD
!
! Local module variables
!
! Number of coupling models.
!
      integer :: N_mctmodels
!
! Sparse matrix weights
!
      integer, dimension(:), pointer :: sparse_rows
      integer, dimension(:), pointer :: sparse_cols
      integer, dimension(:), pointer :: dst_grid_imask
      integer, dimension(2) :: src_grid_dims, dst_grid_dims
      real(m8), dimension(:), pointer :: sparse_weights
      TYPE T_DST_GRID
        integer, pointer :: dst_mask(:)
      END TYPE T_DST_GRID
      TYPE (T_DST_GRID), allocatable :: O2A_CPLMASK(:,:)
      TYPE (T_DST_GRID), allocatable :: A2O_CPLMASK(:,:)
      TYPE (T_DST_GRID), allocatable :: W2A_CPLMASK(:,:)
!
! Number of parallel nodes assigned to each model in the coupled
! system.
!
      integer :: NnodesATM
      integer :: NnodesWAV
      integer :: NnodesOCN
      integer :: NnodesHYD
      integer :: NnodesIOM !CR
!
! Parallel nodes assined to the atmosphere model.
!
      integer :: peATM_frst ! first atmosphere parallel node
      integer :: peATM_last ! last atmosphere parallel node
!
! Parallel nodes assined to the ocean model.
!
      integer :: peOCN_frst ! first ocean parallel node
      integer :: peOCN_last ! last ocean parallel node
      integer, dimension(:), pointer :: roms_fwcoup
      integer, dimension(:), pointer :: roms_2wcoup
      integer, dimension(:), pointer :: roms_facoup
      integer, dimension(:), pointer :: roms_2acoup
!
! Time interval (seconds) between coupling of models.
!
      real(m8) :: TI_ATM2WAV ! atmosphere to wave coupling interval
      real(m8) :: TI_ATM2OCN ! atmosphere to ocean coupling interval
      real(m8) :: TI_WAV2ATM ! wave to atmosphere coupling interval
      real(m8) :: TI_WAV2OCN ! wave to ocean coupling interval
      real(m8) :: TI_OCN2WAV ! ocean to wave coupling interval
      real(m8) :: TI_OCN2ATM ! ocean to atmosphere coupling interval
      real(m8) :: TI_OCN2HYD ! ocean to hydrology coupling interval
      real(m8) :: TI_HYD2OCN ! hydrology to ocean coupling interval
!
! Number of atmosphere model time-steps and atmosphere model ID.
!
      integer :: Natm_grids
      integer :: Nocn_grids
      integer :: Nwav_grids
      integer :: Nhyd_grids
      real(m8), dimension(:), pointer :: dtocn
      real(m8), dimension(:), pointer :: dtwav
      real(m8), dimension(:), pointer :: dtatm
      real(m8), dimension(:), pointer :: dthyd
!
      integer, dimension(:), pointer :: wrf_e_we
      integer, dimension(:,:), pointer :: nOCN2ATM
      integer, dimension(:,:), pointer :: nATM2OCN
      integer, dimension(:,:), pointer :: nOCNFATM
      integer, dimension(:,:), pointer :: nATMFOCN
!
! Coupled model components IDs.
!
      integer, dimension(:), pointer :: ocnids
      integer, dimension(:), pointer :: wavids
      integer, dimension(:), pointer :: atmids
      integer, dimension(:), pointer :: hydids
      integer :: OCNid
      integer :: WAVid
      integer :: ATMid
      integer :: HYDid
      integer, dimension(:), pointer :: moving_nest
! If WRF has a moving nest then we need to keep some information.
      integer, dimension(:), pointer :: pargrid_ratio
      integer, dimension(:), pointer :: full_Isize
      integer, dimension(:), pointer :: full_Jsize
      integer, dimension(:), pointer :: parentid
      integer, dimension(:), pointer :: moving_nest_istart
      integer, dimension(:), pointer :: moving_nest_jstart
      CONTAINS
      SUBROUTINE allocate_coupler_params
!=======================================================================
! !
! This routine initialize all the coupler vars. !
! !
!=======================================================================
      integer :: i
      allocate (nOCN2ATM(Nocn_grids,Natm_grids))
      allocate (nATM2OCN(Natm_grids,Nocn_grids))
      allocate (nOCNFATM(Nocn_grids,Natm_grids))
      allocate (nATMFOCN(Natm_grids,Nocn_grids))
      allocate(O2A_CPLMASK(Nocn_grids,Natm_grids))
      allocate(A2O_CPLMASK(Natm_grids,Nocn_grids))
      allocate (moving_nest(Natm_grids))
! If WRF has a moving nest, for now, it has to be the last grid.
      allocate (pargrid_ratio(Natm_grids))
      allocate (full_Isize(Natm_grids))
      allocate (full_Jsize(Natm_grids))
! allocate (parentid(Natm_grids))
      allocate (moving_nest_istart(Natm_grids))
      allocate (moving_nest_jstart(Natm_grids))
      RETURN
      END SUBROUTINE allocate_coupler_params
      END MODULE mct_coupler_params
