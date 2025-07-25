      SUBROUTINE set_grid (ng, model)
!
!svn $Id: set_grid.F 1054 2021-03-06 19:47:12Z arango $
!=======================================================================
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!================================================== Hernan G. Arango ===
!                                                                      !
!  This routine sets application grid and associated variables and     !
!  parameters. It called only once during the initialization stage.    !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_scalars
!
      USE analytical_mod
      USE distribute_mod, ONLY : mp_bcasti
      USE metrics_mod,    ONLY : metrics
      USE strings_mod,    ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      integer :: tile
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/set_grid.F"
!
!-----------------------------------------------------------------------
!  Set horizontal grid, bathymetry, and Land/Sea masking (if any).
!  Use analytical functions or read in from a grid NetCDF.
!-----------------------------------------------------------------------
!
!$OMP MASTER
      CALL get_grid (ng, model)
!$OMP END MASTER
      CALL mp_bcasti (ng, model, exit_flag)
!$OMP BARRIER
      IF (FoundError(exit_flag, NoError, 72, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Set vertical terrain-following coordinate transformation function.
!-----------------------------------------------------------------------
!
!$OMP MASTER
      CALL set_scoord (ng)
!$OMP END MASTER
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  Set barotropic time-steps average weighting function.
!-----------------------------------------------------------------------
!
!$OMP MASTER
      CALL set_weights (ng)
!$OMP END MASTER
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  Compute various metric term combinations.
!-----------------------------------------------------------------------
!
      DO tile=first_tile(ng),last_tile(ng),+1
        CALL metrics (ng, tile, model)
      END DO
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  If appropriate, set spatially varying nudging coefficients time
!  scales.
!-----------------------------------------------------------------------
!
      IF (Lnudging(ng)) THEN
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL ana_nudgcoef (ng, tile, model)
        END DO
!$OMP BARRIER
      END IF
!
      RETURN
      END SUBROUTINE set_grid
