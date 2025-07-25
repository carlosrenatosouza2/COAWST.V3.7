      MODULE ROMS_import_mod
!
!svn $Id: roms_import.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module contains several routines to interpolate imported       !
!  to particular model grid during coupling.                           !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
      implicit none
      PUBLIC :: ROMS_import2d
      CONTAINS
!
!***********************************************************************
      SUBROUTINE ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Npts, InpField,                         &
     &                          Imin, Imax, Jmin, Jmax,                 &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OutFmin, OutFmax,                       &
     &                          OutField,                               &
     &                          status)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
!
      USE exchange_2d_mod
      USE distribute_mod,  ONLY : mp_reduce
      USE mp_exchange_mod, ONLY : mp_exchange2d
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, id, gtype
      integer, intent(in) :: Imin, Imax, Jmin, Jmax
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: Npts
      integer, intent(out) :: status
      real(r8), intent(in) :: scale, add_offset
      real(r8), intent(out) :: OutFmin, OutFmax
      real(r8), intent(in) ::  InpField(:)
      real(r8), intent(out) :: OutField(LBi:,LBj:)
!
!  Local variable declarations.
!
      integer :: i, ij, j
      real(r8), dimension(2) :: range
      character (len=3), dimension(2) :: op_handle
!
!-----------------------------------------------------------------------
!  Import 2D field.
!-----------------------------------------------------------------------
!
      status=0
      range(1)= Large
      range(2)=-Large
!
!  For now couple models grids are identical so no interpolation is
!  necessary. Interpolation logic will be provided in the future.
!
      ij=0
      DO j=Jmin,Jmax
        DO i=Imin,Imax
          ij=ij+1
          OutField(i,j)=scale*InpField(ij)+add_offset
          range(1)=MIN(range(1),OutField(i,j))
          range(2)=MAX(range(2),OutField(i,j))
        END DO
      END DO
!
!  Global reduction for imported field range values.
!
      op_handle(1)='MIN'
      op_handle(2)='MAX'
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      OutFmin=range(1)
      OutFmax=range(2)
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        IF (gtype.eq.r2dvar) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            OutField)
        ELSE IF (gtype.eq.u2dvar) THEN
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            OutField)
        ELSE IF (gtype.eq.v2dvar) THEN
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            OutField)
        END IF
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    OutField)
      END SUBROUTINE ROMS_import2d
      END MODULE ROMS_import_mod
