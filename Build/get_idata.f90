      SUBROUTINE get_idata (ng)
!
!svn $Id: get_idata.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads input data that needs to be obtained only once.  !
!                                                                      !
!  Currently,  this routine is only executed in serial mode by the     !
!  main thread.                                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_parallel
      USE mod_scalars
      USE mod_sources
      USE mod_stepping
!
      USE nf_fread3d_mod, ONLY : nf_fread3d
      USE nf_fread4d_mod, ONLY : nf_fread4d
      USE strings_mod,    ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      logical, dimension(3) :: update =                                 &
     &         (/ .FALSE., .FALSE., .FALSE. /)
!
      integer :: LBi, UBi, LBj, UBj
      integer :: itrc, is
      real(r8) :: time_save = 0.0_r8
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Nonlinear/get_idata.F"
!
      SourceFile=MyFile
!
!  Lower and upper bounds for tiled arrays.
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Turn on input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_on (ng, iNLM, 3, 102, MyFile)
!
!-----------------------------------------------------------------------
!  Read in point Sources/Sinks position, direction, special flag, and
!  mass transport nondimensional shape profile.  Point sources are at
!  U- and V-points.
!-----------------------------------------------------------------------
!
      IF ((iic(ng).eq.0).and.                                           &
     &    (LuvSrc(ng).or.LwSrc(ng).or.ANY(LtracerSrc(:,ng)))) THEN
        CALL get_ngfld (ng, iNLM, idRxpo, SSF(ng)%ncid,                 &
     &                  1, SSF(ng), update(1),                          &
     &                  1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,              &
     &                  SOURCES(ng) % Xsrc)
        IF (FoundError(exit_flag, NoError, 535, MyFile)) RETURN
        CALL get_ngfld (ng, iNLM, idRepo, SSF(ng)%ncid,                 &
     &                  1, SSF(ng), update(1),                          &
     &                  1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,              &
     &                  SOURCES(ng) % Ysrc)
        IF (FoundError(exit_flag, NoError, 541, MyFile)) RETURN
        CALL get_ngfld (ng, iNLM, idRdir, SSF(ng)%ncid,                 &
     &                  1, SSF(ng), update(1),                          &
     &                  1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,              &
     &                  SOURCES(ng) % Dsrc)
        IF (FoundError(exit_flag, NoError, 547, MyFile)) RETURN
        CALL get_ngfld (ng, iNLM, idRvsh, SSF(ng)%ncid,                 &
     &                  1, SSF(ng), update(1),                          &
     &                  1, Nsrc(ng), N(ng), 1, 1, Nsrc(ng), N(ng),      &
     &                  SOURCES(ng) % Qshape)
        IF (FoundError(exit_flag, NoError, 554, MyFile)) RETURN
        DO is=1,Nsrc(ng)
          SOURCES(ng)%Isrc(is)=                                         &
     &                MAX(1,MIN(NINT(SOURCES(ng)%Xsrc(is)),Lm(ng)+1))
          SOURCES(ng)%Jsrc(is)=                                         &
     &                MAX(1,MIN(NINT(SOURCES(ng)%Ysrc(is)),Mm(ng)+1))
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Turn off input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_off (ng, iNLM, 3, 593, MyFile)
!
      RETURN
      END SUBROUTINE get_idata
