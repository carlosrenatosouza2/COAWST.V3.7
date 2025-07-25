      SUBROUTINE wrt_diags (ng)
!
!svn $Id: wrt_diags.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine writes model time-averaged diagnostic fields into   !
!  diagnostics NetCDF file.                                            !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_diags
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
      USE strings_mod,     ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: Fcount, gfactor, gtype, ifield, itrc, ivar, status
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_diags.F"
!
      SourceFile=MyFile
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Write out time-averaged diagnostic fields when appropriate.
!-----------------------------------------------------------------------
!
      if (FoundError(exit_flag, NoError, 68, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
        gfactor=1
!
!  Set time and time-record index.
!
      DIA(ng)%Rindex=DIA(ng)%Rindex+1
      Fcount=DIA(ng)%load
      DIA(ng)%Nrec(Fcount)=DIA(ng)%Nrec(Fcount)+1
!
!  Write out averaged time.
!
      CALL netcdf_put_fvar (ng, iNLM, DIA(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), DIAtime(ng:),        &
     &                      (/DIA(ng)%Rindex/), (/1/),                  &
     &                      ncid = DIA(ng)%ncid,                        &
     &                      varid = DIA(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 92, MyFile)) RETURN
!
!  Write out 2D momentum diagnostic fields.
!
      DO ivar=1,NDM2d
        ifield=idDu2d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*u2dvar
          status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid,                    &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % umask,                          &
     &                       DIAGS(ng) % DiaU2d(:,:,ivar),              &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 112, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        ifield=idDv2d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*v2dvar
          status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid,                    &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % vmask,                          &
     &                       DIAGS(ng) % DiaV2d(:,:,ivar),              &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 135, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out 3D momentum diagnostic fields.
!
      DO ivar=1,NDM3d
        ifield=idDu3d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid,                    &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       GRID(ng) % umask_dia,                      &
     &                       DIAGS(ng) % DiaU3d(:,:,:,ivar),            &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 164, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        ifield=idDv3d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid,                    &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       GRID(ng) % vmask_dia,                      &
     &                       DIAGS(ng) % DiaV3d(:,:,:,ivar),            &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 187, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out tracer diagnostic fields.
!
      DO itrc=1,NT(ng)
        DO ivar=1,NDT
          ifield=idDtrc(itrc,ivar)
          IF (Dout(ifield,ng)) THEN
            scale=1.0_dp/dt(ng)
            gtype=gfactor*r3dvar
            status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid,                  &
     &                         DIA(ng)%Vid(ifield),                     &
     &                         DIA(ng)%Rindex, gtype,                   &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), scale,     &
     &                         GRID(ng) % rmask,                        &
     &                         DIAGS(ng) % DiaTrc(:,:,:,itrc,ivar),     &
     &                         SetFillVal = .FALSE.)
            IF (FoundError(status, nf90_noerr, 218, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,10) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            END IF
          END IF
        END DO
      END DO
!
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!
      CALL netcdf_sync (ng, iNLM, DIA(ng)%name, DIA(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 359, MyFile)) RETURN
      IF (Master) WRITE (stdout,20) DIA(ng)%Rindex
!
  10  FORMAT (/,' WRT_DIAGS - error while writing variable: ',a,/,13x,  &
     &        'into diagnostics NetCDF file for time record: ',i0)
  20  FORMAT (6x,'WRT_DIAGS   - wrote diagnostics fields',t58,          &
     &        'in record = ',i0)
      RETURN
      END SUBROUTINE wrt_diags
