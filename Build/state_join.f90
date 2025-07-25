      SUBROUTINE state_join (ng, model, InpName, OutName, InpStrRec,    &
     &                       InpEndRec, OutStrRec)
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads and writes ROMS NLM background state solution    !
!  for specified input and output records. Primarily, it is used to    !
!  concatenate the NLM solution from concurrent interval window        !
!  history NetCDF files. The resulting solution is used to linearize   !
!  the tangent linear and adjoint kernels.                             !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     InpName      Input  history filename (string)                    !
!     InpName      Output history filename (string)                    !
!     InpStrRec    Starting Input  history time record (integer)       !
!     InpEndRec    Ending   Input  history time record (integer)       !
!     OutStrRec    Starting Output history time record (integer)       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_coupling
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
!
      USE dateclock_mod,   ONLY : time_string
      USE nf_fread2d_mod,  ONLY : nf_fread2d
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE nf_fread3d_mod,  ONLY : nf_fread3d
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
      USE strings_mod,     ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
      integer, intent(in) :: InpStrRec, InpEndRec
      integer, intent(inout) :: OutStrRec
!
      character (len=*) :: InpName, OutName
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: InpId, InpRec, OutId, OutRec, Tindex
      integer :: gtype, i, ic, status
      integer :: Vsize(4)
!
      real(r8) :: Fmin, Fmax
      real(dp) :: Fscl, stime
!
      character (len=15) :: Tstring
      character (len=22) :: t_code
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/state_join.F"
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Process forward background solution.
!-----------------------------------------------------------------------
!
!  Open Input NetCDF file for reading.
!
      CALL netcdf_open (ng, model, InpName, 0, InpId)
      IF (FoundError(exit_flag, NoError, 89, MyFile)) THEN
        WRITE (stdout,10) TRIM(InpName)
        RETURN
      END IF
!
!  Open Output NetCDF file for writing.
!
      CALL netcdf_open (ng, model, OutName, 1, OutId)
      IF (FoundError(exit_flag, NoError, 97, MyFile)) THEN
        WRITE (stdout,10) TRIM(InpName)
        RETURN
      END IF
!
!  Inquire about the input variables.
!
      CALL netcdf_inq_var (ng, model, InpName,                          &
     &                     ncid = InpId)
      IF (FoundError(exit_flag, NoError, 106, MyFile)) RETURN
!
!  Set Vsize to zero to deativate interpolation of input data to model
!  grid in "nf_fread2d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
!
!  Scan the variable list and read in needed variables.
!
      OutRec=OutStrRec-1
      Tindex=1
      Fscl=1.0_dp
!
      REC_LOOP : DO InpRec=InpStrRec,InpEndRec
        OutRec=OutRec+1
        VAR_LOOP : DO i=1,n_var
!
!  Time.
!
          CHECK1 : IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            CALL netcdf_get_time (ng, model, InpName,                   &
     &                            TRIM(var_name(i)),                    &
     &                            Rclock%DateNumber, stime,             &
     &                            ncid = InpID,                         &
     &                            start = (/InpRec/),                   &
     &                            total = (/1/))
            IF (FoundError(exit_flag, NoError, 134, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idtime)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              RETURN
            END IF
!
            CALL netcdf_put_fvar (ng, model, OutName,                   &
     &                            TRIM(var_name(i)), stime,             &
     &                            (/OutRec/), (/1/),                    &
     &                            ncid = OutId,                         &
     &                            varid = var_id(i))
            IF (FoundError(exit_flag, NoError, 148, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idtime)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              RETURN
            ELSE
              CALL time_string (stime, t_code)
              WRITE (Tstring,'(f15.4)') stime*sec2day
              IF (Master) THEN
                WRITE (stdout,20) t_code,                               &
     &                            ng, TRIM(ADJUSTL(Tstring)), InpRec,   &
     &                            Tindex, TRIM(InpName),                &
     &                            ng, TRIM(ADJUSTL(Tstring)), OutRec,   &
     &                            Tindex, TRIM(OutName)
              END IF
            END IF
!
!  Free-surface.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idFsur))) THEN
            gtype=r2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        OCEAN(ng) % zeta(:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 180, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idFsur)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % rmask,                        &
     &                         OCEAN(ng) % zeta(:,:,Tindex),            &
     &                         SetFillVal = .FALSE.)
            IF (FoundError(status, nf90_noerr, 198, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idFsur)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idFsur)), Fmin, Fmax
              END IF
            END IF
!
!  2D U-momentum component.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUbar))) THEN
            gtype=u2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask_full,                    &
     &                        OCEAN(ng) % ubar(:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 272, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idUbar)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % umask_full,                   &
     &                         OCEAN(ng) % ubar(:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 289, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idUbar)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idUbar)), Fmin, Fmax
              END IF
            END IF
!
!  2D V-momentum component.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVbar))) THEN
            gtype=v2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask_full,                    &
     &                        OCEAN(ng) % vbar(:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 363, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idVbar)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % vmask_full,                   &
     &                         OCEAN(ng) % vbar(:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 380, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idVbar)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idVbar)), Fmin, Fmax
              END IF
            END IF
!
!  Time-averaged U-flux component for 2D equations.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUfx1))) THEN
            gtype=u2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        COUPLING(ng) % DU_avg1)
            IF (FoundError(status, nf90_noerr, 501, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idUfx1)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % umask,                        &
     &                         COUPLING(ng) % DU_avg1)
            IF (FoundError(status, nf90_noerr, 518, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idUfx1)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idUfx1)), Fmin, Fmax
              END IF
            END IF
!
!  Time-averaged U-flux component for 3D equations.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUfx2))) THEN
            gtype=u2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask,                         &
     &                        COUPLING(ng) % DU_avg2)
            IF (FoundError(status, nf90_noerr, 545, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idUfx2)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % umask,                        &
     &                         COUPLING(ng) % DU_avg2)
            IF (FoundError(status, nf90_noerr, 562, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idUfx2)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idUfx2)), Fmin, Fmax
              END IF
            END IF
!
!  Time-averaged V-flux component for 2D equations.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVfx1))) THEN
            gtype=v2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask_full,                    &
     &                        COUPLING(ng) % DV_avg1)
            IF (FoundError(status, nf90_noerr, 636, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idVfx1)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % vmask_full,                   &
     &                         COUPLING(ng) % DV_avg1)
            IF (FoundError(status, nf90_noerr, 653, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idVfx1)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idVfx1)), Fmin, Fmax
              END IF
            END IF
!
!  Time-averaged U-flux component for 3D equations.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVfx2))) THEN
            gtype=v2dvar
            status=nf_fread2d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask_full,                    &
     &                        COUPLING(ng) % DV_avg2)
            IF (FoundError(status, nf90_noerr, 680, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idVfx2)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite2d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, Fscl,                &
     &                         GRID(ng) % vmask_full,                   &
     &                         COUPLING(ng) % DV_avg2)
            IF (FoundError(status, nf90_noerr, 697, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idVfx2)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idVfx2)), Fmin, Fmax
              END IF
            END IF
!
!  3D U-momentum component.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUvel))) THEN
            gtype=u3dvar
            status=nf_fread3d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % umask_full,                    &
     &                        OCEAN(ng) % u(:,:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 724, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idUvel)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite3d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), Fscl,      &
     &                         GRID(ng) % umask_full,                   &
     &                         OCEAN(ng) % u(:,:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 741, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idUvel)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idUvel)), Fmin, Fmax
              END IF
            END IF
!
!  3D V-momentum component.
!
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvel))) THEN
            gtype=v3dvar
            status=nf_fread3d(ng, model, InpName, InpId,                &
     &                        var_name(i), var_id(i),                   &
     &                        InpRec, gtype, Vsize,                     &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % vmask_full,                    &
     &                        OCEAN(ng) % v(:,:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 815, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idVvel)), InpRec,        &
     &                            TRIM(InpName)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite3d(ng, model, OutId,                        &
     &                         var_id(i), OutRec, gtype,                &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), Fscl,      &
     &                         GRID(ng) % vmask_full,                   &
     &                         OCEAN(ng) % v(:,:,:,Tindex))
            IF (FoundError(status, nf90_noerr, 832, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,40) TRIM(Vname(1,idVvel)), OutRec,        &
     &                            TRIM(OutName)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            ELSE
              IF (Master) THEN
                WRITE (stdout,50) TRIM(Vname(2,idVvel)), Fmin, Fmax
              END IF
            END IF
          END IF CHECK1
!
!  Tracer type variables.
!
          TRACER1_LOOP : DO itrc=1,NT(ng)
            gtype=r3dvar
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
              status=nf_fread3d(ng, model, InpName, InpId,              &
     &                          var_name(i), var_id(i),                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          OCEAN(ng) % t(:,:,:,Tindex,itrc))
              IF (FoundError(status, nf90_noerr, 957, MyFile)) THEN
                IF (Master) THEN
                  WRITE (stdout,30) TRIM(Vname(1,idTvar(itrc))),        &
     &                              InpRec, TRIM(InpName)
                END IF
                exit_flag=2
                ioerror=status
                RETURN
              END IF
!
              status=nf_fwrite3d(ng, model, OutId,                      &
     &                           var_id(i), OutRec, gtype,              &
     &                           LBi, UBi, LBj, UBj, 1, N(ng), Fscl,    &
     &                           GRID(ng) % rmask,                      &
     &                           OCEAN(ng) % t(:,:,:,Tindex,itrc))
              IF (FoundError(status, nf90_noerr, 974, MyFile)) THEN
                IF (Master) THEN
                  WRITE (stdout,40) TRIM(Vname(1,idTvar(itrc))),        &
     &                              OutRec, TRIM(OutName)
                END IF
                exit_flag=3
                ioerror=status
                RETURN
              ELSE
                IF (Master) THEN
                  WRITE (stdout,50) TRIM(Vname(2,idTvar(itrc))),        &
     &                              Fmin, Fmax
                END IF
              END IF
            END IF
          END DO TRACER1_LOOP
!
!  Tracer type variables vertical diffusion.
!
          TRACER2_LOOP : DO itrc=1,NAT
            gtype=w3dvar
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idDiff(itrc)))) THEN
              status=nf_fread3d(ng, model, InpName, InpId,              &
     &                          var_name(i), var_id(i),                 &
     &                          InpRec, gtype, Vsize,                   &
     &                          LBi, UBi, LBj, UBj, 0, N(ng),           &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % rmask,                       &
     &                          MIXING(ng) % Akt(:,:,:,itrc))
              IF (FoundError(status, nf90_noerr, 1005, MyFile)) THEN
                IF (Master) THEN
                  WRITE (stdout,30) TRIM(Vname(1,idDiff(itrc))),        &
     &                              InpRec, TRIM(InpName)
                END IF
                exit_flag=2
                ioerror=status
                RETURN
              END IF
!
              status=nf_fwrite3d(ng, model, OutId,                      &
     &                           var_id(i), OutRec, gtype,              &
     &                           LBi, UBi, LBj, UBj, 0, N(ng), Fscl,    &
     &                           GRID(ng) % rmask,                      &
     &                           MIXING(ng) % Akt(:,:,:,itrc))
              IF (FoundError(status, nf90_noerr, 1022, MyFile)) THEN
                IF (Master) THEN
                  WRITE (stdout,40) TRIM(Vname(1,idDiff(itrc))),        &
     &                              OutRec, TRIM(OutName)
                END IF
                exit_flag=3
                ioerror=status
                RETURN
              ELSE
                IF (Master) THEN
                  WRITE (stdout,50) TRIM(Vname(2,idDiff(itrc))),        &
     &                              Fmin, Fmax
                END IF
              END IF
            END IF
          END DO TRACER2_LOOP
        END DO VAR_LOOP
      END DO REC_LOOP
!
!  Update output record value.
!
      OutStrRec=OutRec
!
!  Close input and output files for compleate synchronization.
!
      CALL netcdf_close (ng, iNLM, InpId, InpName, .FALSE.)
      IF (FoundError(exit_flag, NoError, 1051, MyFile)) RETURN
!
      CALL netcdf_close (ng, iNLM, OutId, OutName, .FALSE.)
      IF (FoundError(exit_flag, NoError, 1054, MyFile)) RETURN
!
  10  FORMAT (/,' STATE_JOIN - unable to open grid NetCDF file: ',a)
  20  FORMAT ('NLM: STATE_JOIN - Concatenating state fields,',t75,a,    &
     &        /,19x,'(Grid ',i2.2,', t = ',a,', InpRec=',i4.4,          &
     &         ', Index=',i1,', InpFile: ',a,')',                       &
     &        /,19x,'(Grid ',i2.2,', t = ',a,', OutRec=',i4.4,          &
     &         ', Index=',i1,', OutFile: ',a,')')
  30  FORMAT (/,' STATE_JOIN - error while reading variable: ',a,2x,    &
     &        'at time record = ',i0,/,14x,'in input NetCDF file: ',a)
  40  FORMAT (/,' STATE_JOIN - error while writing variable: ',a,2x,    &
     &        'at time record = ',i0,/,14x,'in output NetCDF file: ',a)
  50  FORMAT (16x,'- ',a,/,19x,'(Min = ',1p,e15.8,                      &
     &        ' Max = ',1p,e15.8,')')
!
      RETURN
      END SUBROUTINE state_join
