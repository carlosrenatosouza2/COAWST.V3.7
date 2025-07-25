      MODULE state_read_mod
!
!svn $Id: state_read.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads the ROMS ocean state from requested NetCDF file. !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     tile         Domaing partition (integer)                         !
!     model        Calling model identifier (integer)                  !
!     LBi          I-dimension Lower bound (integer)                   !
!     UBi          I-dimension Upper bound (integer)                   !
!     LBj          J-dimension Lower bound (integer)                   !
!     UBj          J-dimension Upper bound (integer)                   !
!     LBij         IJ-dimension Lower bound (integer)                  !
!     UBij         IJ-dimension Upper bound (integer)                  !
!     Lout         State array index to process (integer)              !
!     rec          NetCDF file time record to read (integer)           !
!     nopen        Flag to open NetCDF file (integer)                  !
!                    nopen = 0   => file already opened for access     !
!                    nopen > 0   => open at entry and close at exit    !
!     ncfileid     NetCDF file ID, if already opened (integer)         !
!     ncname       NetCDF filename (string)                            !
!     rmask        State land/sea mask on RHO-points (real 2D array)   !
!     umask        State land/sea mask on U-points   (real 2D array)   !
!     vmask        State land/sea mask on V-points   (real 2D array)   !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     s_t          State tracers               (:,:,N,Lout,NT)         !
!     s_u          State 3D U-velocity         (:,:,N,Lout)            !
!     s_v          State 3D V-velocity         (:,:,N,Lout)            !
!     s_zeta       State free-surface          (:,:,Lout)              !
!                                                                      !
!=======================================================================
!
      implicit none
      PUBLIC  :: state_read
      CONTAINS
!
!***********************************************************************
      SUBROUTINE state_read (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj, LBij, UBij,            &
     &                       Lout, rec,                                 &
     &                       nopen, ncfileid, ncname,                   &
     &                       rmask, umask, vmask,                       &
     &                       s_t, s_u, s_v,                             &
     &                       s_zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE dateclock_mod,      ONLY : time_string
      USE distribute_mod,     ONLY : mp_bcasti
      USE nf_fread2d_mod,     ONLY : nf_fread2d
      USE nf_fread3d_mod,     ONLY : nf_fread3d
      USE strings_mod,        ONLY : FoundError
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBij, UBij
      integer, intent(in) :: Lout, rec, nopen
      integer, intent(inout) :: ncfileid
      character (len=*), intent(in) :: ncname
!
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
      real(r8), intent(inout) :: s_t(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: s_u(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: s_v(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: s_zeta(LBi:,LBj:,:)
!
!  Local variable declarations.
!
      logical :: Lreport = .FALSE.
!
      integer :: Sstr, Send
      integer :: i, j, k
      integer :: ifield, it
      integer :: gtype, ncid, status, varid
      integer, dimension(4) :: Vsize
!
      real(r8) :: Fmin, Fmax
      real(dp) :: stime, scale
!
      character (len=15) :: Tstring
      character (len=22) :: t_code
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/state_read.F"
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Read in requested model state record. Load data into state array
!  index Lout.
!-----------------------------------------------------------------------
!
!  Turn on time wall clock.
!
      CALL wclock_on (ng, model, 80, 238, MyFile)
!
!  Determine file and variables ids.
!
      IF (nopen.gt.0) THEN
        CALL netcdf_open (ng, model, ncname, 0, ncid)
        IF (FoundError(exit_flag, NoError, 245, MyFile)) RETURN
        ncfileid=ncid
      ELSE
        ncid=ncfileid
      END IF
      DO i=1,4
        Vsize(i)=0
      END DO
!
!  Synchronize NetCDF data to disk. It allows processes open for reading
!  data to update the number of records written by other processes.
!  The writer flushes buffers to disk, and the reader makes sure that
!  it is reading from disk rather than from previously cached buffers.
!
      CALL netcdf_sync (ng, model, ncname, ncid)
      IF (FoundError(exit_flag, NoError, 261, MyFile)) RETURN
!
!  Read in time.
!
      CALL netcdf_get_time (ng, model, ncname, Vname(1,idtime),         &
     &                      Rclock%DateNumber, stime,                   &
     &                      ncid, (/rec/), (/1/))
      IF (FoundError(exit_flag, NoError, 268, MyFile)) RETURN
!
!  Report information.
!
      IF (Master) THEN
        CALL time_string (stime, t_code)
        Sstr=SCAN(CalledFrom,'/',BACK=.TRUE.)+1
        Send=LEN_TRIM(CalledFrom)
        WRITE (Tstring,'(f15.4)') stime*sec2day
        WRITE (stdout,10) 'Reading state fields,', t_code,              &
     &                    ng, Tstring, TRIM(ncname), rec, Lout,         &
     &                    CalledFrom(Sstr:Send)
      END IF
!
!  Read in free-surface.
!
      gtype=r2dvar
      scale=1.0_r8
      CALL netcdf_inq_varid (ng, model, ncname, Vname(1,idFsur),        &
     &                       ncid, varid)
      IF (FoundError(exit_flag, NoError, 288, MyFile)) RETURN
      status=nf_fread2d(ng, model, ncname, ncid,                        &
     &                  Vname(1,idFsur), varid,                         &
     &                  rec, gtype, Vsize,                              &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  scale, Fmin, Fmax,                              &
     &                  rmask,                                          &
     &                  s_zeta(:,:,Lout))
      IF (FoundError(status, nf90_noerr, 299, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,30) TRIM(Vname(1,idFsur)), rec, TRIM(ncname)
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      ELSE
        IF (Master.and.Lreport) THEN
          WRITE (stdout,40) TRIM(Vname(2,idFsur)), Fmin, Fmax
        END IF
      END IF
!
!  Read in 3D U-momentum component.
!
      gtype=u3dvar
      scale=1.0_r8
      CALL netcdf_inq_varid (ng, model, ncname, Vname(1,idUvel),        &
     &                       ncid, varid)
      IF (FoundError(exit_flag, NoError, 537, MyFile)) RETURN
      status=nf_fread3d(ng, model, ncname, ncid,                        &
     &                  Vname(1,idUvel), varid,                         &
     &                  rec, gtype, Vsize,                              &
     &                  LBi, UBi, LBj, UBj, 1, N(ng),                   &
     &                  scale, Fmin, Fmax,                              &
     &                  umask,                                          &
     &                  s_u(:,:,:,Lout))
      IF (FoundError(status, nf90_noerr, 548, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,30) TRIM(Vname(1,idUvel)), rec, TRIM(ncname)
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      ELSE
        IF (Master.and.Lreport) THEN
          WRITE (stdout,40) TRIM(Vname(2,idUvel)), Fmin, Fmax
        END IF
      END IF
!
!  Read in 3D U-momentum component.
!
      gtype=v3dvar
      scale=1.0_r8
      CALL netcdf_inq_varid (ng, model, ncname, Vname(1,idVvel),        &
     &                       ncid, varid)
      IF (FoundError(exit_flag, NoError, 600, MyFile)) RETURN
      status=nf_fread3d(ng, model, ncname, ncid,                        &
     &                  Vname(1,idVvel), varid,                         &
     &                  rec, gtype, Vsize,                              &
     &                  LBi, UBi, LBj, UBj, 1, N(ng),                   &
     &                  scale, Fmin, Fmax,                              &
     &                  vmask,                                          &
     &                  s_v(:,:,:,Lout))
      IF (FoundError(status, nf90_noerr, 611, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,30) TRIM(Vname(1,idVvel)), rec, TRIM(ncname)
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      ELSE
        IF (Master.and.Lreport) THEN
          WRITE (stdout,40) TRIM(Vname(2,idVvel)), Fmin, Fmax
        END IF
      END IF
!
!  Read in tracers.
!
      gtype=r3dvar
      scale=1.0_r8
      DO it=1,NT(ng)
        CALL netcdf_inq_varid (ng, model, ncname, Vname(1,idTvar(it)),  &
     &                         ncid, varid)
        IF (FoundError(exit_flag, NoError, 664, MyFile)) RETURN
        status=nf_fread3d(ng, model, ncname, ncid,                      &
     &                    Vname(1,idTvar(it)), varid,                   &
     &                    rec, gtype, Vsize,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    scale, Fmin, Fmax,                            &
     &                    rmask,                                        &
     &                    s_t(:,:,:,Lout,it))
        IF (FoundError(status, nf90_noerr, 675, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,30) TRIM(Vname(1,idTvar(it))), rec,           &
     &                        TRIM(ncname)
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        ELSE
          IF (Master.and.Lreport) THEN
            WRITE (stdout,40) TRIM(Vname(2,ifield)), Fmin, Fmax
          END IF
        END IF
      END DO
!
!  Close current file.
!
      IF (nopen.gt.0) THEN
        CALL netcdf_close (ng, model, ncid, ncname, .FALSE.)
        IF (FoundError(exit_flag, NoError, 767, MyFile)) RETURN
      END IF
!
!  Turn off time wall clock.
!
      CALL wclock_off (ng, model, 80, 774, MyFile)
!
  10  FORMAT (5x,'STATE_READ - ',a,t75,a,                               &
     &        /,19x,'(Grid ',i2.2,', t = ',a,', File: ',a,              &
     &        ', Rec=',i4.4,', Index=',i1,')',                          &
     &        /,19x,'Called from ''',a,'''')
  20  FORMAT (' STATE_READ - unable to open NetCDF file: ',a)
  30  FORMAT (' STATE_READ - error while reading variable: ',a,2x,      &
     &        'at time record = ',i3,/,14x,'in NetCDF file: ',a)
  40  FORMAT (16x,'- ',a,/,19x,'(Min = ',1p,e15.8,                      &
     &        ' Max = ',1p,e15.8,')')
!
      RETURN
      END SUBROUTINE state_read
!
      END MODULE state_read_mod
