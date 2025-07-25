      SUBROUTINE def_diags (ng, ldef)
!
!svn $Id: def_diags.F 1054 2021-03-06 19:47:12Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine creates diagnostics NetCDF file, it defines its        !
!  dimensions, attributes, and variables.                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE def_var_mod, ONLY : def_var
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
      logical, intent(in) :: ldef
!
!  Local variable declarations.
!
      logical :: got_var(NV)
!
      integer, parameter :: Natt = 25
      integer :: i, ifield, itrc, ivar, j, nvd3, nvd4, nvd5
      integer :: recdim, status
      integer :: DimIDs(nDimID)
      integer :: t2dgrd(3), u2dgrd(3), v2dgrd(3)
      integer :: def_dim
      integer :: t3dgrd(4), u3dgrd(4), v3dgrd(4), w3dgrd(4)
!
      real(r8) :: Aval(6)
!
      character (len= 13) :: Prefix
      character (len=120) :: Vinfo(Natt)
      character (len=256) :: ncname
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_diags.F"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Set and report file name.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 84, MyFile)) RETURN
      ncname=DIA(ng)%name
!
      IF (Master) THEN
        IF (ldef) THEN
          WRITE (stdout,10) ng, TRIM(ncname)
        ELSE
          WRITE (stdout,20) ng, TRIM(ncname)
        END IF
      END IF
!
!=======================================================================
!  Create a new diagnostics NetCDF file.
!=======================================================================
!
      DEFINE : IF (ldef) THEN
        CALL netcdf_create (ng, iNLM, TRIM(ncname), DIA(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 101, MyFile)) THEN
          IF (Master) WRITE (stdout,30) TRIM(ncname)
          RETURN
        END IF
!
!-----------------------------------------------------------------------
!  Define file dimensions.
!-----------------------------------------------------------------------
!
        DimIDs=0
!
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'xi_rho',        &
     &                 IOBOUNDS(ng)%xi_rho, DimIDs( 1))
        IF (FoundError(exit_flag, NoError, 114, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'xi_u',          &
     &                 IOBOUNDS(ng)%xi_u, DimIDs( 2))
        IF (FoundError(exit_flag, NoError, 118, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'xi_v',          &
     &                 IOBOUNDS(ng)%xi_v, DimIDs( 3))
        IF (FoundError(exit_flag, NoError, 122, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'xi_psi',        &
     &                 IOBOUNDS(ng)%xi_psi, DimIDs( 4))
        IF (FoundError(exit_flag, NoError, 126, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'eta_rho',       &
     &                 IOBOUNDS(ng)%eta_rho, DimIDs( 5))
        IF (FoundError(exit_flag, NoError, 130, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'eta_u',         &
     &                 IOBOUNDS(ng)%eta_u, DimIDs( 6))
        IF (FoundError(exit_flag, NoError, 134, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'eta_v',         &
     &                 IOBOUNDS(ng)%eta_v, DimIDs( 7))
        IF (FoundError(exit_flag, NoError, 138, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'eta_psi',       &
     &                 IOBOUNDS(ng)%eta_psi, DimIDs( 8))
        IF (FoundError(exit_flag, NoError, 142, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 's_rho',         &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 187, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 's_w',           &
     &                 N(ng)+1, DimIDs(10))
        IF (FoundError(exit_flag, NoError, 191, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'tracer',        &
     &                 NT(ng), DimIDs(11))
        IF (FoundError(exit_flag, NoError, 195, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname, 'boundary',      &
     &                 4, DimIDs(14))
        IF (FoundError(exit_flag, NoError, 243, MyFile)) RETURN
        status=def_dim(ng, iNLM, DIA(ng)%ncid, ncname,                  &
     &                 TRIM(ADJUSTL(Vname(5,idtime))),                  &
     &                 nf90_unlimited, DimIDs(12))
        IF (FoundError(exit_flag, NoError, 254, MyFile)) RETURN
        recdim=DimIDs(12)
!
!  Set number of dimensions for output variables.
!
        nvd3=3
        nvd4=4
        nvd5=5
!
!  Define dimension vectors for staggered tracer type variables.
!
        t2dgrd(1)=DimIDs( 1)
        t2dgrd(2)=DimIDs( 5)
        t2dgrd(3)=DimIDs(12)
        t3dgrd(1)=DimIDs( 1)
        t3dgrd(2)=DimIDs( 5)
        t3dgrd(3)=DimIDs( 9)
        t3dgrd(4)=DimIDs(12)
!
!  Define dimension vectors for staggered u-momentum type variables.
!
        u2dgrd(1)=DimIDs( 2)
        u2dgrd(2)=DimIDs( 6)
        u2dgrd(3)=DimIDs(12)
        u3dgrd(1)=DimIDs( 2)
        u3dgrd(2)=DimIDs( 6)
        u3dgrd(3)=DimIDs( 9)
        u3dgrd(4)=DimIDs(12)
!
!  Define dimension vectors for staggered v-momentum type variables.
!
        v2dgrd(1)=DimIDs( 3)
        v2dgrd(2)=DimIDs( 7)
        v2dgrd(3)=DimIDs(12)
        v3dgrd(1)=DimIDs( 3)
        v3dgrd(2)=DimIDs( 7)
        v3dgrd(3)=DimIDs( 9)
        v3dgrd(4)=DimIDs(12)
!
!  Define dimension vector for staggered w-momentum type variables.
!
        w3dgrd(1)=DimIDs( 1)
        w3dgrd(2)=DimIDs( 5)
        w3dgrd(3)=DimIDs(10)
        w3dgrd(4)=DimIDs(12)
!
!  Initialize unlimited time record dimension.
!
        DIA(ng)%Rindex=0
!
!  Initialize local information variable arrays.
!
        DO i=1,Natt
          DO j=1,LEN(Vinfo(1))
            Vinfo(i)(j:j)=' '
          END DO
        END DO
        DO i=1,6
          Aval(i)=0.0_r8
        END DO
!
!  Set long name prefix string.
!
        Prefix='time-averaged'
!
!-----------------------------------------------------------------------
!  Define time-recordless information variables.
!-----------------------------------------------------------------------
!
        CALL def_info (ng, iNLM, DIA(ng)%ncid, ncname, DimIDs)
        IF (FoundError(exit_flag, NoError, 400, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Define variables and their attributes.
!-----------------------------------------------------------------------
!
!  Define model time.
!
        Vinfo( 1)=Vname(1,idtime)
        WRITE (Vinfo( 2),'(a,a)') 'averaged ', TRIM(Vname(2,idtime))
        WRITE (Vinfo( 3),'(a,a)') 'seconds since ', TRIM(Rclock%string)
        Vinfo( 4)=TRIM(Rclock%calendar)
        Vinfo(14)=Vname(4,idtime)
        status=def_var(ng, iNLM, DIA(ng)%ncid, DIA(ng)%Vid(idtime),     &
     &                 NF_TOUT, 1, (/recdim/), Aval, Vinfo, ncname,     &
     &                 SetParAccess = .TRUE.)
        IF (FoundError(exit_flag, NoError, 416, MyFile)) RETURN
!
!  Define 2D momentum diagnostic fields.
!
        DO ivar=1,NDM2d
          ifield=idDu2d(ivar)
          IF (Dout(ifield,ng)) THEN
            Vinfo( 1)=Vname(1,ifield)
            WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,ifield))
            Vinfo( 3)=Vname(3,ifield)
            Vinfo(14)=Vname(4,ifield)
            Vinfo(16)=Vname(1,idtime)
            Vinfo(22)='coordinates'
            Aval(5)=REAL(u2dvar,r8)
            status=def_var(ng, iNLM, DIA(ng)%ncid, DIA(ng)%Vid(ifield), &
     &                     NF_FOUT, nvd3, u2dgrd, Aval, Vinfo, ncname)
            IF (FoundError(exit_flag, NoError, 437, MyFile)) RETURN
          END IF
          ifield=idDv2d(ivar)
          IF (Dout(ifield,ng)) THEN
            Vinfo( 1)=Vname(1,ifield)
            WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,ifield))
            Vinfo( 3)=Vname(3,ifield)
            Vinfo(14)=Vname(4,ifield)
            Vinfo(16)=Vname(1,idtime)
            Vinfo(22)='coordinates'
            Aval(5)=REAL(v2dvar,r8)
            status=def_var(ng, iNLM, DIA(ng)%ncid, DIA(ng)%Vid(ifield), &
     &                     NF_FOUT, nvd3, v2dgrd, Aval, Vinfo, ncname)
            IF (FoundError(exit_flag, NoError, 453, MyFile)) RETURN
          END IF
        END DO
!
!  Define 3D momentum diagnostic fields.
!
        DO ivar=1,NDM3d
          ifield=idDu3d(ivar)
          IF (Dout(ifield,ng)) THEN
            Vinfo( 1)=Vname(1,ifield)
            WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,ifield))
            Vinfo( 3)=Vname(3,ifield)
            Vinfo(14)=Vname(4,ifield)
            Vinfo(16)=Vname(1,idtime)
            Vinfo(22)='coordinates'
            Aval(5)=REAL(u3dvar,r8)
            status=def_var(ng, iNLM, DIA(ng)%ncid, DIA(ng)%Vid(ifield), &
     &                     NF_FOUT, nvd4, u3dgrd, Aval, Vinfo, ncname)
            IF (FoundError(exit_flag, NoError, 476, MyFile)) RETURN
          END IF
          ifield=idDv3d(ivar)
          IF (Dout(ifield,ng)) THEN
            Vinfo( 1)=Vname(1,ifield)
            WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,ifield))
            Vinfo( 3)=Vname(3,ifield)
            Vinfo(14)=Vname(4,ifield)
            Vinfo(16)=Vname(1,idtime)
            Vinfo(22)='coordinates'
            Aval(5)=REAL(v3dvar,r8)
            status=def_var(ng, iNLM, DIA(ng)%ncid, DIA(ng)%Vid(ifield), &
     &                     NF_FOUT, nvd4, v3dgrd, Aval, Vinfo, ncname)
            IF (FoundError(exit_flag, NoError, 492, MyFile)) RETURN
          END IF
        END DO
!
!  Define tracer diagnostic fields.
!
        DO itrc=1,NT(ng)
          DO ivar=1,NDT
            ifield=idDtrc(itrc,ivar)
            IF (Dout(ifield,ng)) THEN
              Vinfo( 1)=Vname(1,ifield)
              WRITE (Vinfo( 2),'(a,1x,a)') Prefix, TRIM(Vname(2,ifield))
              Vinfo( 3)=Vname(3,ifield)
              Vinfo(14)=Vname(4,ifield)
              Vinfo(16)=Vname(1,idtime)
              Vinfo(22)='coordinates'
              Aval(5)=REAL(r3dvar,r8)
              status=def_var(ng, iNLM, DIA(ng)%ncid,                    &
     &                       DIA(ng)%Vid(ifield), NF_FOUT,              &
     &                       nvd4, t3dgrd, Aval, Vinfo, ncname)
              IF (FoundError(exit_flag, NoError,                        &
     &                       519, MyFile)) RETURN
            END IF
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Leave definition mode.
!-----------------------------------------------------------------------
!
        CALL netcdf_enddef (ng, iNLM, ncname, DIA(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 622, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out time-recordless, information variables.
!-----------------------------------------------------------------------
!
        CALL wrt_info (ng, iNLM, DIA(ng)%ncid, ncname)
        IF (FoundError(exit_flag, NoError, 629, MyFile)) RETURN
      END IF DEFINE
!
!=======================================================================
!  Open an existing diagnostics file, check its contents, and prepare
!  for appending data.
!=======================================================================
!
      QUERY : IF (.not.ldef) THEN
        ncname=DIA(ng)%name
!
!  Open diagnostics file for read/write.
!
        CALL netcdf_open (ng, iNLM, ncname, 1, DIA(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 643, MyFile)) THEN
          WRITE (stdout,50) TRIM(ncname)
          RETURN
        END IF
!
!  Inquire about the dimensions and check for consistency.
!
        CALL netcdf_check_dim (ng, iNLM, ncname,                        &
     &                         ncid = DIA(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 652, MyFile)) RETURN
!
!  Inquire about the variables.
!
        CALL netcdf_inq_var (ng, iNLM, ncname,                          &
     &                       ncid = DIA(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 658, MyFile)) RETURN
!
!  Initialize logical switches.
!
        DO i=1,NV
          got_var(i)=.FALSE.
        END DO
!
!  Scan variable list from input NetCDF and activate switches for
!  diagnostics variables. Get variable IDs.
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
            DIA(ng)%Vid(idtime)=var_id(i)
          END IF
          DO ivar=1,NDM2d
            IF (TRIM(var_name(i)).eq.                                   &
     &          TRIM(Vname(1,idDu2d(ivar)))) THEN
              got_var(idDu2d(ivar))=.TRUE.
              DIA(ng)%Vid(idDu2d(ivar))=var_id(i)
            ELSE IF (TRIM(var_name(i)).eq.                              &
     &               TRIM(Vname(1,idDv2d(ivar)))) THEN
              got_var(idDv2d(ivar))=.TRUE.
              DIA(ng)%Vid(idDv2d(ivar))=var_id(i)
            END IF
          END DO
          DO ivar=1,NDM3d
            IF (TRIM(var_name(i)).eq.                                   &
     &          TRIM(Vname(1,idDu3d(ivar)))) THEN
              got_var(idDu3d(ivar))=.TRUE.
              DIA(ng)%Vid(idDu3d(ivar))=var_id(i)
            ELSE IF (TRIM(var_name(i)).eq.                              &
     &               TRIM(Vname(1,idDv3d(ivar)))) THEN
              got_var(idDv3d(ivar))=.TRUE.
              DIA(ng)%Vid(idDv3d(ivar))=var_id(i)
            END IF
          END DO
          DO itrc=1,NT(ng)
            DO ivar=1,NDT
              ifield=idDtrc(itrc,ivar)
              IF (TRIM(var_name(i)).eq.TRIM(Vname(1,ifield))) THEN
                got_var(ifield)=.TRUE.
                DIA(ng)%Vid(ifield)=var_id(i)
              END IF
            END DO
          END DO
        END DO
!
!  Check if diagnostics variables are available in input NetCDF file.
!
        IF (.not.got_var(idtime)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        DO ivar=1,NDM2d
          ifield=idDu2d(ivar)
          IF (.not.got_var(ifield).and.Dout(ifield,ng))  THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,ifield)),        &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
          ifield=idDv2d(ivar)
          IF (.not.got_var(ifield).and.Dout(ifield,ng))  THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,ifield)),        &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
        DO ivar=1,NDM3d
          ifield=idDu3d(ivar)
          IF (.not.got_var(ifield).and.Dout(ifield,ng)) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,ifield)),        &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
          ifield=idDv3d(ivar)
          IF (.not.got_var(ifield).and.Dout(ifield,ng)) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,ifield)),        &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
        DO itrc=1,NT(ng)
          DO ivar=1,NDT
            ifield=idDtrc(itrc,ivar)
            IF (.not.got_var(ifield).and.Dout(ifield,ng)) THEN
              IF (Master) WRITE (stdout,60) TRIM(Vname(1,ifield)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              RETURN
            END IF
          END DO
        END DO
!
!  Set unlimited time record dimension to the appropriate value.
!
        IF (nRST(ng).eq.nDIA(ng)) THEN
          IF (ndefDIA(ng).gt.0) THEN
            DIA(ng)%Rindex=((ntstart(ng)-1)-                            &
     &                      ndefDIA(ng)*((ntstart(ng)-1)/ndefDIA(ng)))/ &
     &                     nDIA(ng)
          ELSE
            DIA(ng)%Rindex=(ntstart(ng)-1)/nDIA(ng)
          END IF
        ELSE
          DIA(ng)%Rindex=rec_size
        END IF
      END IF QUERY
!
!  Set initial averaged time.
!
      IF (ntsDIA(ng).eq.1) THEN
        DIAtime(ng)=time(ng)-0.5_r8*REAL(nDIA(ng),r8)*dt(ng)
      ELSE
        DIAtime(ng)=time(ng)+REAL(ntsDIA(ng),r8)*dt(ng)-                &
     &              0.5_r8*REAL(nDIA(ng),r8)*dt(ng)
      END IF
!
  10  FORMAT (6x,'DEF_DIAGS   - creating  diagnostics', t43,            &
     &        ' file, Grid ',i2.2,': ', a)
  20  FORMAT (6x,'DEF_DIAGS   - inquiring diagnostics', t43,            &
     &        ' file, Grid ',i2.2,': ', a)
  30  FORMAT (/,' DEF_DIAGS - unable to create diagnostics NetCDF',     &
     &        ' file: ',a)
  40  FORMAT (1pe11.4,1x,'millimeter')
  50  FORMAT (/,' DEF_DIAGS - unable to open diagnostics NetCDF',       &
     &        ' file: ',a)
  60  FORMAT (/,' DEF_DIAGS - unable to find variable: ',a,2x,          &
     &        ' in diagnostics NetCDF file: ',a)
!
      RETURN
      END SUBROUTINE def_diags
