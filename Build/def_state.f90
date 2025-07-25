      SUBROUTINE def_state (ng, model, label, Lcreate, S, Lclose)
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine creates an ocean state NetCDF file according to input  !
!  derived (TYPE T_IO) structure S. It defines its dimensions,         !
!  attributes, and variables.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     label        Identification label (string)                       !
!     ldef         Switch to define a new file (T) or restart (F)      !
!     S            File derived type structure (TYPE T_IO)             !
!     Lclose       Switch to close (.TRUE.) NetCDF after created.      !
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
      logical, intent(in) :: Lcreate, Lclose
!
      integer, intent(in) :: ng, model
!
      character (len=*), intent(in) :: label
!
      TYPE(T_IO), intent(inout) :: S(Ngrids)
!
!  Local variable declarations.
!
      logical :: got_var(NV)
!
      integer, parameter :: Natt = 25
      integer :: i, j, ifield, itrc, nrec, nvd, nvd3, nvd4
      integer :: recdim, status, varid
      integer :: DimIDs(nDimID)
      integer :: t2dgrd(3), u2dgrd(3), v2dgrd(3)
      integer :: def_dim
      integer :: t3dgrd(4), u3dgrd(4), v3dgrd(4), w3dgrd(4)
!
      real(r8) :: Aval(6)
!
      character (len=120) :: Vinfo(Natt)
      character (len=256) :: ncname
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_state.F"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Set and report file name.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 105, MyFile)) RETURN
      ncname=S(ng)%name
!
      IF (Master) THEN
        IF (Lcreate) THEN
          WRITE (stdout,10) TRIM(label), ng, TRIM(ncname)
        ELSE
          WRITE (stdout,20) TRIM(label), ng, TRIM(ncname)
        END IF
      END IF
!
!=======================================================================
!  Create a new ocean state NetCDF file.
!=======================================================================
!
      DEFINE : IF (Lcreate) THEN
        CALL netcdf_create (ng, model, TRIM(ncname), S(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 122, MyFile)) THEN
          IF (Master) WRITE (stdout,30) TRIM(label), TRIM(ncname)
          RETURN
        END IF
!
!-----------------------------------------------------------------------
!  Define file dimensions.
!-----------------------------------------------------------------------
!
        DimIDs=0
!
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'xi_rho',         &
     &                 IOBOUNDS(ng)%xi_rho, DimIDs( 1))
        IF (FoundError(exit_flag, NoError, 135, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'xi_u',           &
     &                 IOBOUNDS(ng)%xi_u, DimIDs( 2))
        IF (FoundError(exit_flag, NoError, 139, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'xi_v',           &
     &                 IOBOUNDS(ng)%xi_v, DimIDs( 3))
        IF (FoundError(exit_flag, NoError, 143, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'xi_psi',         &
     &                 IOBOUNDS(ng)%xi_psi, DimIDs( 4))
        IF (FoundError(exit_flag, NoError, 147, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'eta_rho',        &
     &                 IOBOUNDS(ng)%eta_rho, DimIDs( 5))
        IF (FoundError(exit_flag, NoError, 151, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'eta_u',          &
     &                 IOBOUNDS(ng)%eta_u, DimIDs( 6))
        IF (FoundError(exit_flag, NoError, 155, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'eta_v',          &
     &                 IOBOUNDS(ng)%eta_v, DimIDs( 7))
        IF (FoundError(exit_flag, NoError, 159, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'eta_psi',        &
     &                 IOBOUNDS(ng)%eta_psi, DimIDs( 8))
        IF (FoundError(exit_flag, NoError, 163, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'N',              &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 205, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 's_rho',          &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 209, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 's_w',            &
     &                 N(ng)+1, DimIDs(10))
        IF (FoundError(exit_flag, NoError, 213, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'tracer',         &
     &                 NT(ng), DimIDs(11))
        IF (FoundError(exit_flag, NoError, 217, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname, 'boundary',       &
     &                 4, DimIDs(14))
        IF (FoundError(exit_flag, NoError, 260, MyFile)) RETURN
        status=def_dim(ng, model, S(ng)%ncid, ncname,                   &
     &                 TRIM(ADJUSTL(Vname(5,idtime))),                  &
     &                 nf90_unlimited, DimIDs(12))
        IF (FoundError(exit_flag, NoError, 283, MyFile)) RETURN
        recdim=DimIDs(12)
!
!  Set number of dimensions for output variables.
!
        nvd3=3
        nvd4=4
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
        S(ng)%Rindex=0
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
!-----------------------------------------------------------------------
!  Define time-recordless information variables.
!-----------------------------------------------------------------------
!
        CALL def_info (ng, model, S(ng)%ncid, ncname, DimIDs)
        IF (FoundError(exit_flag, NoError, 431, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Define time-varying variables.
!-----------------------------------------------------------------------
!
!  Define model time.
!
        Vinfo( 1)=Vname(1,idtime)
        Vinfo( 2)=Vname(2,idtime)
        WRITE (Vinfo( 3),'(a,a)') 'seconds since ', TRIM(Rclock%string)
        Vinfo( 4)=TRIM(Rclock%calendar)
        Vinfo(14)=Vname(4,idtime)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idtime),        &
     &                 NF_TOUT, 1, (/recdim/), Aval, Vinfo,ncname,      &
     &                 SetParAccess = .TRUE.)
        IF (FoundError(exit_flag, NoError, 447, MyFile)) RETURN
!
!  Define free-surface.
!
        Vinfo( 1)=Vname(1,idFsur)
        Vinfo( 2)=Vname(2,idFsur)
        Vinfo( 3)='nondimensional'
        Vinfo(14)=Vname(4,idFsur)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idFsur,ng),r8)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idFsur),        &
     &                 NF_FOUT, nvd3, t2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 463, MyFile)) RETURN
!
!  Define 2D U-momentum component.
!
        Vinfo( 1)=Vname(1,idUbar)
        Vinfo( 2)=Vname(2,idUbar)
        Vinfo( 3)='nondimensional'
        Vinfo(14)=Vname(4,idUbar)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idUbar,ng),r8)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idUbar),        &
     &                 NF_FOUT, nvd3, u2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 498, MyFile)) RETURN
!
!  Define 2D V-momentum component.
!
        Vinfo( 1)=Vname(1,idVbar)
        Vinfo( 2)=Vname(2,idVbar)
        Vinfo( 3)='nondimensional'
        Vinfo(14)=Vname(4,idVbar)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idVbar,ng),r8)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idVbar),        &
     &                 NF_FOUT, nvd3, v2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 533, MyFile)) RETURN
!
!  Define 3D U-momentum component.
!
        Vinfo( 1)=Vname(1,idUvel)
        Vinfo( 2)=Vname(2,idUvel)
        Vinfo( 3)='nondimensional'
        Vinfo(14)=Vname(4,idUvel)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idUvel,ng),r8)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idUvel),        &
     &                 NF_FOUT, nvd4, u3dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 569, MyFile)) RETURN
!
!  Define 3D V-momentum component.
!
        Vinfo( 1)=Vname(1,idVvel)
        Vinfo( 2)=Vname(2,idVvel)
        Vinfo( 3)='nondimensional'
        Vinfo(14)=Vname(4,idVvel)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idVvel,ng),r8)
        status=def_var(ng, model, S(ng)%ncid, S(ng)%Vid(idVvel),        &
     &                 NF_FOUT, nvd4, v3dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 604, MyFile)) RETURN
!
!  Define tracer type variables.
!
        DO itrc=1,NT(ng)
          Vinfo( 1)=Vname(1,idTvar(itrc))
          Vinfo( 2)=Vname(2,idTvar(itrc))
          Vinfo( 3)='nondimensional'
          Vinfo(14)=Vname(4,idTvar(itrc))
          Vinfo(16)=Vname(1,idtime)
          Vinfo(22)='coordinates'
          Aval(5)=REAL(r3dvar,r8)
          status=def_var(ng, model, S(ng)%ncid, S(ng)%Tid(itrc),        &
     &                   NF_FOUT, nvd4, t3dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 647, MyFile)) RETURN
        END DO
!
!-----------------------------------------------------------------------
!  Leave definition mode.
!-----------------------------------------------------------------------
!
        CALL netcdf_enddef (ng, model, ncname, S(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 748, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out time-recordless, information variables.
!-----------------------------------------------------------------------
!
        CALL wrt_info (ng, model, S(ng)%ncid, ncname)
        IF (FoundError(exit_flag, NoError, 755, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out time-recordless, information variables.
!-----------------------------------------------------------------------
!
        IF (Lclose) THEN
          CALL netcdf_close (ng, model, S(ng)%ncid, ncname, .FALSE.)
          IF (FoundError(exit_flag, NoError, 763, MyFile)) RETURN
        END IF
      END IF DEFINE
!
!=======================================================================
!  Open an existing ocean state NetCDF file, check its contents, and
!  prepare for appending data.
!=======================================================================
!
      QUERY: IF (.not.Lcreate) THEN
        ncname=S(ng)%name
!
!  Open ocean state file for read/write.
!
        CALL netcdf_open (ng, model, ncname, 1, S(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 779, MyFile)) THEN
          WRITE (stdout,50) TRIM(label), TRIM(ncname)
          RETURN
        END IF
!
!  Inquire about the dimensions and check for consistency.
!
        CALL netcdf_check_dim (ng, model, ncname,                       &
     &                         ncid = S(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 788, MyFile)) RETURN
!
!  Inquire about the variables.
!
        CALL netcdf_inq_var (ng, model, ncname,                         &
     &                       ncid = S(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 794, MyFile)) RETURN
!
!  Initialize logical switches.
!
        DO i=1,NV
          got_var(i)=.FALSE.
        END DO
!
!  Scan variable list from input NetCDF and activate switches for
!  Hessian eigenvectors variables. Get variable IDs.
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
            S(ng)%Vid(idtime)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idFsur))) THEN
            got_var(idFsur)=.TRUE.
            S(ng)%Vid(idFsur)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUbar))) THEN
            got_var(idUbar)=.TRUE.
            S(ng)%Vid(idUbar)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVbar))) THEN
            got_var(idVbar)=.TRUE.
            S(ng)%Vid(idVbar)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUvel))) THEN
            got_var(idUvel)=.TRUE.
            S(ng)%Vid(idUvel)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvel))) THEN
            got_var(idVvel)=.TRUE.
            S(ng)%Vid(idVvel)=var_id(i)
          END IF
          DO itrc=1,NT(ng)
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
              got_var(idTvar(itrc))=.TRUE.
              S(ng)%Tid(itrc)=var_id(i)
            END IF
          END DO
        END DO
!
!  Check if ocean state variables are available in input NetCDF file.
!
        IF (.not.got_var(idtime)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idFsur)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idFsur)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idUbar)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idUbar)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVbar)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idVbar)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idUvel)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idUvel)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVvel)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idVvel)),          &
     &                                  TRIM(label), TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        DO itrc=1,NT(ng)
          IF (.not.got_var(idTvar(itrc))) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,idTvar(itrc))),  &
     &                                    TRIM(label), TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
!
!  Set unlimited time record dimension to the appropriate value.
!
        S(ng)%Rindex=rec_size
      END IF QUERY
!
  10  FORMAT (6x,'DEF_STATE - creating  ',a,t43,' file, Grid ',i2.2,    &
     &        ': ', a)
  20  FORMAT (6x,'DEF_STATE - inquiring ',a,t43,' file, Grid ',i2.2,    &
     &        ': ', a)
  30  FORMAT (/,' DEF_STATE - unable to create ',a,' NetCDF file:',1x,a)
  40  FORMAT (1pe11.4,1x,'millimeter')
  50  FORMAT (/,' DEF_STATE - unable to open ',a,' NetCDF file: ',a)
  60  FORMAT (/,' DEF_STATE - unable to find variable: ',a,2x,' in ',a, &
     &        ' NetCDF file: ',a)
!
      RETURN
      END SUBROUTINE def_state
