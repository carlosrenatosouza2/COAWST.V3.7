MODULE module_interp

   USE input_module

   real                                :: missing_value
   parameter (MISSING_VALUE=1.00E30)


  CONTAINS
!--------------------------------------------------------

  SUBROUTINE interp( data_in, nx, ny, nz, &
                     data_out, nxout, nyout, nzout, &
                     z_data, z_levs, number_of_zlevs, cname)

  USE module_model_basics

  implicit none

 ! Arguments
  integer                                                       :: nx, ny, nz, number_of_zlevs
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim) :: z_data
  real, dimension(nx,ny,nz)                                     :: data_in
  real, allocatable, dimension(:,:,:)                           :: data_out
  real, dimension(number_of_zlevs)                              :: z_levs
  character (len=*), intent(in)                                 :: cname

  ! Local variables
  integer                                                       :: nxout, nyout, nzout
  real, allocatable, dimension(:,:,:)                           :: SCR2
  real, dimension(bottom_top_dim)                               :: data_in_1d, z_data_1d
  real, dimension(number_of_zlevs)                              :: data_out_1d
  integer                                                       :: i,j,k, kk
  real                                                          :: ptarget, dpmin, dp, pbot, zbot
  real                                                          :: tbotextrap, tvbotextrap, expon, exponi
  real                                                          :: zlev, plev, tlev, gamma
  integer                                                       :: kupper, kin

  expon=287.04*.0065/9.81
  exponi=1./expon


    IF ( ALLOCATED(SCR2) ) DEALLOCATE(SCR2)
    IF ( ALLOCATED(data_out) ) DEALLOCATE(data_out)
    nxout = nx
    nyout = ny
    nzout = nz

    !! We may be dealing with a staggered field
    IF ( nz == 1 ) THEN   !2D field 
      IF ( nx .gt. west_east_dim ) THEN
        ALLOCATE(SCR2(west_east_dim,south_north_dim,nz))
        SCR2 = 0.5*(data_in(1:west_east_dim,:,:)+data_in(2:west_east_dim+1,:,:))
        nxout = west_east_dim
      ELSE IF ( ny .gt. south_north_dim ) THEN
        ALLOCATE(SCR2(west_east_dim,south_north_dim,nz))
        SCR2 = 0.5*(data_in(:,1:south_north_dim,:)+data_in(:,2:south_north_dim+1,:))
        nyout = south_north_dim
      ELSE
        ALLOCATE(SCR2(nx,ny,nz))
        SCR2 = data_in
      ENDIF
    ELSE                  !3D field
      IF ( nx .gt. west_east_dim ) THEN
        ALLOCATE(SCR2(west_east_dim,south_north_dim,bottom_top_dim))
        SCR2 = 0.5*(data_in(1:west_east_dim,:,:)+data_in(2:west_east_dim+1,:,:))
        nxout = west_east_dim
      ELSE IF ( ny .gt. south_north_dim ) THEN
        ALLOCATE(SCR2(west_east_dim,south_north_dim,bottom_top_dim))
        SCR2 = 0.5*(data_in(:,1:south_north_dim,:)+data_in(:,2:south_north_dim+1,:))
        nyout = south_north_dim
      ELSE IF ( nz .gt. bottom_top_dim ) THEN  
        ALLOCATE(SCR2(west_east_dim,south_north_dim,bottom_top_dim))
        SCR2 = 0.5*(data_in(:,:,1:bottom_top_dim)+data_in(:,:,2:bottom_top_dim+1))
        nzout = bottom_top_dim
      ELSE
        ALLOCATE(SCR2(nx,ny,nz))
        SCR2 = data_in
      ENDIF
    ENDIF


    IF ( iprogram .ge. 6 .AND. (nzout .eq. bottom_top_dim ) .AND.   &
         (vertical_type == 'p' .or. vertical_type == 'z') ) THEN

      ALLOCATE(data_out(west_east_dim,south_north_dim,number_of_zlevs))
      DO i=1,west_east_dim
      DO j=1,south_north_dim
  
        DO k=1,bottom_top_dim
          data_in_1d(k) = SCR2(i,j,k)
          z_data_1d(k)  = z_data(i,j,k)
        ENDDO
  
        CALL interp_1d( data_in_1d, z_data_1d, bottom_top_dim, &
                        data_out_1d, z_levs, number_of_zlevs,  &
                        vertical_type)
  
        DO k=1,number_of_zlevs
          data_out(i,j,k) = data_out_1d(k)
        ENDDO

      ENDDO
      ENDDO
      
      nzout = number_of_zlevs

    ELSE

      ALLOCATE(data_out(nxout,nyout,nzout))
      data_out = SCR2
      DEALLOCATE(SCR2)

    ENDIF

!!! STOP here if we don't want to extrapolate
    IF ( .not. extrapolate .OR. iprogram < 6 .OR. nz .lt. bottom_top_dim ) RETURN

    ! First find where about 400hPa/7km is located
    kk = 0
    find_kk : DO k = 1, nzout
       kk = k
       IF ( vertical_type == 'p' .AND. z_levs(k) <= 400. ) EXIT find_kk
       IF ( vertical_type == 'z' .AND. z_levs(k) >= 7. )   EXIT find_kk
    END DO find_kk
  
    IF ( vertical_type == 'p' ) THEN
      !!! Need to do something special for height and temparature
      IF ( cname=="height" .OR. cname=="geopt" .OR. &
           cname=="tk" .OR. cname=="tc" .OR. cname=="theta" ) THEN
        DO k = 1, kk
          DO j = 1, nyout
          DO i = 1, nxout
            IF ( data_out(i,j,k) == MISSING_VALUE .AND. 100.*z_levs(k) < PSFC(i,j) ) THEN

!             We are below the first model level, but above the ground
!             We need meter for the calculations so, GEOPT/G
  
              zlev = (((100.*z_levs(k) - PRES(i,j,1))*HGT(i,j) +  &
                     (PSFC(i,j) - 100.*z_levs(k))*GEOPT(i,j,1)/9.81 ) /   &
                     (PSFC(i,j) - PRES(i,j,1))) 
              IF ( cname == "height" ) data_out(i,j,k) = zlev / 1000.
              IF ( cname == "geopt" )  data_out(i,j,k) = zlev * 9.81
              IF ( cname(1:1) == "t") THEN
                tlev = TK(i,j,1) + (GEOPT(i,j,1)/9.81-zlev)*.0065
                IF ( cname == "tk" ) data_out(i,j,k) = tlev 
                IF ( cname == "tc" ) data_out(i,j,k) = tlev - 273.15
                IF ( cname == "theta" ) THEN
                  gamma = (287.04/1004.)*(1.+(0.608-0.887)*QV(i,j,k))
                  data_out(i,j,k) = tlev * (1000./z_levs(k))**gamma
                ENDIF
              ENDIF
  
            ELSEIF ( data_out(i,j,k) == MISSING_VALUE ) THEN

!             We are below both the ground and the lowest data level.
!             First, find the model level that is closest to a "target" pressure
!             level, where the "target" pressure is delta-p less that the local
!             value of a horizontally smoothed surface pressure field.  We use
!             delta-p = 150 hPa here. A standard lapse rate temperature profile
!             passing through the temperature at this model level will be used
!             to define the temperature profile below ground.  This is similar
!             to the Benjamin and Miller (1990) method, except that for
!             simplicity, they used 700 hPa everywhere for the "target" pressure.
!             Code similar to what is implemented in RIP4

              ptarget = (PSFC(i,j)*.01) - 150.
              dpmin=1.e4
              kupper = 0
              loop_kIN : DO kin=nz,1,-1
                 kupper = kin
                 dp=abs( (PRES(i,j,kin)*.01) - ptarget )
                 IF (dp.gt.dpmin) exit loop_kIN
                 dpmin=min(dpmin,dp)
              ENDDO loop_kIN

              pbot=max(PRES(i,j,1),PSFC(i,j))
              zbot=min(GEOPT(i,j,1)/9.81,HGT(i,j))   ! need height in meter

              tbotextrap=TK(i,j,kupper)*(pbot/PRES(i,j,kupper))**expon
              tvbotextrap=virtual(tbotextrap,QV(i,j,1))

!             Calculations use height in meter, but we want the output in km
              zlev = (zbot+tvbotextrap/.0065*(1.-(100.*z_levs(k)/pbot)**expon)) 
              IF ( cname == "height" ) data_out(i,j,k) = zlev / 1000.
              IF ( cname == "geopt" )  data_out(i,j,k) = zlev * 9.81
              IF ( cname(1:1) == "t") THEN
                tlev = TK(i,j,1) + (GEOPT(i,j,1)/9.81-zlev)*.0065
                IF ( cname == "tk" ) data_out(i,j,k) = tlev 
                IF ( cname == "tc" ) data_out(i,j,k) = tlev - 273.15
                IF ( cname == "theta" ) THEN
                  gamma = (287.04/1004.)*(1.+(0.608-0.887)*QV(i,j,k))
                  data_out(i,j,k) = tlev * (1000./z_levs(k))**gamma
                ENDIF
              ENDIF
              
            ENDIF


          ENDDO
          ENDDO
        ENDDO

      ENDIF
    ENDIF
  
    IF ( vertical_type == 'z' ) THEN
      !!! Need to do something special for height and temparature
      IF ( cname=="pressure" .OR. &
           cname=="tk" .OR. cname=="tc" .OR. cname=="theta" ) THEN
        DO k = 1, kk
          DO j = 1, nyout
          DO i = 1, nxout
            IF ( data_out(i,j,k) == MISSING_VALUE .AND. 1000.*z_levs(k) > HGT(i,j) ) THEN

!             We are below the first model level, but above the ground
!             We need meter for the calculations so, GEOPT/G
  
              plev = (((1000.*z_levs(k) - GEOPT(i,j,1)/9.81)*PSFC(i,j) +  &
                     (HGT(i,j) - 1000.*z_levs(k))*PRES(i,j,1)) /   &
                     (HGT(i,j) - GEOPT(i,j,1)/9.81)) 
              IF ( cname == "pressure" ) data_out(i,j,k) = plev * 0.01
              IF ( cname(1:1) == "t") THEN
                tlev = TK(i,j,1) + (GEOPT(i,j,1)/9.81-1000.*z_levs(k))*.0065
                IF ( cname == "tk" ) data_out(i,j,k) = tlev 
                IF ( cname == "tc" ) data_out(i,j,k) = tlev - 273.15
                IF ( cname == "theta" ) THEN
                  gamma = (287.04/1004.)*(1.+(0.608-0.887)*QV(i,j,k))
                  data_out(i,j,k) = tlev * (1000./plev)**gamma
                ENDIF
              ENDIF
  
            ELSEIF ( data_out(i,j,k) == MISSING_VALUE ) THEN

!             We are below both the ground and the lowest data level.
!             First, find the model level that is closest to a "target" pressure
!             level, where the "target" pressure is delta-p less that the local
!             value of a horizontally smoothed surface pressure field.  We use
!             delta-p = 150 hPa here. A standard lapse rate temperature profile
!             passing through the temperature at this model level will be used
!             to define the temperature profile below ground.  This is similar
!             to the Benjamin and Miller (1990) method, except that for
!             simplicity, they used 700 hPa everywhere for the "target" pressure.
!             Code similar to what is implemented in RIP4

              ptarget = (PSFC(i,j)*.01) - 150.
              dpmin=1.e4
              kupper = 0
              loop_kIN_z : DO kin=nz,1,-1
                 kupper = kin
                 dp=abs( (PRES(i,j,kin)*.01) - ptarget )
                 IF (dp.gt.dpmin) exit loop_kIN_z
                 dpmin=min(dpmin,dp)
              ENDDO loop_kIN_z

              pbot=max(PRES(i,j,1),PSFC(i,j))
              zbot=min(GEOPT(i,j,1)/9.81,HGT(i,j))   ! need height in meter

              tbotextrap=TK(i,j,kupper)*(pbot/PRES(i,j,kupper))**expon
              tvbotextrap=virtual(tbotextrap,QV(i,j,1))

!             Calculations use height in meter, but we want the output in km
              plev = pbot*(1.+0.0065/tvbotextrap*(zbot-1000.*z_levs(k)))**exponi
              IF ( cname == "pressure" ) data_out(i,j,k) = plev * 0.01
              IF ( cname(1:1) == "t") THEN
                tlev = TK(i,j,1) + (GEOPT(i,j,1)/9.81-1000.*z_levs(k))*.0065
                IF ( cname == "tk" ) data_out(i,j,k) = tlev 
                IF ( cname == "tc" ) data_out(i,j,k) = tlev - 273.15
                IF ( cname == "theta" ) THEN
                  gamma = (287.04/1004.)*(1.+(0.608-0.887)*QV(i,j,k))
                  data_out(i,j,k) = tlev * (1000./plev)**gamma
                ENDIF
              ENDIF
              
            ENDIF


          ENDDO
          ENDDO
        ENDDO

      ENDIF
    ENDIF


    !!! All fields and geopt at higher levels come here
    DO j = 1, nyout
    DO i = 1, nxout
      DO k = 1, kk     
         if ( data_out(i,j,k) == MISSING_VALUE ) data_out(i,j,k) = data_in(i,j,1)
      END DO
      DO k = kk+1, nzout
         if ( data_out(i,j,k) == MISSING_VALUE ) data_out(i,j,k) = data_in(i,j,nz)
      END DO
    END DO
    END DO



  END SUBROUTINE interp

!----------------------------------------------

  SUBROUTINE interp_1d( a, xa, na, b, xb, nb, vertical_type)

  implicit none

 ! Arguments
  integer, intent(in)              :: na, nb
  real, intent(in), dimension(na)  :: a, xa
  real, intent(in), dimension(nb)  :: xb
  real, intent(out), dimension(nb) :: b
  character (len=1)                :: vertical_type

  ! Local variables
  integer                          :: n_in, n_out
  real                             :: w1, w2
  logical                          :: interp

  IF ( vertical_type == 'p' ) THEN

    DO n_out = 1, nb

      b(n_out) = missing_value
      interp = .false.
      n_in = 1

      DO WHILE ( (.not.interp) .and. (n_in < na) )
        IF( (xa(n_in)   >= xb(n_out)) .and. &
            (xa(n_in+1) <= xb(n_out))        ) THEN
          interp = .true.
          w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
          w2 = 1. - w1
          b(n_out) = w1*a(n_in) + w2*a(n_in+1)
        END IF
        n_in = n_in +1
      ENDDO

    ENDDO

  ELSE

    DO n_out = 1, nb
  
      b(n_out) = missing_value
      interp = .false.
      n_in = 1
  
      DO WHILE ( (.not.interp) .and. (n_in < na) )
        IF( (xa(n_in)   <= xb(n_out)) .and. &
            (xa(n_in+1) >= xb(n_out))        ) THEN
          interp = .true.
          w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
          w2 = 1. - w1
          b(n_out) = w1*a(n_in) + w2*a(n_in+1)
        END IF
        n_in = n_in +1
      ENDDO

    ENDDO

  END IF

  END SUBROUTINE interp_1d

!-------------------------------------------------------------------------


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_interp_info
   ! Purpose: Make sure all the info we will need for interpolations is available
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE get_interp_info ()

      IMPLICIT NONE

      ! Local variables
      integer                             :: istatus, ii
      integer, dimension(2)               :: loc_of_min_z
      real, allocatable, dimension(:,:,:) :: ph_tmp, phb_tmp
      real, allocatable, dimension(:,:,:) :: tmp3D
      integer                             :: found
      integer                             :: iloc, itype, idm, natt
      character (len=50)                  :: cname
      integer, dimension(4)               :: istart, iend


      found = 0

      IF (vertical_type == 'p') THEN
        istatus = nf_inq_varid ( ncid, "P", iloc )
           found = found + istatus
        istatus = nf_inq_varid ( ncid, "PB", iloc )
           found = found + istatus

        IF ( found /= 0 .AND. iprogram == 6 ) THEN
          !! Probably wrfinput prior to v3.1, so it may not have P and PB - lets try something else
          found = 0
          IF ( debug_level >= 300 ) &
            CALL mprintf(.true.,STDOUT, '   INFO: Probably old wrfinput data - try getting MU and MUB ' )

          istatus = nf_inq_varid ( ncid, "QVAPOR", iloc )
             found = found + istatus
          istatus = nf_inq_varid ( ncid, "MU", iloc )
             found = found + istatus
          istatus = nf_inq_varid ( ncid, "MUB", iloc )
             found = found + istatus
          istatus = nf_inq_varid ( ncid, "P_TOP", iloc )
             found = found + istatus
          istatus = nf_inq_varid ( ncid, "ZNU", iloc )
             found = found + istatus
          istatus = nf_inq_varid ( ncid, "ZNW", iloc )
             found = found + istatus
        ENDIF  

        IF ( found == 0 ) THEN
          CALL mprintf(.true.,STDOUT, '   Interpolating to PRESSURE levels ')
          CALL mprintf(.true.,STDOUT, '   ')
        ELSE
          CALL mprintf(.true.,STDOUT, '   WARNING: Asked to interpolate to PRESSURE, ' )
          CALL mprintf(.true.,STDOUT, '            but we do not have enough information ' )
          CALL mprintf(.true.,STDOUT, '            Will output data on MODEL LEVELS ' )
          vertical_type = 'n'
          extrapolate = .FALSE.
        ENDIF

      ENDIF


      IF (vertical_type == 'z' .AND. interp_method == 1 ) THEN
        istatus = nf_inq_varid ( ncid, "PH", iloc )
           found = found + istatus
        istatus = nf_inq_varid ( ncid, "PHB", iloc )
           found = found + istatus

        IF ( found == 0 ) THEN
          CALL mprintf(.true.,STDOUT, '   Interpolating to USER SPECIFIED HEIGHT levels ')
          CALL mprintf(.true.,STDOUT, '   ')
        ELSE
          CALL mprintf(.true.,STDOUT, '   WARNING: Asked to interpolate to USER SPECIFIED HEIGHT,' )
          CALL mprintf(.true.,STDOUT, '            but we do not have enough information ' )
          CALL mprintf(.true.,STDOUT, '            Will output data on MODEL LEVELS ' )
          vertical_type = 'n'
          extrapolate = .FALSE.
        ENDIF

      ENDIF

      IF (vertical_type == 'z' .AND. interp_method == -1 ) THEN
        found = 0

        istatus = nf_inq_varid ( ncid, "PH", iloc )
          found = found + istatus
        istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
        ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        istart = 1
        iend   = 1
        do ii = 1,3
          iend(ii) = ncDims(ishape(ii))
        end do
        CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
        ALLOCATE(ph_tmp(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))-1))
        ph_tmp = 0.5 * (tmp3D(:,:,1:ncDims(ishape(3))-1) + tmp3D(:,:,2:ncDims(ishape(3))))

        istatus = nf_inq_varid ( ncid, "PHB", iloc )
           found = found + istatus
        CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
        ALLOCATE(phb_tmp(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))-1))
        phb_tmp = 0.5 * (tmp3D(:,:,1:ncDims(ishape(3))-1) + tmp3D(:,:,2:ncDims(ishape(3))))
        DEALLOCATE(tmp3D)

        IF ( found == 0 ) THEN
          ph_tmp = ( (ph_tmp+phb_tmp) / 9.81)/1000.        !!! convert to height in km
          !!! Generate nice heights. 
          !!! Get location of lowest height on model grid
          !!! Use column above this point as our heights to interpolate to
          !!! Adjust lowerst and heighest heights a bit
          number_of_zlevs = bottom_top_dim
          loc_of_min_z = minloc(ph_tmp(:,:,1))
          interp_levels(1:number_of_zlevs) =                                   &
                    ph_tmp(loc_of_min_z(1),loc_of_min_z(2),1:number_of_zlevs)
          interp_levels(1) = interp_levels(1) + 0.002
          interp_levels(1) = MAX(interp_levels(1), interp_levels(2)/2.0)  !! no neg values
          interp_levels(number_of_zlevs) = interp_levels(number_of_zlevs) - 0.002
          CALL mprintf(.true.,STDOUT, '   Interpolating to GENERATED HEIGHT levels ')
          CALL mprintf(.true.,STDOUT, '   ')
        ELSE
          CALL mprintf(.true.,STDOUT, '   WARNING: Asked to interpolate to GENERATED HEIGHT, ' )
          CALL mprintf(.true.,STDOUT, '            but we do not have enough information ' )
          CALL mprintf(.true.,STDOUT, '            Will output data on MODEL LEVELS ' )
          vertical_type = 'n'
          extrapolate = .FALSE.
        ENDIF

        IF (ALLOCATED(ph_tmp))  DEALLOCATE(ph_tmp)
        IF (ALLOCATED(phb_tmp)) DEALLOCATE(phb_tmp)
      ENDIF
    

   END SUBROUTINE get_interp_info



   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_interp_array
   ! Purpose: Get array we will use when interpolting 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE get_interp_array (local_time)

      USE module_pressure
      USE module_arrays

      IMPLICIT NONE

      ! Local variables
      integer                             :: istatus, wrftype, found, ii
      real, allocatable, dimension(:,:,:) :: real_array
      character (len=3)                   :: memorder
      character (len=128)                 :: cname
      integer, dimension(2)               :: loc_of_min_z
      real, allocatable, dimension(:,:,:) :: p_tmp, ph_tmp, phb_tmp
      real, allocatable, dimension(:)     :: tmp1D
      real, allocatable, dimension(:,:)   :: tmp2D
      real, allocatable, dimension(:,:,:) :: tmp3D
      integer                             :: iloc, itype, idm, natt
      integer, dimension(4)               :: istart, iend
      integer                             :: local_time



      IF (vertical_type == 'p') THEN
        !! First find out which input variable are available - depend on input file
        found = 0
        istatus = nf_inq_varid ( ncid, "P", iloc )
           found = found + istatus
        istatus = nf_inq_varid ( ncid, "PB", iloc )
           found = found + istatus


        IF ( found /= 0 .AND. iprogram == 6 ) THEN
        !! probably an old wrfinput file - calculate PRES from MU and MUB
          IF ( debug_level >= 300 ) &
            CALL mprintf(.true.,STDOUT, '   INFO: Probably old wrfinput data - try getting MU and MUB ' )

          istatus = nf_inq_varid ( ncid, "QVAPOR", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
          istart = 1
          istart(idm) = local_time
          iend   = 1
          do ii = 1,3
            iend(ii) = ncDims(ishape(ii))
          end do
          CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
          ALLOCATE(QV(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
          QV = tmp3D
          DEALLOCATE(tmp3D)

          IF ( ALLOCATED(real_array) ) DEALLOCATE(real_array)
          ALLOCATE(real_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))

          istatus = nf_inq_varid ( ncid, "MU", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          ALLOCATE(tmp2D(ncDims(ishape(1)),ncDims(ishape(2))))
          istart = 1
          istart(idm) = local_time
          iend   = 1
          do ii = 1,2
            iend(ii) = ncDims(ishape(ii))
          end do
          CALL NCVGT(ncid,iloc,istart,iend,tmp2D,istatus)
          ALLOCATE(MU(ncDims(ishape(1)),ncDims(ishape(2))))
          MU = tmp2D
          istatus = nf_inq_varid ( ncid, "MUB", iloc )
          CALL NCVGT(ncid,iloc,istart,iend,tmp2D,istatus)
          ALLOCATE(MUB(ncDims(ishape(1)),ncDims(ishape(2))))
          MUB = tmp2D
          DEALLOCATE(tmp2D)

          istatus = nf_inq_varid ( ncid, "ZNU", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          ALLOCATE(tmp1D(ncDims(ishape(1))))
          istart = 1
          istart(idm) = local_time
          iend   = 1
          iend(1) = ncDims(ishape(1))
          CALL NCVGT(ncid,iloc,istart,iend,tmp1D,istatus)
          ALLOCATE(ZNU(ncDims(ishape(1))))
          ZNU = tmp1D
          DEALLOCATE(tmp1D)
          istatus = nf_inq_varid ( ncid, "ZNW", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          iend(1) = ncDims(ishape(1))
          CALL NCVGT(ncid,iloc,istart,iend,tmp1D,istatus)
          ALLOCATE(ZNW(ncDims(ishape(1))))
          ZNW = tmp1D
          DEALLOCATE(tmp1D)

          istatus = nf_inq_varid ( ncid, "P_TOP", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          ALLOCATE(tmp1D(ncDims(ishape(1))))
          istatus = nf_get_var_real(ncid, iloc, tmp1D)
          PTOP = tmp1D(1)
          DEALLOCATE(tmp1D)

          CALL pressure(real_array)       

        ELSE
          istatus = nf_inq_varid ( ncid, "P", iloc )
          istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
          ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
          istart = 1
          istart(idm) = local_time
          iend   = 1
          do ii = 1,3
            iend(ii) = ncDims(ishape(ii))
          end do
          CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
          IF ( ALLOCATED(real_array) ) DEALLOCATE(real_array)
          ALLOCATE(real_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
          real_array = tmp3D

          istatus = nf_inq_varid ( ncid, "PB", iloc )
          CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
          real_array = real_array + tmp3D
          DEALLOCATE(tmp3D)
        ENDIF  

        cname = 'PRES'
        CALL keep_arrays(cname, real_array)

        IF (ALLOCATED(vert_array)) DEALLOCATE(vert_array)
        ALLOCATE(vert_array(west_east_dim,south_north_dim,bottom_top_dim))
        vert_array = 0.01 * real_array     !! pressure array in HPa

        IF (ALLOCATED(QV))    DEALLOCATE(QV)
        IF (ALLOCATED(MU))    DEALLOCATE(MU)
        IF (ALLOCATED(MUB))   DEALLOCATE(MUB)
        IF (ALLOCATED(ZNU))   DEALLOCATE(ZNU)
        IF (ALLOCATED(ZNW))   DEALLOCATE(ZNW)
        IF (ALLOCATED(p_tmp)) DEALLOCATE(p_tmp)
      ENDIF



      IF (vertical_type == 'z' ) THEN

        istatus = nf_inq_varid ( ncid, "PH", iloc )
        istatus = nf_inq_var(ncid, iloc, cname, itype, idm, ishape, natt)
        IF (ALLOCATED(vert_array)) DEALLOCATE(vert_array)
        ALLOCATE(vert_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))-1))
        ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        istart = 1
        istart(idm) = local_time
        iend   = 1
        do ii = 1,3
          iend(ii) = ncDims(ishape(ii))
        end do
        CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
        vert_array = 0.5 * (tmp3D(:,:,1:ncDims(ishape(3))-1) + tmp3D(:,:,2:ncDims(ishape(3))))

        istatus = nf_inq_varid ( ncid, "PHB", iloc )
        CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
        vert_array = vert_array + 0.5 * (tmp3D(:,:,1:ncDims(ishape(3))-1) + tmp3D(:,:,2:ncDims(ishape(3))))
        DEALLOCATE(tmp3D)
        
        vert_array = ( (vert_array) / 9.81)/1000.        !! height array in km

      ENDIF


   END SUBROUTINE get_interp_array



   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_keep_array
   ! Purpose: Get array we will use when interpolting 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE get_keep_array ( local_time, good_to_go, cname, cname2 )

      USE module_pressure
      USE module_arrays


      IMPLICIT NONE

      integer                             :: istatus, wrftype
      real, allocatable, dimension(:,:,:) :: real_array
      character (len=3)                   :: memorder
      character (len=*)                   :: cname
      character (len=*), optional         :: cname2
      character (len=128)                 :: stagger
      character (len=128), dimension(3)   :: dimnames
      integer, dimension(3)               :: domain_start, domain_end
      integer, intent(inout)              :: good_to_go
      character (len=50)                  :: cval
      integer                             :: iloc, itype, idm, natt, ii
      integer, dimension(4)               :: istart, iend
      real, allocatable, dimension(:,:)   :: tmp2D
      real, allocatable, dimension(:,:,:) :: tmp3D
      integer                             :: local_time
      real                                :: tmp1D


      istatus = nf_inq_varid ( ncid, cname, iloc )
      cval = TRIM(cname)
      IF ( istatus /= 0 .AND. PRESENT (cname2) ) THEN
         IF ( debug_level >= 300. ) print*, "Trying to find: ", trim(cname2)
         istatus = nf_inq_varid ( ncid, cname2, iloc )
         cval = TRIM(cname2)
      END IF

      IF ( istatus /= 0 ) THEN
        ! WE do not have this field
        good_to_go = good_to_go + istatus
        return
      END IF

      istatus = nf_inq_var(ncid, iloc, cval, itype, idm, ishape, natt)

      istart = 1
      iend   = 1
      istart(idm) = local_time
      DO ii = 1, idm-1
         iend(ii) = ncDims(ishape(ii))
      END DO

      istatus = 0
      IF (idm == 4) THEN
        ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        CALL NCVGT(ncid,iloc,istart,iend,tmp3D,istatus)
      ELSEIF (idm == 3) THEN
        ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        ALLOCATE(tmp2D(ncDims(ishape(1)),ncDims(ishape(2))))
        CALL NCVGT(ncid,iloc,istart,iend,tmp2D,istatus)
        tmp3D(:,:,1) = tmp2D
        DEALLOCATE(tmp2D)
      ELSEIF (idm == 1) THEN
        ALLOCATE(tmp3D(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        CALL NCVGT(ncid,iloc,istart,iend,tmp1D,istatus)
        tmp3D(:,1,1) = tmp1D
      END IF
      IF (istatus /= 0) return

      IF ( ALLOCATED(real_array) ) DEALLOCATE(real_array)
      ALLOCATE(real_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
      real_array = tmp3D
     

      IF ( istatus == 0 ) CALL keep_arrays(trim(cval), real_array)
      good_to_go = good_to_go + istatus


      IF ( ALLOCATED(real_array) ) DEALLOCATE(real_array)

   END SUBROUTINE get_keep_array


END MODULE module_interp
