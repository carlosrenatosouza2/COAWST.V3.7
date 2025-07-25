MODULE input_module

   USE gridinfo_module
   USE misc_definitions_module
   USE module_debug
   USE module_model_basics
 
 
   ! WRF I/O API related variables
   integer                             :: handle
 
   integer                             :: field_number, iatts
   character (len=200), dimension(200) :: catts
   real                                :: bucket_mm, bucket_J

   CONTAINS
 
 
!------------------------------------------------------------------------------------------
   SUBROUTINE input_init (file_number, istatus)
 
      implicit none

      integer, intent(in)  :: file_number
      integer, intent(out) :: istatus
      character (len=128)  :: input_fname
      integer              :: rcode, nDims, nVars, unlimDimID, ii, dval
      character (len=80)   :: dname
  
  
      IF ( debug_level .ge. 500 ) print*,"DEBUG: SUBROUTINE input_init"
     
      istatus = 0
      input_fname = ' '
      input_fname = trim(input_file_names(file_number))

      rcode = nf_open(trim(input_fname), NF_NOWRITE, ncid )
      IF (.not. readOnce ) THEN
        rcode = nf_inq(ncid, nDims, nVars, nAtts, unlimDimID)
        allocate(ishape(nDims))
        allocate(ncDims(nDims))
        ishape = 0
        ncDims = 0
        do ii = 1, nDims
          rcode = nf_inq_dim(ncid, ii, dname, dval)
          ncDims(ii) = dval 
          IF ( debug_level .ge. 500 )  print*,"NAME=",dname, "  VAL=",dval
        enddo
        readOnce = .TRUE.
      ENDIF  
     

   END SUBROUTINE input_init

!------------------------------------------------------------------------------------------
 
   SUBROUTINE read_next_field (domain_start, domain_end, &
                               cname, cunits, cdesc, memorder, &
                               stagger, real_array, local_time, istatus)
 
      IMPLICIT none

  
      ! Arguments
      integer, dimension(3)                  :: domain_start, domain_end
      character (len=*), intent(out)         :: cname, memorder, stagger, cunits, cdesc
      real, allocatable, dimension(:,:,:)    :: real_array
      integer                                :: local_time
      integer, intent(inout)                 :: istatus
  
      ! Local variables
      integer                                :: rcode
      integer                                :: iloc, itype, idm, natt, ii
      integer, dimension(4)                  :: istart, iend
      real, allocatable, dimension(:,:)      :: tmp2D
      integer, allocatable, dimension(:,:,:) :: tmp3D_int
      integer, allocatable, dimension(:,:)   :: tmp2D_int
  

      IF ( debug_level .ge. 500 ) print*,"DEBUG: SUBROUTINE read_next_field"
      
  5   field_number = field_number + 1

      istatus = nf_inq_var(ncid, field_number, cname, itype, idm, ishape, natt)
      IF (istatus /= 0) return
      domain_start = 1
      domain_end   = 1
      istart = 1
      iend   = 1
      IF (idm .lt. 3 ) return 	! don't want scalars or 1D arrays - so cycle

      istart(idm) = local_time
      DO ii = 1, idm-1
         iend(ii) = ncDims(ishape(ii))
      END DO
      domain_start = istart(1:idm-1)
      domain_end   = iend(1:idm-1)

      istatus = 0
      IF ( ALLOCATED(real_array) ) DEALLOCATE(real_array)
      IF (idm == 4) THEN
        ALLOCATE(real_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        IF (itype == 5 ) THEN 		!! Real data
          CALL NCVGT(ncid,field_number,istart,iend,real_array,istatus)
        ELSEIF (itype == 4 ) THEN	!! Integer data
          ALLOCATE(tmp3D_int(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
          CALL NCVGT(ncid,field_number,istart,iend,tmp3D_int,istatus)
          real_array = real(tmp3D_int)
          DEALLOCATE(tmp3D_int)
        END IF
      ELSEIF (idm == 3) THEN
        ALLOCATE(real_array(ncDims(ishape(1)),ncDims(ishape(2)),ncDims(ishape(3))))
        IF (itype == 5 ) THEN 		!! Real data
          ALLOCATE(tmp2D(ncDims(ishape(1)),ncDims(ishape(2))))
          CALL NCVGT(ncid,field_number,istart,iend,tmp2D,istatus)
          real_array(:,:,1) = tmp2D
          DEALLOCATE(tmp2D)
        ELSEIF (itype == 4 ) THEN	!! Integer data
          ALLOCATE(tmp2D_int(ncDims(ishape(1)),ncDims(ishape(2))))
          CALL NCVGT(ncid,field_number,istart,iend,tmp2D_int,istatus)
          real_array(:,:,1) = real(tmp2D_int)
          DEALLOCATE(tmp2D_int)
        END IF
      END IF
      IF (istatus /= 0) return


      cunits(:) = ' '
      cdesc(:) = ' '
      rcode = NF_GET_ATT_TEXT(ncid, field_number, "units", cunits )
      rcode = NF_GET_ATT_TEXT(ncid, field_number, "description", cdesc )
      IF (iachar(cunits(1:1)) == 0 ) cunits = "-"
      IF (iachar(cdesc(1:1)) == 0 ) cdesc = cname   !!! Because .ctl must have a description

      rcode = NF_GET_ATT_TEXT(ncid, field_number, "MemoryOrder", memorder )
      stagger(:) = ' '
      rcode = NF_GET_ATT_TEXT(ncid, field_number, "stagger", stagger )
      IF (iachar(stagger(1:1)) == 0 ) stagger = "-"


      IF ( debug_level .ge. 300 ) THEN
        print*,"DEBUG: Read Variable " ,trim(cname)
      ENDIF

      

      IF ( debug_level .ge. 500 ) THEN
        print*,"DEBUG: Variable Information"
        print*,"       ndim     = ",idm-1
        print*,"       memorder = ",trim(memorder)
        print*,"       stagger  = ",trim(stagger)
        print*,"       cdesc    = ",trim(cdesc)
        print*,"       cunits   = ",trim(cunits)
        print*,"       dims     = ",iend(1:idm-1)
        IF (itype == 5) print*,"       type     = REAL"
        IF (itype == 4) print*,"       type     = INTEGER"
        print*,"       min/max  = ",minval(real_array),"   ",maxval(real_array)
      ENDIF
      IF ( debug_level .ge. 900 ) THEN
        IF ( idm == 4 ) &
        print*,"       sample vert. levels = ",real_array(domain_end(1)/2, domain_end(2)/2,:)
      ENDIF

 
   END SUBROUTINE read_next_field

!------------------------------------------------------------------------------------------
   
   SUBROUTINE read_global_attrs ()
 
      implicit none
  
      ! Local variables
      integer                          :: istatus
      character (len=19)               :: start_date
      character (len=80)               :: att_name, value_chr
      integer                          :: iatt, ivtype, attlen
      real                             :: value_real
      integer                          :: value_int
  
      iatts = 0
      
      !! Any unknown program (including WRFSI) will be 0
      iprogram = 0
      title(:) = ' ' 
      istatus = NF_GET_ATT_TEXT(ncid, nf_global, 'TITLE', title)
      IF ( INDEX(title,'OUTPUT FROM GEOGRID') /= 0 ) iprogram = 1 !! geogrid output
      IF ( INDEX(title,'OUTPUT FROM GRIDGEN') /= 0 ) iprogram = 1 !! old geogrid output
      IF ( INDEX(title,'OUTPUT FROM METGRID') /= 0 ) iprogram = 3 !! metgrid output
      IF ( INDEX(title,'OUTPUT FROM OBSGRID') /= 0 ) iprogram = 3 !! obsgrid output
      IF ( INDEX(title,'OUTPUT FROM REAL_EM') /= 0 ) iprogram = 6 !! real.exe output
      IF ( INDEX(title,'OUTPUT FROM WRF') /= 0 )     iprogram = 8 !! wrf.exe output
      IF ( iprogram == 0 ) THEN
         print*," "
         print*,"  WARNING --- I do not recognize this data."
         print*,"             ",trim(title)
         print*,"              Will make an attempt to read it."
         print*," "
      END IF

      !! Make sure we are working with the unstaggered values here
      istatus = NF_GET_ATT_INT(ncid, nf_global, 'WEST-EAST_GRID_DIMENSION', west_east_dim)
           west_east_dim = west_east_dim - 1
      istatus = NF_GET_ATT_INT(ncid, nf_global, 'SOUTH-NORTH_GRID_DIMENSION', south_north_dim)
           south_north_dim = south_north_dim - 1
      istatus = NF_GET_ATT_INT(ncid, nf_global, 'BOTTOM-TOP_GRID_DIMENSION', bottom_top_dim)
           IF ( iprogram .le. 1 ) bottom_top_dim = 24  !!!  to make room for any 3D datasets
           IF ( iprogram .ge. 6 ) bottom_top_dim = bottom_top_dim - 1

      istatus = NF_GET_ATT_TEXT(ncid, nf_global, 'SIMULATION_START_DATE', start_date)
      istatus = NF_GET_ATT_INT(ncid, nf_global,  'MAP_PROJ', map_proj)
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'DX', dx) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'DY', dy) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'CEN_LAT', cen_lat) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'CEN_LON', cen_lon) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'TRUELAT1', truelat1) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'TRUELAT2', truelat2) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'MOAD_CEN_LAT', moad_cen_lat) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'STAND_LON', stand_lon) 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'BUCKET_MM', bucket_mm) 
        IF ( istatus /= 0 ) bucket_mm = 0
        IF ( bucket_mm < 0 ) bucket_mm = 0
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'BUCKET_J', bucket_J) 
        IF ( istatus /= 0 ) bucket_J = 0
        IF ( bucket_J < 0 ) bucket_J = 0 
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'POLE_LAT', pole_lat) 
        IF ( istatus /= 0 ) pole_lat = 0
      istatus = NF_GET_ATT_REAL(ncid, nf_global, 'POLE_LON', pole_lon) 
        IF ( istatus /= 0 ) pole_lon = 0


      !! Just needed for meta data in the .ctl file
      iatts = 0
      DO iatt = 1,nAtts
        istatus = nf_inq_attname(ncid,nf_global,iatt,att_name)
        istatus = nf_inq_att(ncid,nf_global,att_name,ivtype,attlen )
        IF (ivtype == 2) THEN
          value_chr(:) = ' ' 
          istatus = NF_GET_ATT_TEXT(ncid, nf_global, att_name, value_chr )
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",A)') trim(att_name), trim(value_chr)
        ELSEIF (ivtype == 4) THEN
          istatus = NF_GET_ATT_INT(ncid, nf_global, att_name, value_int )
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",i5)') trim(att_name), value_int
        ELSEIF (ivtype == 5) THEN
          istatus = NF_GET_ATT_REAL(ncid, nf_global, att_name, value_real )
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",f12.2)') trim(att_name), value_real
        END IF
      END DO


   END SUBROUTINE read_global_attrs
 
!------------------------------------------------------------------------------------------
 
   SUBROUTINE input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0
      istatus = nf_close(ncid)
 
 
   END SUBROUTINE input_close

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_next_time (valid_date, datestr, file_time, istatus)

      USE date_pack

      implicit none

      integer               :: istatus
      character (len=*)     :: valid_date, datestr
      integer               :: iloc, itype, idm, natt, ii, file_time
      character (len=50)    :: cval
      integer, dimension(4) :: istart, iend
      integer               :: isec_diff


        istatus = nf_inq_varid ( ncid, "Times", iloc )
        istatus = nf_inq_var(ncid, iloc, cval, itype, idm, ishape, natt)

        istart = 1
        iend   = 1
        iend(1) = ncDims(ishape(1))

        get_times : DO ii=1,ncDims(ishape(2))
          file_time = ii
          istart(2) = ii
          istatus = NF_GET_VARA_TEXT (ncid,iloc,istart,iend,datestr)
          IF ( istatus /= 0 ) EXIT get_times
          call get_diff_dates (datestr, valid_date, isec_diff)
          !IF ( TRIM(datestr) == TRIM(valid_date) ) EXIT get_times
          IF ( abs(isec_diff) .LE. tacc ) EXIT get_times
        END DO get_times

        IF ( TRIM(datestr) < TRIM(valid_date) ) istatus = -1


   END SUBROUTINE arw_get_next_time

!------------------------------------------------------------------------------------------

END MODULE input_module
