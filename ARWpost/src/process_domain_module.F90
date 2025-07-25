MODULE process_domain_module
   
   USE input_module
   USE output_module
   USE module_model_basics
   USE module_arrays
   USE module_interp
   
   integer                             :: next_file
   logical                             :: run_out_of_files
   logical                             :: file_is_open

#ifdef MEM_CHECK
  interface
      subroutine check() bind(c,name='_g95_memory_done')
      end subroutine
   end interface
#endif
 

   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_domain
   ! Purpose: Process the input data
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE process_domain ()
   
      USE date_pack
      USE gridinfo_module
      USE misc_definitions_module
      USE module_debug
   
      IMPLICIT NONE

      ! Local variables
      integer                                :: idiff, n_times, i
      character (len=19)                     :: valid_date, temp_date, datestr
      character (len=128)                    :: cname, stagger, cunits, cdesc
      integer                                :: istatus, reclength
      integer                                :: frames_in_outfile
      integer                                :: good_to_go
   

      ! Compute number of times that we will process
      CALL geth_idts(end_date, start_date, idiff)
      CALL mprintf((idiff < 0),ERROR,'Ending date is earlier than starting date in namelist.')
   
      ! Check that the interval evenly divides the range of times to process
      n_times = idiff / interval_seconds
      CALL mprintf((mod(idiff, interval_seconds) /= 0),WARN, &
                   'In namelist, interval_seconds does not evenly divide into'// &
                   '(end_date - start_date). Check to make sure all requested times '// &
                   'have be processed.')
   
      ! 
      ! DO TIME_INDEPENDANT PROCESSING
      ! 

      ! Initialize the input module to read input data
      CALL input_init(1, istatus)
      CALL mprintf((istatus /= 0),ERROR, 'input_init(): Error opening input.')
   

      ! Read global attributes from the input file 
      CALL read_global_attrs ()
      IF ( iprogram == 1 ) n_times = 0


      ! Open .dat and .ctl files
      DO cunit=10,100
        INQUIRE(unit=cunit, opened=is_used)
        if (.not. is_used) exit
      END DO
      ctlfile = trim(output_root_name)//'.ctl'
      OPEN(cunit,file=ctlfile)

      DO dunit=10,100
        INQUIRE(unit=dunit, opened=is_used)
        IF (.not. is_used) EXIT
      END DO

      IF      ( west_east_dim .gt. 2 .AND. south_north_dim .gt. 2 ) THEN
        reclength = (west_east_dim)*(south_north_dim)
      ELSE IF ( west_east_dim .gt. 2 .AND. south_north_dim .eq. 2 ) THEN
        reclength = west_east_dim
      ELSE IF ( west_east_dim .eq. 2 .AND. south_north_dim .gt. 2 ) THEN
        reclength = south_north_dim
      ELSE IF ( west_east_dim .eq. 2 .AND. south_north_dim .eq. 2 ) THEN
        reclength = 1
      END IF
#ifdef RECL4
      reclength = reclength*4
#endif
      datfile = trim(output_root_name)//'.dat'



      ! If interp_method /= 0, then we need to know if this can be done
      ! Only need to do this for wrfinput and wrfout data
      IF ( iprogram .ge. 6 .AND. interp_method /= 0 ) THEN
         CALL get_interp_info ()
      ELSE
        vertical_type = 'n'   !! In case user set this for other programs
        extrapolate = .FALSE.
        number_of_zlevs = bottom_top_dim
      END IF


      CALL input_close()       !!! Will be opened again by next process if needed

      !
      ! BEGIN TIME-DEPENDANT PROCESSING
        next_file = 1
        ivars     = 0
      
      ! Loop over all times to be processed
      could_not_find = plot_these_fields      !!! Keep a list of requested fields
      run_out_of_files = .FALSE.
      file_is_open = .FALSE.
      rec = 0
      frames_in_outfile = frames_per_outfile


      all_files : DO time=0,n_times
   
         IF ( iprogram == 1 ) THEN
           temp_date = "0000-00-00_00:00:00" 
         ELSE 
           CALL geth_newdate(valid_date, trim(start_date), time*interval_seconds)
           temp_date = ' '
           write(temp_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)
         ENDIF
         IF ( time == 0 ) tdef_date = temp_date

         IF ( split_output .AND. frames_in_outfile == frames_per_outfile ) THEN
            INQUIRE(unit=dunit, opened=is_used)
            IF ( is_used ) CLOSE (dunit)
            datfile = trim(output_root_name)//'_'//temp_date(1:16)//'.dat'
            rec = 0
            frames_in_outfile = 0
         END IF

         IF ( rec == 0 ) THEN
            OPEN(dunit,file=datfile,form='unformatted',access="direct", &
                 status="replace", recl=reclength)
         END IF

         frames_in_outfile = frames_in_outfile + 1

         CALL get_fields(temp_date)

         IF (run_out_of_files) EXIT all_files
   
      ENDDO all_files  ! Loop over n_times


      CALL mprintf(.true.,STDOUT, ' ')
      CALL mprintf(.true.,STDOUT, 'DONE Processing Data')

      IF ( extrapolate ) THEN
        CALL mprintf(.true.,STDOUT, ' ')
        print*, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        print*, '+++  You have chosen to extrapolate data                                '
        print*, '+++      data below ground will be extrapolated                         '
        print*, '+++      data above model top will be set to the value at model top     '
        IF (PTOP > 100.*interp_levels(number_of_zlevs) .AND. vertical_type == 'p' ) THEN
        print*, '+++                                                                     '
        print 10, ' +++  WARNING: Highest requested pressure level (',&
                    interp_levels(number_of_zlevs),' mb) is above PTOP '
        print 10, ' +++           use all pressure level data above ', PTOP*0.01,' mb with caution '
   10   format (A,F6.2,A)
        ENDIF
        print*, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      ENDIF


      ! Create the .ctl file
      CALL mprintf(.true.,STDOUT, ' ')
      CALL mprintf(.true.,STDOUT, 'CREATING .ctl file')
      CALL create_ctl ( )

      ! Do some final cleaning up
      IF ( ALLOCATED(XLAT) ) DEALLOCATE(XLAT)
      IF ( ALLOCATED(XLONG) ) DEALLOCATE(XLONG)
      IF ( ALLOCATED(ishape) ) DEALLOCATE(ishape)
      IF ( ALLOCATED(ncDims) ) DEALLOCATE(ncDims)
      IF ( ALLOCATED(vert_array) ) DEALLOCATE(vert_array)
      IF ( ALLOCATED(input_file_names) ) DEALLOCATE(input_file_names)


#ifdef MEM_CHECK
   call check()
#endif

   
   END SUBROUTINE process_domain


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_fields
   ! Purpose: Read all fields in input file and process required output fields
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE get_fields (valid_date)

      USE module_get_file_names
      USE module_basic_arrays
      USE module_diagnostics
      USE date_pack

      IMPLICIT NONE

      ! Local variables
      integer                            :: istatus, i, j, k, ii, jj, kk
      integer                            :: isec_needed, isec_found 
      real, allocatable, dimension(:,:,:):: real_array
      character (len=3)                  :: memorder
      character (len=19)                 :: datestr, valid_date
      character (len=128)                :: cname, stagger, cunits, cdesc
      character (len=128), dimension(3)  :: dimnames
      integer, dimension(3)              :: domain_start, domain_end
      real, allocatable, dimension(:,:,:):: data_out 
      real, allocatable, dimension(:,:,:):: SCR
      integer                            :: nxout, nyout, nzout
      character (len=20)                 :: dummy, dummy2
      integer                            :: is_there
      integer                            :: good_to_go, file_time


      ! Initialize the input module to read static input data for this domain
      CALL mprintf(.true.,STDOUT, ' ')
      CALL mprintf(.true.,STDOUT, ' Processing  time --- %s', s1=trim(valid_date))
      get_right_time : DO
        IF ( next_file > number_of_input_files ) THEN
           CALL mprintf(.true.,STDOUT, '   --- WE HAVE RUN OUT OF INPUT FILES ---')
           run_out_of_files = .TRUE.
           RETURN
        END IF
        IF ( .not. file_is_open) THEN
          CALL input_init(next_file, istatus)
          CALL mprintf((istatus /= 0),ERROR, 'input_init(): Error opening input.')
          file_is_open = .TRUE.
        END IF
        CALL arw_get_next_time(valid_date, datestr, file_time, istatus)
        IF ( istatus /= 0 ) THEN     ! might be in a next file
           CALL input_close()
           file_is_open = .FALSE.
           CALL mprintf(.true.,STDOUT, '   Date not in this file - see if there are more files ')
           next_file = next_file + 1
           CYCLE get_right_time
        END IF
        call get_seconds (datestr, isec_needed)
        call get_seconds (valid_date, isec_found)
        IF ( TRIM(datestr) .EQ. TRIM(valid_date) ) THEN
           CALL mprintf(.true.,STDOUT, '   Found the right date - continue ' )
           EXIT get_right_time
        ELSEIF ( abs(isec_needed-isec_found) .LE. tacc ) THEN
           CALL mprintf(.true.,STDOUT, '   Found  %s ', s1=trim(datestr))
           CALL mprintf(.true.,STDOUT, '   Date is close enough - continue ' )
           valid_date = datestr
           EXIT get_right_time
        ELSEIF ( TRIM(datestr) .GT. TRIM(valid_date) ) THEN
           CALL mprintf(.true.,STDOUT, '   Found  %s  before  %s', s1=trim(datestr),s2=trim(valid_date))
           run_out_of_files = .TRUE.
           RETURN
        ENDIF
      ENDDO get_right_time


      !! IF we are interpolting we need the pressure/height array to interpolate to
      IF ( vertical_type /= 'n' ) THEN
         CALL get_interp_array (file_time)
      ENDIF


      !! Get and keep PRES and TK
      CALL process_basic_arrays ( file_time )

   
      ! Read fields using the input module; we know that there are no more
      !   fields to be read when read_next_field() returns a non-zero status.
      istatus = 0
      field_number = 0
      process_all_fields : DO WHILE (istatus == 0)  
        CALL read_next_field(domain_start, domain_end, cname, cunits, cdesc, &
                             memorder, stagger, real_array, file_time, istatus)
        IF (istatus == 0) THEN
          IF ( INDEX(trim(cname),"_U") /= 0 .OR. INDEX(trim(cname),"_V") /= 0 ) THEN
             IF ( INDEX(trim(cname),"MAPFAC") /= 0 ) CYCLE process_all_fields
             IF ( INDEX(trim(cname),"XLAT")   /= 0 ) CYCLE process_all_fields
             IF ( INDEX(trim(cname),"XLONG")  /= 0 ) CYCLE process_all_fields
          ENDIF

!!
!! DECIDE IF WE WANT THE FIELD AND IF YES, INTERPOLATE TO CORRECT GRID IF NEEEDED
!! assume we got the grid info up at the top somewhere
!!

            dummy = ","//trim(cname)//","
            is_there = INDEX(plot_these_fields,trim(dummy))
            IF ( ( INDEX(plot,'all') /= 0 .OR. is_there /= 0) .AND. domain_end(2) /= 1) THEN
              IF (ALLOCATED(SCR)) DEALLOCATE(SCR)
              ALLOCATE(SCR(domain_end(1),domain_end(2),domain_end(3)))
              SCR = real_array

              IF ( bucket_J > 0 .AND.           &
                  ( trim(cname)=='ACSWUPT'  .OR. &
                    trim(cname)=='ACSWUPTC' .OR. &
                    trim(cname)=='ACSWDNT'  .OR. &
                    trim(cname)=='ACSWDNTC' .OR. &
                    trim(cname)=='ACSWUPB'  .OR. &
                    trim(cname)=='ACSWUPBC' .OR. &
                    trim(cname)=='ACSWDNB'  .OR. &
                    trim(cname)=='ACSWDNBC' .OR. &
                    trim(cname)=='ACLWUPT'  .OR. &
                    trim(cname)=='ACLWUPTC' .OR. &
                    trim(cname)=='ACLWDNT'  .OR. &
                    trim(cname)=='ACLWDNTC' .OR. &
                    trim(cname)=='ACLWUPB'  .OR. &
                    trim(cname)=='ACLWUPBC' .OR. &
                    trim(cname)=='ACLWDNB'  .OR. &
                    trim(cname)=='ACLWDNBC' ) ) THEN
                dummy2 = "I_"//trim(cname)
                CALL get_keep_array ( file_time , good_to_go, "TMP_ARRAY", dummy2 )
                SCR(:,:,1) = SCR(:,:,1) + TMP_ARRAY(:,:)*bucket_J
                IF ( debug_level >= 500 ) print*,"Adding bucket values to ",trim(cname)
              END IF
              IF ( bucket_mm > 0 .AND. (trim(cname)=='RAINNC' .OR. trim(cname)=='RAINC') ) THEN
                dummy2 = "I_"//trim(cname)
                CALL get_keep_array ( file_time , good_to_go, "TMP_ARRAY", dummy2 )
                SCR(:,:,1) = SCR(:,:,1) + TMP_ARRAY(:,:)*bucket_mm
                IF ( debug_level >= 500 ) print*,"Adding bucket values to ",trim(cname)
              END IF
            
              CALL interp( SCR, domain_end(1), domain_end(2), domain_end(3), &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
                           
            ! Write the fields we want out to the .dat file, also keeps a list of what is written out
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
              
            ENDIF

        ENDIF    !! end "istatus==0"

      END DO process_all_fields


      IF ( debug_level .ge. 300 ) THEN
        print*," "
        print*,"DEBUG: Done with fields in input file - see if we need diagnostics"
      ENDIF
      IF ( (INDEX(plot,'list') /= 0 .OR. INDEX(plot,'file') /= 0) .AND. &
           iprogram /= 1 ) THEN
!! Do we have any DIAGNOSTICS to process?
        CALL process_diagnostics ( file_time )
      END IF


!! We are DONE for this time




   !! print a list of the requested fields we could not find
   IF ( len_trim(could_not_find) > 1 ) THEN
     CALL mprintf(.true.,STDOUT, '   ')
     CALL mprintf(.true.,STDOUT, '   WARNING: The following requested fields could not be found ' )

     could_not_find = could_not_find(2:len_trim(could_not_find))
     is_there = INDEX(could_not_find,",")
     DO WHILE ( is_there > 1 ) 
       PRINT*,"           ", could_not_find(1:is_there-1)
       could_not_find = could_not_find(is_there+1:len_trim(could_not_find))
       is_there = INDEX(could_not_find,",")
     END DO
   
   CALL mprintf(.true.,STDOUT, '   ')
   ENDIF
    

   CALL clobber_arrays


   END SUBROUTINE get_fields


END MODULE process_domain_module
