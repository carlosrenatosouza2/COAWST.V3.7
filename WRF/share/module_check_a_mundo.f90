























   MODULE module_check_a_mundo


























      USE module_state_description
      USE module_model_constants
      USE module_wrf_error
      USE module_configure

      IMPLICIT NONE



   CONTAINS



   SUBROUTINE check_nml_consistency







      IMPLICIT NONE

      LOGICAL :: exists, vnest
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      INTEGER :: i, j, oops, d1_value, EDMFMAX, SCHUMAX
      INTEGER :: id, factor
      LOGICAL :: km_opt_already_done , diff_opt_already_done
      INTEGER :: count_opt
      LOGICAL :: lon_extent_is_global , lat_extent_is_global
      LOGICAL :: rinblw_already_done
      LOGICAL :: fsbm_table1_exists, fsbm_table2_exists
      INTEGER :: count_fatal_error
      INTEGER :: len1, len2, len_loop

      
      
      

      INTERFACE
         INTEGER FUNCTION bep_nurbm()
         END FUNCTION bep_nurbm

         INTEGER FUNCTION bep_ndm()
         END FUNCTION bep_ndm

         INTEGER FUNCTION bep_nz_um()
         END FUNCTION bep_nz_um

         INTEGER FUNCTION bep_ng_u()
         END FUNCTION bep_ng_u

         INTEGER FUNCTION bep_nwr_u()
         END FUNCTION bep_nwr_u

         INTEGER FUNCTION bep_bem_nurbm()
         END FUNCTION bep_bem_nurbm

         INTEGER FUNCTION bep_bem_ndm()
         END FUNCTION bep_bem_ndm

         INTEGER FUNCTION bep_bem_nz_um()
         END FUNCTION bep_bem_nz_um

         INTEGER FUNCTION bep_bem_ng_u()
         END FUNCTION bep_bem_ng_u

         INTEGER FUNCTION bep_bem_nwr_u()
         END FUNCTION bep_bem_nwr_u

         INTEGER FUNCTION bep_bem_nf_u()
         END FUNCTION bep_bem_nf_u

         INTEGER FUNCTION bep_bem_ngb_u()
         END FUNCTION bep_bem_ngb_u

         INTEGER FUNCTION bep_bem_nbui_max() 
         END FUNCTION bep_bem_nbui_max
      END INTERFACE






   count_fatal_error = 0
   model_config_rec % wrf_hydro = 0







  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_diag_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_diag_opt(:)   = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_ptype_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_ptype_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_vil_opt(i)    .EQ. 1 ) then
      model_config_rec%afwa_vil_opt(:)    = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_radar_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_radar_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_severe_opt(i) .EQ. 1 ) then
      model_config_rec%afwa_severe_opt(:) = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_icing_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_icing_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_cloud_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_cloud_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_vis_opt(i)    .EQ. 1 ) then
      model_config_rec%afwa_vis_opt(:)    = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_therm_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_therm_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_turb_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_turb_opt(:)   = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_buoy_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_buoy_opt(:)   = 1
      exit
    endif
  enddo





  do i=1,model_config_rec%max_dom
    if ( ( model_config_rec%afwa_ptype_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_vil_opt(i)    .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_radar_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_severe_opt(i) .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_icing_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_cloud_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_vis_opt(i)    .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_therm_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_turb_opt(i)   .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_buoy_opt(i)   .EQ. 1 ) ) then
      model_config_rec%afwa_diag_opt(i)=1
    endif
  enddo




   model_config_rec%nested(1)    = .FALSE.
   DO i=2,model_config_rec%max_dom
      model_config_rec%nested(i) = .TRUE.
   END DO 




   DO i=2,model_config_rec%max_dom
      model_config_rec%periodic_x(i)   = .FALSE.
      model_config_rec%symmetric_xs(i) = .FALSE.
      model_config_rec%symmetric_xe(i) = .FALSE.
      model_config_rec%open_xs(i)      = .FALSE.
      model_config_rec%open_xe(i)      = .FALSE.
      model_config_rec%periodic_y(i)   = .FALSE.
      model_config_rec%symmetric_ys(i) = .FALSE.
      model_config_rec%symmetric_ye(i) = .FALSE.
      model_config_rec%open_ys(i)      = .FALSE.
      model_config_rec%open_ye(i)      = .FALSE.
      model_config_rec%polar(i)        = .FALSE.
      model_config_rec%specified(i)    = .FALSE.
   END DO 




   IF ( model_config_rec%specified(1) ) THEN
      model_config_rec%spec_zone = 1
      model_config_rec%relax_zone = model_config_rec%spec_bdy_width - model_config_rec%spec_zone
   END IF









      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) THEN
            WRITE(wrf_err_message,FMT='(A,I2,A)') 'Domain #',i,': grid turned OFF'
            CALL wrf_debug ( 0, wrf_err_message )
            CYCLE
         END IF
         id = i
         factor = 1
         call get_moad_factor ( id,  model_config_rec % parent_id,  &
                                model_config_rec % parent_grid_ratio,  &
                                model_config_rec % max_dom, factor )
         model_config_rec % dx(i) = model_config_rec % dx(1) / REAL(factor)
         model_config_rec % dy(i) = model_config_rec % dy(1) / REAL(factor)
         WRITE(wrf_err_message,FMT='(A,I2,A,F9.3,A)') 'Domain #',i,': dx = ',model_config_rec % dx(i),' m'
         CALL wrf_debug ( 0, wrf_err_message )
      END DO










      km_opt_already_done = .FALSE.
      diff_opt_already_done = .FALSE.
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. -1 ) THEN
            model_config_rec % km_opt(i) = model_config_rec % km_opt(1)
            IF ( .NOT. km_opt_already_done ) THEN
               wrf_err_message = 'Setting blank km_opt entries to domain #1 values.'
               CALL wrf_debug ( 1, wrf_err_message )
               wrf_err_message = ' --> The km_opt entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, wrf_err_message )
            END IF
            km_opt_already_done = .TRUE.
         END IF
         IF ( model_config_rec % diff_opt(i) .EQ. -1 ) THEN
            model_config_rec % diff_opt(i) = model_config_rec % diff_opt(1)
            IF ( .NOT. diff_opt_already_done ) THEN
               wrf_err_message = 'Setting blank diff_opt entries to domain #1 values.'
               CALL wrf_debug ( 1, wrf_err_message )
               wrf_err_message = ' --> The diff_opt entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, wrf_err_message )
            END IF
            diff_opt_already_done = .TRUE.
         END IF
      ENDDO







      IF ( ( model_config_rec %   km_opt(1) .EQ. -1 ) .OR. &
           ( model_config_rec % diff_opt(1) .EQ. -1 ) ) THEN
            wrf_err_message = '--- ERROR: Both km_opt and diff_opt need to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              model_config_rec % diff_opt(i) .NE. 2  ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme can only work with diff_opt=2 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or diff_opt in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO




      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              model_config_rec % bl_pbl_physics(i) .NE. 0  ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme can only work with bl_pbl_physics=0 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or bl_pbl_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO







      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              (model_config_rec % sf_sfclay_physics(i) .NE. nosfcscheme     .AND. &
               model_config_rec % sf_sfclay_physics(i) .NE. sfclayscheme    .AND. &
               model_config_rec % sf_sfclay_physics(i) .NE. sfclayrevscheme .AND. &
               model_config_rec % sf_sfclay_physics(i) .NE. mynnsfcscheme  ) ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme works with sf_sfclay_physics = 0,1,5,91 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or sf_sfclay_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_urban_physics(i) == bepscheme     ) THEN
           model_config_rec % num_urban_ndm  = bep_ndm()
           model_config_rec % num_urban_nz   = bep_nz_um()
           model_config_rec % num_urban_ng   = bep_ng_u()
           model_config_rec % num_urban_nwr  = bep_nwr_u()
         END IF
         IF ( model_config_rec % sf_urban_physics(i) == bep_bemscheme ) THEN 
           model_config_rec % num_urban_ndm  = bep_bem_ndm()
           model_config_rec % num_urban_nz   = bep_bem_nz_um()
           model_config_rec % num_urban_ng   = bep_bem_ng_u()
           model_config_rec % num_urban_nwr  = bep_bem_nwr_u()
           model_config_rec % num_urban_nf   = bep_bem_nf_u()
           model_config_rec % num_urban_ngb  = bep_bem_ngb_u()
           model_config_rec % num_urban_nbui = bep_bem_nbui_max()
         END IF
      ENDDO




      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_surface_mosaic .EQ. 1 .AND. &
              (model_config_rec % sf_urban_physics(i) .EQ. 2 .OR. &
               model_config_rec % sf_urban_physics(i) .EQ. 3 ) ) THEN
            wrf_err_message = '--- ERROR: mosaic option cannot work with urban options 2 and 3 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix sf_surface_mosaic and sf_urban_physics in namelist.input.'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Either: use Noah LSM without the mosaic option, OR change the urban option to 1 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO




      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_surface_physics(i) .NE. LSMSCHEME .AND.  &
             model_config_rec % sf_surf_irr_scheme(i) .EQ. CHANNEL ) THEN
              wrf_err_message = '--- ERROR: irrigation Opt 1 works only with Noah-LSM'
              CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO








      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( ( model_config_rec % sf_surf_irr_scheme(i) .EQ. CHANNEL   )   .OR. &
                ( model_config_rec % sf_surf_irr_scheme(i) .EQ. SPRINKLER )   .OR. &
                ( model_config_rec % sf_surf_irr_scheme(i) .EQ. DRIP      ) ) .AND. &
              ( model_config_rec % irr_num_hours(i) .LE. 0 ) ) THEN
            oops = oops + 1
         END IF
      ENDDO
      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: irr_num_hours must be greater than zero to work with irrigation'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_surf_irr_scheme(i) .EQ. CHANNEL   ) THEN
            model_config_rec % sf_surf_irr_alloc = CHANNEL
         END IF
         IF ( model_config_rec % sf_surf_irr_scheme(i) .EQ. SPRINKLER ) THEN
            model_config_rec % sf_surf_irr_alloc = SPRINKLER
         END IF
         IF ( model_config_rec % sf_surf_irr_scheme(i) .EQ. DRIP      ) THEN
            model_config_rec % sf_surf_irr_alloc = DRIP    
         END IF
      ENDDO




      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % shcu_physics(i) == dengshcuscheme .AND. &
              (model_config_rec % bl_pbl_physics(i) /= myjpblscheme .AND. &
               model_config_rec % bl_pbl_physics(i) /= mynnpblscheme2 ) ) THEN
            wrf_err_message = '--- ERROR: Deng shallow convection can only work with MYJ or MYNN (with bl_mynn_edmf off) PBL '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix shcu_physics or bl_pbl_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO




      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%shcu_physics(i) .EQ. dengshcuscheme ) .AND. &
              ( model_config_rec%icloud .EQ. 3 ) ) THEN
              oops = oops + 1
         END IF
      ENDDO

      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: Options shcu_physics = 5 and icloud = 3 should not be used together'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- ERROR: Choose either one in namelist.input and rerun the model'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%cu_physics(i) .EQ. scalesasscheme ) THEN
              oops = oops + 1
         END IF
      ENDDO

      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: Option cu_physics = 4 should not be used for ARW; cu_physics = 95 is suggested'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- ERROR: Choose a different cu_physics option in the namelist.input file'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF








      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( (model_config_rec % ra_lw_physics(i) == goddardlwscheme .OR. &
                 model_config_rec % ra_sw_physics(i) == goddardswscheme) .AND. &
                 model_config_rec % mp_physics(i) /= nuwrf4icescheme ) .OR. &
              (  model_config_rec % mp_physics(i) == nuwrf4icescheme .AND. &
                (model_config_rec % ra_lw_physics(i) /= goddardlwscheme .AND. &
                 model_config_rec % ra_sw_physics(i) /= goddardswscheme) ) ) THEN
            wrf_err_message = '--- WARNING: Goddard radiation and Goddard 4ice microphysics are not used together'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- WARNING: These options may be best to use together.'
            CALL wrf_message ( wrf_err_message )
         END IF
      ENDDO










      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_surface_physics(i)     .NE. &
              model_config_rec % sf_surface_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_surface_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_surface_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sf_sfclay_physics(i)     .NE. &
              model_config_rec % sf_sfclay_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_sfclay_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_sfclay_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % mp_physics(i)     .NE. &
              model_config_rec % mp_physics(1) ) THEN
            wrf_err_message = '--- NOTE: mp_physics must be equal for all domains '
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE:     ----> Setting all mp_physics entries to value defined in the inner most domain'
            CALL wrf_debug ( 1, wrf_err_message )
         END IF
      ENDDO
      d1_value = model_config_rec%mp_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%mp_physics(i) = d1_value
      END DO




      IF ( model_config_rec % mp_physics(1) .EQ. FAST_KHAIN_LYNN_SHPUND ) THEN
         INQUIRE(FILE='./SBM_input_33/BLKD_SDC.dat', EXIST=fsbm_table1_exists)
         IF (.not.fsbm_table1_exists ) THEN
            wrf_err_message = "--- ERROR: Input directory SBM_input_33 doesn't exist !!!"
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Download this directory of table files from http://www2.mmm.ucar.edu/wrf/src/wrf_files/'
             CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
         INQUIRE(FILE='./scattering_tables_2layer_high_quad_1dT_1%fw_110/GRAUPEL_+00C_000fvw.sct', EXIST=fsbm_table2_exists)
         IF (.not.fsbm_table2_exists ) THEN
            wrf_err_message = "--- ERROR: Input directory scattering_tables_2layer_high_quad_1dT_1%fw_110 doesn't exist !!!"
            CALL wrf_message ( TRIM( wrf_err_message ) )
            wrf_err_message = '--- ERROR: Download this directory of input table files from http://www2.mmm.ucar.edu/wrf/src/wrf_files/'
            CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
      END IF





      IF ( model_config_rec%afwa_diag_opt(1) .EQ. 1 ) THEN
         IF ( ( model_config_rec % mp_physics(1) .EQ. GSFCGCESCHEME   ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. ETAMPNEW        ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. THOMPSON        ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WSM5SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WSM6SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WDM6SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. MORR_TWO_MOMENT ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. MORR_TM_AERO    ) ) THEN 
            
         ELSE
            wrf_err_message = '--- WARNING: the AFWA diagnostics option knows only about the following MP schemes:'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- GSFCGCESCHEME, ETAMPNEW, THOMPSON, WSM5SCHEME, WSM6SCHEME, MORR_TWO_MOMENT, MORR_TM_AERO, WDM6SCHEME'
            CALL wrf_message ( wrf_err_message )
         END IF
      END IF






      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % ra_lw_physics(i)     .NE. &
              model_config_rec % ra_lw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_lw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % ra_sw_physics(i)     .NE. &
              model_config_rec % ra_sw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_sw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_sw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO






         IF ( ( model_config_rec % use_wps_input == 0 ) .AND. &
              ( model_config_rec % time_step .EQ. -1 ) ) THEN

            wrf_err_message = '--- ERROR: Known problem.  time_step must be set to a positive integer'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1

         END IF





      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. model_config_rec % bl_pbl_physics(1) ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. 0                                    ) ) THEN
            wrf_err_message = '--- ERROR: bl_pbl_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix bl_pbl_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO







      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % cu_physics(i) .NE. model_config_rec % cu_physics(1) ) .AND. &
              ( model_config_rec % cu_physics(i) .NE. 0                                ) ) THEN
            wrf_err_message = '--- ERROR: cu_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix cu_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO







      IF ( ( model_config_rec%fractional_seaice .EQ. 0 ).AND. &
              ( model_config_rec%tice2tsk_if2cold ) ) THEN
            wrf_err_message = '--- WARNING: You set tice2tsk_if2cold = .true.,  but fractional_seaice = 0'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- WARNING: tice2tsk_if2cold will have no effect on results.'
            CALL wrf_debug ( 1, wrf_err_message )
      END IF





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%fine_input_stream(i) .NE. 0 ).AND. &
              ( model_config_rec%io_form_auxinput2 .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If fine_input_stream /= 0, io_form_auxinput2 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput2 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO





            IF  ( model_config_rec%num_metgrid_levels .LE. 20 ) THEN
            wrf_err_message = 'Linear vertical interpolation is recommended with input vertical resolution this coarse, changing lagrange_order to 1' 
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec%lagrange_order = 1
            END IF





      d1_value = model_config_rec%sf_urban_physics(1)
      DO i = 2, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%sf_urban_physics(i) /= d1_value ) THEN
            wrf_err_message = '--- NOTE:   sf_urban_physics option must be identical in each domain'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE:   ----> Resetting namelist values to that defined on the inner most domain'
            CALL wrf_debug ( 1, wrf_err_message )
         ENDIF
      END DO
      d1_value = model_config_rec%sf_urban_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         model_config_rec%sf_urban_physics(i) = d1_value
      END DO




      IF ( model_config_rec%seaice_albedo_opt == 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( ( model_config_rec%sf_surface_physics(i) /= LSMSCHEME ) .AND. &
                 ( model_config_rec%sf_surface_physics(i) /= NOAHMPSCHEME ) ) THEN

               write (wrf_err_message, '(" --- ERROR:   seaice_albedo_opt == 1 works only with ")')
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               write (wrf_err_message, '("              sf_surface_physics == ", I2, " (Noah) or ", I2, " (Noah-MP).")') &
               LSMSCHEME, NOAHMPSCHEME
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1

            END IF
            
         END DO

      END IF







      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%shcu_physics(i) == nscvshcuscheme .AND. model_config_rec%cu_physics(i) == nsasscheme) THEN
            WRITE(wrf_err_message, '(" --- ERROR: NSCV shallow convection scheme is already included in NSAS ")')
            CALL wrf_message ( TRIM ( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END DO







   IF ( model_config_rec%bucket_mm .GT. 0. ) THEN
      model_config_rec%bucketr_opt = 1
   END IF






   IF ( model_config_rec%bucket_J .GT. 0. ) THEN
      model_config_rec%bucketf_opt = 1
   END IF






   DO i = 1, model_config_rec % max_dom
      IF ( model_config_rec%prec_acc_dt(i) .GT. 0. ) THEN
         model_config_rec%prec_acc_opt = 1
      END IF
   END DO






   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % sppt(i) .ne. 0)  then
           model_config_rec % sppt_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc. are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for SPPT'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % rand_perturb(i) .ne. 0)  then
           model_config_rec % rand_perturb_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF (( model_config_rec % spp_conv(i) .ne. 0).or.( model_config_rec % spp_pbl(i) .ne. 0).or. (model_config_rec % spp_lsm(i) .ne. 0)  &
           .or. ( model_config_rec % spp(i) .ne. 0))  then
           model_config_rec % spp_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
         IF ( model_config_rec % spp(i) .ne. 0)  then
           model_config_rec % spp_conv=1
           model_config_rec % spp_pbl=1
           model_config_rec % spp_lsm=1
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % stoch_vertstruc_opt(i) ==1 )  then
           model_config_rec % skebs_vertstruc=1       
                                                      
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_vertstruc_opt" is obsolete.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
           wrf_err_message = '             Please replace with namelist parameter "skebs_vertstruc" in V3.7 and later versions.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         endif
   ENDDO

   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % stoch_force_opt(i) ==1 )  THEN
           model_config_rec % skebs(i)=1    
                                            
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_force_opt" is obsolete.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
           wrf_err_message = '             Please replace with namelist parameter "skebs" in V3.7 and later versions.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         ENDIF
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % skebs(i) .ne. 0)  then
           model_config_rec % skebs_on=1
         endif
   ENDDO






   IF ( model_config_rec % skebs_vertstruc     .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % sppt_vertstruc      .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % rand_pert_vertstruc .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF





   IF ( model_config_rec % perturb_bdy .EQ. 1 ) then
        model_config_rec % skebs_on=1
         wrf_err_message = '--- WARNING: perturb_bdy=1 option uses SKEBS pattern and may'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         wrf_err_message = '             increase computation time.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
   ENDIF








   IF ( model_config_rec % perturb_chem_bdy .EQ. 1 ) then

      wrf_err_message = '--- ERROR: This option is only for WRF_CHEM.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1


         wrf_err_message = '--- WARNING: perturb_chem_bdy=1 option uses RAND pattern and may'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         wrf_err_message = '             increase computation time.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )


   ENDIF





   IF ( ( model_config_rec%traj_opt .EQ. 0 ) .AND. &
        ( model_config_rec%num_traj .NE. 0 ) ) THEN
         wrf_err_message = '--- WARNING: traj_opt is zero, but num_traj is not zero; setting num_traj to zero.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         model_config_rec%num_traj = 0
   END IF











   IF      ( model_config_rec%multi_bdy_files ) THEN
      IF ( INDEX ( TRIM(model_config_rec%bdy_inname) , "_<date>" ) .GT. 0 ) THEN
         
      ELSE
         wrf_err_message = '--- ERROR: Need bdy_inname = "wrfbdy_d<domain>_<date>"'
         CALL wrf_debug ( 0, TRIM(wrf_err_message) )
         count_fatal_error = count_fatal_error + 1



      END IF
   ELSE IF ( .NOT. model_config_rec%multi_bdy_files ) THEN
      IF ( INDEX ( TRIM(model_config_rec%bdy_inname) , "_<date>" ) .EQ. 0 ) THEN
         
      ELSE
         wrf_err_message = '--- ERROR: Remove bdy_inname = "wrfbdy_d<domain>_<date>"'
         CALL wrf_debug ( 0, TRIM(wrf_err_message) )
         count_fatal_error = count_fatal_error + 1





      END IF
   END IF





      IF ( model_config_rec%hypsometric_opt .EQ. 2 &
           .AND. model_config_rec%adjust_heights ) THEN
         wrf_err_message = '--- NOTE: hypsometric_opt is 2, setting adjust_heights = F'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%adjust_heights = .false.
      ENDIF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%km_opt(i) .EQ. SMS_3DTKE ) .AND. &
              ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) ) THEN
            oops = oops + 1
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: cu_physics = 11 cannot work with 3DTKE scheme '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Choose another bl_pbl_physics OR use another cu_physics option '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF
      





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) THEN
            wrf_err_message = '--- NOTE: cu_physics is 11, setting icloud = 1 and cu_rad_feedback = T'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%cu_rad_feedback(i) = .true.
            model_config_rec%icloud = 1
         END IF
      ENDDO
      




      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%aercu_opt .GT. 0 .AND.       &
              ( model_config_rec%cu_physics(i) .NE. MSKFSCHEME .OR. &
              model_config_rec%mp_physics(i) .NE. MORR_TM_AERO ) ) THEN
              oops = oops + 1
         END IF
      ENDDO

      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: aercu_opt requires cu_physics = 11, and mp_physics = 40 '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Fix these options in namelist.input if you would like to use aercu_opt'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF





      IF ( model_config_rec % aercu_opt .GT. 0 ) THEN
         model_config_rec % alevsiz_cu = 30
         model_config_rec % no_src_types_cu = 10
         DO i = 1, model_config_rec % max_dom
            model_config_rec % scalar_pblmix(i) = 1
         END DO

         wrf_err_message = '--- NOTE: aercu_opt is in use, setting:  ' // &
                           'alevsiz_cu=30, no_src_types_cu=10, scalar_pblmix = 1'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

      END IF







      IF ( model_config_rec%sst_update .EQ. 0 ) THEN
         model_config_rec%io_form_auxinput4 = 0
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            wrf_err_message = '--- NOTE: sst_update is 0, ' // &
                  'setting io_form_auxinput4 = 0 and auxinput4_interval = 0 for all domains'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%auxinput4_interval(i)   = 0
            model_config_rec%auxinput4_interval_y(i) = 0
            model_config_rec%auxinput4_interval_d(i) = 0
            model_config_rec%auxinput4_interval_h(i) = 0
            model_config_rec%auxinput4_interval_m(i) = 0
            model_config_rec%auxinput4_interval_s(i) = 0
         ENDDO
      ELSE
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF







      IF ( model_config_rec%sst_update .EQ. 1 ) THEN
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF

         IF ( ( model_config_rec%auxinput4_interval(1)   .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_y(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_d(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_h(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_m(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_s(1) .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, one of the auxinput4_interval settings must be /= 0'
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            wrf_err_message = '--- Set auxinput4_interval_s to the same value as interval_seconds (usually a pretty good guess).'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      END IF






      model_config_rec%alloc_qndropsource = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%progn(i) .EQ. 1 ) THEN
            model_config_rec%alloc_qndropsource = 1
         END IF
      END DO






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%grid_sfdda(i) .GT. 0 ).AND. &
              ( model_config_rec%grid_fdda (i) .NE. 1 ) ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda >= 1, then grid_fdda must also = 1 for that domain '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Change grid_fdda or grid_sfdda in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO









      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE

         IF ( model_config_rec%grid_fdda(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: grid_fdda is 0 for domain ', &
                         i, ', setting gfdda interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%gfdda_end_y(i) = 0
            model_config_rec%gfdda_end_d(i) = 0
            model_config_rec%gfdda_end_h(i) = 0
            model_config_rec%gfdda_end_m(i) = 0
            model_config_rec%gfdda_end_s(i) = 0
            model_config_rec%gfdda_interval(i)   = 0
            model_config_rec%gfdda_interval_y(i) = 0
            model_config_rec%gfdda_interval_d(i) = 0
            model_config_rec%gfdda_interval_h(i) = 0
            model_config_rec%gfdda_interval_m(i) = 0
            model_config_rec%gfdda_interval_s(i) = 0
         END IF

         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 0 ) .AND. &
              ( model_config_rec%pxlsm_soil_nudge(i) .EQ. 0 ) ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') &
                         '--- NOTE: both grid_sfdda and pxlsm_soil_nudge are 0 for domain ', &
                         i, ', setting sgfdda interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%sgfdda_end_y(i) = 0
            model_config_rec%sgfdda_end_d(i) = 0
            model_config_rec%sgfdda_end_h(i) = 0
            model_config_rec%sgfdda_end_m(i) = 0
            model_config_rec%sgfdda_end_s(i) = 0
            model_config_rec%sgfdda_interval(i)   = 0
            model_config_rec%sgfdda_interval_y(i) = 0
            model_config_rec%sgfdda_interval_d(i) = 0
            model_config_rec%sgfdda_interval_h(i) = 0
            model_config_rec%sgfdda_interval_m(i) = 0
            model_config_rec%sgfdda_interval_s(i) = 0
         END IF

         IF ( model_config_rec%obs_nudge_opt(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: obs_nudge_opt is 0 for domain ', &
                         i, ', setting obs nudging interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%fdda_end(i) = 0
            model_config_rec%auxinput11_interval(i)   = 0
            model_config_rec%auxinput11_interval_y(i) = 0
            model_config_rec%auxinput11_interval_d(i) = 0
            model_config_rec%auxinput11_interval_h(i) = 0
            model_config_rec%auxinput11_interval_m(i) = 0
            model_config_rec%auxinput11_interval_s(i) = 0
            model_config_rec%auxinput11_end(i)   = 0
            model_config_rec%auxinput11_end_y(i) = 0
            model_config_rec%auxinput11_end_d(i) = 0
            model_config_rec%auxinput11_end_h(i) = 0
            model_config_rec%auxinput11_end_m(i) = 0
            model_config_rec%auxinput11_end_s(i) = 0
         END IF

      ENDDO      





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         model_config_rec%fasdas(i) = 0
         IF ( model_config_rec%grid_sfdda(i) .EQ. 2 ) THEN
            model_config_rec%fasdas(i) = 1
         END IF
      ENDDO




    rinblw_already_done = .FALSE.
    DO j = 1, model_config_rec%max_dom
    IF ( .NOT. model_config_rec % grid_allowed(j) ) CYCLE
    IF (model_config_rec%grid_sfdda(j) .EQ. 1 ) THEN
      DO i = 2, model_config_rec%max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%rinblw(i) .EQ. -1 ) THEN
            model_config_rec%rinblw(i) = model_config_rec % rinblw(1)
            IF ( .NOT. rinblw_already_done ) THEN
               wrf_err_message = 'Setting blank rinblw entries to domain #1 values.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
               wrf_err_message = ' --> The rinblw entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            END IF
            rinblw_already_done = .TRUE.
         END IF
       ENDDO




       IF ( model_config_rec%rinblw(1) .EQ. -1 ) THEN
            wrf_err_message = '--- ERROR: rinblw needs to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
       END IF
    END IF
    END DO




    DO i = 1, model_config_rec%max_dom
     IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
        wrf_err_message = 'FASDAS is active. Mixed Layer fdda is inactive'
        CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
     END IF











     END DO







      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. QNSEPBLSCHEME ) .AND. &
              ( model_config_rec%mfshconv(i) .NE. 0 ) ) THEN
            model_config_rec%mfshconv(i) = 0
            oops = oops + 1
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 4, implies mfshconv must be 0, resetting'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%shcu_physics(i) .EQ. GRIMSSHCUSCHEME ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. SHINHONGSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
               
            ELSE
               model_config_rec%shcu_physics(i) = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6,11 implies shcu_physics cannot be 3, resetting'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME2 ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME3 ) ) THEN
              model_config_rec % bl_mynn_edmf(i) = 0
              model_config_rec % bl_mynn_output(i) = 0
         END IF
      ENDDO





      oops = 0
      EDMFMAX = MAXVAL(model_config_rec%bl_mynn_edmf(1:model_config_rec%max_dom))
      SCHUMAX = MAXVAL(model_config_rec%shcu_physics(1:model_config_rec%max_dom))
         IF ( ( ( EDMFMAX .GT. 0 ) .AND. ( SCHUMAX .GT. 0 ) ) .OR. &
              ( ( EDMFMAX .GT. 0 ) .AND. ( model_config_rec%ishallow .GT. 0 ) ) ) THEN
            wrf_err_message = '--- ERROR: bl_mynn_edmf > 0 requires both shcu_physics=0 & ishallow=0' 
            CALL wrf_message(wrf_err_message)
            wrf_err_message = 'when using MYNN PBL, by default bl_mynn_edmf is turned on'
            CALL wrf_message(wrf_err_message)
            wrf_err_message = 'Modify namelist.input so that shcu_physics nor ishallow is used when bl_mynn_edmf is turned on'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%icloud_bl .eq. 1) THEN
           IF ( model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2 .OR. &
                model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3 ) THEN
              
           ELSE
              model_config_rec%icloud_bl = 0
              oops = oops + 1
           END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'Need MYNN PBL for icloud_bl = 1, resetting to 0'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF






      model_config_rec%cu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%cu_physics(i) .NE. NOCUSCHEME ) THEN
            model_config_rec%cu_used = 1
         END IF
      ENDDO






      model_config_rec%shcu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%shcu_physics(i) .NE. NOSHCUSCHEME ) THEN
            model_config_rec%shcu_used = 1
         END IF
      ENDDO






      model_config_rec%gwd_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%gwd_opt(i) .NE. NOGWDOPT ) THEN
            model_config_rec%gwd_used = 1
         END IF
      ENDDO





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%icloud .eq. 3) THEN
           IF ( model_config_rec%mp_physics(i) .EQ. WSM3SCHEME .OR. &
                model_config_rec%mp_physics(i) .EQ. KESSLERSCHEME ) THEN
                oops = oops + 1
           END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: Need microphysics schemes with QICE array for icloud = 3'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Choose a microphysics scheme other than WSM3 and Kessler'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF






      IF ( MAXVAL( model_config_rec%grid_fdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_gfdda = 0
      ELSE
         IF ( model_config_rec%io_form_gfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_fdda /= 0, io_form_gfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_gfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF
      IF ( MAXVAL( model_config_rec%grid_sfdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_sgfdda = 0
      ELSE
         IF ( model_config_rec%io_form_sgfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda /= 0, io_form_sgfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_sgfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF





      IF ( model_config_rec%p_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( ( MAX ( model_config_rec%auxhist23_interval  (i) , &
                         model_config_rec%auxhist23_interval_d(i) , &
                         model_config_rec%auxhist23_interval_h(i) , &
                         model_config_rec%auxhist23_interval_m(i) , &
                         model_config_rec%auxhist23_interval_s(i) ) == 0 ) .OR. &
                 (  model_config_rec%io_form_auxhist23 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: p_lev_diags requires auxhist23 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist23_interval (max_dom) and io_form_auxhist23'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 23 for pressure-level diags'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            model_config_rec%p_lev_interval(i) = model_config_rec%auxhist23_interval  (i)*   60 + &
                                                 model_config_rec%auxhist23_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist23_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist23_interval_m(i)*   60 + &
                                                 model_config_rec%auxhist23_interval_s(i)
         END DO
      END IF






      IF ( model_config_rec%z_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( ( MAX ( model_config_rec%auxhist22_interval  (i) , &
                         model_config_rec%auxhist22_interval_d(i) , &
                         model_config_rec%auxhist22_interval_h(i) , &
                         model_config_rec%auxhist22_interval_m(i) , &
                         model_config_rec%auxhist22_interval_s(i) ) == 0 ) .OR. &
                 (  model_config_rec%io_form_auxhist22 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: z_lev_diags requires auxhist22 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist22_interval (max_dom) and io_form_auxhist22'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 22 for height-level diags'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            model_config_rec%z_lev_interval(i) = model_config_rec%auxhist22_interval  (i)*   60 + &
                                                 model_config_rec%auxhist22_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist22_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist22_interval_m(i)*   60 + &
                                                 model_config_rec%auxhist22_interval_s(i)
         END DO
      END IF










      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         count_opt = 0
         IF ( model_config_rec%mean_diag_interval_s (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_m (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_h (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_d (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_mo(i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval   (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( count_opt .GT. 1 ) THEN
            wrf_err_message = '--- ERROR:  Only use one of: mean_diag_interval, _s, _m, _h, _d, _mo '
            CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
      END DO



      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%mean_diag_interval_s (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_s (i)
            model_config_rec%mean_freq = 1
         END IF
         IF ( model_config_rec%mean_diag_interval_m (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_m (i)
            model_config_rec%mean_freq = 2
         END IF
         IF ( model_config_rec%mean_diag_interval_h (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_h (i)
            model_config_rec%mean_freq = 3
         END IF
         IF ( model_config_rec%mean_diag_interval_d (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_d (i)
            model_config_rec%mean_freq = 4
         END IF
         IF ( model_config_rec%mean_diag_interval_mo(i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_mo(i)
            model_config_rec%mean_freq = 5
         END IF
         IF ( model_config_rec%mean_diag_interval   (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval   (i)
            model_config_rec%mean_freq = 2
         END IF
      END DO



      IF ( model_config_rec%mean_diag .EQ. 1 ) THEN
         count_opt = 0
         DO i = 1, model_config_rec % max_dom
            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( model_config_rec%mean_interval   (i) .GT. 0 ) THEN
               count_opt = count_opt + 1
            END IF
         END DO
         IF ( count_opt .LT. 1 ) THEN
            wrf_err_message = '--- ERROR:  mean_diag = 1, but no computation interval given'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '            Use one of: mean_diag_interval, _s, _m, _h, _d, _mo '
            CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
      END IF





      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%history_interval(1) .EQ. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  nwp_diagnostics requires the use of "history_interval" namelist.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace interval variable with "history_interval".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF






      IF ( model_config_rec % nwp_diagnostics == 1 ) model_config_rec % do_radar_ref = 1





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%hailcast_opt(i) .NE. 0 ) .AND. &
              (model_config_rec%cu_physics(i) .NE. 0) ) THEN
              wrf_err_message = '--- hailcast_opt and cu_physics cannot both be turned on for the same domain. You must turn one of them off (=0).'
              CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
              count_fatal_error = count_fatal_error + 1
         ENDIF
      ENDDO








      IF ( model_config_rec%omlcall .NE. 0 ) THEN
         wrf_err_message = '--- ERROR:  The namelist.input variable "omlcall" has been renamed.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace "omlcall" with the new name "sf_ocean_physics".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF







      IF ( model_config_rec%use_adaptive_time_step ) THEN
         IF ( ( model_config_rec%cu_physics(1) .EQ. BMJSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SCALESASSCHEME) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SASSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. OSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. KSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. NSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. TIEDTKESCHEME ) ) THEN
            wrf_err_message = '--- WARNING: If use_adaptive_time_step, must use cudt=0 for the following CU schemes:'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '---          BMJ, all SAS, Tiedtke'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '---          CUDT=0 has been done for you.'
            CALL wrf_debug ( 1, wrf_err_message )
            DO i = 1, model_config_rec % max_dom
               model_config_rec%cudt(i) = 0
            END DO
         END IF
      END IF







      IF ( .NOT. model_config_rec%dfi_opt .EQ. DFI_NODFI ) THEN
         IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
            model_config_rec%time_step_dfi = model_config_rec%time_step
            IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
               wrf_err_message = '--- ERROR: DFI Timestep or standard WRF time step must be specified.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END IF
      END IF






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%cu_rad_feedback(i) .EQV. .TRUE. )  .OR. &
              ( model_config_rec%cu_rad_feedback(i) .EQV. .true. ) ) THEN
            IF ( ( model_config_rec%cu_physics(1) .EQ. GFSCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. G3SCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. GDSCHEME     ) ) THEN
               wrf_err_message = '--- WARNING: Turning on cu_rad_feedback also requires setting cu_diag== 1'
               CALL wrf_debug ( 1, wrf_err_message )
               model_config_rec%cu_diag(i) = 1
            ELSE
               model_config_rec%cu_diag(i) = 0
            END IF
         END IF
      END DO






       DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%cu_diag(i) .EQ. G3TAVE ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. GDSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. GFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFCUPSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. G3SCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using cu_diag=1 requires use of one of the following CU schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Freitas (GF) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell 3D (G3) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Kain�Fritsch Cumulus Potential (KF-CuP) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Devenyi (GD) CU scheme'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO






       DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%kf_edrates(i) .EQ. KFEDRATES ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. KFETASCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. MSKFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFSCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using kf_edrates=1 requires use of one of the following KF schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Kain-Fritsch (cu_physics=1)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Multi-scale Kain-Fritsch (cu_physics=11)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          old Kain-Fritsch (cu_physics=99)'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO





      IF ( wrf_dm_on_monitor() ) THEN
         CALL wrf_tsin_exist ( exists )
         IF ( exists ) THEN
            IF ( model_config_rec%solar_diagnostics == 1 ) THEN
               model_config_rec%process_time_series = 2
            ELSE
               model_config_rec%process_time_series = 1
            END IF
         ELSE
            model_config_rec%process_time_series = 0
         END IF
      END IF
      CALL wrf_dm_bcast_integer(model_config_rec%process_time_series, 1)






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%cu_physics(i) .EQ. GDSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. GFSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. KFCUPSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. G3SCHEME ) ) THEN
            model_config_rec%cu_diag(i) = 1
         ELSE
            model_config_rec%cu_diag(i) = 0
         END IF
      END DO





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              ( model_config_rec%sf_sfclay_physics(i) .NE. TEMFSFCSCHEME ) )  THEN
            wrf_err_message = '--- ERROR: Using bl_pbl_physics=10 requires sf_sfclay_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         ELSEIF ( ( model_config_rec%bl_pbl_physics(i) .NE. TEMFPBLSCHEME ) .AND. &
                  ( model_config_rec%sf_sfclay_physics(i) .EQ. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using sf_sfclay_physics=10 requires bl_pbl_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO      





      IF ( model_config_rec%tmn_update .EQ. 1 .AND. &
           model_config_rec%lagday .EQ. 1 ) THEN
           wrf_err_message = '--- ERROR: Using tmn_update=1 requires lagday=150 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF





      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              (model_config_rec%dfi_opt .NE. DFI_NODFI) )  THEN
            wrf_err_message = '--- ERROR: DFI not available for bl_pbl_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO      





      IF ( model_config_rec%restart ) THEN
         model_config_rec%dfi_opt = DFI_NODFI
      END IF










      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%grav_settling(i) .NE. FOGSETTLING0 ) THEN
                model_config_rec%grav_settling(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: mp_physics == 28, already has gravitational fog settling; resetting grav_settling to 0'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF




      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%use_aero_icbc .AND. model_config_rec%scalar_pblmix(i) .NE. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 1
                oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: For mp_physics == 28 and use_aero_icbc is true, recommend to turn on scalar_pblmix'
         CALL wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'resetting scalar_pblmix = 1'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

      
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ((model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
             (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
            IF ( model_config_rec%bl_mynn_mixscalars(i) .EQ. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: MYNN is set to mix scalars, turning off scalar_pblmix'
         CALL wrf_message ( wrf_err_message )
      END IF




     DO i=1,model_config_rec%max_dom
       IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
       IF ((model_config_rec%vert_refine_method(i) .NE. 0) .AND. (model_config_rec%vert_refine_fact .NE. 1)) THEN
         wrf_err_message = '--- ERROR: vert_refine_fact is ndown specific and cannot be used with vert_refine_method, and vice versa.'
         CALL wrf_debug ( 1, wrf_err_message )
       ENDIF
     ENDDO




     DO i=1,model_config_rec%max_dom
       IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
       IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
         DO j=1,model_config_rec%max_dom
           IF ((model_config_rec%vert_refine_method(i) .NE. model_config_rec%vert_refine_method(j)) .AND. (model_config_rec%vert_refine_method(j) .NE. 0)) THEN
             write(wrf_err_message,'(A,I1,A,I2,A,I1,A,I2,A)') '--- ERROR: vert_refine_method differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Only one type of vertical grid nesting can be used at a time.'
              CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
              count_fatal_error = count_fatal_error + 1
           ENDIF
         ENDDO
       ENDIF
     ENDDO





      IF ((model_config_rec%max_dom .GT. 1) .AND. (model_config_rec%vert_refine_fact .EQ. 1)) THEN
        DO i=1,model_config_rec%max_dom
          IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
          IF (((model_config_rec%parent_id(i) .NE. 0) .AND. (model_config_rec%parent_id(i) .NE. model_config_rec%grid_id(i))) .AND. (model_config_rec%vert_refine_method(i) .EQ. 0)) THEN
            DO j=1,model_config_rec%max_dom
              IF ((i .NE. j) .AND. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(j))) THEN
                IF (model_config_rec%e_vert(i) .NE. model_config_rec%e_vert(j)) THEN
                  write(wrf_err_message,'(A,I2,A,I2,A)') '--- ERROR: e_vert differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Set vert_refine_method or make e_vert consistent.'
                  CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
                  count_fatal_error = count_fatal_error + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF






      DO i=1,model_config_rec%max_dom
        IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
        IF ((model_config_rec%parent_id(i) .EQ. 0) .OR. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(i))) THEN
          IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
            write(wrf_err_message,'(A,I1,A,I2,A)') '--- ERROR: vert_refine_method=',model_config_rec%vert_refine_method(i),' for grid_id=',model_config_rec%grid_id(i),', must be 0 for a non-nested domain.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO




      DO i = 1, model_config_rec % max_dom
        IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
        IF (model_config_rec%vert_refine_method(i) .EQ. 1) THEN
          j = model_config_rec%parent_id(i)
          IF (MOD(model_config_rec%e_vert(i)-1, model_config_rec%e_vert(j)-1) .NE. 0) THEN
            write(wrf_err_message,'(A,I2,A,I2,A)') "--- ERROR: grid_id=",i," and parent (grid_id=",j,") have incompatible e_vert's for vertical nesting with integer refinement."
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO




      IF ( model_config_rec % max_ts_level .gt. model_config_rec %e_vert(1)-1 )  then
        wrf_err_message = ' max_ts_level must be <= number of znu half layers '
        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
        wrf_err_message = ' max_ts_level is reset to the number of znu half layers '
        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
        model_config_rec % max_ts_level = model_config_rec %e_vert(1)-1
      ENDIF






      DO i = 2, model_config_rec % max_dom
        IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
        IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
          IF ( ( ( model_config_rec%ra_lw_physics(i) .EQ. 0                   ) .OR. &
                 ( model_config_rec%ra_lw_physics(i) .EQ. RRTMSCHEME          ) .OR. &
                 ( model_config_rec%ra_lw_physics(i) .EQ. RRTMG_LWSCHEME      ) ) .AND. &
               ( ( model_config_rec%ra_sw_physics(i) .EQ. 0                   ) .OR. &
                 ( model_config_rec%ra_sw_physics(i) .EQ. SWRADSCHEME         ) .OR. &
                 ( model_config_rec%ra_sw_physics(i) .EQ. RRTMG_SWSCHEME      ) ) ) THEN
             
             
          ELSE
            wrf_err_message = '--- ERROR: vert_refine_method=2 only works with ra_lw/sw_physics=1 (RRTM/Dudhia) or ra_lw/sw_physics=4 (RRTMG)'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
        END IF
      END DO





      oops = 0 
      DO i = 2, model_config_rec % max_dom
        IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
        IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
          IF ( model_config_rec%hybrid_opt .NE. 0 ) THEN
            oops = oops + 1
          END IF
        END IF
      END DO

      IF ( oops .GT. 0 ) THEN
        wrf_err_message = '--- ERROR: vert_refine_method=2 only works with hybrid_opt = 0 '
        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
        count_fatal_error = count_fatal_error + 1
      END IF





      oops = 0 
      DO i = 2, model_config_rec % max_dom
        IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
        IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
          IF ( model_config_rec%feedback .NE. 0 ) THEN
            oops = oops + 1
          END IF
        END IF
      END DO

      IF ( oops .GT. 0 ) THEN
        wrf_err_message = '--- ERROR: vert_refine_method=2 only works with feedback = 0 '
        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
        count_fatal_error = count_fatal_error + 1
      END IF




      IF (  model_config_rec % polar(1) .AND. &
            model_config_rec % fft_filter_lat .LT. 90. .AND. &
            model_config_rec % traj_opt .NE. 0 ) THEN
         CALL wrf_debug ( 0, '--- ERROR: Trajectories not supported on global domain' )
         count_fatal_error = count_fatal_error + 1
      END IF







      lon_extent_is_global = .FALSE.
      IF ( ABS ( model_config_rec % e_we(1) * model_config_rec % dx(1) - 2. * piconst / reradius ) .LT. model_config_rec % dx(1) ) THEN
         lon_extent_is_global = .TRUE.
      END IF

      lat_extent_is_global = .FALSE.
      IF ( ABS ( model_config_rec % e_sn(1) * model_config_rec % dy(1) -      piconst / reradius ) .LT. model_config_rec % dy(1) ) THEN
         lat_extent_is_global = .TRUE.
      END IF

      IF ( ( .NOT. model_config_rec % polar(1) ) .AND. &
           ( lon_extent_is_global .AND. lat_extent_is_global ) ) THEN
         CALL wrf_debug ( 0, '--- ERROR: Domain size is global, set &bdy_control polar=.TRUE.' )
         count_fatal_error = count_fatal_error + 1
      END IF













      model_config_rec%auxinput10_begin_d     =       model_config_rec%gfdda_begin_d
      model_config_rec%auxinput10_begin_h     =       model_config_rec%gfdda_begin_h
      model_config_rec%auxinput10_begin_m     =       model_config_rec%gfdda_begin_m
      model_config_rec%auxinput10_begin_s     =       model_config_rec%gfdda_begin_s
      model_config_rec%auxinput10_begin_y     =       model_config_rec%gfdda_begin_y
      model_config_rec%auxinput10_end_d       =       model_config_rec%gfdda_end_d
      model_config_rec%auxinput10_end_h       =       model_config_rec%gfdda_end_h
      model_config_rec%auxinput10_end_m       =       model_config_rec%gfdda_end_m
      model_config_rec%auxinput10_end_s       =       model_config_rec%gfdda_end_s
      model_config_rec%auxinput10_end_y       =       model_config_rec%gfdda_end_y
      model_config_rec%auxinput10_inname      =       model_config_rec%gfdda_inname
      model_config_rec%auxinput10_interval    =       model_config_rec%gfdda_interval
      model_config_rec%auxinput10_interval_d  =       model_config_rec%gfdda_interval_d
      model_config_rec%auxinput10_interval_h  =       model_config_rec%gfdda_interval_h
      model_config_rec%auxinput10_interval_m  =       model_config_rec%gfdda_interval_m
      model_config_rec%auxinput10_interval_s  =       model_config_rec%gfdda_interval_s
      model_config_rec%auxinput10_interval_y  =       model_config_rec%gfdda_interval_y
      model_config_rec%io_form_auxinput10     =       model_config_rec%io_form_gfdda
      model_config_rec%auxinput9_begin_d      =       model_config_rec%sgfdda_begin_d
      model_config_rec%auxinput9_begin_h      =       model_config_rec%sgfdda_begin_h
      model_config_rec%auxinput9_begin_m      =       model_config_rec%sgfdda_begin_m
      model_config_rec%auxinput9_begin_s      =       model_config_rec%sgfdda_begin_s
      model_config_rec%auxinput9_begin_y      =       model_config_rec%sgfdda_begin_y
      model_config_rec%auxinput9_end_d        =       model_config_rec%sgfdda_end_d
      model_config_rec%auxinput9_end_h        =       model_config_rec%sgfdda_end_h
      model_config_rec%auxinput9_end_m        =       model_config_rec%sgfdda_end_m
      model_config_rec%auxinput9_end_s        =       model_config_rec%sgfdda_end_s
      model_config_rec%auxinput9_end_y        =       model_config_rec%sgfdda_end_y
      model_config_rec%auxinput9_inname       =       model_config_rec%sgfdda_inname
      model_config_rec%auxinput9_interval     =       model_config_rec%sgfdda_interval
      model_config_rec%auxinput9_interval_d   =       model_config_rec%sgfdda_interval_d
      model_config_rec%auxinput9_interval_h   =       model_config_rec%sgfdda_interval_h
      model_config_rec%auxinput9_interval_m   =       model_config_rec%sgfdda_interval_m
      model_config_rec%auxinput9_interval_s   =       model_config_rec%sgfdda_interval_s
      model_config_rec%auxinput9_interval_y   =       model_config_rec%sgfdda_interval_y
      model_config_rec%io_form_auxinput9      =       model_config_rec%io_form_sgfdda












      IF ( model_config_rec % use_wps_input .EQ. 1 ) THEN
         IF ( ( .NOT. model_config_rec % use_surface )  .AND. &
              ( model_config_rec % force_sfc_in_vinterp .GT. 0 ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting force_sfc_in_vinterp=0.'
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec % force_sfc_in_vinterp = 0
         END IF
         IF ( ( .NOT. model_config_rec % use_surface )  .AND. &
              ( model_config_rec % lowest_lev_from_sfc ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting lowest_lev_from_sfc=F.'
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec % lowest_lev_from_sfc = .FALSE.
         END IF
      END IF








      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )  ) THEN
         wrf_err_message = '--- ERROR: RRTMG FAST schemes must be built with a default compile-time flag'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- ERROR: Run ./clean -a, ./configure, ./compile scripts again'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF






      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_LWSCHEME )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMK_SWSCHEME )  ) THEN
         wrf_err_message = '--- ERROR: RRTMG-based KIAPS schemes must be built with a default compile-time flag'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- ERROR: Run ./clean -a, ./configure, ./compile scripts again'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF






      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_LWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_SWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )  ) THEN
         wrf_err_message = '--- NOTE: RRTMG radiation is used, namelist ' // &
                           'value for o3input (ozone input) is used '
         CALL wrf_debug ( 1, wrf_err_message )
      ELSE
         model_config_rec % o3input = 0
         wrf_err_message = '--- NOTE: RRTMG radiation is not used, setting:  ' // &
                           'o3input=0 to avoid data pre-processing'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF






      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % e_we(i) /  model_config_rec % nproc_x .LT. 10 ) .OR. &
              ( model_config_rec % e_sn(i) /  model_config_rec % nproc_y .LT. 10 ) ) THEN
            WRITE ( wrf_err_message , * ) 'For domain ',i,', the domain size is too small for this many processors, ', & 
                                          'or the decomposition aspect ratio is poor.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , * ) 'Minimum decomposed computational patch size, either x-dir or y-dir, is 10 grid cells.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , fmt='(a,i5,a,i4,a,i4)' ) &
                                          'e_we = ', model_config_rec % e_we(i),', nproc_x = ',model_config_rec % nproc_x, &
                                          ', with cell width in x-direction = ', &
                                          model_config_rec % e_we(i) /  model_config_rec % nproc_x
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , fmt='(a,i5,a,i4,a,i4)' ) &
                                          'e_sn = ', model_config_rec % e_sn(i),', nproc_y = ',model_config_rec % nproc_y, &
                                          ', with cell width in y-direction = ', &
                                          model_config_rec % e_sn(i) /  model_config_rec % nproc_y
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            wrf_err_message = '--- ERROR: Reduce the MPI rank count, or redistribute the tasks.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            oops = oops + 1
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         count_fatal_error = count_fatal_error + 1
      END IF









      IF ( model_config_rec%clean_atm_diag > 0 ) THEN

         wrf_err_message = '--- NOTE: "Clean" atmosphere diagnostics can only be used in WRF-Chem' 
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         model_config_rec%calc_clean_atm_diag = 0

      ENDIF





      IF ( count_fatal_error .GT. 0 ) THEN
         WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  ', count_fatal_error, &
                                            ' namelist settings are wrong. Please check and reset these options'
         CALL wrf_error_fatal3("<stdin>",2378,&
wrf_err_message  )
      END IF

   END SUBROUTINE check_nml_consistency



   SUBROUTINE setup_physics_suite










      USE module_domain, ONLY : change_to_lower_case

      IMPLICIT NONE

      INTEGER :: i
      INTEGER :: max_dom
      LOGICAL :: have_mods
      INTEGER, DIMENSION( max_domains ) :: orig_mp_physics, orig_cu_physics, orig_ra_lw_physics, orig_ra_sw_physics, &
                                           orig_bl_pbl_physics, orig_sf_sfclay_physics, orig_sf_surface_physics
      CHARACTER, DIMENSION( max_domains ) :: modified_mp_option, modified_cu_option, modified_ra_lw_option, modified_ra_sw_option, &
                                             modified_bl_pbl_option, modified_sf_sfclay_option, modified_sf_surface_option
      CHARACTER (LEN=256) :: physics_suite_lowercase
      CHARACTER (LEN=32) :: formatstring

      
      
      
      

      wrf_debug_level = model_config_rec%debug_level

      max_dom = model_config_rec % max_dom

      
      
      
      
      modified_mp_option(1:max_dom) = ' '
      orig_mp_physics(1:max_dom) = model_config_rec % mp_physics(1:max_dom)

      modified_cu_option(1:max_dom) = ' '
      orig_cu_physics(1:max_dom) = model_config_rec % cu_physics(1:max_dom)

      modified_ra_lw_option(1:max_dom) = ' '
      orig_ra_lw_physics(1:max_dom) = model_config_rec % ra_lw_physics(1:max_dom)

      modified_ra_sw_option(1:max_dom) = ' '
      orig_ra_sw_physics(1:max_dom) = model_config_rec % ra_sw_physics(1:max_dom)

      modified_bl_pbl_option(1:max_dom) = ' '
      orig_bl_pbl_physics(1:max_dom) = model_config_rec % bl_pbl_physics(1:max_dom)

      modified_sf_sfclay_option(1:max_dom) = ' '
      orig_sf_sfclay_physics(1:max_dom) = model_config_rec % sf_sfclay_physics(1:max_dom)

      modified_sf_surface_option(1:max_dom) = ' '
      orig_sf_surface_physics(1:max_dom) = model_config_rec % sf_surface_physics(1:max_dom)

      CALL change_to_lower_case(trim(model_config_rec % physics_suite), physics_suite_lowercase)

      

      
      IF ( trim(physics_suite_lowercase) == 'none' ) THEN
         wrf_err_message = '*************************************'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'No physics suite selected.'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'Physics options will be used directly from the namelist.'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = '*************************************'
         call wrf_debug ( 1, wrf_err_message )
         RETURN
      END IF

      CALL wrf_message ('*************************************')
      CALL wrf_message ('Configuring physics suite '''//trim(physics_suite_lowercase)//'''')
      CALL wrf_message ('')

      
      
      
      SELECT CASE ( trim(physics_suite_lowercase) )

      
      
      
      CASE ('conus')
         DO i = 1, max_dom

            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = TIEDTKESCHEME               
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = THOMPSON                    
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME        
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME        
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = MYJPBLSCHEME        
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = MYJSFCSCHEME  
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME   

         END DO

      
      
      
      CASE ('tropical')
         DO i = 1, max_dom

            IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = NTIEDTKESCHEME              
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = WSM6SCHEME                  
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME        
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME        
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = YSUSCHEME           
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = SFCLAYSCHEME  
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME   

         END DO

      CASE DEFAULT
         CALL wrf_error_fatal3("<stdin>",2506,&
'Unrecognized physics suite' )

      END SELECT

      WRITE (formatstring, '(A,I3,A)') '(A21,', max_dom, '(I6,A1))'

      
      
      
      WHERE (model_config_rec % mp_physics(1:max_dom) == orig_mp_physics(1:max_dom)) modified_mp_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'mp_physics: ', &
                                                    (model_config_rec % mp_physics(i), modified_mp_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % cu_physics(1:max_dom) == orig_cu_physics(1:max_dom)) modified_cu_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'cu_physics: ', &
                                                    (model_config_rec % cu_physics(i), modified_cu_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % ra_lw_physics(1:max_dom) == orig_ra_lw_physics(1:max_dom)) modified_ra_lw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_lw_physics: ', &
                                                    (model_config_rec % ra_lw_physics(i), modified_ra_lw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % ra_sw_physics(1:max_dom) == orig_ra_sw_physics(1:max_dom)) modified_ra_sw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_sw_physics: ', &
                                                    (model_config_rec % ra_sw_physics(i), modified_ra_sw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % bl_pbl_physics(1:max_dom) == orig_bl_pbl_physics(1:max_dom)) modified_bl_pbl_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'bl_pbl_physics: ', &
                                                    (model_config_rec % bl_pbl_physics(i), modified_bl_pbl_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % sf_sfclay_physics(1:max_dom) == orig_sf_sfclay_physics(1:max_dom)) &
            modified_sf_sfclay_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_sfclay_physics: ', (model_config_rec % sf_sfclay_physics(i), modified_sf_sfclay_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      WHERE (model_config_rec % sf_surface_physics(1:max_dom) == orig_sf_surface_physics(1:max_dom)) &
            modified_sf_surface_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_surface_physics: ', (model_config_rec % sf_surface_physics(i), modified_sf_surface_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      
      
      
      have_mods = ANY (modified_mp_option(1:max_dom) == '*') &
             .OR. ANY (modified_cu_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_lw_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_sw_option(1:max_dom) == '*') &
             .OR. ANY (modified_bl_pbl_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_sfclay_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_surface_option(1:max_dom) == '*')

      IF (have_mods) THEN
         CALL wrf_message ('')
         CALL wrf_message ('(* = option overrides suite setting)')
      END IF

      CALL wrf_message ('*************************************')


   END SUBROUTINE setup_physics_suite



   SUBROUTINE set_physics_rconfigs










      IMPLICIT NONE

      INTEGER :: numsoiltemp , nummosaictemp
      INTEGER :: i






      IF ( any(model_config_rec%sf_urban_physics > 0 ) ) THEN
      
         model_config_rec%urban_map_zrd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nwr * &
                                          model_config_rec%num_urban_nz
         model_config_rec%urban_map_zwd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nwr * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_gd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_ng
         model_config_rec%urban_map_zd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_zdf = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz 
         model_config_rec%urban_map_bd  = model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_wd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_gbd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_ngb * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_fbd = model_config_rec%num_urban_ndm       * &
                                          (model_config_rec%num_urban_nz - 1)  * &
                                          model_config_rec%num_urban_nf        * &
                                          model_config_rec%num_urban_nbui

      END IF     
      




      IF ( model_config_rec % sf_surface_mosaic .EQ. 1 ) THEN
      
      numsoiltemp = model_config_rec % num_soil_layers
      nummosaictemp = model_config_rec % mosaic_cat
      
         model_config_rec % mosaic_cat_soil = numsoiltemp * nummosaictemp

         wrf_err_message = '--- NOTE: Noah-mosaic is in use, setting:  ' // &
                           'mosaic_cat_soil = mosaic_cat * num_soil_layers'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF     
      




      CALL RANDOM_SEED ( SIZE = model_config_rec % seed_dim )






      model_config_rec % fft_used = 0
      IF ( ( model_config_rec % polar(1) ) .AND. &
           ( model_config_rec % fft_filter_lat .LT. 90. ) ) THEN
         model_config_rec % fft_used = 1
      END IF






      model_config_rec % aercu_used = 0
      IF ( model_config_rec %aercu_opt .GT. 0 ) THEN
         model_config_rec % aercu_used = 1
      END IF






      model_config_rec % cam_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % mp_physics(i)     .EQ. CAMMGMPSCHEME   ) .OR. &
              ( model_config_rec % bl_pbl_physics(i) .EQ. CAMUWPBLSCHEME  ) .OR. &
              ( model_config_rec % shcu_physics(i)   .EQ. CAMUWSHCUSCHEME ) ) THEN
            model_config_rec % cam_used = 1
         END IF
      ENDDO



      





      IF (( model_config_rec % ra_lw_physics(1) .EQ. CAMLWSCHEME ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. CAMSWSCHEME )) THEN
         model_config_rec % paerlev = 29
         model_config_rec % levsiz = 59
         model_config_rec % cam_abs_dim1 = 4
         model_config_rec % cam_abs_dim2 = model_config_rec % e_vert(1)

         wrf_err_message = '--- NOTE: CAM radiation is in use, setting:  ' // &
                           'paerlev=29, levsiz=59, cam_abs_dim1=4, cam_abs_dim2=e_vert'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF







      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % mp_physics(i) .EQ. MILBRANDT2MOM ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOM     ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMG    ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMCCN  ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOM     ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOMLFO  ) .OR. &
              ( model_config_rec % do_radar_ref  .EQ. 1             ) ) THEN
            model_config_rec % compute_radar_ref = 1
         END IF
      ENDDO







      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( model_config_rec % fmoist_run(i) .EQV. .TRUE.  ) THEN
            model_config_rec % fmoisti_run(i) = 1
         ELSE 
            model_config_rec % fmoisti_run(i) = 0
         END IF
         IF ( model_config_rec % fmoist_interp(i) .EQV. .TRUE.  ) THEN
            model_config_rec % fmoisti_interp(i) = 1
         ELSE 
            model_config_rec % fmoisti_interp(i) = 0
         END IF
      ENDDO






      DO i = 1, model_config_rec % max_dom
         IF ( .NOT. model_config_rec % grid_allowed(i) ) CYCLE
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME2 ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME3 ) ) THEN
            model_config_rec % bl_mynn_edmf = 0
         END IF
      ENDDO






      IF (( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME      ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME      ) .OR. &
          ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_LWSCHEME      ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMK_SWSCHEME      ) .OR. &
          ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )) THEN
         model_config_rec % levsiz = 59
         model_config_rec % alevsiz = 12
         model_config_rec % no_src_types = 6

         wrf_err_message = '--- NOTE: One of the RRTMG radiation schemes is in use, setting:  ' // &
                           'levsiz=59, alevsiz=12, no_src_types=6'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF






      IF      (   model_config_rec % sf_surface_physics(1) .EQ. NOLSMSCHEME  ) THEN
         model_config_rec % num_soil_layers = 5
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. SLABSCHEME   ) THEN
         model_config_rec % num_soil_layers = 5
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. LSMSCHEME    ) THEN
         model_config_rec % num_soil_layers = 4
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. NOAHMPSCHEME ) THEN
         model_config_rec % num_soil_layers = 4
      ELSE IF ( ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) .AND. &
                ( model_config_rec % num_soil_layers .EQ. 6 ) ) THEN
         model_config_rec % num_soil_layers = 6
      ELSE IF ( ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) .AND. &
                ( model_config_rec % num_soil_layers .EQ. 9 ) ) THEN
         model_config_rec % num_soil_layers = 9
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) THEN
         model_config_rec % num_soil_layers = 6
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME  ) THEN
         model_config_rec % num_soil_layers = 2
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. CLMSCHEME    ) THEN
         model_config_rec % num_soil_layers = 10
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. SSIBSCHEME   ) THEN
         model_config_rec % num_soil_layers = 3
      ELSE
         CALL wrf_debug       ( 0 , '--- ERROR: Unknown sf_surface_physics has no associated number of soil levels' )
         WRITE (wrf_err_message, FMT='(A,I6)') '--- ERROR: sf_surface_physics = ' , model_config_rec % sf_surface_physics(1)
         CALL wrf_error_fatal3("<stdin>",2829,&
TRIM(wrf_err_message) )
      END IF 

      WRITE (wrf_err_message, FMT='(A,I6)') '--- NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_debug ( 1, wrf_err_message )

   END SUBROUTINE set_physics_rconfigs



   RECURSIVE SUBROUTINE get_moad_factor ( id, parent_id, parent_grid_ratio, max_dom, factor )
      IMPLICIT NONE
      INTEGER                     :: max_dom
      INTEGER, DIMENSION(max_dom) :: parent_id, parent_grid_ratio
      INTEGER                     :: factor, id
   
      IF ( id .EQ. 1 ) THEN
         RETURN
      ELSE
         factor = factor * parent_grid_ratio(id)
         CALL get_moad_factor ( parent_id(id), parent_id, parent_grid_ratio, max_dom, factor )
      END IF
   END  SUBROUTINE get_moad_factor



   END MODULE module_check_a_mundo


