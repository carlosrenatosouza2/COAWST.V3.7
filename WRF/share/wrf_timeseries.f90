
































SUBROUTINE calc_ts_locations( grid )

   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure, ONLY : model_config_rec, grid_config_rec_type, model_to_grid_config_rec
   USE module_dm, ONLY : wrf_dm_min_real
   USE module_llxy
   USE module_state_description
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: ntsloc_temp
   INTEGER :: i, j, k, iunit
   REAL :: ts_rx, ts_ry, ts_xlat, ts_xlong, ts_hgt
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   CHARACTER (LEN=24) :: ts_profile_filename
   INTEGER, PARAMETER :: TS_FIELDS = 7
   CHARACTER (LEN=2), DIMENSION(TS_FIELDS) :: &
      ts_file_endings = (/ 'UU', 'VV', 'PH', 'TH', 'QV' ,'WW', 'PR'/) 
   INTEGER   ierr
   CHARACTER (len=19) simulation_start_date
   INTEGER simulation_start_year   , &
           simulation_start_month  , &
           simulation_start_day    , &
           simulation_start_hour   , &
           simulation_start_minute , &
           simulation_start_second

   TYPE (PROJ_INFO) :: ts_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey


   IF ( grid%ntsloc .LE. 0 ) RETURN

   IF ( grid%dfi_stage == DFI_FST ) THEN
      CALL get_ijk_from_grid ( grid ,                               &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               ips, ipe, jps, jpe, kps, kpe,        &
                               imsx, imex, jmsx, jmex, kmsx, kmex,  &
                               ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                               imsy, imey, jmsy, jmey, kmsy, kmey,  &
                               ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
      
      CALL map_init(ts_proj)
   
      IF (ips <= 1 .AND. 1 <= ipe .AND. &
          jps <= 1 .AND. 1 <= jpe) THEN
         known_lat = grid%xlat(1,1)
         known_lon = grid%xlong(1,1)
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      known_lat = wrf_dm_min_real(known_lat)
      known_lon = wrf_dm_min_real(known_lon)
   
      
      IF (config_flags%map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, ts_proj,               &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_LC) THEN
         CALL map_set(PROJ_LC, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, ts_proj,                            &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &
                      lat0     = config_flags%pole_lat,                 &
                      lon0     = config_flags%pole_lon,                 &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)

      
      ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, ts_proj,                      &

                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
      END IF
   
      
      ierr = 0
      CALL nl_get_simulation_start_year   ( 1 , simulation_start_year   )
      CALL nl_get_simulation_start_month  ( 1 , simulation_start_month  )
      CALL nl_get_simulation_start_day    ( 1 , simulation_start_day    )
      CALL nl_get_simulation_start_hour   ( 1 , simulation_start_hour   )
      CALL nl_get_simulation_start_minute ( 1 , simulation_start_minute )
      CALL nl_get_simulation_start_second ( 1 , simulation_start_second )
      WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
              simulation_start_year,simulation_start_month,simulation_start_day,simulation_start_hour, &
              simulation_start_minute,simulation_start_second


      
      
      IF (.NOT. grid%have_calculated_tslocs) THEN
         grid%have_calculated_tslocs = .TRUE.
         WRITE(message, '(A43,I3,A15,A19)') 'Computing time series locations for domain ', grid%id, &
                                            ' starting from ',simulation_start_date
         CALL wrf_message(message)
   
         ntsloc_temp = 0

         
         DO k=1,grid%ntsloc
            
            IF (config_flags%map_proj == 0 .OR. grid%tslist_ij) THEN 
               ts_rx = grid%itsloc(k)  
               ts_ry = grid%jtsloc(k)
            
            ELSE
               CALL latlon_to_ij(ts_proj, grid%lattsloc(k), grid%lontsloc(k), ts_rx, ts_ry)
            END IF
            

            ntsloc_temp = ntsloc_temp + 1
            grid%itsloc(ntsloc_temp) = NINT(ts_rx)
            grid%jtsloc(ntsloc_temp) = NINT(ts_ry)
            grid%id_tsloc(ntsloc_temp) = k
   
            
            IF (grid%itsloc(ntsloc_temp) < ids .OR. grid%itsloc(ntsloc_temp) > ide .OR. &
                grid%jtsloc(ntsloc_temp) < jds .OR. grid%jtsloc(ntsloc_temp) > jde) THEN
               ntsloc_temp = ntsloc_temp - 1
   
            END IF
   
         END DO
   
         grid%next_ts_time = 1
   
         grid%ntsloc_domain = ntsloc_temp
   
         DO k=1,grid%ntsloc_domain

            
            IF (grid%itsloc(k) < ips .OR. grid%itsloc(k) > ipe .OR. &
                grid%jtsloc(k) < jps .OR. grid%jtsloc(k) > jpe) THEN
               ts_xlat  = 1.E30
               ts_xlong = 1.E30
               ts_hgt   = 1.E30
            ELSE
               ts_xlat  = grid%xlat(grid%itsloc(k),grid%jtsloc(k))
               ts_xlong = grid%xlong(grid%itsloc(k),grid%jtsloc(k))
               ts_hgt   = grid%ht(grid%itsloc(k),grid%jtsloc(k))
            END IF
            ts_xlat  = wrf_dm_min_real(ts_xlat)
            ts_xlong = wrf_dm_min_real(ts_xlong)
            ts_hgt   = wrf_dm_min_real(ts_hgt)
   
            IF ( wrf_dm_on_monitor() ) THEN
               iunit = get_unused_unit()
               IF ( iunit <= 0 ) THEN
                  CALL wrf_error_fatal3("<stdin>",235,&
'Error in calc_ts_locations: could not find a free Fortran unit.')
               END IF
               WRITE(grid%ts_filename(k),'(A)') TRIM(grid%nametsloc(grid%id_tsloc(k)))//'.d00.TS'
               i = LEN_TRIM(grid%ts_filename(k))
               WRITE(grid%ts_filename(k)(i-4:i-3),'(I2.2)') grid%id
               OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(k)), FORM='FORMATTED', STATUS='REPLACE')
               IF ( .NOT. grid%tslist_ij ) THEN 
                  WRITE(UNIT=iunit, &
                        FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7,A2,A19)') &
                        grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                        ' '//grid%nametsloc(grid%id_tsloc(k)), &
                        ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
                        grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                        ts_xlat, ',', ts_xlong, ') ', &
                        ts_hgt,' meters','  ',simulation_start_date(1:19)
               ELSE
                  WRITE(UNIT=iunit, &
                        FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7,A2,A19)') &
                        grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                        ' '//grid%nametsloc(grid%id_tsloc(k)), &
                        ' (', ts_xlat, ',', ts_xlong, ') (', &
                        grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                        ts_xlat, ',', ts_xlong, ') ', &
                        ts_hgt,' meters','  ',simulation_start_date(1:19)
               END IF
               CLOSE(UNIT=iunit)

               ts_profile_filename = grid%ts_filename(k)
               DO j=1,SIZE(ts_file_endings)
                  
                  iunit = get_unused_unit()
                  IF ( iunit <= 0 ) THEN
                     CALL wrf_error_fatal3("<stdin>",268,&
'Error in calc_ts_locations: could not find a free Fortran unit.')
                  END IF
                  i = LEN_TRIM(ts_profile_filename)
                  WRITE(ts_profile_filename(i-1:i),'(A2)') ts_file_endings(j)
                  OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), FORM='FORMATTED', STATUS='REPLACE')
                  IF ( .NOT. grid%tslist_ij ) THEN 
                     WRITE(UNIT=iunit, &
                           FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7,A2,A19)') &
                           grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                           ' '//grid%nametsloc(grid%id_tsloc(k)), &
                           ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
                           grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                           ts_xlat, ',', ts_xlong, ') ', &
                           ts_hgt,' meters','  ',simulation_start_date
                  ELSE
                     WRITE(UNIT=iunit, &
                           FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7,A2,A19)') &
                           grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                           ' '//grid%nametsloc(grid%id_tsloc(k)), &
                           ' (', ts_xlat, ',', ts_xlong, ') (', &
                           grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                           ts_xlat, ',', ts_xlong, ') ', &
                           ts_hgt,' meters,','  ',simulation_start_date
                 END IF
                  CLOSE(UNIT=iunit)
               END DO
            END IF
         END DO
   
      END IF
   END IF

END SUBROUTINE calc_ts_locations


SUBROUTINE calc_ts( grid )

   USE module_domain
   USE module_configure, ONLY : model_config_rec, grid_config_rec_type, model_to_grid_config_rec
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   
   INTEGER :: i, k, mm, n, ix, iy, rc
   REAL :: earth_u, earth_v,                       &
           output_t, output_q, clw, xtime_minutes
   REAL, PARAMETER :: MISSING = -999.0
   REAL, ALLOCATABLE, DIMENSION(:) :: p8w
   REAL, ALLOCATABLE, DIMENSION(:) :: earth_u_profile, earth_v_profile
   TYPE (grid_config_rec_type) :: config_flags

   
       
       
   LOGICAL, PARAMETER :: ts_model_level = .FALSE. 

   
   ALLOCATE ( earth_u_profile(grid%max_ts_level), earth_v_profile(grid%max_ts_level) )

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN

   n = grid%next_ts_time

   ALLOCATE(p8w(grid%sm32:grid%em32))
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   DO i=1,grid%ntsloc_domain

      ix = grid%itsloc(i)
      iy = grid%jtsloc(i)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN
       
         IF (ts_model_level) THEN
   
            
            
            
            earth_u = grid%u_2(ix,1,iy)*grid%cosa(ix,iy)-grid%v_2(ix,1,iy)*grid%sina(ix,iy)
            earth_v = grid%v_2(ix,1,iy)*grid%cosa(ix,iy)+grid%u_2(ix,1,iy)*grid%sina(ix,iy)
            IF (grid%use_theta_m == 1) THEN
               output_t = (grid%t_2(ix,1,iy)+T0)/(1.+R_v/R_d*grid%moist(ix,1,iy,P_QV)) - T0
            ELSE
               output_t =  grid%t_2(ix,1,iy)
            END IF
            output_q = grid%moist(ix,1,iy,P_QV)
   
         ELSE
   
            
            
            
            DO k=1,grid%max_ts_level
                
                IF (config_flags%tslist_unstagger_winds) THEN
                    earth_u_profile(k) = &
                    ((grid%u_2(ix,k,iy)*grid%cosa(ix,iy)-grid%v_2(ix,k,iy)*grid%sina(ix,iy)) + &
                    (grid%u_2(ix+1,k,iy)*grid%cosa(ix+1,iy)-grid%v_2(ix+1,k,iy)*grid%sina(ix+1,iy)))/2.0 
                    earth_v_profile(k) = &
                    ((grid%v_2(ix,k,iy)*grid%cosa(ix,iy)+grid%u_2(ix,k,iy)*grid%sina(ix,iy)) + &
                    (grid%v_2(ix,k,iy+1)*grid%cosa(ix,iy+1)+grid%u_2(ix,k,iy+1)*grid%sina(ix,iy+1)))/2.0 
                ELSE
                    earth_u_profile(k) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)-grid%v_2(ix,k,iy)*grid%sina(ix,iy) 
                    earth_v_profile(k) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)+grid%u_2(ix,k,iy)*grid%sina(ix,iy)
                END IF
            END DO
            earth_u = grid%u10(ix,iy)*grid%cosa(ix,iy)-grid%v10(ix,iy)*grid%sina(ix,iy)
            earth_v = grid%v10(ix,iy)*grid%cosa(ix,iy)+grid%u10(ix,iy)*grid%sina(ix,iy)
            output_q = grid%q2(ix,iy)
            output_t = grid%t2(ix,iy)
   
         END IF
   
         
         CALL calc_p8w(grid, ix, iy, p8w, grid%sm32, grid%em32)
         clw=0.
         DO mm = 1, num_moist
            IF ( (mm == P_QC) .OR. (mm == P_QR) .OR. (mm == P_QI) .OR. &
                 (mm == P_QS) .OR. (mm == P_QG) ) THEN
               DO k=grid%sm32,grid%em32-1
                  clw=clw+grid%moist(ix,k,iy,mm)*(p8w(k)-p8w(k+1))
               END DO
           END IF
         END DO
         clw = clw / g
   
         CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )
         grid%ts_hour(n,i) = xtime_minutes / 60.
            
            DO k=1,grid%max_ts_level
            grid%ts_u_profile(n,i,k)   = earth_u_profile(k)
            grid%ts_v_profile(n,i,k)   = earth_v_profile(k)
            grid%ts_w_profile(n,i,k)   = (grid%w_2(ix,k,iy)+grid%w_2(ix,k+1,iy))/2.0 
            grid%ts_gph_profile(n,i,k) = 0.5*((grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)) &
                                             +(grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy)))/9.81 
            IF (grid%use_theta_m == 1) THEN
               grid%ts_th_profile(n,i,k)  = (grid%t_2(ix,k,iy) + T0)/(1.+R_v/R_d*grid%moist(ix,k,iy,P_QV))
            ELSE
               grid%ts_th_profile(n,i,k)  =  grid%t_2(ix,k,iy) + T0
            END IF
            grid%ts_qv_profile(n,i,k)  = grid%moist(ix,k,iy,P_QV)
            grid%ts_p_profile(n,i,k)   = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
            END DO
         grid%ts_u(n,i)    = earth_u
         grid%ts_v(n,i)    = earth_v
         grid%ts_t(n,i)    = output_t
         grid%ts_q(n,i)    = output_q
         grid%ts_psfc(n,i) = grid%psfc(ix,iy)
         grid%ts_glw(n,i)  = grid%glw(ix,iy)
         grid%ts_gsw(n,i)  = grid%gsw(ix,iy)
         grid%ts_hfx(n,i)  = grid%hfx(ix,iy)
         grid%ts_lh(n,i)   = grid%lh(ix,iy)
         grid%ts_clw(n,i)  = clw
         grid%ts_rainc(n,i)  = grid%rainc(ix,iy)
         grid%ts_rainnc(n,i) = grid%rainnc(ix,iy)
         grid%ts_tsk(n,i)  = grid%tsk(ix,iy)
         IF ( model_config_rec%process_time_series == 2 ) THEN
            
            grid%ts_cldfrac2d(n,i) = grid%cldfrac2d(ix,iy)
            grid%ts_wvp(n,i) = grid%wvp(ix,iy)
            grid%ts_lwp(n,i) = grid%lwp(ix,iy)
            grid%ts_iwp(n,i) = grid%iwp(ix,iy)
            grid%ts_swp(n,i) = grid%swp(ix,iy)
            grid%ts_wp_sum(n,i) = grid%wp_sum(ix,iy)
            grid%ts_lwp_tot(n,i) = grid%lwp_tot(ix,iy)
            grid%ts_iwp_tot(n,i) = grid%iwp_tot(ix,iy)
            grid%ts_wp_tot_sum(n,i) = grid%wp_tot_sum(ix,iy)
            grid%ts_re_qc(n,i) = grid%re_qc(ix,iy)
            grid%ts_re_qi(n,i) = grid%re_qi(ix,iy)
            grid%ts_re_qs(n,i) = grid%re_qs(ix,iy)
            grid%ts_re_qc_tot(n,i) = grid%re_qc_tot(ix,iy)
            grid%ts_re_qi_tot(n,i) = grid%re_qi_tot(ix,iy)
            grid%ts_tau_qc(n,i) = grid%tau_qc(ix,iy)
            grid%ts_tau_qi(n,i) = grid%tau_qi(ix,iy)
            grid%ts_tau_qs(n,i) = grid%tau_qs(ix,iy)
            grid%ts_tau_qc_tot(n,i) = grid%tau_qc_tot(ix,iy)
            grid%ts_tau_qi_tot(n,i) = grid%tau_qi_tot(ix,iy)
            grid%ts_cbaseht(n,i) = grid%cbaseht(ix,iy)
            grid%ts_ctopht(n,i) = grid%ctopht(ix,iy)
            grid%ts_cbaseht_tot(n,i) = grid%cbaseht_tot(ix,iy)
            grid%ts_ctopht_tot(n,i) = grid%ctopht_tot(ix,iy)
            grid%ts_clrnidx(n,i) = grid%clrnidx(ix,iy)
            grid%ts_sza(n,i) = grid%sza(ix,iy)
            grid%ts_swdown(n,i) = grid%swdown(ix,iy)
            grid%ts_swddni(n,i) = grid%swddni(ix,iy)
            grid%ts_swddif(n,i) = grid%swddif(ix,iy)
            
            if ( config_flags%swint_opt == 2 .or. &
                 config_flags%ra_sw_physics == RRTMG_SWSCHEME .or. &
                 config_flags%ra_sw_physics == RRTMG_SWSCHEME_FAST ) then
              grid%ts_swdownc(n,i) = grid%swdownc(ix,iy)
              grid%ts_swddnic(n,i) = grid%swddnic(ix,iy)
              if ( config_flags%swint_opt == 2 ) then  
                grid%ts_swdown2(n,i) = grid%swdown2(ix,iy)
                grid%ts_swddni2(n,i) = grid%swddni2(ix,iy)
                grid%ts_swddif2(n,i) = grid%swddif2(ix,iy)
                grid%ts_swdownc2(n,i) = grid%swdownc2(ix,iy)
                grid%ts_swddnic2(n,i) = grid%swddnic2(ix,iy)
              else
                grid%ts_swdown2(n,i) = MISSING
                grid%ts_swddni2(n,i) = MISSING
                grid%ts_swddif2(n,i) = MISSING
                grid%ts_swdownc2(n,i) = MISSING
                grid%ts_swddnic2(n,i) = MISSING
              end if
            else
              grid%ts_swdownc(n,i) = MISSING
              grid%ts_swddnic(n,i) = MISSING
              grid%ts_swdown2(n,i) = MISSING
              grid%ts_swddni2(n,i) = MISSING
              grid%ts_swddif2(n,i) = MISSING
              grid%ts_swdownc2(n,i) = MISSING
              grid%ts_swddnic2(n,i) = MISSING
            end if
         END IF
         grid%ts_tslb(n,i) = grid%tslb(ix,1,iy)

      ELSE
         DO k=1,grid%max_ts_level
         grid%ts_u_profile(n,i,k)     = 1.E30
         grid%ts_v_profile(n,i,k)     = 1.E30
         grid%ts_w_profile(n,i,k)     = 1.E30 
         grid%ts_gph_profile(n,i,k)   = 1.E30
         grid%ts_th_profile(n,i,k)    = 1.E30
         grid%ts_qv_profile(n,i,k)    = 1.E30
         grid%ts_p_profile(n,i,k)     = 1.E30
         END DO
         grid%ts_hour(n,i) = 1.E30
         grid%ts_u(n,i)    = 1.E30
         grid%ts_v(n,i)    = 1.E30
         grid%ts_t(n,i)    = 1.E30
         grid%ts_q(n,i)    = 1.E30
         grid%ts_psfc(n,i) = 1.E30
         grid%ts_glw(n,i)  = 1.E30
         grid%ts_gsw(n,i)  = 1.E30
         grid%ts_hfx(n,i)  = 1.E30
         grid%ts_lh(n,i)   = 1.E30
         grid%ts_clw(n,i)  = 1.E30
         grid%ts_rainc(n,i)  = 1.E30
         grid%ts_rainnc(n,i) = 1.E30
         IF ( model_config_rec%process_time_series == 2 ) THEN
            
            grid%ts_cldfrac2d(n,i) = 1.E30
            grid%ts_wvp(n,i) = 1.E30
            grid%ts_lwp(n,i) = 1.E30
            grid%ts_iwp(n,i) = 1.E30
            grid%ts_swp(n,i) = 1.E30
            grid%ts_wp_sum(n,i) = 1.E30
            grid%ts_lwp_tot(n,i) = 1.E30
            grid%ts_iwp_tot(n,i) = 1.E30
            grid%ts_wp_tot_sum(n,i) = 1.E30
            grid%ts_re_qc(n,i) = 1.E30
            grid%ts_re_qi(n,i) = 1.E30
            grid%ts_re_qs(n,i) = 1.E30
            grid%ts_re_qc_tot(n,i) = 1.E30
            grid%ts_re_qi_tot(n,i) = 1.E30
            grid%ts_tau_qc(n,i) = 1.E30
            grid%ts_tau_qi(n,i) = 1.E30
            grid%ts_tau_qs(n,i) = 1.E30
            grid%ts_tau_qc_tot(n,i) = 1.E30
            grid%ts_tau_qi_tot(n,i) = 1.E30
            grid%ts_cbaseht(n,i) = 1.E30
            grid%ts_ctopht(n,i) = 1.E30
            grid%ts_cbaseht_tot(n,i) = 1.E30
            grid%ts_ctopht_tot(n,i) = 1.E30
            grid%ts_clrnidx(n,i) = 1.E30
            grid%ts_sza(n,i) = 1.E30
            grid%ts_swdown(n,i) = 1.E30
            grid%ts_swddni(n,i) = 1.E30
            grid%ts_swddif(n,i) = 1.E30
            grid%ts_swdownc(n,i) = 1.E30
            grid%ts_swddnic(n,i) = 1.E30
            grid%ts_swdown2(n,i) = 1.E30
            grid%ts_swddni2(n,i) = 1.E30
            grid%ts_swddif2(n,i) = 1.E30
            grid%ts_swdownc2(n,i) = 1.E30
            grid%ts_swddnic2(n,i) = 1.E30
         END IF
         grid%ts_tsk(n,i)  = 1.E30
         grid%ts_tslb(n,i) = 1.E30

      END IF
   END DO

   DEALLOCATE(p8w, earth_u_profile, earth_v_profile)
 
   grid%next_ts_time = grid%next_ts_time + 1

   IF ( grid%next_ts_time > grid%ts_buf_size ) CALL write_ts(grid)

END SUBROUTINE calc_ts


SUBROUTINE write_ts( grid )

   USE module_domain, ONLY : domain
   USE module_dm, ONLY : wrf_dm_min_reals
   USE module_state_description
   USE module_configure, ONLY : model_config_rec

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: i, n, ix, iy, iunit, k
   REAL, ALLOCATABLE, DIMENSION(:,:) :: ts_buf
   CHARACTER (LEN=24) :: ts_profile_filename
   CHARACTER (LEN=26) :: profile_format

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN

   ALLOCATE(ts_buf(grid%ts_buf_size,grid%max_ts_locs))

   ts_buf(:,:) = grid%ts_hour(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_hour(:,:),grid%ts_buf_size*grid%max_ts_locs)

   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_u_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO
 
   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_v_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO
 
   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_w_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_w_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO

   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_gph_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_gph_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO

   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_th_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_th_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO

   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_qv_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_qv_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO

   DO k=1,grid%max_ts_level
   ts_buf(:,:) = grid%ts_p_profile(:,:,k)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p_profile(:,:,k),grid%ts_buf_size*grid%max_ts_locs)
   END DO
   ts_buf(:,:) = grid%ts_u(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_v(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_t(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_q(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_psfc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_psfc(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_glw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_glw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_gsw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_gsw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_hfx(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_hfx(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_lh(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_lh(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_clw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_clw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_rainc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_rainc(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_rainnc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_rainnc(:,:),grid%ts_buf_size*grid%max_ts_locs)

   IF ( model_config_rec%process_time_series == 2 ) THEN
      
      ts_buf(:,:) = grid%ts_cldfrac2d(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_cldfrac2d(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_wvp(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_wvp(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_lwp(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_lwp(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_iwp(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_iwp(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swp(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swp(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_wp_sum(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_wp_sum(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_lwp_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_lwp_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_iwp_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_iwp_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_wp_tot_sum(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_wp_tot_sum(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_re_qc(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_re_qc(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_re_qi(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_re_qi(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_re_qs(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_re_qs(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_re_qc_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_re_qc_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_re_qi_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_re_qi_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_tau_qc(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tau_qc(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_tau_qi(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tau_qi(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_tau_qs(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tau_qs(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_tau_qc_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tau_qc_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_tau_qi_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tau_qi_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_cbaseht(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_cbaseht(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_ctopht(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_ctopht(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_cbaseht_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_cbaseht_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_ctopht_tot(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_ctopht_tot(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_clrnidx(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_clrnidx(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_sza(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_sza(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swdown(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swdown(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddni(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddni(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddif(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddif(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swdownc(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swdownc(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddnic(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddnic(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swdown2(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swdown2(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddni2(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddni2(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddif2(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddif2(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swdownc2(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swdownc2(:,:),grid%ts_buf_size*grid%max_ts_locs)

      ts_buf(:,:) = grid%ts_swddnic2(:,:)
      CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_swddnic2(:,:),grid%ts_buf_size*grid%max_ts_locs)
   END IF

   ts_buf(:,:) = grid%ts_tsk(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tsk(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_tslb(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tslb(:,:),grid%ts_buf_size*grid%max_ts_locs)

   DEALLOCATE(ts_buf)

   IF ( wrf_dm_on_monitor() ) THEN

      iunit = get_unused_unit()
      IF ( iunit <= 0 ) THEN
         CALL wrf_error_fatal3("<stdin>",791,&
'Error in write_ts: could not find a free Fortran unit.')
      END IF

      DO i=1,grid%ntsloc_domain

         ix = grid%itsloc(i)
         iy = grid%jtsloc(i)

         OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(i)), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            IF ( model_config_rec%process_time_series == 1 ) THEN
               WRITE(UNIT=iunit,FMT='(i2,f13.6,i5,i5,i5,1x,14(f13.5,1x))')  &
                                 grid%id, grid%ts_hour(n,i),                &
                                 grid%id_tsloc(i), ix, iy,                  &
                                 grid%ts_t(n,i),                            &
                                 grid%ts_q(n,i),                            &
                                 grid%ts_u(n,i),                            &
                                 grid%ts_v(n,i),                            &
                                 grid%ts_psfc(n,i),                         &
                                 grid%ts_glw(n,i),                          &
                                 grid%ts_gsw(n,i),                          &
                                 grid%ts_hfx(n,i),                          &
                                 grid%ts_lh(n,i),                           &
                                 grid%ts_tsk(n,i),                          &
                                 grid%ts_tslb(n,i),                         &
                                 grid%ts_rainc(n,i),                        &
                                 grid%ts_rainnc(n,i),                       &
                                 grid%ts_clw(n,i)
            ELSE
               
               WRITE(UNIT=iunit,FMT='(i2,f13.6,i5,i5,i5,1x,49(f13.5,1x))')  &
                                 grid%id, grid%ts_hour(n,i),                &
                                 grid%id_tsloc(i), ix, iy,                  &
                                 grid%ts_t(n,i),                            &
                                 grid%ts_q(n,i),                            &
                                 grid%ts_u(n,i),                            &
                                 grid%ts_v(n,i),                            &
                                 grid%ts_psfc(n,i),                         &
                                 grid%ts_glw(n,i),                          &
                                 grid%ts_gsw(n,i),                          &
                                 grid%ts_hfx(n,i),                          &
                                 grid%ts_lh(n,i),                           &
                                 grid%ts_tsk(n,i),                          &
                                 grid%ts_tslb(n,i),                         &
                                 grid%ts_rainc(n,i),                        &
                                 grid%ts_rainnc(n,i),                       &
                                 grid%ts_clw(n,i),                          &
                                 grid%ts_cldfrac2d(n,i),                    &
                                 grid%ts_wvp(n,i),                          &
                                 grid%ts_lwp(n,i),                          &
                                 grid%ts_iwp(n,i),                          &
                                 grid%ts_swp(n,i),                          &
                                 grid%ts_wp_sum(n,i),                       &
                                 grid%ts_lwp_tot(n,i),                      &
                                 grid%ts_iwp_tot(n,i),                      &
                                 grid%ts_wp_tot_sum(n,i),                   &
                                 grid%ts_re_qc(n,i),                        &
                                 grid%ts_re_qi(n,i),                        &
                                 grid%ts_re_qs(n,i),                        &
                                 grid%ts_re_qc_tot(n,i),                    &
                                 grid%ts_re_qi_tot(n,i),                    &
                                 grid%ts_tau_qc(n,i),                       &
                                 grid%ts_tau_qi(n,i),                       &
                                 grid%ts_tau_qs(n,i),                       &
                                 grid%ts_tau_qc_tot(n,i),                   &
                                 grid%ts_tau_qi_tot(n,i),                   &
                                 grid%ts_cbaseht(n,i),                      &
                                 grid%ts_ctopht(n,i),                       &
                                 grid%ts_cbaseht_tot(n,i),                  &
                                 grid%ts_ctopht_tot(n,i),                   &
                                 grid%ts_clrnidx(n,i),                      &
                                 grid%ts_sza(n,i),                          &
                                 grid%ts_swdown(n,i),                       &
                                 grid%ts_swddni(n,i),                       &
                                 grid%ts_swddif(n,i),                       &
                                 grid%ts_swdownc(n,i),                      &
                                 grid%ts_swddnic(n,i),                      &
                                 grid%ts_swdown2(n,i),                      &
                                 grid%ts_swddni2(n,i),                      &
                                 grid%ts_swddif2(n,i),                      &
                                 grid%ts_swdownc2(n,i),                     &
                                 grid%ts_swddnic2(n,i)
            END IF
         END DO
         CLOSE(UNIT=iunit)
         
         
         profile_format = '(f13.6,1x,000(f13.5,1x))'
         k= LEN_TRIM(profile_format)
         WRITE(profile_format(12:14),'(I3.3)') grid%max_ts_level

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",888,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         WRITE(ts_profile_filename,'(A)') TRIM(grid%nametsloc(grid%id_tsloc(i)))//'.d00.TS'
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-4:k-3),'(I2.2)') grid%id
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'UU'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)           &
                              grid%ts_hour(n,i),                      &
                              grid%ts_u_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",910,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'VV'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             & 
                              grid%ts_v_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",929,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'WW'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             & 
                              grid%ts_w_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",948,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'PH'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             &
                              grid%ts_gph_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)
         
         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",968,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'TH'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)   &
                              grid%ts_hour(n,i),              &
                              grid%ts_th_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit) 
       
         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",988,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'QV'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             &
                              grid%ts_qv_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)
       
         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",1008,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'PR'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             &
                              grid%ts_p_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)


      END DO

   END IF

   grid%next_ts_time = 1

END SUBROUTINE write_ts


SUBROUTINE calc_p8w(grid, ix, iy, p8w, k_start, k_end)

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: ix, iy, k_start, k_end
   REAL, DIMENSION(k_start:k_end), INTENT(OUT) :: p8w

   
   INTEGER :: k
   REAL    :: z0, z1, z2, w1, w2 
   REAL, DIMENSION(k_start:k_end)   :: z_at_w
   REAL, DIMENSION(k_start:k_end-1) :: z


   DO k = k_start, k_end
      z_at_w(k) = (grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy))/g
   END DO

   DO k = k_start, k_end-1
      z(k) = 0.5*(z_at_w(k) + z_at_w(k+1))
   END DO

   DO k = k_start+1, k_end-1
      p8w(k) = grid%fnm(k)*(grid%p(ix,k,iy)+grid%pb(ix,k,iy)) + &
               grid%fnp(k)*(grid%p(ix,k-1,iy)+grid%pb(ix,k-1,iy))
   END DO

   z0 = z_at_w(k_start)
   z1 = z(k_start)
   z2 = z(k_start+1)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_start) = w1*(grid%p(ix,k_start,iy)+grid%pb(ix,k_start,iy)) + &
                  w2*(grid%p(ix,k_start+1,iy)+grid%pb(ix,k_start+1,iy))

   z0 = z_at_w(k_end)
   z1 = z(k_end-1)
   z2 = z(k_end-2)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_end) = exp(w1*log(grid%p(ix,k_end-1,iy)+grid%pb(ix,k_end-1,iy)) + &
                    w2*log(grid%p(ix,k_end-2,iy)+grid%pb(ix,k_end-2,iy)))

END SUBROUTINE calc_p8w
