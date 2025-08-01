






































module module_fr_fire_driver


use module_fr_fire_model
use module_fr_fire_phys, only : fire_params , init_fuel_cats
use module_fr_fire_util
use module_fr_fire_core, only: ignition_line_type
use module_fr_fire_atm, only: add_fire_tracer_emissions 

USE module_domain, only: domain
USE module_configure, only: grid_config_rec_type
use module_model_constants, only: reradius,    & 
                                  g,           & 
                                  pi2            

implicit none


private
public fire_driver_em,use_atm_vars

logical:: use_atm_vars=.true.   

logical:: fmoist_run, fmoist_interp, fire_run  

contains

subroutine fire_driver_em ( grid , config_flags                    & 
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w           &  
            )




    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_fuel_sub, halo_fire_tign_sub, halo_fire_wind_f_sub, &
halo_fire_wind_a_sub, halo_fire_ph_sub, halo_fire_zsf_sub, halo_fire_longlat_sub, &
halo_fire_phb_sub, halo_fire_z0_sub, halo_fire_lfn_sub, halo_fire_mfg_sub, &
halo_fire_mag_sub

    implicit none

    TYPE(domain) , TARGET :: grid                             
    TYPE (grid_config_rec_type) , INTENT(IN)  :: config_flags 
    integer, intent(in)::     fire_ifun_start,fire_ifun_end,tsteps 
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde,                              &
             ims,ime, kms,kme, jms,jme,                              &
             ips,ipe, kps,kpe, jps,jpe,                              &
             ifds,ifde, jfds,jfde,                                   &
             ifms,ifme, jfms,jfme,                                   &
             ifps,ifpe, jfps,jfpe 
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in), optional::rho,z_at_w,dz8w 


    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    TYPE(ignition_line_type), DIMENSION(fire_max_ignitions):: ignition_line
    integer::fire_ifun,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::need_lfn_update,restart
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    real:: unit_fxlong ,unit_fxlat
    type(fire_params)::fp
    real:: time_start, dt
    real:: moisture_time
    logical:: run_advance_moisture,run_fuel_moisture, moisture_initializing
    real::    dt_moisture







    
    fp%vx => grid%uf         
    fp%vy => grid%vf         
    fp%zsf => grid%zsf       
    fp%dzdxf => grid%dzdxf   
    fp%dzdyf => grid%dzdyf   
    fp%bbb => grid%bbb       
    fp%betafl => grid%betafl 
    fp%phiwc => grid%phiwc   
    fp%r_0 => grid%r_0       
    fp%fgip => grid%fgip     
    fp%ischap => grid%ischap 
    fp%iboros => grid%iboros 
    fp%fmc_g => grid%fmc_g   
            
    
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)

    
    call set_flags(config_flags)

    
    ir=grid%sr_x 
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart

    
    dt = grid%dt
    time_start = itimestep * dt

    

    
    fmoist_run    = config_flags%fmoist_run
    fmoist_interp = config_flags%fmoist_interp 
    if(fire_fmc_read.ne.0.and.fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')
    fire_run = .not. config_flags%fmoist_only

    
    moisture_time = time_start
    run_advance_moisture = .false. 
    run_fuel_moisture = .false. 
    moisture_initializing = fire_ifun_start < 3
    
    
    
    if(fmoist_run)then
        if(moisture_initializing)then
            if(fire_ifun_end>2)call crash('initialization must be run separately')
            grid%fmoist_lasttime=moisture_time 
            grid%fmoist_nexttime=moisture_time 
            call message('moisture initialization')
            run_advance_moisture = .true.
        else 
            if(config_flags%fmoist_freq > 0)then  
                if(mod(grid%itimestep,config_flags%fmoist_freq) .eq. 0)then
                    write(msg,'(a,i10,a,i10)')'moisture model runs because timestep ',grid%itimestep,' is a multiple of ',config_flags%fmoist_freq
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            else
                if(.not. moisture_time  < grid%fmoist_nexttime) then 
                    write(msg,'(a,f12.2,a)')'moisture model runs because time ',grid%fmoist_nexttime,'s has arrived'
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            endif
            if(run_advance_moisture)then 
                dt_moisture  = moisture_time - grid%fmoist_lasttime  
                grid%fmoist_lasttime = moisture_time
                if(config_flags%fmoist_freq > 0)then
                    write(msg,'(a,f12.2,a,i10,a)')'moisture time step is ',dt_moisture,'s running every ',config_flags%fmoist_freq,' steps'
                    call message(msg)
                else
                    grid%fmoist_nexttime = moisture_time + config_flags%fmoist_dt
                    write(msg,'(a,f12.2,a,f12.2,a)')'moisture time step is ',dt_moisture,'s next run at ',grid%fmoist_nexttime,'s'
                    call message(msg)
                endif
                if(fmoist_interp)then
                    call message('moisture interpolation to fuels will run because moisture model does')
                    run_fuel_moisture=.true.
                endif
            endif
        endif
    elseif(itimestep.eq.1.and.fmoist_interp)then
            call message('initializing, moisture interpolation to fuels will run from input data')
            run_fuel_moisture=.true.
    endif

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,i4)') &
       'fire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' test steps',tsteps
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call message(msg)

    do istep=0,tsteps 
      itimestep = grid%itimestep + istep 

      need_lfn_update=.false.
      do fire_ifun=fire_ifun_start,fire_ifun_end

        
        
        
        
        
        


       if(fire_run)then
        if(need_lfn_update)then







CALL HALO_FIRE_LFN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

        endif

        if(fire_ifun.eq.1)then







CALL HALO_FIRE_LONGLAT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )











CALL HALO_FIRE_PHB_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_FIRE_Z0_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        elseif(fire_ifun.eq.2)then







CALL HALO_FIRE_ZSF_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        elseif(fire_ifun.eq.3)then







CALL HALO_FIRE_WIND_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_FIRE_PH_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        elseif(fire_ifun.eq.4)then







CALL HALO_FIRE_WIND_F_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


            if(run_fuel_moisture)then
            






CALL HALO_FIRE_MFG_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

            endif

        elseif(fire_ifun.eq.6)then







CALL HALO_FIRE_TIGN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        endif
       endif

        
        
        call fire_driver_phys ( &
            fire_ifun,need_lfn_update,                  &
            ids,ide-1, kds,kde, jds,jde-1,                          &
            ims,ime, kms,kme, jms,jme,                          &
            ips,min(ipe,ide-1), kps,kpe, jps,min(jpe,jde-1),                          & 
            ifds,ifde-ir, jfds,jfde-jr,                    &
            ifms,ifme, jfms,jfme,                    &
            ifps,min(ifpe,ifde-ir), jfps,min(jfpe,jfde-jr),      &
            ir,jr,                                      & 
            grid%num_tiles,                             & 
            grid%i_start,min(grid%i_end,ide-1),                    &
            grid%j_start,min(grid%j_end,jde-1),                    &
            time_start,                                 &
            itimestep,restart,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            unit_fxlong,unit_fxlat,                           & 
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            config_flags%fire_wind_height,              &  
            fire_num_ignitions,                                & 
            fire_ignition_longlat,      &
            ignition_line,              &
            grid%u_2,grid%v_2,           &          
            grid%ph_2,grid%phb,               & 
            grid%z0,                        & 
            grid%ht,                        &                         
            grid%xlong,grid%xlat,                         & 
            grid%lfn, &
            grid%lfn_hist, &  
            config_flags%fire_is_real_perim,  & 
            grid%lfn_0,grid%lfn_1,grid%lfn_2,grid%lfn_s0,grid%lfn_s1,grid%lfn_s2,grid%lfn_s3,grid%flame_length,grid%ros_front, & 
            grid%tign_g,grid%fuel_frac,          & 
            grid%fire_area,                               & 
            grid%burnt_area_dt,                               & 
            lfn_out,                                      & 
            grid%avg_fuel_frac,                           & 
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & 
            grid%grnhfx_fu,grid%grnqfx_fu, & 
            grid%uah,grid%vah,                          &
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & 
            grid%ros,                                   & 
            grid%fxlong,grid%fxlat,grid%fz0,                           &       
            grid%nfuel_cat,                               & 
            grid%fuel_time,                      & 
            config_flags%nfmc,         & 
            run_advance_moisture,run_fuel_moisture,dt_moisture,     &    
            config_flags%fmep_decay_tlag,                               & 
            grid%rainc, grid%rainnc,                       & 
            grid%t2, grid%q2, grid%psfc,               & 
            grid%rain_old,                   & 
            grid%t2_old, grid%q2_old, grid%psfc_old,   & 
            grid%rh_fire,                    & 
            grid%fmc_gc,                      & 
            grid%fmep,                      & 
            grid%fmc_equi,                      & 
            grid%fmc_lag,                      & 
            fp%fmc_g , &                             
            fp, &
            grid, & 
            ids,ide,jds,jde,kds,kde, & 
            ims,ime,jms,jme,kms,kme, & 
            ips,ipe,jps,jpe,kps,kpe, &
            config_flags & 
        )

   
          if(fire_run)then
            if(fire_ifun.eq.2)then







CALL HALO_FIRE_FUEL_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


                call message('halo exchange on lfn width 2')






CALL HALO_FIRE_LFN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

          endif
          if(run_fuel_moisture)then
            if(fire_ifun.eq.3)then
                     






CALL HALO_FIRE_MAG_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

                endif
            endif
            endif


     if(fire_ifun.eq.6 .AND. config_flags%tracer_opt.eq.3)then
       call add_fire_tracer_emissions(config_flags%tracer_opt,grid%dt,grid%dx,grid%dy, &
                 ifms,ifme,jfms,jfme,    &
                 ifps,ifpe,jfps,jfpe,    &
                 ids,ide,kds,kde,jds,jde,          &
                 ims,ime,kms,kme,jms,jme,          &
                 ips,ipe,kps,kpe,jps,jpe,          &
                 rho,dz8w,                         &
                 grid%burnt_area_dt,grid%fgip, &
                 grid%tracer,config_flags%fire_tracer_smoke)
     endif

      enddo
    enddo
    if(tsteps>0)call crash('fire_driver_em: test run of uncoupled fire model completed')

end subroutine fire_driver_em






subroutine fire_driver_phys (ifun,need_lfn_update,    &
    ids,ide, kds,kde, jds,jde,                    & 
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & 
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & 
    ir,jr,                                        & 
    num_tiles,i_start,i_end,j_start,j_end,        & 
    time_start,                                   & 
    itimestep,restart,ifuelread,nfuel_cat0,dt,dx,dy,      & 
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & 
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    fire_wind_height,                             & 
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_line,                                &
    u,v,                                       & 
    ph,phb,                                       &
    z0,zs,                                        & 
    xlong,xlat,                                   &
    lfn,                                      &
    lfn_hist,                                      & 
    fire_is_real_perim,                            & 
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front, & 
    tign,fuel_frac,                           & 
    fire_area,                                    & 
    burnt_area_dt,                                    & 
    lfn_out,                                      & 
    avg_fuel_frac,                                &
    grnhfx,grnqfx,canhfx,canqfx,                  & 
    grnhfx_fu,grnqfx_fu,                 & 
    uah,vah,                                      & 
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & 
    ros,                                          &
    fxlong,fxlat,fz0,                                 & 
    nfuel_cat,                                    & 
    fuel_time,                                & 
    nfmc,                                     & 
    run_advance_moisture,run_fuel_moisture,dt_moisture,& 
    fmep_decay_tlag,                              & 
    rainc,rainnc,              & 
    t2, q2, psfc,               & 
    rain_old,                   & 
    t2_old, q2_old, psfc_old,   & 
    rh_fire,                    & 
    fmc_gc,                    &  
    fmep,                       &  
    fmc_equi,                    &  
    fmc_lag,                    &  
    fmc_g, &                          
    fp,                                           & 
    grid,                                         & 
    ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, &
    ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, &
    ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu, &
    config_flags & 
    )

USE module_dm, only:wrf_dm_maxval
USE module_domain, only: domain

implicit none

    type(domain) , target :: grid                             



integer, intent(in)::ifun,                        &
    ids,ide, kds,kde, jds,jde,                    & 
    ims,ime, kms,kme, jms,jme,                    & 
    ips,ipe, kps,kpe, jps,jpe,                    & 
    ifds, ifde, jfds, jfde,                       & 
    ifms, ifme, jfms, jfme,                       & 
    ifps, ifpe, jfps, jfpe,                       & 
    ir,jr,                                        & 
    nfmc,                                     & 
    itimestep,                                    & 
    ifuelread,                                    & 
                                                       
                                                       
                                                       
                                                       
    nfuel_cat0,                                   & 
    num_tiles                                       

integer, intent(in)::ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
                     ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
                     ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu   

type (grid_config_rec_type), intent(in)          :: config_flags 

logical, intent(in)::restart
    

logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  

real, intent(in):: &
    time_start,                                   & 
    dt,                                           & 
    dx,dy,                                        & 
    u_frame,v_frame,                              & 
    unit_fxlong,unit_fxlat,                       & 
    fire_crwn_hgt,                                & 
    fire_ext_grnd,                                & 
    fire_ext_crwn,                                & 
    fire_wind_height 


integer, intent(in):: num_ignitions                 
integer, intent(in):: ignition_longlat       
TYPE (ignition_line_type), DIMENSION(num_ignitions), intent(out):: ignition_line

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v, & 
                              ph, phb                      
real,intent(in),dimension(ims:ime, jms:jme)::   z0, &    
                                                zs       
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & 
    vah                                              

real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat 
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     
    lfn_hist,                                  &     
    lfn_out                                    

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front  

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    burnt_area_dt                                        
real, intent(out), dimension(ims:ime, jms:jme)::  &
   grnhfx_fu,grnqfx_fu                                        

real, intent(out), dimension(ims:ime, jms:jme):: &  
    avg_fuel_frac,                               &  
    grnhfx,                                      &  
    grnqfx,                                      &  
    canhfx,                                      &  
    canqfx                                         

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  
    fgrnhfx,                                      &  
    fgrnqfx,                                      &  
    fcanhfx,                                      &  
    fcanqfx,                                      &   
    ros                                             
    

logical, intent(in)::run_advance_moisture,run_fuel_moisture
real, intent(in)::dt_moisture
real, intent(in)::fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(out),dimension(ims:ime,jms:jme):: rh_fire
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi,fmc_lag
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fmc_g





real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat,fz0 
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   

type(fire_params),intent(inout)::fp
    

real :: dxf,dyf,latm, s
integer :: its,ite,jts,jte,kts,kte, &            
    ij,i,j,k,id,pid,ipe1,jpe1,ite1,jte1, &
    ifts,ifte,jfts,jfte                          
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex

    
  logical :: fire_is_real_perim




dxf=dx/ir
dyf=dy/jr



!$OMP CRITICAL(FIRE_DRIVER_CRIT)
write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',dxf,dyf
call message(msg)
write(msg,7001)'atm domain      ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory      ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch       ','ips',ips,ipe,jps,jpe
call message(msg)                    
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'fire patch      ','ifps',ifps,ifpe,jfps,jfpe
call message(msg)                    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)


call check_fmesh(ids,ide,ifds,ifde,ir,'id')           
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) 
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) 

!$OMP SINGLE

if(ifun.eq.1) then
   
   call init_fuel_cats(fmoist_run .or. fmoist_interp) 
endif
!$OMP END SINGLE


pid=0
if(fire_print_file.gt.0)then
    if(itimestep.le.fire_print_file.or.mod(itimestep,fire_print_file).eq.0)pid=itimestep 
endif

if(ifun.eq.3)then
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,1,0,0,u,'u')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,0,1,v,'v')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,1,0,ph,'ph')
endif


kts=kps
kte=kpe


ipe1=ifval(ipe.eq.ide,ipe+1,ipe)
jpe1=ifval(jpe.eq.jde,jpe+1,jpe)


!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ite1,jte1,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id = ifval(pid.ne.0,pid+ij*10000,0) 

    ignitions_done_tile(ij)=0

    
    its = i_start(ij)  
    ite = i_end(ij)    
    jts = j_start(ij)  
    jte = j_end(ij)    
    ifts= (its-ids)*ir+ifds       
    ifte= (ite-ids+1)*ir+ifds-1   
    jfts= (jts-jds)*jr+jfds       
    jfte= (jte-jds+1)*jr+jfds-1   
        

    ite1=ifval(ite.eq.ide,ite+1,ite)
    jte1=ifval(jte.eq.jde,jte+1,jte)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i3,1x,a,i7,1x,a,i3)')'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)


!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    if(run_advance_moisture)then
      if(ifun.eq.3)then 
      
      
          call message('advance_moisture start')
          call advance_moisture(    &
              itimestep.eq.1,             & 
              ims,ime,  jms,jme,          & 
              its,ite,  jts,jte,          & 
              nfmc,                       & 
              dt_moisture,                & 
              fmep_decay_tlag,            & 
              rainc, rainnc,              & 
              t2, q2, psfc,               & 
              rain_old,                   & 
              t2_old, q2_old, psfc_old,   & 
              rh_fire,                    & 
              fmc_gc,                     & 
              fmep,                       & 
              fmc_equi,                   &  
              fmc_lag                    &  
          )
          call message('advance_moisture end')
      endif
    endif
        
    if(fire_run)then

     if(ifun.eq.1)then   

      if(restart)then
          
          call message('restart - topo initialization skipped')

      else

















!!$OMP CRITICAL(FIRE_DRIVER_CRIT)

!!$OMP END CRITICAL(FIRE_DRIVER_CRIT)


        if(ignition_longlat .eq.0)then
            
            

            
            
            
            
            call set_ideal_coord( dxf,dyf, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                fxlong,fxlat          )
            call interpolate_z2fire(id,             & 
                ids,ide,jds,jde,                    &
                ims,ime,jms,jme,                    &
                ips,ipe,jps,jpe,                    &
                its,ite,jts,jte,                    &
                ifds,ifde,jfds,jfde,                &
                ifms,ifme,jfms,jfme,                &
                ifts,ifte,jfts,jfte,                &
                ir,jr,                              &
                z0,                                 &
                fz0,1)
            
            
            
            
            
        elseif(use_atm_vars)then
            
            

         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlat,'xlat',id)
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlong,'xlong',id)
        call interpolate_z2fire(id,                 & 
            ids,ide,  jds,jde,                    & 
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & 
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & 
            xlat,                                       & 
            fxlat,0)                                      

        call interpolate_z2fire(id,                 & 
            ids,ide,  jds,jde,                    & 
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & 
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & 
            xlong,                                       & 
            fxlong,0)                                      

        call interpolate_z2fire(id,                       & 
            ids,ide,  jds,jde,                            &
            ims,ime,  jms,jme,                            &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       &
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        &
            z0,                                           &
            fz0,1)

        endif

     endif

    elseif(ifun.eq.2)then  
               
        
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fp%zsf,'driver_phys:zsf')        

    elseif(ifun.eq.3)then  


      if(use_atm_vars)then                                  

        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,z0,'z0',id)
        call write_array_m3(its,ite1,kts,kde-1,jts,jte,ims,ime,kms,kme,jms,jme,u,'u_2',id)
        call write_array_m3(its,ite,kts,kde-1,jts,jte1,ims,ime,kms,kme,jms,jme,v,'v_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,ph,'ph_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,phb,'phb',id)
        call interpolate_atm2fire(id,                     & 
            fire_wind_height,                             & 
            ids,ide, kds,kde, jds,jde,                    & 
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe, jps,jpe,                             &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & 
            ifms, ifme, jfms, jfme,                       &
            ifps, ifpe, jfps, jfpe,                       & 
            ifts, ifte, jfts, jfte,                       &
            ir,jr,                                        & 
            u_frame, v_frame,                             & 
            u,v,                                          & 
            ph,phb,                                       &
            z0,zs,                                        & 
            uah,vah,                                      & 
            fp%vx,fp%vy,fz0)                                    

      endif

    
    
     
    elseif(ifun.eq.4)then

      

      if(run_fuel_moisture)then
        call message('fuel_moisture start')
        call fuel_moisture(                &
        id,                                     & 
        nfmc,                                &
        ids,ide, jds,jde,               & 
        ims,ime, jms,jme,           &
        ips,ipe,jps,jpe,                &
        its,ite,jts,jte,                     &
        ifds, ifde, jfds, jfde,         & 
        ifms, ifme, jfms, jfme,     &
        ifts,ifte,jfts,jfte,                 &
        ir,jr,                                   & 
        nfuel_cat,                         & 
        fmc_gc,                             & 
        fmc_g                                & 
        )
        call message('fuel_moisture end')
      endif 

     endif 

    endif 



    if(fire_run)then
      call fire_model (id,ifun,restart,need_lfn_update,  &
        run_fuel_moisture,                      & 
        num_ignitions,                          & 
        ifuelread,nfuel_cat0,                   & 
        ifds,ifde,jfds,jfde,                    & 
        ifms,ifme,jfms,jfme,                    & 
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & 
        time_start,dt,                          & 
        dxf,dyf,                                & 
        ignition_line,                          & 
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & 
        lfn,                                     & 
        lfn_hist,                                & 
        fire_is_real_perim,                      & 
        lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front,  & 
        lfn_out,tign,fuel_frac,                     & 
        fire_area,                              & 
        burnt_area_dt,                              & 
        fgrnhfx,fgrnqfx,                        & 
        ros,                                    & 
        nfuel_cat,                              & 
        fuel_time,                              & 
        fp,                                      & 
        grid,            &  
        ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
        ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
        ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  &
    )

    if(ifun.eq.6)then 
    
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fire_driver:fgrnqfx')
    
        
        if(use_atm_vars)then                                  
          call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fuel_frac,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            avg_fuel_frac)
          call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnhfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnhfx)

          call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnqfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnqfx)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
          write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',fire_atm_feedback
!$OMP end CRITICAL(FIRE_DRIVER_CRIT)
	  call message(msg)
          s = 1./(ir*jr)
          do j=jts,jte
            do i=its,ite
                
                grnhfx_fu(i,j)=grnhfx(i,j)*s
                grnqfx_fu(i,j)=grnqfx(i,j)*s
                
                avg_fuel_frac(i,j)=avg_fuel_frac(i,j)*s
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)*s
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)*s
                
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
          enddo

          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')
       endif

    endif 
  endif 
enddo 
!$OMP END PARALLEL DO

if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
endif

if (ifun.eq.3)then
    ignitions_done=ignitions_done_tile(1) 
    do i=1,ignitions_done
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
        write(msg,'(2(a,i4,1x))')'fire_driver_phys: checking ignition ',i,' of ',ignitions_done
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
        call message(msg)
        ignited_patch(i)=0
        do ij=1,num_tiles
            ignited_patch(i)=ignited_patch(i)+ignited_tile(i,ij)
        enddo
        call message('fire_driver_phys: checking ignitions, collect counts')
        call wrf_dm_maxval(ignited_patch(i),idex,jdex)
        call message('fire_driver_phys: collected')
        if(ignited_patch(i).eq.0)then
            call crash('fire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo

 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,1,0,0,uah,'uah')
 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,1,vah,'vah')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vx,'uf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vy,'vf')
    if(pid.gt.0)then
        call write_array_m(ips,ipe1,jps,jpe,ims,ime,jms,jme,uah,'uah',pid)
        call write_array_m(ips,ipe,jps,jpe1,ims,ime,jms,jme,vah,'vah',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe1,kds,kde+1,jps,jpe,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe,kds,kde+1,jps,jpe1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
endif

if(ifun.eq.5)then
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
endif

if(ifun.eq.6)then
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnhfx,'fgrnhfx')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnqfx,'fgrnqfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnhfx,'grnhfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnqfx,'grnqfx')
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
endif

end subroutine fire_driver_phys




subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)
    USE module_configure, only : grid_config_rec_type
    implicit none


    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    TYPE (ignition_line_type), DIMENSION(fire_max_ignitions), intent(out):: ignition_line 
    integer, intent(out)::fire_num_ignitions,fire_ignition_longlat
    real, intent(out)::unit_fxlong,unit_fxlat

    integer::i
    logical:: real,ideal
    real::lat_ctr,lon_ctr

    
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')
    
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')

    fire_ignition_longlat=0  
    if(ideal)then
       
       fire_ignition_longlat=0
       ignition_line(1)%start_x=config_flags%fire_ignition_start_x1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_y1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_x1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_y1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_x2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_y2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_x2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_y2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_x3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_y3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_x3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_y3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_x4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_y4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_x4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_y4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_x5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_y5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_x5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_y5
    endif
    if(real)then
        
       fire_ignition_longlat=1
       ignition_line(1)%start_x=config_flags%fire_ignition_start_lon1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_lat1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_lon1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_lat1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_lon2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_lat2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_lon2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_lat2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_lon3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_lat3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_lon3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_lat3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_lon4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_lat4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_lon4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_lat4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_lon5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_lat5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_lon5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_lat5
    endif
    
       ignition_line(1)%ros=config_flags%fire_ignition_ros1 
       ignition_line(1)%radius=config_flags%fire_ignition_radius1 
       ignition_line(1)%start_time=config_flags%fire_ignition_start_time1 
       ignition_line(1)%end_time=config_flags%fire_ignition_end_time1 
       ignition_line(2)%ros=config_flags%fire_ignition_ros2 
       ignition_line(2)%radius=config_flags%fire_ignition_radius2 
       ignition_line(2)%start_time=config_flags%fire_ignition_start_time2 
       ignition_line(2)%end_time=config_flags%fire_ignition_end_time2 
       ignition_line(3)%ros=config_flags%fire_ignition_ros3 
       ignition_line(3)%radius=config_flags%fire_ignition_radius3 
       ignition_line(3)%start_time=config_flags%fire_ignition_start_time3 
       ignition_line(3)%end_time=config_flags%fire_ignition_end_time3 
       ignition_line(4)%ros=config_flags%fire_ignition_ros4 
       ignition_line(4)%radius=config_flags%fire_ignition_radius4 
       ignition_line(4)%start_time=config_flags%fire_ignition_start_time4 
       ignition_line(4)%end_time=config_flags%fire_ignition_end_time4 
       ignition_line(5)%ros=config_flags%fire_ignition_ros5 
       ignition_line(5)%radius=config_flags%fire_ignition_radius5 
       ignition_line(5)%start_time=config_flags%fire_ignition_start_time5
       ignition_line(5)%end_time=config_flags%fire_ignition_end_time5

    
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            
            if(ignition_line(i)%radius.gt.0.)fire_num_ignitions=i
            
            if(ignition_line(i)%end_x.eq.0.)ignition_line(i)%end_x=ignition_line(i)%start_x
            if(ignition_line(i)%end_y.eq.0.)ignition_line(i)%end_y=ignition_line(i)%start_y
            if(ignition_line(i)%end_time.eq.0.)ignition_line(i)%end_time=ignition_line(i)%start_time
        enddo

    if(fire_ignition_longlat .eq. 0)then
       
       
       unit_fxlong=1.  
       unit_fxlat=1.
       
    else
       
       lat_ctr=config_flags%cen_lat
       lon_ctr=config_flags%cen_lon
       
       unit_fxlat=pi2/(360.*reradius)  
       unit_fxlong=cos(lat_ctr*pi2/360.)*unit_fxlat  
    endif

end subroutine fire_ignition_convert





subroutine interpolate_atm2fire(id,               & 
    fire_wind_height,                             & 
    ids,ide, kds,kde, jds,jde,                    & 
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & 
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & 
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & 
    u_frame, v_frame,                             & 
    u,v,                                          & 
    ph,phb,                                       &
    z0,zs,                                        &
    uah,vah,                                      &
    uf,vf,z0f)                                          
    
implicit none



integer, intent(in)::id                           
real, intent(in):: fire_wind_height                 
integer, intent(in)::                             &
    ids,ide, kds,kde, jds,jde,                    & 
    ims,ime, kms,kme, jms,jme,                    & 
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              & 
    ifds, ifde, jfds, jfde,                       & 
    ifms, ifme, jfms, jfme,                       & 
    ifps, ifpe, jfps, jfpe,                       & 
    ifts,ifte,jfts,jfte,                          & 
    ir,jr                                         
real, intent(in):: u_frame, v_frame                 
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
    u,v,                                          & 
    ph,phb                                          
real,intent(in),dimension(ims:ime,jms:jme)::&
    z0,                                           & 
    zs                                              
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & 
    vah                                              
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    uf,vf                                           
real,intent(in),dimension(ifms:ifme,jfms:jfme)::z0f 
    
    

character(len=256)::msg
real, dimension(its-2:ite+2,jts-2:jte+2):: ua,va   
real, dimension(its-2:ite+2,kds:kde,jts-2:jte+2):: altw,altub,altvb,hgtu,hgtv 
integer:: i,j,k,ifts1,ifte1,jfts1,jfte1,ite1,jte1
integer::itst,itet,jtst,jtet,itsu,iteu,jtsu,jteu,itsv,itev,jtsv,jtev
integer::kdmax,its1,jts1,ips1,jps1
integer::itsou,iteou,jtsou,jteou,itsov,iteov,jtsov,jteov
real:: ground,loght,loglast,logz0,logfwh,ht,zr
real::r_nan
integer::i_nan
equivalence (i_nan,r_nan)
real::fire_wind_height_local,z0fc
real::ust_d,wsf,wsf1,uf_temp,vf_temp
real,parameter::vk_kappa=0.4




i_nan=2147483647
ua=r_nan
va=r_nan
altw=r_nan
altub=r_nan
hgtu=r_nan
hgtv=r_nan


if(kds.ne.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,*)'WARNING: bottom index kds=',kds,' should be 1?'
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif




























    ite1=snode(ite,ide,1)
    jte1=snode(jte,jde,1)
    
    call print_3d_stats(its,ite1,kds,kde,jts,jte,ims,ime,kms,kme,jms,jme,u,'wind U in')
    call print_3d_stats(its,ite,kds,kde,jts,jte1,ims,ime,kms,kme,jms,jme,v,'wind V in')

    if(fire_print_msg.gt.0)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
       write(msg,'(a,f7.2,a)')'interpolate_atm2fire: log-interpolation of wind to',fire_wind_height,' m'
       call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    endif

    
       
    
    itst=ifval(ids.eq.its,its,its-1)
    itet=ifval(ide.eq.ite,ite,ite+1)
    jtst=ifval(jds.ge.jts,jts,jts-1)
    jtet=ifval(jde.eq.jte,jte,jte+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'atm input  ','tile',its,ite,jts,jte
  call message(msg)
  write(msg,7001)'altw       ','tile',itst,itet,jtst,jtet
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)

    kdmax=kde-1   

    do j = jtst,jtet
      do k=kds,kdmax+1
        do i = itst,itet       
          altw(i,k,j) = (ph(i,k,j)+phb(i,k,j))/g             
        enddo
      enddo
    enddo


    itsu=ifval(ids.eq.its,its+1,its)  
    iteu=ifval(ide.eq.ite,ite,ite+1)  
    jtsu=ifval(jds.ge.jts,jts,jts-1)
    jteu=ifval(jde.eq.jte,jte,jte+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'u interp at','tile',itsu,iteu,jtsu,jteu
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif

    do j = jtsu,jteu          
      do k=kds,kdmax+1
        do i = itsu,iteu       
          altub(i,k,j)= 0.5*(altw(i-1,k,j)+altw(i,k,j))      
        enddo
      enddo
      do k=kds,kdmax
        do i = itsu,iteu       
          hgtu(i,k,j) =  0.5*(altub(i,k,j)+altub(i,k+1,j)) - altub(i,kds,j)  
        enddo
      enddo
    enddo


    jtsv=ifval(jds.eq.jts,jts+1,jts)  
    jtev=ifval(jde.eq.jte,jte,jte+1)  
    itsv=ifval(ids.ge.its,its,its-1)
    itev=ifval(ide.eq.ite,ite,ite+1)

if(fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'v interp at','tile',itsv,itev,jtsv,jtev
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
    do j = jtsv,jtev          
      do k=kds,kdmax+1
        do i = itsv,itev       
          altvb(i,k,j)= 0.5*(altw(i,k,j-1)+altw(i,k,j))      
        enddo
      enddo
      do k=kds,kdmax
        do i = itsv,itev       
          hgtv(i,k,j) =  0.5*(altvb(i,k,j)+altvb(i,k+1,j)) - altvb(i,kds,j)  
        enddo
      enddo
    enddo

        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,altub,'altub',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,altvb,'altvb',id)
        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,hgtu,'hgtu',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,hgtv,'hgtv',id)

if (fire_lsm_zcoupling) then
  logfwh = log(fire_lsm_zcoupling_ref)
  fire_wind_height_local = fire_lsm_zcoupling_ref
else
  logfwh = log(fire_wind_height)
  fire_wind_height_local = fire_wind_height
endif

    

    do j = jtsu,jteu            

      do i = itsu,iteu       
        zr = 0.5*(z0(i,j)+z0(i-1,j)) 
        if(fire_wind_height_local > zr)then       
          do k=kds,kdmax
            ht = hgtu(i,k,j)      
            if( .not. ht < fire_wind_height_local) then 
              loght = log(ht)
              if(k.eq.kds)then               
                logz0 = log(zr)
                ua(i,j)= u(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           
                loglast=log(hgtu(i,k-1,j))
                ua(i,j)= u(i,k-1,j) + (u(i,k,j) - u(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 10
            endif
            if(k.eq.kdmax)then                 
              ua(i,j)=u(i,k,j) 
            endif
          enddo
10        continue
        else  
          ua(i,j)=0.
        endif
      enddo
    enddo

    do j = jtsu,jteu 
      ua(itsu-1,j)=ua(itsu,j)
    enddo


    

    do j = jtsv,jtev
      do i = itsv,itev
        zr = 0.5*(z0(i,j-1)+z0(i,j)) 
        if(fire_wind_height_local > zr)then       
          do k=kds,kdmax
            ht = hgtv(i,k,j)      
            if( .not. ht < fire_wind_height_local) then 
              loght = log(ht)
              if(k.eq.kds)then               
                logz0 = log(zr)
                va(i,j)= v(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           
                loglast=log(hgtv(i,k-1,j))
                va(i,j)= v(i,k-1,j) + (v(i,k,j) - v(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 11
            endif
            if(k.eq.kdmax)then                 
              va(i,j)=v(i,k,j) 
            endif
          enddo
11        continue
        else  
          va(i,j)=0.
        endif
      enddo
    enddo

    do i = itsv,itev 
      va(i,jtsv-1)=va(i,jtsv)
    enddo


    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

    call write_array_m(its,ite1,jts,jte,ims,ime,jms,jme,uah,'uah_n',id) 
    call write_array_m(its,ite,jts,jte1,ims,ime,jms,jme,vah,'vah_n',id)

    ips1 = ifval(ips.eq.ids,ips+1,ips)

    call continue_at_boundary(1,1,0., & 
       its-2,ite+2,jts-2,jte+2,                  &
       ids+1,ide,jds,jde, &     
       ips1,ipe,jps,jpe, &     
       itsu,iteu,jtsu,jteu, & 
       itsou,iteou,jtsou,jteou, & 
       ua)                           

    jps1 = ifval(jps.eq.jds,jps+1,jps)

    call continue_at_boundary(1,1,0., & 
       its-2,ite+2,jts-2,jte+2,                  & 
       ids,ide,jds+1,jde, &      
       ips,ipe,jps1,jpe, &        
       itsv,itev,jtsv,jtev, & 
       itsov,iteov,jtsov,jteov, & 
       va)                           


    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

        call write_array_m(itsou,iteou,jtsou,jteou,its-2,ite+2,jts-2,jte+2,ua,'ua',id)
        call write_array_m(itsov,iteov,jtsov,jteov,its-2,ite+2,jts-2,jte+2,va,'va',id)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)

     write(msg,12)'atm mesh wind U at',fire_wind_height,' m'
     call print_2d_stats(itsou,iteou,jtsou,jteou,its-2,ite+2,jts-2,jte+2,ua,msg)
     write(msg,12)'atm mesh wind V at',fire_wind_height,' m'
     call print_2d_stats(itsov,iteov,jtsov,jteov,its-2,ite+2,jts-2,jte+2,va,msg)
12  format(a,f6.2,a)
    call print_2d_stats(its,ite1,jts,jte,ims,ime,jms,jme,uah,'UAH')
    call print_2d_stats(its,ite,jts,jte1,ims,ime,jms,jme,vah,'VAH')


!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
































    
    
    
    
    

    call interpolate_2d(  &
        its-2,ite+2,jts-2,jte+2,                  & 
        itsou,iteou,jtsou,jteou,& 
        ifms,ifme,jfms,jfme,    & 
        ifts,ifte,jfts,jfte,& 
        ir,jr,                  & 
        real(ids),real(jds),ifds-0.5,jfds+(jr-1)*0.5, & 
        ua,                     & 
        uf)                      

    call interpolate_2d(  &
        its-2,ite+2,jts-2,jte+2,                  & 
        itsov,iteov,jtsov,jteov,& 
        ifms,ifme,jfms,jfme,    & 
        ifts,ifte,jfts,jfte,& 
        ir,jr,                  & 
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds-0.5, & 
        va,                     & 
        vf)                      


if (fire_lsm_zcoupling) then
  do j = jfts,jfte
    do i = ifts,ifte
      uf_temp=uf(i,j)
      vf_temp=vf(i,j)
      wsf=max(sqrt(uf_temp**2.+vf_temp**2.),0.1)
      z0fc=z0f(i,j)
      ust_d=wsf*vk_kappa/log(fire_lsm_zcoupling_ref/z0fc)
      wsf1=(ust_d/vk_kappa)*log((fire_wind_height+z0fc)/z0fc)
      uf(i,j)=wsf1*uf_temp/wsf
      vf(i,j)=wsf1*vf_temp/wsf
    enddo
  enddo
endif


call print_2d_stats_vec(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,vf,'fire wind (m/s)')

        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,'uf1',id)
        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,vf,'vf1',id)


return

end subroutine interpolate_atm2fire





subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)

implicit none

integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s

character(len=128)msg

if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_fire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call crash(msg)
endif
end subroutine check_fmesh




subroutine set_flags(config_flags)
implicit none
TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags




fire_print_msg          = config_flags%fire_print_msg
fire_print_file         = config_flags%fire_print_file
fuel_left_method        = config_flags%fire_fuel_left_method
fuel_left_irl           = config_flags%fire_fuel_left_irl
fuel_left_jrl           = config_flags%fire_fuel_left_jrl
fire_const_time         = config_flags%fire_const_time
fire_const_grnhfx       = config_flags%fire_const_grnhfx
fire_const_grnqfx       = config_flags%fire_const_grnqfx
fire_atm_feedback       = config_flags%fire_atm_feedback
boundary_guard          = config_flags%fire_boundary_guard
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
fire_advection          = config_flags%fire_advection
fire_lsm_reinit         = config_flags%fire_lsm_reinit
fire_lsm_reinit_iter    = config_flags%fire_lsm_reinit_iter 
fire_upwinding_reinit   = config_flags%fire_upwinding_reinit 
fire_lsm_band_ngp       = config_flags%fire_lsm_band_ngp
fire_lsm_zcoupling      = config_flags%fire_lsm_zcoupling
fire_lsm_zcoupling_ref  = config_flags%fire_lsm_zcoupling_ref 
fire_viscosity_bg       = config_flags%fire_viscosity_bg 
fire_viscosity_band     = config_flags%fire_viscosity_band 
fire_viscosity_ngp      = config_flags%fire_viscosity_ngp
fire_slope_factor       = config_flags%fire_slope_factor 
fire_fmc_read           = config_flags%fire_fmc_read




end subroutine set_flags

end module module_fr_fire_driver
