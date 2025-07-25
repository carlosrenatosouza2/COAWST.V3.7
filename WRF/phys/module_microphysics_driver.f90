
























MODULE module_microphysics_driver
CONTAINS

SUBROUTINE microphysics_driver(                                          &
                       th, rho, pi_phy, p                                &
                      ,ht, dz8w, p8w, dt,dx,dy                           &
                      ,mp_physics, spec_zone                             &
                      ,specified, channel_switch                         &
                      ,warm_rain                                         &
                      ,t8w                                               &
                      ,chem_opt, progn                                   &
                      ,cldfra, cldfra_old, exch_h, nsource               &
                      ,qlsink, precr, preci, precs, precg                &
                      ,xland,snowh,itimestep                             &
                      ,f_ice_phy,f_rain_phy,f_rimef_phy                  &
                      ,lowlyr,sr, id                                     &
                      ,ids,ide, jds,jde, kds,kde                         &
                      ,ims,ime, jms,jme, kms,kme                         &
                      ,ips,ipe, jps,jpe, kps,kpe                         &
                      ,i_start,i_end,j_start,j_end,kts,kte               &
                      ,num_tiles, naer                                   &
                      ,irrigation,sf_surf_irr_scheme, irr_daily_amount   &   
                      ,irr_start_hour,irr_num_hours,julian_in            &
                      ,irr_start_julianday,irr_end_julianday             &
                      ,irr_freq,irr_ph,irr_rand_field                    &
                      ,gmt,xtime                                         &

                      
                      ,dlf,dlf2,t_phy,p_hyd,p8w_hyd,tke_pbl,z_at_w,qfx   &
                      ,rliq,turbtype3d,smaw3d,wsedl3d,cldfra_old_mp      &
                      ,cldfra_mp,cldfra_mp_all,lradius,iradius           &
                      ,cldfrai,cldfral,cldfra_conv                       &
                      ,alt                                               &
                      ,accum_mode,aitken_mode,coarse_mode                &
                      ,icwmrsh3d,icwmrdp3d,shfrc3d,cmfmc3d,cmfmc2_3d     &
                      ,config_flags,fnm,fnp,rh_old_mp,lcd_old_mp         &

                      ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr   &
                      ,qic_curr,qip_curr,qid_curr &
                      ,qnic_curr,qnip_curr,qnid_curr &
                      ,qndrop_curr,qni_curr,qh_curr,qnh_curr             &
                      ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr      &
                      ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr      &
                      ,qnwfa_curr,qnifa_curr                             & 
                      ,f_qnwfa,f_qnifa                                   & 
                      ,qvolg_curr,qvolh_curr                             &
                      ,qir_curr,qib_curr                                 & 
                      ,qi2_curr,qni2_curr,qir2_curr,qib2_curr            & 
                      ,qvoli_curr,qaoli_curr                             & 
                      ,qvoli2_curr,qaoli2_curr                           & 
                      ,qi3_curr,qni3_curr,qvoli3_curr,qaoli3_curr        & 
                      ,effr_curr,ice_effr_curr,tot_effr_curr             &
                      ,qic_effr_curr,qip_effr_curr,qid_effr_curr         &             
                      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni      &
                      ,f_qns,f_qnr,f_qng,f_qnc,f_qnn,f_qh,f_qnh          &
                      ,            f_qzr,f_qzi,f_qzs,f_qzg,f_qzh         &
                      ,f_qvolg,f_qvolh                                   &
                      ,f_qic,f_qip,f_qid &
                      ,f_qnic,f_qnip,f_qnid &
                      ,f_qir,f_qib                                       & 
                      ,f_qi2,f_qni2,f_qir2,f_qib2                        & 
                      ,f_qvoli,f_qaoli                                   & 
                      ,f_qvoli2,f_qaoli2                                 & 
                      ,f_qi3,f_qni3,f_qvoli3,f_qaoli3                    & 
                      ,f_effr,f_ice_effr,f_tot_effr                      &
                      ,f_qic_effr,f_qip_effr,f_qid_effr                  &                 
                      ,cu_used                                           &
                      ,qrcuten, qscuten, qicuten, qccuten                &
                      ,qt_curr,f_qt                                      &
                      ,mp_restart_state,tbpvs_state,tbpvs0_state         & 
                      ,hail,ice2                                         & 

                      ,phys_tot, physc, physe, physd, physs, physm, physf& 
                      ,acphys_tot, acphysc, acphyse, acphysd  & 
                      ,acphyss, acphysm, acphysf              & 

                      ,re_cloud_gsfc, re_rain_gsfc, re_ice_gsfc          &
                      ,re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc       & 
                      ,precr3d, preci3d, precs3d, precg3d, prech3d       &


                      ,u,v,w,z                                          &   
                      ,rainnc,    rainncv                                &
                      ,snownc,    snowncv                                &
                      ,hailnc,    hailncv                                &
                      ,graupelnc, graupelncv                             &
                      ,qnwfa2d, qnifa2d                                  & 
                      ,refl_10cm                                         & 
                      ,vmi3d                                             & 
                      ,di3d                                              & 
                      ,rhopo3d                                           & 
                      ,phii3d                                            & 
                      ,vmi3d_2                                           & 
                      ,di3d_2                                            & 
                      ,rhopo3d_2                                         & 
                      ,phii3d_2                                          & 
                      ,vmi3d_3                                           & 
                      ,di3d_3                                            & 
                      ,rhopo3d_3                                         & 
                      ,phii3d_3                                          & 
                      ,itype                                             & 
                      ,itype_2                                           & 
                      ,itype_3                                           & 



                      ,ri_curr                                           &
                      ,diagflag,   do_radar_ref                          &
                      ,ke_diag                                           &
                      ,re_cloud, re_ice, re_snow                         & 
                      ,has_reqc, has_reqi, has_reqs                      & 
                      ,ccn_conc                                          & 
                      ,scalar,num_scalar                                   &
                      ,kext_ql,kext_qs,kext_qg            &
                      ,kext_qh,kext_qa                         &
                      ,kext_qic,kext_qid,kext_qip         &
                      ,kext_ft_qic,kext_ft_qid,kext_ft_qip         &
                      ,kext_ft_qs,kext_ft_qg            &
                      ,height,tempc &
                      ,TH_OLD                                            &
                      ,QV_OLD                                            &
                      ,xlat,xlong,ivgtyp                                 &
                      ,qrimef_curr,f_qrimef                              &
                      ,aercu_opt                                         &
                      ,sbmradar,num_sbmradar                             &
                      ,sbm_diagnostics                                   &
                      ,aerocu,aercu_fct,no_src_types_cu                  &
                      ,PBL,EFCG,EFIG,EFSG,WACT,CCN1_GS,CCN2_GS           &
                      ,CCN3_GS,CCN4_GS,CCN5_GS,CCN6_GS,CCN7_GS           &
                      ,NR_CU,QR_CU,NS_CU,QS_CU,CU_UAF,mskf_refl_10cm     &
                                                                         )


   USE module_state_description, ONLY :                                  &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME    &
                    ,WSM6SCHEME, ETAMPNEW, FER_MP_HIRES, THOMPSON, THOMPSONAERO, FAST_KHAIN_LYNN_SHPUND, MORR_TWO_MOMENT     &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, NSSL_2MOM, NSSL_2MOMCCN, NSSL_2MOMG  &
                    ,NSSL_1MOM,NSSL_1MOMLFO, FER_MP_HIRES_ADVECT & 
                    ,WSM7SCHEME, WDM7SCHEME &
                    ,NUWRF4ICESCHEME &
                    ,MILBRANDT2MOM , CAMMGMPSCHEME,FULL_KHAIN_LYNN, P3_1CATEGORY, P3_1CATEGORY_NC, P3_2CATEGORY, MORR_TM_AERO, JENSEN_ISHMAEL,SPRINKLER 

  USE module_dm, ONLY : &
                 local_communicator, mytask,  wrf_dm_min_real, wrf_dm_max_real


   USE module_model_constants
   USE module_wrf_error
   USE module_configure, only: grid_config_rec_type



   USE module_mp_kessler
   USE module_mp_lin
   USE module_mp_sbu_ylin
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_wsm7
   USE module_mp_etanew
   USE module_mp_fer_hires
   USE module_mp_thompson
   USE module_mp_full_sbm
   USE module_mp_fast_sbm
   USE module_mp_gsfcgce
   USE module_mp_gsfcgce_4ice_nuwrf, only: gsfcgce_4ice_nuwrf
   USE module_mp_morr_two_moment
   USE module_mp_p3
   USE module_mp_jensen_ishmael
   USE module_mp_morr_two_moment_aero
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_wdm7
   USE module_mp_milbrandt2mom
   USE module_mp_cammgmp_driver, ONLY: CAMMGMP 

   USE module_mp_nssl_2mom

   USE module_mp_HWRF
   USE module_mixactivate, only: prescribe_aerosol_mixactivate


   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep
   USE module_irrigation


   
   
   
   
   
   
   
   
   
   

   


   IMPLICIT NONE











































































































































  INTEGER,parameter :: iunit=6
  INTEGER :: mpi_error_code=1

   TYPE(grid_config_rec_type),  INTENT(IN   ) , OPTIONAL   :: config_flags
   INTEGER,    INTENT(IN   )    :: mp_physics
   LOGICAL,    INTENT(IN   )    :: specified
   INTEGER, OPTIONAL, INTENT(IN   )    :: chem_opt, progn
   INTEGER, OPTIONAL, INTENT(IN   )    :: hail, ice2 

   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme,num_scalar
   INTEGER,      INTENT(IN   )    ::     num_sbmradar
   INTEGER,      INTENT(IN   )    ::     sbm_diagnostics
   INTEGER, OPTIONAL, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe
   INTEGER,      INTENT(IN   )    ::                         kts,kte
   INTEGER,      INTENT(IN   )    ::     itimestep,num_tiles,spec_zone
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT) ::                                         th



   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                             &
                                                                 rho, &
                                                                dz8w, &
                                                                 p8w, &
                                                              pi_phy, &
                                                                   p

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT), OPTIONAL ::                        phys_tot,  &
                                                           physc,     &
                                                           physe,     &
                                                           physd,     &
                                                           physs,     &
                                                           physm,     &
                                                           physf,     &
                                                           acphys_tot,  &
                                                           acphysc,     &
                                                           acphyse,     &
                                                           acphysd,     &
                                                           acphyss,     &
                                                           acphysm,     &
                                                           acphysf



  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT), OPTIONAL ::                     re_cloud_gsfc,   &
                                                       re_rain_gsfc,    &
                                                       re_ice_gsfc,     &
                                                       re_snow_gsfc,    &
                                                       re_graupel_gsfc, &
                                                       re_hail_gsfc
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         OPTIONAL,          &
         INTENT(INOUT) ::   &
         precr3d, & 
         preci3d, & 
         precs3d, & 
         precg3d, & 
         prech3d    


    REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),INTENT(INOUT), OPTIONAL :: th_old,qv_old
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT), OPTIONAL   :: scalar
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_sbmradar),INTENT(INOUT) :: sbmradar
    INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN), OPTIONAL::   IVGTYP
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN), OPTIONAL    :: XLAT, XLONG



   REAL,INTENT(IN), OPTIONAL ::accum_mode,aitken_mode,coarse_mode  

   REAL , DIMENSION( kms:kme ) ,                                      &
        INTENT(IN   ) , OPTIONAL ::                                        fnm,  & 
                                                                fnp     

   REAL, DIMENSION( ims:ime, jms:jme ),                               &
        INTENT(IN), OPTIONAL ::                                                 &
                                                                 qfx, &    
                                                                 rliq      
 
 
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(IN), OPTIONAL ::                                                   &
                                                                 dlf, &    
                                                                dlf2, &    
                                                               t_phy, &    
                                                               p_hyd, &    
                                                             p8w_hyd, &    
                                                              z_at_w, &    
                                                             tke_pbl, &    
                                                          turbtype3d, &    
                                                              smaw3d, &    
                                                                 alt, &    
                                                           icwmrsh3d, &    
                                                           icwmrdp3d, &    
                                                             shfrc3d, &    
                                                             cmfmc3d, &    
                                                           cmfmc2_3d       

 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(INOUT) , OPTIONAL ::                                                &
                                                       cldfra_old_mp, &    
                                                           rh_old_mp, &    
                                                          lcd_old_mp       


REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(INOUT) , OPTIONAL::                                                 &
                                                            wsedl3d, &    
                                                          cldfra_mp, &    
                                                      cldfra_mp_all, &    
                                                            cldfrai, &    
                                                            cldfral, &    
                                                            lradius, &    
                                                            iradius, &    
                                                        cldfra_conv 




   REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY


   REAL, OPTIONAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &



         qlsink, & 
         precr, & 
         preci, & 
         precs, & 
         precg    



   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN), OPTIONAL   :: SNOWH

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(OUT)   :: SR

   REAL, INTENT(IN   ) :: dt,dx,dy

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR




   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: refl_10cm
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: vmi3d,di3d,rhopo3d,    & 
                                                                         vmi3d_2,di3d_2,rhopo3d_2,  & 
                                                                         phii3d,         & 
                                                                         phii3d_2,       & 
                                                                         vmi3d_3,di3d_3,rhopo3d_3,  & 
                                                                         phii3d_3,       & 
                                                                         itype,itype_2,itype_3             
   LOGICAL,  OPTIONAL,   INTENT(IN   )    :: channel_switch
   REAL, OPTIONAL,  INTENT(INOUT   ) :: naer  
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) , OPTIONAL :: qnwfa2d, qnifa2d
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                  u,v,w, z, t8w                                       &
                 ,cldfra, cldfra_old, exch_h                      &
                 ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr &
                 ,qt_curr,qndrop_curr,qni_curr,qh_curr,qnh_curr   &
                 ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr    &
                 ,qic_curr,qip_curr,qid_curr &
                 ,qnic_curr,qnip_curr,qnid_curr &
                 ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr    &
                 ,qir_curr,qib_curr                               & 
                 ,qi2_curr,qni2_curr,qir2_curr,qib2_curr          & 
                 ,qvoli_curr,qaoli_curr                           & 
                 ,qvoli2_curr,qaoli2_curr                         & 
                 ,qi3_curr,qni3_curr,qvoli3_curr,qaoli3_curr      & 
                 ,effr_curr,ice_effr_curr,tot_effr_curr           &
                 ,qic_effr_curr,qip_effr_curr,qid_effr_curr           &
                 ,kext_ql,kext_qs,kext_qg          &
                 ,kext_qh,kext_qa                       &
                 ,kext_qic,kext_qip,kext_qid,tempc,height      &
                 ,kext_ft_qic,kext_ft_qip,kext_ft_qid &
                 ,kext_ft_qs,kext_ft_qg                           &
                 ,qnwfa_curr,qnifa_curr                           & 
                 ,qvolg_curr,qvolh_curr, qrimef_curr

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN) :: qrcuten, qscuten, qicuten, qccuten
   INTEGER, INTENT(IN), optional ::     cu_used


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) :: ri_curr


   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                   &
         OPTIONAL,                                                &
         INTENT(OUT ) ::                                          &
                  nsource


   REAL, DIMENSION( ims:ime , jms:jme ),                          &
         INTENT(INOUT),                                           &
         OPTIONAL   ::                                            &
                                                           RAINNC &
                                                         ,RAINNCV &
                                                          ,SNOWNC &
                                                         ,SNOWNCV &
                                                       ,GRAUPELNC &
                                                      ,GRAUPELNCV &
                                                          ,HAILNC &
                                                          ,HAILNCV
                                                          



   INTEGER,OPTIONAL,INTENT(IN   )    ::                        id

   REAL , DIMENSION( ims:ime , jms:jme ) , OPTIONAL ,             &
         INTENT(IN)   ::                                       ht

   REAL, DIMENSION (:), OPTIONAL, INTENT(INOUT) :: mp_restart_state &
                                         ,tbpvs_state,tbpvs0_state


   LOGICAL, OPTIONAL :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt    &
                       ,f_qns,f_qnr,f_qng,f_qnn,f_qnc,f_qh,f_qnh,f_qzr       &
                      ,f_effr,f_ice_effr,f_tot_effr &
                       ,f_qic_effr,f_qip_effr,f_qid_effr &
                      ,f_qic,f_qip,f_qid &
                      ,f_qnic,f_qnip,f_qnid                                  &
                       ,f_qzi,f_qzs,f_qzg,f_qzh,f_qvolg,f_qvolh              &
                       ,f_qrimef                                             &
                       ,f_qir,f_qib                                          & 
                       ,f_qi2,f_qni2,f_qir2,f_qib2                           & 
                       ,f_qvoli,f_qaoli                                      & 
                       ,f_qvoli2,f_qaoli2                                    & 
                       ,f_qi3,f_qni3,f_qvoli3,f_qaoli3                       & 
                       ,f_qnwfa, f_qnifa                         


   LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
   INTEGER, OPTIONAL, INTENT(IN) :: ke_diag 
   REAL, INTENT(IN) :: ccn_conc 
   INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) ::  & 
                 re_cloud, re_ice, re_snow
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs

  INTEGER,           INTENT(IN   )    :: aercu_opt
  INTEGER, OPTIONAL, INTENT(IN   )    :: PBL
  INTEGER,           INTENT(IN   )    :: no_src_types_cu
  REAL,              INTENT(IN   )    :: aercu_fct
  REAL,    OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types_cu), INTENT(INOUT) & 
                                      :: aerocu
  REAL,    OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) &
                                      :: EFCG,           &
                                         EFIG,           &
                                         EFSG,           &
                                         WACT,           &
                                         CCN1_GS,        &
                                         CCN2_GS,        &
                                         CCN3_GS,        &
                                         CCN4_GS,        &
                                         CCN5_GS,        &
                                         CCN6_GS,        &
                                         CCN7_GS,        &
                                         NR_CU,          &
                                         QR_CU,          &
                                         NS_CU,          &
                                         QS_CU
   REAL,   OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(INOUT) &
                                      :: CU_UAF
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: mskf_refl_10cm





   INTEGER :: i,j,k,its,ite,jts,jte,ij,sz,n
   LOGICAL :: channel
   LOGICAL :: nssl_progn = .false.
   REAL    :: z0, z1, z2, w1, w2

   integer, parameter :: ntot = 50
   real :: wmin, wmax
   integer :: ierr


   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN):: IRRIGATION 
   REAL,  OPTIONAL, INTENT(IN)::  irr_daily_amount, julian_in, xtime, gmt
   INTEGER, OPTIONAL, INTENT(IN ):: sf_surf_irr_scheme, irr_start_hour, irr_num_hours, &
                                    irr_start_julianday,irr_end_julianday,irr_freq,irr_ph
   REAL, PARAMETER    :: PI_GRECO=3.14159
   INTEGER  :: end_hour,a,b,xt24,irr_day,timing
   REAL :: constants_irrigation,tloc,irr_start,phase
   INTEGER, OPTIONAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: irr_rand_field






   channel = .FALSE.
   IF ( PRESENT ( channel_switch ) ) channel = channel_switch

   if (mp_physics .eq. 0) return
   IF( specified ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF


   IF ( .false. ) THEN
      wmax = maxval( w(ips:ipe,kps:kpe,jps:jpe) )
      wmin = minval( w(ips:ipe,kps:kpe,jps:jpe) )
      wmax = wrf_dm_max_real ( wmax )
      wmin = wrf_dm_min_real ( wmin )
      WRITE( wrf_err_message , * ) 'microphysics_driver: GLOBAL w max/min = ', wmax, wmin
      CALL wrf_message ( wrf_err_message )
   ENDIF


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, i,j,k,n )

   DO ij = 1 , num_tiles
       IF (channel) THEN
         its = max(i_start(ij),ids)
         ite = min(i_end(ij),ide-1)
       ELSE
         its = max(i_start(ij),ids+sz)
         ite = min(i_end(ij),ide-1-sz)
       ENDIF
       jts = max(j_start(ij),jds+sz)
       jte = min(j_end(ij),jde-1-sz)

       sf_surf_irr: SELECT CASE(sf_surf_irr_scheme)
            CASE(SPRINKLER)
             CALL sprinkler_irrigation(  julian_in                    & 
     &          ,irrigation, irr_daily_amount,rho,dz8w                & 
     &          ,irr_start_hour,irr_num_hours,irr_start_julianday     & 
     &          ,irr_end_julianday,irr_freq,irr_ph,qr_curr            &
     &          ,gmt,xtime,dt,irr_rand_field                          &
     &          ,ids,ide, jds,jde                                     & 
     &          ,ims,ime, jms,jme, kms,kme                            & 
     &          ,its,ite, jts,jte               )
       END SELECT sf_surf_irr




       IF( PRESENT(qlsink) ) qlsink(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precr ) ) precr(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(preci ) ) preci(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precs ) ) precs(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precg ) ) precg(its:ite,kts:kte,jts:jte)  = 0.


       IF( PRESENT(chem_opt) .AND. PRESENT(progn) ) THEN
       
       
       IF ( mp_physics==NSSL_2MOMCCN .or. mp_physics==NSSL_2MOM .or. mp_physics==NSSL_2MOMG ) THEN
         IF ( progn > 0 ) THEN
          IF ( .not. (chem_opt == 0 .or. chem_opt == 401) ) nssl_progn = .true.
         ELSE
           nssl_progn = .false. 
         ENDIF
       ENDIF
       
       
       IF( (chem_opt==0 .OR. chem_opt==401) .AND. progn==1 .AND. (mp_physics==LINSCHEME  .OR. mp_physics==MORR_TWO_MOMENT)) THEN
          IF( PRESENT( QNDROP_CURR ) ) THEN
             CALL wrf_debug ( 100 , 'microphysics_driver: calling prescribe_aerosol_mixactivate' )

             call prescribe_aerosol_mixactivate (               &
                  id, itimestep, dt, naer,                      &
                  ccn_conc, chem_opt,                           & 
                  rho, th, pi_phy, w, cldfra, cldfra_old,       &
                  z, dz8w, p8w, t8w, exch_h,                    &
                  qv_curr, qc_curr, qi_curr, qndrop_curr,       &
                  nsource,                                      &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  F_QC=f_qc, F_QI=f_qi                          )
          END IF
       ELSEIF ( (chem_opt==0 .OR. chem_opt==401) .AND. progn==1 .AND. (mp_physics==NSSL_2MOMCCN .or.      &
                 mp_physics==NSSL_2MOM .or. mp_physics==NSSL_2MOMG)) THEN

       ELSEIF ( progn==1 .AND. mp_physics/=LINSCHEME .AND. mp_physics/=MORR_TWO_MOMENT &
                .AND. mp_physics/=NSSL_2MOM .AND. mp_physics/=NSSL_2MOMCCN .AND. mp_physics/=NSSL_2MOMG ) THEN
             call wrf_error_fatal3("<stdin>",758,&
             "SETTINGS ERROR: Prognostic cloud droplet number can only be used with the mp_physics=LINSCHEME or MORRISON or NSSL_2MOM.")
       END IF
       END IF


     micro_select: SELECT CASE(mp_physics)

        CASE (KESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling kessler' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT( QC_CURR ) .AND.  &
                                           PRESENT( QR_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                                           PRESENT( Z       ))  THEN
               CALL kessler(                                        &
                  T=th                                              &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,RHO=rho, PII=pi_phy,DT_IN=dt, Z=z, XLV=xlv, CP=cp &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0,RHOWATER=rhowater           &
                 ,DZ8W=dz8w                                         &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",787,&
'arguments not present for calling kessler' )
             ENDIF

        CASE (THOMPSONAERO)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling thompson' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR )   .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR )   .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR )   .AND.  &
                  PRESENT( QNR_CURR) .AND. PRESENT ( QNI_CURR)   .AND.  &
                  PRESENT( QNC_CURR) .AND. PRESENT ( QNWFA_CURR) .AND.  &
                  PRESENT( QNIFA_CURR).AND.PRESENT ( QNWFA2D)    .AND.  &
                  PRESENT( QNIFA2D)                              .AND.  &
                  PRESENT( SNOWNC)   .AND. PRESENT ( SNOWNCV)    .AND.  &
                  PRESENT( GRAUPELNC).AND. PRESENT ( GRAUPELNCV) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL mp_gt_driver(                          &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     NI=qni_curr,                        &
                     NR=qnr_curr,                        &
                     NC=qnc_curr,                        &
                     NWFA=qnwfa_curr,                    &
                     NIFA=qnifa_curr,                    &
                     NWFA2D=qnwfa2d,                     &
                     NIFA2D=qnifa2d,                     &
                     TH=th,                              &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DT_IN=dt,                           &
                     ITIMESTEP=itimestep,                &
                     RAINNC=RAINNC,                      &
                     RAINNCV=RAINNCV,                    &
                     SNOWNC=SNOWNC,                      &
                     SNOWNCV=SNOWNCV,                    &
                     GRAUPELNC=GRAUPELNC,                &
                     GRAUPELNCV=GRAUPELNCV,              &
                     SR=SR,                              &
                     REFL_10CM=refl_10cm,                &
                     diagflag=diagflag,                  &
                     ke_diag = ke_diag,                  &
                     do_radar_ref=do_radar_ref,          &
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & 
                     has_reqi=has_reqi,                  & 
                     has_reqs=has_reqs,                  & 
                 IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                 IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                 ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
             ELSE
                CALL wrf_error_fatal3("<stdin>",845,&
'arguments not present for calling thompson_et_al' )
             ENDIF

        CASE (THOMPSON)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling thompson' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QNR_CURR) .AND. PRESENT ( QNI_CURR) .AND.  &


                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) ) THEN
             CALL mp_gt_driver(                          &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     NI=qni_curr,                        &
                     NR=qnr_curr,                        &
                     TH=th,                              &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DT_IN=dt,                           &
                     ITIMESTEP=itimestep,                &
                     RAINNC=RAINNC,                      &
                     RAINNCV=RAINNCV,                    &
                     SNOWNC=SNOWNC,                      &
                     SNOWNCV=SNOWNCV,                    &
                     GRAUPELNC=GRAUPELNC,                &
                     GRAUPELNCV=GRAUPELNCV,              &
                     SR=SR,                              &
                     REFL_10CM=refl_10cm,                &
                     diagflag=diagflag,                  &
                     ke_diag = ke_diag,                  &
                     do_radar_ref=do_radar_ref,          &
                     re_cloud=re_cloud,                  & 
                     re_ice=re_ice,                      & 
                     re_snow=re_snow,                    & 
                     has_reqc=has_reqc,                  & 
                     has_reqi=has_reqi,                  & 
                     has_reqs=has_reqs,                  & 
                 IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                 IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                 ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
             ELSE
                CALL wrf_error_fatal3("<stdin>",895,&
'arguments not present for calling thompson_et_al' )
             ENDIF
       CASE (FAST_KHAIN_LYNN_SHPUND)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbm' )
               CALL fast_sbm(W=w,U=u,V=v,TH_OLD=th_old              &
                 ,CHEM_new=scalar,N_CHEM=num_scalar                 &
                 ,ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p                     &
                 ,PI_PHY=pi_phy,TH_PHY=th                           &
                 ,xland=xland,domain_id=id                          &
                 ,ivgtyp=ivgtyp                                     &
                 ,xlat=xlat                                         &
                 ,xlong=xlong                                       &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QV_OLD=qv_old                                     &
                 ,QNC=qnc_curr                                      &
                 ,QNR=qnr_curr                                      &
                 ,QNI=qni_curr                                      &
                 ,QNS=qns_curr                                      &
                 ,QNG=qng_curr                                      &
                 ,QNA=qnn_curr                                      &
                 ,sbmradar=sbmradar,num_sbmradar=num_sbmradar       &
                 ,sbm_diagnostics=sbm_diagnostics                   &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diagflag=diagflag                                 &  
                 ,RAINNC=rainnc                                     &
                 ,RAINNCV=rainncv                                   &
                 ,SNOWNC=snownc                                     &
                 ,SNOWNCV=snowncv                                   &
                 ,GRAUPELNC=graupelnc                               &
                 ,GRAUPELNCV=graupelncv                             &
                 ,SR=sr                                             &
                                                                    )


       CASE (FULL_KHAIN_LYNN)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbm' )
               CALL sbm(W=w,U=u,V=v,TH_OLD=th_old          &
                 ,CHEM_new=scalar,N_CHEM=num_scalar                     &
                 ,ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,xland=xland                                       &
                 ,ivgtyp=ivgtyp                                      &
                 ,xlat=xlat                                        &
                 ,xlong=xlong                                        &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QIP=qip_curr                                        &
                 ,QIC=qic_curr                                        &
                 ,QID=qid_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,QV_OLD=qv_old                                     &
                 ,QNC=qnc_curr                                      &
                 ,QNR=qnr_curr                                      &
                 ,QNIP=qnip_curr                                      &
                 ,QNIC=qnic_curr                                      &
                 ,QNID=qnid_curr                                      &
                 ,QNS=qns_curr                                      &
                 ,QNG=qng_curr                                      &
                 ,QNH=qng_curr                                      &
                 ,QNA=qnn_curr                                      &
                 ,EFFR=effr_curr                                  &
                 ,ICE_EFFR=ice_effr_curr                                  &
                 ,TOT_EFFR=tot_effr_curr                                  &
                 ,QIC_EFFR=qic_effr_curr                                  &
                 ,QIP_EFFR=qip_effr_curr                                  &
                 ,QID_EFFR=qid_effr_curr                                  &
                 ,height=height                                        &
                 ,tempc=tempc                                         &
                 ,kext_ql=kext_ql                                       &
                 ,kext_qs=kext_qs                                       &
                 ,kext_qg=kext_qg                                       &
                 ,kext_qh=kext_qh                                       &
                 ,kext_qa=kext_qa                                       &
                 ,kext_qic=kext_qic                                       &
                 ,kext_qip=kext_qip                                       &
                 ,kext_qid=kext_qid                                       &
                 ,kext_ft_qic=kext_ft_qic                                       &
                 ,kext_ft_qip=kext_ft_qip                                       &
                 ,kext_ft_qid=kext_ft_qid                                       &
                 ,kext_ft_qs=kext_ft_qs                                       &
                 ,kext_ft_qg=kext_ft_qg                                       &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,REFL_10CM=refl_10cm                 &  
                 ,diagflag=diagflag                   &  
                 ,do_radar_ref=do_radar_ref           &  
                 ,RAINNC=rainnc                       &
                 ,RAINNCV=rainncv                     &
                 ,SNOWNC=snownc                       &
                 ,SNOWNCV=snowncv                     &
                 ,GRAUPELNC=graupelnc                 &
                 ,GRAUPELNCV=graupelncv               &
                 ,HAILNC=hailnc                       &
                 ,HAILNCV=hailncv                     &
                 ,SR=sr                               &
                                                      )


     CASE (JENSEN_ISHMAEL)
        CALL wrf_debug(100, 'microphysics_driver: jensen_ishmael ')
        IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. &
             PRESENT (QI_CURR).AND. PRESENT (QNI_CURR) .AND. &
             PRESENT (QVOLI_CURR) .AND. PRESENT (QAOLI_CURR) .AND. &
             PRESENT (QI2_CURR).AND. PRESENT (QNI2_CURR) .AND. &
             PRESENT (QVOLI2_CURR) .AND. PRESENT (QAOLI2_CURR) .AND. &
             PRESENT (QI3_CURR).AND. PRESENT (QNI3_CURR) .AND. &
             PRESENT (QVOLI3_CURR).AND. PRESENT (QAOLI3_CURR)) THEN
             CALL mp_jensen_ishmael(             &
             ITIMESTEP=itimestep,                &  
             DT_IN=dt,                           &  
             P=p,                                &  
             DZ=dz8w,                            &  
             TH=th,                              &  
             QV=qv_curr,                         &  
             QC=qc_curr,                         &  
             QR=qr_curr,                         &  
             NR=qnr_curr,                        &  
             QI1=qi_curr,                        &  
             NI1=qni_curr,                       &  
             AI1=qvoli_curr,                     &  
             CI1=qaoli_curr,                     &  
             QI2=qi2_curr,                       &  
             NI2=qni2_curr,                      &  
             AI2=qvoli2_curr,                    &  
             CI2=qaoli2_curr,                    &  
             QI3=qi3_curr,                       &  
             NI3=qni3_curr,                      &  
             AI3=qvoli3_curr,                    &  
             CI3=qaoli3_curr,                    &  
             IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
             IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
             ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte, &
             RAINNC=RAINNC,                      &
             RAINNCV=RAINNCV,                    &
             SNOWNC=SNOWNC,                      &
             SNOWNCV=SNOWNCV,                    &
             diag_effc3d=re_cloud,               &
             diag_effi3d=re_ice,                 &
             diag_dbz3d=refl_10cm,               &
             diag_vmi3d_1=vmi3d,                 &
             diag_di3d_1=di3d,                   &
             diag_rhopo3d_1=rhopo3d,             &
             diag_phii3d_1=phii3d,               &
             diag_vmi3d_2=vmi3d_2,               &
             diag_di3d_2=di3d_2,                 &
             diag_rhopo3d_2=rhopo3d_2,           &
             diag_phii3d_2=phii3d_2,             &
             diag_vmi3d_3=vmi3d_3,               &
             diag_di3d_3=di3d_3,                 &
             diag_rhopo3d_3=rhopo3d_3,           &
             diag_phii3d_3=phii3d_3,             &
             diag_itype_1=itype,                 &
             diag_itype_2=itype_2,               &
             diag_itype_3=itype_3                &
             )
        ELSE
           Call wrf_error_fatal3("<stdin>",1065,&
'arguments not present for calling jensen_ishamel')
        ENDIF

    CASE (MORR_TWO_MOMENT)
         CALL wrf_debug(100, 'microphysics_driver: calling morrison two moment')
         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QS_CURR) .AND. PRESENT (QG_CURR) .AND. &
         PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QNS_CURR) .AND. PRESENT (QNI_CURR).AND. &
         PRESENT (QNR_CURR) .AND. PRESENT (QNG_CURR).AND. &
         PRESENT (QSCUTEN).AND. &
         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
         PRESENT ( W      )  ) THEN
         CALL mp_morr_two_moment(                            &
                     ITIMESTEP=itimestep,                &  
                     TH=th,                              &  
                     QV=qv_curr,                         &  
                     QC=qc_curr,                         &  
                     QR=qr_curr,                         &  
                     QI=qi_curr,                         &  
                     QS=qs_curr,                         &  
                     QG=qg_curr,                         &  
                     NI=qni_curr,                        &  
                     NS=qns_curr,                        &  
                     NR=qnr_curr,                        &  
                     NG=qng_curr,                        &  
                     RHO=rho,                            &  
                     PII=pi_phy,                         &  
                     P=p,                                &  
                     DT_IN=dt,                           &  
                     DZ=dz8w,                            &  
                     HT=ht,                              &  
                     W=w                                 &  
                    ,RAINNC=RAINNC                       &  
                    ,RAINNCV=RAINNCV                     &  
                    ,SNOWNC=SNOWNC                       &  
                    ,SNOWNCV=SNOWNCV                     &  
                    ,GRAUPELNC=GRAUPELNC                 &  
                    ,GRAUPELNCV=GRAUPELNCV               &  
                    ,SR=SR                               &  
                    ,REFL_10CM=refl_10cm                 &  
                    ,diagflag=diagflag                   &  
                    ,do_radar_ref=do_radar_ref           &  
                    ,qrcuten=qrcuten                     &  
                    ,qscuten=qscuten                     &  
                    ,qicuten=qicuten                     &  
                    ,F_QNDROP=f_qndrop                   &  
                 ,QNDROP=qndrop_curr                     &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,QLSINK=qlsink                                     & 
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   & 
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",1123,&
'arguments not present for calling morrison two moment')
        ENDIF

    CASE (P3_1CATEGORY)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')










         CALL mp_p3_wrapper_wrf(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=1                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                                                                    )




    CASE (P3_1CATEGORY_NC)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')










         CALL mp_p3_wrapper_wrf(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     nc_3d=qnc_curr,                   &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=1                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                                                                    )
    CASE (P3_2CATEGORY)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')










         CALL mp_p3_wrapper_wrf_2cat(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     QI2_3d=qi2_curr,                     &
                     QIR2_3d=qir2_curr,                    &
                     QNI2_3d=qni2_curr,                   &
                     QIB2_3d=qib2_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     nc_3d=qnc_curr,                   &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=2                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                 ,diag_vmi2_3d=vmi3d_2                                       &
                 ,diag_di2_3d=di3d_2                                         &
                 ,diag_rhopo2_3d=rhopo3d_2                                   &
                                                                    )


    CASE (MORR_TM_AERO)
         CALL wrf_debug(100, 'microphysics_driver: calling morrison two moment')
         CALL mp_morr_two_moment_aero(                            &
                     ITIMESTEP=itimestep,                &  
                     TH=th,                              &  
                     QV=qv_curr,                         &  
                     QC=qc_curr,                         &  
                     QR=qr_curr,                         &  
                     QI=qi_curr,                         &  
                     QS=qs_curr,                         &  
                     QG=qg_curr,                         &  
                     NI=qni_curr,                        &  
                     NS=qns_curr,                        &  
                     NR=qnr_curr,                        &  
                     NG=qng_curr,                        &  
                     NC=qnc_curr,                        &  
                     KZH=exch_h,                         &  
                     RHO=rho,                            &  
                     PII=pi_phy,                         &  
                     P=p,                                &  
                     DT_IN=dt,                           &  
                     DZ=dz8w,                            &  
                     HT=ht,                              &  
                     W=w                                 &  
                    ,RAINNC=RAINNC                       &  
                    ,RAINNCV=RAINNCV                     &  
                    ,SNOWNC=SNOWNC                       &  
                    ,SNOWNCV=SNOWNCV                     &  
                    ,GRAUPELNC=GRAUPELNC                 &  
                    ,GRAUPELNCV=GRAUPELNCV               &  
                    ,SR=SR                               &  
                    ,REFL_10CM=refl_10cm                 &  
                    ,MSKF_REFL_10CM=MSKF_REFL_10CM       &  
                    ,diagflag=diagflag                   &  
                    ,do_radar_ref=do_radar_ref           &  
                    ,qrcuten=qrcuten                     &  
                    ,qscuten=qscuten                     &  
                    ,qicuten=qicuten                     &  
                    ,F_QNDROP=f_qndrop                   &  
                 ,QNDROP=qndrop_curr                     &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,PBL=PBL                                & 
                 ,aerocu=aerocu                          & 
                 ,aercu_opt=aercu_opt                    & 
                 ,aercu_fct=aercu_fct                    & 
                 ,no_src_types_cu=no_src_types_cu        & 
                 ,EFCG=EFCG                              & 
                 ,EFIG=EFIG                              & 
                 ,EFSG=EFSG                              & 
                 ,WACT=WACT                              & 
                 ,CCN1_GS=CCN1_GS                        & 
                 ,CCN2_GS=CCN2_GS                        & 
                 ,CCN3_GS=CCN3_GS                        & 
                 ,CCN4_GS=CCN4_GS                        & 
                 ,CCN5_GS=CCN5_GS                        & 
                 ,CCN6_GS=CCN6_GS                        & 
                 ,CCN7_GS=CCN7_GS                        & 
                 ,NR_CU=NR_CU                            & 
                 ,QR_CU=QR_CU                            & 
                 ,NS_CU=NS_CU                            & 
                 ,QS_CU=QS_CU                            & 
                 ,CU_UAF=CU_UAF                          & 
                 ,QLSINK=qlsink                                     & 
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   & 
                                                                    )

    CASE (MILBRANDT2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling milbrandt2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  ) THEN


         CALL mp_milbrandt2mom_driver(                   &
                     ITIMESTEP=itimestep,                &
                     p8w=p8w,                              &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     QH=qh_curr,                         &
                     NC=qnc_curr,                        &
                     NR=qnr_curr,                        &
                     NI=qni_curr,                        &
                     NS=qns_curr,                        &
                     NG=qng_curr,                        &
                     NH=qnh_curr,                        &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT_IN=dt,                           &
                     DZ=dz8w,                            &
                     W=w,                                &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &

                     Zet      = refl_10cm,               & 
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",1402,&
'arguments not present for calling milbrandt2mom')
        ENDIF


















































    CASE (NSSL_1MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND.  &
             PRESENT (QR_CURR) .AND.  &
             PRESENT (QI_CURR) .AND.  &
             PRESENT (QS_CURR) .AND.  &
             PRESENT (QG_CURR) .AND.  &
             PRESENT (QH_CURR) .AND.  &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &






                     VHW=qvolg_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",1512,&
'arguments not present for calling nssl_1mom')
        ENDIF


    CASE (NSSL_1MOMLFO)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND.  &
             PRESENT (QR_CURR) .AND.  &
             PRESENT (QI_CURR) .AND.  &
             PRESENT (QS_CURR) .AND.  &
             PRESENT (QG_CURR) .AND.  &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",1561,&
'arguments not present for calling nssl_1momlfo')
        ENDIF

    CASE (NSSL_2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNdrop_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  .AND.         &
             PRESENT (QVOLH_CURR) .AND. F_QVOLH ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &
 
                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     CHL=qnh_curr,                       &
                     VHW=qvolg_curr,                     &
                     VHL=qvolh_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     nssl_progn=nssl_progn,              &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  
                     qscuten=qscuten,                    &  
                     qicuten=qicuten,                    &  
                     qccuten=qccuten,                    &  
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & 
                     has_reqi=has_reqi,                  & 
                     has_reqs=has_reqs,                  & 
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )

        ELSE
           Call wrf_error_fatal3("<stdin>",1638,&
'arguments not present for calling nssl_2mom')
        ENDIF

    CASE (NSSL_2MOMG)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNdrop_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
 
                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     VHW=qvolg_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     nssl_progn=nssl_progn,              &
                      diagflag = diagflag,               &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  
                     qscuten=qscuten,                    &  
                     qicuten=qicuten,                    &  
                     qccuten=qccuten,                    &  
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & 
                     has_reqi=has_reqi,                  & 
                     has_reqs=has_reqs,                  & 
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )

        ELSE
           Call wrf_error_fatal3("<stdin>",1709,&
'arguments not present for calling nssl_2momg')
        ENDIF

    CASE (NSSL_2MOMCCN)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl_2momccn')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNDROP_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  .AND.         &
             PRESENT (QVOLH_CURR) .AND. F_QVOLH  .AND.         &
             PRESENT( QNN_CURR )                          ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &

                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     CHL=qnh_curr,                       &
                     VHW=qvolg_curr,                     &
                     VHL=qvolh_curr,                     &
                     cn=qnn_curr,                        &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     nssl_progn=nssl_progn,              &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  
                     qscuten=qscuten,                    &  
                     qicuten=qicuten,                    &  
                     qccuten=qccuten,                    &  
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & 
                     has_reqi=has_reqi,                  & 
                     has_reqs=has_reqs,                  & 
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal3("<stdin>",1787,&
'arguments not present for calling nssl_2momccn')
        ENDIF

        CASE (GSFCGCESCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling GSFCGCE' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( HAIL    ) .AND. PRESENT ( ICE2    ) .AND.  &
                  PRESENT( Z       ) .AND. PRESENT ( W       )  ) THEN
               CALL gsfcgce(                                        &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G                          &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,ITIMESTEP=itimestep                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv ,SR=sr             &
                 ,GRAUPELNC=graupelnc ,GRAUPELNCV=graupelncv        &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,F_QG=f_qg                                         &
                 ,QG=qg_curr                                        &
                 ,IHAIL=hail, ICE2=ice2                             &
                                                                    )







             ELSE
                CALL wrf_error_fatal3("<stdin>",1831,&
'arguments not present for calling GSFCGCE' )
             ENDIF

        CASE (NUWRF4ICESCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling NUWRF4ICE' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( HAIL    ) .AND. PRESENT ( ICE2    ) .AND.  &
                  PRESENT( Z       ) .AND. PRESENT ( W       )  ) THEN
               CALL gsfcgce_4ice_nuwrf(                             &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QH=qh_curr                                        &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G, W=w                     &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,ITIMESTEP=itimestep, XLAND=xland, DX=dx           &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv ,SR=sr             &
                 ,GRAUPELNC=graupelnc ,GRAUPELNCV=graupelncv        &
                 ,HAILNC=hailnc, HAILNCV=hailncv                    &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,F_QG=f_qg                                         &
                 ,QG=qg_curr                                        &

                                                                       

                 ,PHYSC=physc, PHYSE=physe, PHYSD=physd             &
                 ,PHYSS=physs, PHYSM=physm, PHYSF=physf             &
                 ,ACPHYSC=acphysc, ACPHYSE=acphyse, ACPHYSD=acphysd  &
                 ,ACPHYSS=acphyss, ACPHYSM=acphysm, ACPHYSF=acphysf  &
                 ,RE_CLOUD_GSFC=re_cloud_gsfc                       &
                 ,RE_RAIN_GSFC=re_rain_gsfc                         &
                 ,RE_ICE_GSFC=re_ice_gsfc                           &
                 ,RE_SNOW_GSFC=re_snow_gsfc                         &
                 ,RE_GRAUPEL_GSFC=re_graupel_gsfc                   &
                 ,RE_HAIL_GSFC=re_hail_gsfc                         &
                 ,PRECR3D=precr3d,PRECI3D=preci3d,PRECS3D=precs3d   &
                 ,PRECG3D=precg3d,PRECH3D=prech3d                   &

                                                                    )

               do j=jts,jte
                  do k=kts,kte
                     do i=its,ite
                        
                        phys_tot(i,k,j) = physc(i,k,j) + physe(i,k,j) + &
                             physd(i,k,j) + physs(i,k,j) + physm(i,k,j) + &
                             physf(i,k,j)
                        
                        acphys_tot(i,k,j) = acphysc(i,k,j) + acphyse(i,k,j) + &
                             acphysd(i,k,j) + acphyss(i,k,j) + acphysm(i,k,j) + &
                             acphysf(i,k,j)
                        
                     end do
                  end do
               end do

             ELSE
                CALL wrf_error_fatal3("<stdin>",1903,&
'arguments not present for calling NUWRF4ICESFCGCE' )
             ENDIF

        CASE (LINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling lin_et_al' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( Z       ) ) THEN
               CALL lin_et_al(                                      &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QLSINK=qlsink                                     &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G,  CP=cp                  &
                 ,RAIR=r_d, RVAPOR=R_v                              &
                 ,XLS=xls, XLV=xlv, XLF=xlf                         &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0                             &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv                    &
                 ,GRAUPELNC=graupelnc, GRAUPELNCV=graupelncv, SR=sr &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   &
                 ,F_QG=f_qg, F_QNDROP=f_qndrop                      &
                 ,QG=qg_curr                                        &
                 ,QNDROP=qndrop_curr                                &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1944,&
'arguments not present for calling lin_et_al' )
             ENDIF

       CASE (SBU_YLINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbu_ylin' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RI_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( Z       ) ) THEN
               CALL sbu_ylin(                                       &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,RI3D=ri_curr                                      &

                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w                                  &






                 ,RAINNC=rainnc, RAINNCV=rainncv                    &


                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &





                                                                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1986,&
'arguments not present for calling sbu_ylin' )
             ENDIF


        CASE (WSM3SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm3' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( W       )                            ) THEN
             CALL wsm3(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QCI=qc_curr                                       &
                 ,QRS=qr_curr                                       &
                 ,W=w,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w              &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2023,&
'arguments not present for calling wsm3' )
             ENDIF

        CASE (WSM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm5(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2064,&
'arguments not present for calling wsm5' )
             ENDIF

        CASE (WSM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm6(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2107,&
'arguments not present for calling wsm6' )
             ENDIF

        CASE (WSM7SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm7' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm7(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,HAIL=hailnc ,HAILNCV=hailncv                      &
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2153,&
'arguments not present for calling wsm7' )
             ENDIF

        CASE (WDM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT( QNN_CURR ) .AND.  &
                  PRESENT ( QNC_CURR ) .AND. PRESENT( QNR_CURR ).AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm5(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & 
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,ITIMESTEP=itimestep                               & 
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2199,&
'arguments not present for calling wdm5')
             ENDIF

       CASE (WDM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QNN_CURR ) .AND. PRESENT ( QNC_CURR ) .AND. &
                  PRESENT( QNR_CURR ).AND.                            &
                 PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm6(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & 
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,xland=xland                                       &  
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,ITIMESTEP=itimestep                               & 
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     & 
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
               CALL wrf_error_fatal3("<stdin>",2249,&
'arguments not present for calling wdm6')
             ENDIF

        CASE (WDM7SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm7' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR ) .AND.                            &
                  PRESENT( QNN_CURR ) .AND. PRESENT ( QNC_CURR ) .AND. &
                  PRESENT( QNR_CURR ).AND.                            &
                 PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm7(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & 
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,xland=xland                                       &  
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  
                 ,diagflag=diagflag                                 &  
                 ,do_radar_ref=do_radar_ref                         &  
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,HAIL=hailnc ,HAILNCV=hailncv                      &
                 ,ITIMESTEP=itimestep                               &
                 ,has_reqc=has_reqc                                 &  
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
               CALL wrf_error_fatal3("<stdin>",2302,&
'arguments not present for calling wdm7')
             ENDIF

        CASE (ETAMPNEW)    
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN
               CALL ETAMP_NEW(                                      &
                  ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QT=qt_curr                                        &
                 ,LOWLYR=LOWLYR,SR=SR                               &
                 ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY         &
                 ,F_RIMEF_PHY=F_RIMEF_PHY                           &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,MP_RESTART_STATE=mp_restart_state                 &
                 ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state &
                                                                    )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2333,&
'arguments not present for calling etampnew' )
             ENDIF
        CASE (FER_MP_HIRES)    
                            
                            
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN





                CALL FER_HIRES(                                      &
                   ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy, GID=id &
                  ,RAINNC=rainnc,RAINNCV=rainncv                     &
                  ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                  ,QV=qv_curr                                        &
                  ,QT=qt_curr                                        &
                  ,LOWLYR=LOWLYR,SR=SR                               &
                  ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY         &
                  ,F_RIMEF_PHY=F_RIMEF_PHY                           &
                  ,QC=qc_curr,QR=Qr_curr,QI=Qi_curr                  &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                  ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                     )

             ELSE
                CALL wrf_error_fatal3("<stdin>",2367,&
'arguments not present for calling fer_hires' )
             ENDIF

        CASE (FER_MP_HIRES_ADVECT)    
                            
                            
                            
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qi_curr ) .AND. &
                  PRESENT( qc_curr ) .and. PRESENT(qrimef_curr) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN





                CALL FER_HIRES_ADVECT(                               &
                   ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy, GID=id &
                  ,RAINNC=rainnc,RAINNCV=rainncv                     &
                  ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                  ,QV=qv_curr                                        &
                  ,LOWLYR=LOWLYR,SR=SR                               &
                  ,QC=qc_curr,QR=Qr_curr,QI=Qi_curr,QRIMEF=qrimef_curr    &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                  ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2400,&
'arguments not present for calling fer_hires' )
             ENDIF

          CASE (CAMMGMPSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling CAMMGMPSCHEME')
             IF ( PRESENT( z          ) .AND. PRESENT( ht          ) .AND. &
                  PRESENT( qs_curr    ) .AND.                              &
                  PRESENT( qv_curr    ) .AND. PRESENT( qc_curr     ) .AND. &
                  PRESENT( qi_curr    ) .AND. PRESENT( f_qc        ) .AND. &
                  PRESENT( qr_curr    ) .AND. PRESENT( qndrop_curr ) .AND. &                  
                  PRESENT( f_qi       ) .AND. PRESENT( qnc_curr    ) .AND. &
                  PRESENT( RAINNCV    ) .AND. PRESENT( SNOWNCV     ) .AND. &
                  PRESENT( qns_curr   ) .AND. PRESENT( qnr_curr    ) .AND. &
                  PRESENT( qni_curr   ) .AND. PRESENT( RAINNC      ) ) THEN
                  
                CALL CAMMGMP(ITIMESTEP=itimestep,DT=dt,P8W=p8w_hyd,P_HYD=p_hyd    &
                     ,T_PHY=t_phy,PI_PHY=pi_phy,Z_AT_W=z_at_w,QFX=qfx             &
                     ,TKE_PBL=tke_pbl,TURBTYPE3D=turbtype3d,SMAW3D=smaw3d     &
                     ,DLF3D=dlf,DLF2_3D=dlf2,RLIQ2D=rliq,Z_SEA_LEVEL=z            &
                     ,KVH3D=exch_h,HT=ht,ALT=alt,ACCUM_MODE=accum_mode            &
                     ,AITKEN_MODE=aitken_mode,COARSE_MODE=coarse_mode             &
                     ,ICWMRSH3D=icwmrsh3d,ICWMRDP3D=icwmrdp3d,SHFRC3D=shfrc3d     &
                     ,CMFMC3D=cmfmc3d,CMFMC2_3D=cmfmc2_3d                         &
                     ,CONFIG_FLAGS=config_flags,F_ICE_PHY=f_ice_phy               &
                     ,F_RAIN_PHY=f_rain_phy                                       &
                     ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde           &
                     ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme           &
                     ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte           &

                     ,TH=th,CLDFRA_OLD_MP=cldfra_old_mp,CLDFRA_MP=cldfra_mp       &
                     ,CLDFRA_MP_ALL=cldfra_mp_all,lradius=lradius,iradius=iradius &
                     ,CLDFRAI=cldfrai,CLDFRAL=cldfral                             &
                     ,CLDFRA_CONV=cldfra_conv,WSEDL3D=wsedl3d                     &
                     ,RAINNC=rainnc,RAINNCV=rainncv,SNOWNC=snownc,SNOWNCV=snowncv &
                     ,SR=sr,QV_CURR=qv_curr,QC_CURR=qc_curr,QI_CURR=qi_curr       &
                     ,QS_CURR=qs_curr,QR_CURR=qr_curr,NC3D=qnc_curr               &
                     ,NI3D=qni_curr,NS3D=qns_curr,NR3D=qnr_curr,QNDROP=qndrop_curr&
                     ,RH_OLD_MP=rh_old_mp,LCD_OLD_MP=lcd_old_mp                   &
                     ,XLAND=XLAND,SNOWH=SNOWH                                     &
                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2442,&
'arguments not present for calling CAMMGMP SCHEME' )
             ENDIF



      CASE DEFAULT

         WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', mp_physics
         CALL wrf_error_fatal3("<stdin>",2451,&
wrf_err_message )

      END SELECT micro_select

   ENDDO
   !$OMP END PARALLEL DO





















   CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )

   RETURN

   END SUBROUTINE microphysics_driver

END MODULE module_microphysics_driver
