

















































MODULE module_mp_gsfcgce_4ice_nuwrf

   USE module_mp_radar

   INTEGER, PARAMETER, PRIVATE:: chunk = 16

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor


   PRIVATE  
   PUBLIC :: gsfcgce_4ice_nuwrf





   REAL,    PRIVATE ::          rd1,  rd2,   al,   cp


   REAL,    PRIVATE ::          c38, c358, c610, c149,             &
                               c879, c172, c409,  c76,             &
                               c218, c580, c141

   REAL,    PRIVATE ::           ag,   bg,   as,   bs,             &
                                 aw,   bw,  bgh,  bgq,             &
                                bsh,  bsq,  bwh,  bwq,             &
                                 ah,   bh,  bh3,  bhh,             &
                                bhh5, bhq,  bh3_2


   REAL,    PRIVATE ::          tnw,  tns,  tng, tnh,              &
                               roqs, roqg, roqr, roqh


   REAL,    PRIVATE ::           zrc,  zgc,  zsc, zhc, vrc,        &  
                                vrc0, vrc1, vrc2, vrc3,            &
                             	 vgc,  vsc, vhc

    REAL,    PRIVATE ::         zgc2, vgc2


   REAL,    PRIVATE ::              rn11a

   REAL,    PRIVATE ::          rn17, rn19b,                       &
                               	bnd3, rn23a,                   &
                               rn23b, rn30b,                       &
                               rn30c 


    REAL,    PRIVATE ::          alv,   alf,   als,    t0,   t00,     &
                                 avc,   afc,   asc,   esi,   rn1, rn2,     &
                                bnd2,   rn3,   rn4,   rn5,  rn50,     &
                                rn51,  rn52,  rn53,   rn6,  rn60,     &
                                rn61,  rn62,  rn63,   rn7,   rn8,     &
                                 rn9,  rn10, rn101, rn102, rn10a,     &
                               rn10b, rn10c,  rn11,  rn12,  rn14,     &
                                rn15, rn15a,  rn16, rn171, rn172,     &
                               rn17a, rn17b, rn17c,  rn18, rn18a,     &
                                rn19, rn191, rn192, rn19a,  rn20,     &
                               rn20a, rn20b,  rn30, rn30a,  rn21,     &
                               bnd21,  rn22,  rn23, rn231, rn232,     &
                                rn25,  rn31,  beta,  rn32,  rn33,     &
                               rn331, rn332,  rn34,  rn35,rnn30a,     &
                               rnn191,rnn192
   REAL,    PRIVATE, DIMENSION( 31 ) ::    rn12a, rn12b, rn13, rn25a


    REAL,    PRIVATE ::         hn9,  hn10,  hn10a, hn14,  hn15a, hn16,  &
                                hn17, hn17a, hn19,  hn19a, hn20,  hn20b  
    REAL,    PRIVATE ::         gn17,gn17a,gn17a2                                     


    REAL,    PRIVATE ::         draimax


  real, PRIVATE ::  xs,sno11,sno00,dsno11,dsno00,sexp11,sexp00,stt,   &
                stexp,sbase,tslopes,dsnomin,dsnomin4,slim

  real, PRIVATE ::  xg,grp11,grp00,dgrp11,dgrp00,gexp11,gexp00,gtt,   &
                gtexp,gbase,tslopeg,dgrpmin,dgrpmin4,glim

  real, PRIVATE :: hai00,hai11,htt0,htt1,haixp 


    REAL,    PRIVATE ::         ag2, bg2, bgh2, bgq2, roqg2, qrog2


    REAL,    PRIVATE ::         rn142, rn152, rn15a2, rn17a2, rn192_2


    REAL,    PRIVATE ::          ami50, ami40, ami100


   REAL,    PRIVATE, DIMENSION( 31 ) ::    BergCon1,  BergCon2,       &
                                           BergCon3,  BergCon4
  REAL, PRIVATE    :: cmin
  REAL, PRIVATE    :: cpi

   REAL,    PRIVATE, DIMENSION( 31 )  ::      aa1,  aa2
   DATA aa1/.7939e-7, .7841e-6, .3369e-5, .4336e-5, .5285e-5,         &
           .3728e-5, .1852e-5, .2991e-6, .4248e-6, .7434e-6,          &
           .1812e-5, .4394e-5, .9145e-5, .1725e-4, .3348e-4,          &
           .1725e-4, .9175e-5, .4412e-5, .2252e-5, .9115e-6,          &
           .4876e-6, .3473e-6, .4758e-6, .6306e-6, .8573e-6,          &
           .7868e-6, .7192e-6, .6513e-6, .5956e-6, .5333e-6,          &
           .4834e-6/
   DATA aa2/.4006, .4831, .5320, .5307, .5319,                        &
           .5249, .4888, .3894, .4047, .4318,                         &
           .4771, .5183, .5463, .5651, .5813,                         &
           .5655, .5478, .5203, .4906, .4447,                         &
           .4126, .3960, .4149, .4320, .4506,                         &
           .4483, .4460, .4433, .4413, .4382,                         &
           .4361/






      REAL ::     xnor, xnos, xnoh, xnog
      REAL ::     rhohail, rhograul











CONTAINS





  SUBROUTINE gsfcgce_4ice_nuwrf(   th                               &
                       ,qv, ql                                      &
                       ,qr, qi                                      &
                       ,qs, qh                                      & 
                       ,rho, pii, p, dt_in, z                       &
                       ,ht, dz8w, grav, w                           &
                       ,rhowater, rhosnow                           &
                       ,itimestep, xland, dx                        &
                       ,ids,ide, jds,jde, kds,kde                   & 
                       ,ims,ime, jms,jme, kms,kme                   & 
                       ,its,ite, jts,jte, kts,kte                   & 
                       ,rainnc, rainncv                             &
                       ,snownc, snowncv, sr                         &
                       ,graupelnc, graupelncv                       &
                       ,refl_10cm, diagflag, do_radar_ref           &
                       ,hailnc, hailncv                             & 
                       ,f_qg, qg                                    &
                       ,physc, physe, physd, physs, physm, physf    &
                       ,acphysc, acphyse, acphysd, acphyss, acphysm, acphysf &
                       ,re_cloud_gsfc, re_rain_gsfc, re_ice_gsfc    &
                       ,re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc & 
                       ,preci3d, precs3d, precg3d, prech3d, precr3d &

                                                                   )



  IMPLICIT NONE




  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte 
  INTEGER,      INTENT(IN   )    ::   itimestep
  
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                              ql, &
                                                              qr, &
                                                              qi, &
                                                              qs, &
                                                              qg, &
                                                              qh





  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &
                                                             rho, &
                                                             pii, &
                                                               p, &
                                                            dz8w, &
                                                               z, &
                                                               w

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                           physc, &
                                                           physe, &
                                                           physd, &
                                                           physs, &
                                                           physm, &
                                                           physf, &
                                                           acphysc, &
                                                           acphyse, &
                                                           acphysd, &
                                                           acphyss, &
                                                           acphysm, &
                                                           acphysf, &
                                                         preci3d, &
                                                         precs3d, &
                                                         precg3d, &
                                                         prech3d, &
                                                         precr3d

  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                               rainnc,    &
                                                       rainncv,   &
                                                       snownc,    &   
                                                       snowncv,   &
                                                       sr,        &
                                                       graupelnc, &
                                                       graupelncv,&
                                                       hailnc, &
                                                       hailncv


  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                               re_cloud_gsfc, &
                                                       re_rain_gsfc,  &
                                                       re_ice_gsfc,   &
                                                       re_snow_gsfc,  &
                                                       re_graupel_gsfc, &
                                                       re_hail_gsfc



  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::           &  
                                                       refl_10cm
  LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
  INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref


  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht

  REAL, INTENT(IN   ) ::                                   dt_in, &
                                                            grav, &
                                                        rhowater, &
                                                         rhosnow, &
                                                              dx 

  LOGICAL, INTENT(IN), OPTIONAL :: F_QG




  INTEGER ::  itaobraun, istatmin, new_ice_sat, id
  INTEGER ::  improve

  INTEGER :: i, j, k, ip, ii, ic
  INTEGER :: iskip, ih, icount, ibud, i24h 
  REAL    :: hour
  REAL    :: dth, dqv, dqrest, dqall, dqall1, rhotot, a1, a2 
 
  REAL, DIMENSION(CHUNK, kms:kme):: th2d, qv2d, ql2d, qr2d
  REAL, DIMENSION(CHUNK, kms:kme):: qi2d, qs2d, qg2d, qh2d
  REAL, DIMENSION(CHUNK, kms:kme):: rho2d, pii2d, p2d, w2d
  REAL, DIMENSION(CHUNK, kms:kme):: refc2d, refr2d, refi2d
  REAL, DIMENSION(CHUNK, kms:kme):: refs2d, refg2d, refh2d
  REAL, DIMENSION(CHUNK, kms:kme):: physc2d, physe2d, physd2d
  REAL, DIMENSION(CHUNK, kms:kme):: physs2d, physm2d, physf2d
  REAL, DIMENSION(CHUNK, kms:kme):: acphysc2d, acphyse2d, acphysd2d
  REAL, DIMENSION(CHUNK, kms:kme):: acphyss2d, acphysm2d, acphysf2d
  REAL, DIMENSION(CHUNK) :: xland1d
  REAL, DIMENSION(CHUNK, kms:kme):: refl_10cm2d




      INTEGER:: NCALL=0







   itaobraun = 0



    improve = 8


    new_ice_sat = 9


    istatmin = 180



    id = 0



    ibud = 0



   call consat_s ( itaobraun)



   call fall_flux(    dt_in,ql, qr, qi, qs, qg, qh, p,        &
                      rho, th, pii, z, dz8w, ht, rainnc,      &
                      rainncv, grav,itimestep,                &
                      preci3d, precs3d, precg3d, prech3d, precr3d,     &
                      snownc, snowncv, sr,                    &
                      graupelnc, graupelncv,                  &
                      hailnc, hailncv,                        &
                      vgc, vgc2,vhc,bhq,                      &
                      improve,                                &
                      ims,ime, jms,jme, kms,kme,              & 
                      its,ite, jts,jte, kts,kte               ) 

      
      
      

      IF (NCALL .EQ. 0) THEN



         xam_r = 3.14159*rhowater/6.
         xbm_r = 3.
         xmu_r = 0.
         xam_s = 3.14159*rhosnow/6.
         xbm_s = 3.
         xmu_s = 0.
         xam_g = 3.14159*rhograul/6.
         xbm_g = 3.
         xmu_g = 0.

         call radar_init
         NCALL = 1
      ENDIF































!$OMP PARALLEL DO &
!$OMP PRIVATE ( ic, j, ii, i, k ) &
!$OMP PRIVATE ( th2d, qv2d, ql2d, qr2d ) &
!$OMP PRIVATE ( qi2d, qs2d, qg2d, qh2d ) &
!$OMP PRIVATE ( rho2d, pii2d, p2d, w2d ) &
!$OMP PRIVATE ( refc2d, refr2d, refi2d, refs2d, refg2d, refh2d ) &
!$OMP PRIVATE ( physc2d, physe2d, physd2d, physs2d, physm2d, physf2d ) &
!$OMP PRIVATE ( acphysc2d, acphyse2d, acphysd2d, acphyss2d, acphysm2d, acphysf2d ) &
!$OMP PRIVATE ( xland1d, refl_10cm2d ) &
!$OMP SCHEDULE(dynamic,1)
      DO ip = 1,((1+(ite-its+1)/CHUNK)*CHUNK)*(jte-jts+1),CHUNK 
       j  = jts+(ip-1)/((1+(ite-its+1)/CHUNK)*CHUNK)
       IF ( j .ge. jts .and. j .le. jte ) THEN 
        ii = its+mod((ip-1),((1+(ite-its+1)/CHUNK)*CHUNK)) 

        DO ic=1,min(CHUNK,ite-ii+1)
          i = ii+ic -1
          xland1d(ic) = xland(i,j)
        ENDDO

         do k = kts, kte
          DO ic=1,min(CHUNK,ite-ii+1) 
            i = ii+ic -1
            th2d(ic,k) = th(i,k,j)
            qv2d(ic,k) = qv(i,k,j)
            ql2d(ic,k) = ql(i,k,j)
            qr2d(ic,k) = qr(i,k,j)
            qi2d(ic,k) = qi(i,k,j)
            qs2d(ic,k) = qs(i,k,j)
            qg2d(ic,k) = qg(i,k,j)
            qh2d(ic,k) = qh(i,k,j)
            rho2d(ic,k) = rho(i,k,j)
            pii2d(ic,k) = pii(i,k,j)
            p2d(ic,k) = p(i,k,j)
            w2d(ic,k) = w(i,k,j)
            refl_10cm2d(ic,k)=refl_10cm(i,k,j)
            refc2d(ic,k)=re_cloud_gsfc(i,k,j)
            refr2d(ic,k)=re_rain_gsfc(i,k,j)
            refi2d(ic,k)=re_ice_gsfc(i,k,j)
            refs2d(ic,k)=re_snow_gsfc(i,k,j)
            refg2d(ic,k)=re_graupel_gsfc(i,k,j)
            refh2d(ic,k)=re_hail_gsfc(i,k,j)
            physc2d(ic,k)=physc(i,k,j)
            physe2d(ic,k)=physe(i,k,j)
            physd2d(ic,k)=physd(i,k,j)
            physs2d(ic,k)=physs(i,k,j)
            physm2d(ic,k)=physm(i,k,j)
            physf2d(ic,k)=physf(i,k,j)
            acphysc2d(ic,k)=acphysc(i,k,j)
            acphyse2d(ic,k)=acphyse(i,k,j)
            acphysd2d(ic,k)=acphysd(i,k,j)
            acphyss2d(ic,k)=acphyss(i,k,j)
            acphysm2d(ic,k)=acphysm(i,k,j)
            acphysf2d(ic,k)=acphysf(i,k,j)
          ENDDO
         enddo

   IF ( min(CHUNK,ite-ii+1) .gt. 0 ) THEN
   call saticel_s( dt_in, dx, itaobraun, istatmin,               &
                   new_ice_sat, id, improve,                     &
                   th2d, qv2d, ql2d, qr2d,                       &
                   qi2d, qs2d, qg2d, qh2d,                       &
                   rho2d, pii2d, p2d, w2d,                       & 
                   itimestep, xland1d,                           &
                   refl_10cm2d, diagflag, do_radar_ref,         & 
                   ids,ide, jds,jde, kds,kde,                    & 
                   ims,ime, jms,jme, kms,kme,                    & 
                   its,ite, jts,jte, kts,kte,                    & 

                   refc2d, refr2d, refi2d, refs2d, refg2d, refh2d,  & 
                   physc2d, physe2d, physd2d, physs2d, physm2d, physf2d,  &
                   acphysc2d, acphyse2d, acphysd2d, acphyss2d, acphysm2d, acphysf2d  &

                  ,ii, j, min(CHUNK,ite-ii+1))

   ENDIF

         do k = kts, kte
          DO ic=1,min(CHUNK,ite-ii+1)
            i = ii+ic -1
            th(i,k,j) = th2d(ic,k)
            qv(i,k,j) = qv2d(ic,k)
            ql(i,k,j) = ql2d(ic,k)
            qr(i,k,j) = qr2d(ic,k)
            qi(i,k,j) = qi2d(ic,k)
            qs(i,k,j) = qs2d(ic,k)
            qg(i,k,j) = qg2d(ic,k)
            qh(i,k,j) = qh2d(ic,k)
            re_cloud_gsfc(i,k,j)=refc2d(ic,k)
            re_rain_gsfc(i,k,j)=refr2d(ic,k)
            re_ice_gsfc(i,k,j)=refi2d(ic,k)
            re_snow_gsfc(i,k,j)=refs2d(ic,k)
            re_graupel_gsfc(i,k,j)=refg2d(ic,k)
            re_hail_gsfc(i,k,j)=refh2d(ic,k)
            refl_10cm(i,k,j)=refl_10cm2d(ic,k)
            physc(i,k,j)=physc2d(ic,k)
            physe(i,k,j)=physe2d(ic,k)
            physd(i,k,j)=physd2d(ic,k)
            physs(i,k,j)=physs2d(ic,k)
            physm(i,k,j)=physm2d(ic,k)
            physf(i,k,j)=physf2d(ic,k)
            acphysc(i,k,j)=acphysc2d(ic,k)
            acphyse(i,k,j)=acphyse2d(ic,k)
            acphysd(i,k,j)=acphysd2d(ic,k)
            acphyss(i,k,j)=acphyss2d(ic,k)
            acphysm(i,k,j)=acphysm2d(ic,k)
            acphysf(i,k,j)=acphysf2d(ic,k)
          ENDDO
         enddo

         ENDIF
      ENDDO 

  END SUBROUTINE gsfcgce_4ice_nuwrf

  SUBROUTINE fall_flux ( dt, ql, qr, qi, qs, qg, qh, p,       &
                      rho, th, pi_mks, z, dz8w, topo, rainnc, &
                      rainncv, grav, itimestep,               &
                      preci3d, precs3d, precg3d, prech3d, precr3d,     &
                      snownc, snowncv, sr,                    &
                      graupelnc, graupelncv,                  &
                      hailnc, hailncv,                        &
                      vgc, vgc2, vhc, bhq,                    &
                      improve,                                &
                      ims,ime, jms,jme, kms,kme,              & 
                      its,ite, jts,jte, kts,kte               ) 







  IMPLICIT NONE
  INTEGER, INTENT(IN   )               :: improve,            &
                                          ims,ime, jms,jme, kms,kme,  &
                                          its,ite, jts,jte, kts,kte 
  INTEGER, INTENT(IN   )               :: itimestep
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(INOUT)               :: ql, qr, qi, qs, qg, qh      
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(IN)                  :: th, pi_mks      

  REAL,    DIMENSION( ims:ime , jms:jme ),                            &
           INTENT(INOUT)               :: rainnc, rainncv,            &
                                          snownc, snowncv, sr,        &
                                          graupelnc, graupelncv,      &
                                          hailnc, hailncv
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(IN   )               :: rho, z, dz8w, p     

  REAL,    INTENT(IN   )               :: dt, grav, vgc, vgc2, vhc, bhq


  REAL,    DIMENSION( ims:ime , jms:jme ),                            &
           INTENT(IN   )               :: topo   
  REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
           INTENT(OUT)               :: preci3d, precs3d, precg3d, prech3d, precr3d


 
  REAL,    DIMENSION( kts:kte )           :: fv
  REAL                                    :: tmp1, term0
  REAL                                :: pptrain, pptsnow,        &
                                         pptgraul, pptice, ppthail
  REAL :: qrz, qiz
  REAL,    DIMENSION( kts:kte )       :: qcz, qsz, qgz, qhz,  &
                                         zz, dzw, prez, rhoz,      &
                                         orhoz,r00
  REAL,    DIMENSION( kts:kte )       :: csed, rsed, ised, ssed, gsed, hsed

  REAL,    DIMENSION( kts:kte )       :: thz, piz

   INTEGER                    :: k, i, j


  REAL, DIMENSION( kts:kte )    :: vtr, vts, vtg, vth, vti

  REAL                          :: dtb, pi, consta, constc, gambp4,    &
                                   gamdp4, gam4pt5, gam4bbar


  REAL                          :: y1, y2, vr, vs, vg
  REAL                          :: vgcr, vgcr2, vscf,   &
                                   vhcr, vhcr2
  REAL                          :: tair, tairc, fexp
  REAL                          :: ftns, ftnsQ, ftng, ftngQ
  REAL                          :: const_vt, const_d, const_m, bb1, bb2
  REAL,    DIMENSION(7)         :: aice, vice
  REAL                          :: ftns0, ftng0
  REAL                          :: fros, fros0
  REAL                          :: qhz2,qgz2



  DATA aice/1.e-6, 1.e-5, 1.e-4, 1.e-3, 0.01, 0.1, 1./  
  DATA vice/5.,15.,30.,35.,40.,45.,50./  
  DATA ftns/1./, ftng/1./



   REAL     ::     rhowater 
   REAL     ::     rhosnow 








   REAL    , PARAMETER ::                              &

             constb = 0.8, constd = 0.11, o6 = 1./6.,            &
             cdrag = 0.6
  REAL    , PARAMETER ::     abar = 19.3, bbar = 0.37,           &
                                      p0 = 1.0e5
  REAL    , PARAMETER ::     rhoe_s = 1.29


  INTEGER                       :: min_q, max_q
  REAL                          :: t_del_tv, del_tv, flux, fluxin, fluxout
  LOGICAL                       :: notlast





   dtb=dt
   pi=acos(-1.)


      xnor = tnw*1.0e8
      rhowater = roqr*1000.
      xnos = tns*1.0e8             
      rhosnow = roqs*1000.


      xnog = tng*1.0e8
      rhograul = roqg*1000.
      xnoh = tnh*1.0e8      
      rhohail = roqh*1000.

   consta=2115.0*0.01**(1-constb)

   constc=78.63*0.01**(1-constd)


   gambp4=gammagce(constb+4.)
   gamdp4=gammagce(constd+4.)
   gam4pt5=gammagce(4.5)
   gam4bbar=gammagce(4.+bbar)










!$OMP PARALLEL DO &
!$OMP FIRSTPRIVATE(dtb, pi, rhowater, rhosnow, gambp4) &
!$OMP FIRSTPRIVATE(consta, constc, gamdp4, gam4pt5, gam4bbar) &
!$OMP PRIVATE(i,k,fv,tmp1,term0,pptrain, pptsnow,pptgraul, pptice, ppthail) &
!$OMP PRIVATE(qcz, qrz, qiz, qsz, qgz, qhz, zz, dzw, prez, rhoz, orhoz,r00) &
!$OMP PRIVATE(csed, rsed, ised, ssed, gsed, hsed) &
!$OMP PRIVATE(thz, piz) &
!$OMP PRIVATE(vtr, vts, vtg, vth, vti) &
!$OMP PRIVATE(y1, y2, vr, vs, vg) &
!$OMP PRIVATE(vgcr, vgcr2, vscf, vhcr, vhcr2) &
!$OMP PRIVATE(tair, tairc, fexp) &
!$OMP PRIVATE(ftns, ftnsQ, ftng, ftngQ) &
!$OMP PRIVATE(const_vt, const_d, const_m, bb1, bb2) &
!$OMP PRIVATE(aice, vice, ftns0, ftng0, fros, fros0, qhz2,qgz2) &
!$OMP PRIVATE(min_q, max_q) &
!$OMP PRIVATE(t_del_tv, del_tv, flux, fluxin, fluxout) &
!$OMP PRIVATE(notlast) &
!$OMP SCHEDULE(dynamic)

 j_loop:  do j = jts, jte
 i_loop:  do i = its, ite

   do k = kts, kte
      preci3d(i,k,j)=0.
      precs3d(i,k,j)=0.
      precg3d(i,k,j)=0.
      prech3d(i,k,j)=0.
      precr3d(i,k,j)=0.
      ised(k)=0.
      ssed(k)=0.
      gsed(k)=0.
      hsed(k)=0.
      rsed(k)=0.
   end do

   pptrain = 0.
   pptsnow = 0.
   pptgraul = 0.
   ppthail = 0.
   pptice  = 0.

   
   do k = kts, kte
      qcz(k)=ql(i,k,j)             
      qsz(k)=qs(i,k,j)
      qhz(k)=qh(i,k,j)
      rhoz(k)=rho(i,k,j)
      r00(k)=rhoz(k)*0.001         
      thz(k)=th(i,k,j)
      piz(k)=pi_mks(i,k,j)
      orhoz(k)=1./rhoz(k)
      prez(k)=p(i,k,j)
      fv(k)=sqrt(rhoe_s/rhoz(k))

      zz(k)=z(i,k,j)
      dzw(k)=dz8w(i,k,j)
   enddo 

      DO k = kts, kte
         qgz(k)=qg(i,k,j)
      ENDDO




    t_del_tv=0.
    del_tv=dtb
    notlast=.true.
    DO while (notlast)

      min_q=kte
      max_q=kts-1



      do k=kts,kte-1

         vtr(k)=0.
         qrz=qr(i,k,j)
         if (qrz .gt. cmin) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)




            tmp1=sqrt(pi*rhowater*xnor/rhoz(k)/qrz)
            tmp1=sqrt(tmp1)
            vtr(k)=consta*gambp4*fv(k)/tmp1**constb
            vtr(k)=vtr(k)/6.

            tair=thz(k)*piz(k)
            tairc=tair-t0
            y1=qrz  
            y2=qcz(k)  
            call vqrqi(1,r00(k),fv(k),y1,y2,tair,vtr(k))     
            vtr(k)=vtr(k) * 0.01 

           if (.not. vtr(k) .gt. 0.0) cycle 

            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vtr(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtr(k))
            endif
          endif
      enddo 

      if (max_q .ge. min_q) then




         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif





         fluxin=0.
         do k=max_q,min_q,-1
            qrz=qr(i,k,j)
            fluxout=rhoz(k)*vtr(k)*qrz
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qrz=qrz+del_tv*flux
            qrz=amax1(0.,qrz)
            qr(i,k,j)=qrz
            fluxin=fluxout
            rsed(k)=rsed(k)+fluxin
         enddo
         if (min_q .eq. 1) then
            pptrain=pptrain+fluxin*del_tv
         else
            qrz=qr(i,min_q-1,j)
            qrz=qrz+del_tv*  &
                          fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qrz=amax1(0.,qrz)         
            qr(i,min_q-1,j)=qrz
         endif

      else
         notlast=.false.
      endif
    ENDDO 




    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1


      do k=kts,kte-1
         vts(k)=0.

         if (qsz(k) .gt. cmin) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)


            tmp1=sqrt(pi*rhosnow*xnos/rhoz(k)/qsz(k))
            tmp1=sqrt(tmp1)
            vts(k)=constc*gamdp4*fv(k)/tmp1**constd
            vts(k)=vts(k)/6.


            y1 = qsz(k)             
            vscf=vsc*fv(k)

            ftns=1.
            ftns0=1.
            tair=thz(k)*piz(k)
            tairc=tair-t0

            qhz2=qhz(k)
            qgz2=qgz(k)
            if (k .lt. kte-2 .and. tairc .ge. -5) then
              qhz2=qhz(k+1) 
              qgz2=qgz(k+1)
            endif
            call sgmap(1,qsz(k),qgz(k),qgz2,qhz(k),qhz2,r00(k),tairc,ftns0)                   
            ftns=ftns0**bsq
            fros=1.
            call sgmap(3,qsz(k),qgz(k),qgz2,qhz(k),qhz2,r00(k),tairc,fros0)  
            fros=fros0**bsq
            vts(k)=max(vscf*(r00(k)*y1)**bsq/ftns/fros,0.e0)


            vts(k)=vts(k) * 0.01  

            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vts(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vts(k))
            endif 
         endif 
      enddo  

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif





         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rhoz(k)*vts(k)*qsz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qsz(k)=qsz(k)+del_tv*flux
            qsz(k)=amax1(0.,qsz(k))
            qs(i,k,j)=qsz(k)
            fluxin=fluxout
            ssed(k)=ssed(k)+fluxin
         enddo
         if (min_q .eq. 1) then
            pptsnow=pptsnow+fluxin*del_tv
         else
            qsz(min_q-1)=qsz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qsz(min_q-1)=amax1(0.,qsz(min_q-1))         
            qs(i,min_q-1,j)=qsz(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO





    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
          vtg(k)=0.

         if (qgz(k) .gt. cmin) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)




                 y1 = qgz(k)             
                 vgcr=vgc*fv(k)
                 vgcr2=vgc2*fv(k)
                 ftng=1.
                 ftng0=1.
                 tair=thz(k)*piz(k)
                 tairc=tair-t0
                 qhz2=qhz(k)
                 qgz2=qgz(k)
                 if (k .lt. kte-2 .and. tairc .ge. -5) then
                    qhz2=qhz(k+1)
                    qgz2=qgz(k+1)
                 endif
                 call sgmap(2,qsz(k),qgz(k),qgz2,qhz(k),qhz2,r00(k),tairc,ftng0) 
                 ftng=ftng0**bgq
                 vtg(k)=amax1(vgcr*(r00(k)*y1)**bgq/ftng, 0.0)
                                       
                 vtg(k)=vtg(k) * 0.01  
                 if (y1.gt.qrog2)then                              
                    ftng=ftng0**bgq2                                                
                    vtg(k)=amax1(vgcr2*(r00(k)*y1)**bgq2/ftng, 0.0)                    
                                       
                    vtg(k)=vtg(k) * 0.01  
                 endif 

            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vtg(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtg(k))
            endif 

         endif 
      enddo 

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif





         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rhoz(k)*vtg(k)*qgz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qgz(k)=qgz(k)+del_tv*flux
            qgz(k)=amax1(0.,qgz(k))
            qg(i,k,j)=qgz(k)
            fluxin=fluxout
            gsed(k)=gsed(k)+fluxin
         enddo
         if (min_q .eq. 1) then
            pptgraul=pptgraul+fluxin*del_tv
         else
            qgz(min_q-1)=qgz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qgz(min_q-1)=amax1(0.,qgz(min_q-1))         
            qg(i,min_q-1,j)=qgz(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO




    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
          vth(k)=0.

         if (qhz(k) .gt. cmin) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)


                 y1 = qhz(k)
                 vhcr=vhc/sqrt(r00(k))                                 
                 vth(k)=amax1(vhcr*(y1*r00(k))**bhq, 0.e0)             
                 vth(k)=vth(k) * 0.01  

            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vth(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vth(k))
            endif

         endif 
      enddo 

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif





         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rhoz(k)*vth(k)*qhz(k)
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qhz(k)=qhz(k)+del_tv*flux
            qhz(k)=amax1(0.,qhz(k))
            qh(i,k,j)=qhz(k)
            fluxin=fluxout
            hsed(k)=hsed(k)+fluxin
         enddo
         if (min_q .eq. 1) then
            ppthail=ppthail+fluxin*del_tv
         else
            qhz(min_q-1)=qhz(min_q-1)+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qhz(min_q-1)=amax1(0.,qhz(min_q-1))         
            qh(i,min_q-1,j)=qhz(min_q-1)
         endif

      else
         notlast=.false.
      endif

    ENDDO





    t_del_tv=0.
    del_tv=dtb
    notlast=.true.

    DO while (notlast)

      min_q=kte
      max_q=kts-1

      do k=kts,kte-1
         qiz=qi(i,k,j)
         vti(k)=0.
         if (qiz .gt. cmin) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)


         vti(k)=0.
         y1=rhoz(k) * 1000. * qiz    
         if (y1 .ge. 1.e-6) then
            y1=qiz
            y2=qcz(k)
             tair=thz(k)*piz(k)
             tairc=tair-t0
             call vqrqi(2,r00(k),fv(k),y1,y2,tair,vti(k))      
          endif  
             vti(k)=vti(k) * 0.01                       


          if (vti(k) .gt. 1.e-20) then
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-topo(i,j))/vti(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vti(k))
            endif
          endif


         endif
      enddo

      if (max_q .ge. min_q) then





         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif






         fluxin=0.
         do k=max_q,min_q,-1
            qiz=qi(i,k,j)
            fluxout=rhoz(k)*vti(k)*qiz
            flux=(fluxin-fluxout)/rhoz(k)/dzw(k)
            qiz=qiz+del_tv*flux
            qiz=amax1(0.,qiz)
            qi(i,k,j)=qiz
            fluxin=fluxout
            ised(k)=ised(k)+fluxin
         enddo
         if (min_q .eq. 1) then
            pptice=pptice+fluxin*del_tv
         else
            qiz=qi(i,min_q-1,j)
            qiz=qiz+del_tv*  &
                         fluxin/rhoz(min_q-1)/dzw(min_q-1)
            qiz=amax1(0.,qiz)         
            qi(i,min_q-1,j)=qiz
         endif

      else
         notlast=.false.
      endif

   ENDDO 

   do k = kts, kte
            preci3d(i,k,j)=ised(k)
            precs3d(i,k,j)=ssed(k)
            precg3d(i,k,j)=gsed(k)
            prech3d(i,k,j)=hsed(k)
            precr3d(i,k,j)=rsed(k)
   end do










   snowncv(i,j) = pptsnow
   snownc(i,j) = snownc(i,j) + pptsnow
   graupelncv(i,j) = pptgraul
   graupelnc(i,j) = graupelnc(i,j) + pptgraul 
   hailncv(i,j) = ppthail
   hailnc(i,j) = hailnc(i,j) + ppthail
   RAINNCV(i,j) = pptrain + pptsnow + pptgraul + pptice + ppthail
   RAINNC(i,j)  = RAINNC(i,j) + pptrain + pptsnow + pptgraul + pptice + ppthail
   sr(i,j) = 0.
   if (RAINNCV(i,j) .gt. 0.) sr(i,j) = (pptsnow + pptgraul + pptice + ppthail) / RAINNCV(i,j) 

  ENDDO i_loop
  ENDDO j_loop

 
  END SUBROUTINE fall_flux



   SUBROUTINE negcor ( X, rho, dz8w,                         &
                      ims,ime, jms,jme, kms,kme,              & 
                      itimestep, ics,                         &
                      its,ite, jts,jte, kts,kte               ) 

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                     X   
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                              rho, dz8w  
  integer, INTENT(IN   ) ::                           itimestep, ics 



  REAL   ::   A0, A1, A2

  A1=0.
  A2=0.
  do k=kts,kte
     do j=jts,jte
        do i=its,ite
        A1=A1+max(X(i,k,j), 0.)*rho(i,k,j)*dz8w(i,k,j)
        A2=A2+max(-X(i,k,j), 0.)*rho(i,k,j)*dz8w(i,k,j)
        enddo
     enddo
  enddo








  A0=0.0

  if (A1.NE.0.0.and.A1.GT.A2) then 
     A0=(A1-A2)/A1

  if (mod(itimestep,540).eq.0) then
     if (ics.eq.1) then
        write(61,*) 'kms=',kms,'  kme=',kme,'  kts=',kts,'  kte=',kte
        write(61,*) 'jms=',jms,'  jme=',jme,'  jts=',jts,'  jte=',jte 
        write(61,*) 'ims=',ims,'  ime=',ime,'  its=',its,'  ite=',ite 
     endif 
     if (ics.eq.1) then
         write(61,*) 'qv timestep=',itimestep
         write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.2) then
             write(61,*) 'ql timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.3) then
             write(61,*) 'qr timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.4) then
             write(61,*) 'qi timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.5) then
             write(61,*) 'qs timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else if (ics.eq.6) then
             write(61,*) 'qg timestep=',itimestep
             write(61,*) '  A1=',A1,'   A2=',A2,'   A0=',A0
     else
             write(61,*) 'wrong cloud specieis number'
     endif 
  endif 

     do k=kts,kte
        do j=jts,jte
           do i=its,ite
           X(i,k,j)=A0*AMAX1(X(i,k,j), 0.0)
           enddo
        enddo
     enddo
  endif

  END SUBROUTINE negcor

  SUBROUTINE consat_s ( itaobraun)  

































  IMPLICIT NONE




 integer ::  itaobraun
 real    :: cn0

 integer k
 real :: ga3, ga4, ga5, ga7, ga8, ga9, ga3g2, ga4g2, ga5g2, ga6d      
 real :: ga3h, ga4h, ga5hh, bc1, dc1, esc, egs, erc, amc, ehs, ehg
 real :: ehw, ehi, ehr, sc13, ga6, ga5gh2, egc, cpi2, grvt, tca, dwv
 real :: dva, amw, ars, rw, cw, ci, cd1, cd2, ga3b, ga4b, ga6b, ga5bh
 real :: ga3g, ga4g, ga5gh, ga3d, ga4d, ga5dh, ac1, ac2, ac3, cc1, eri
 real :: ami, ESR, eiw, ui50, ri50, cmn, y1, egi, egr, apri, bpri




















      cmin=1.e-20
      al = 2.5e10
      cp = 1.004e7
      rd1 = 1.e-3
      rd2 = 2.2

      cpi=4.*atan(1.)
      cpi2=cpi*cpi
      grvt=980.               

      c38=3.799052e3
      c358=35.86
      c610=6.1078e3
      c149=1.496286e-5
      c879=8.794142
      c172=17.26939
      c409=4098.026
      c76=7.66
      c218=21.87456
      c580=5807.695
      c141=1.414435e7


      tca=2.43e3
      dwv=.226
      dva=1.718e-4
      amw=18.016
      ars=8.314e7


      t0=273.16
      t00=238.16
      alv=2.5e10    
      alf=3.336e9
      als=2.8336e10
      avc=alv/cp
      afc=alf/cp
      asc=als/cp
      rw=4.615e6
      cw=4.187e7
      ci=2.093e7













       tnh = 0.01


      roqh = 0.9                  
      cd1=6.e-1                   
      cd2=4.*grvt/(3.*cd1)        
      ah=sqrt(cd2*roqh)           
      bh=0.5                      



         roqg=.3      

         ag=330.22    

         bg=.36
         ag2=544.83   
         bg2=.54
         tng=.04

       roqg2=0.5
       qrog2=2.0            

       qrog2=qrog2*1.e-6
       ag=330.22
       ag2=544.83
       bg=0.36
       bg2=0.54





      tns=.16              
      roqs=.1              
      as=78.63154          
      bs=.11               
        roqs=.05        
        tns=0.1         
        as=151.01
        bs=0.24


      aw=2115.
      bw=.8
      roqr=1.  
      tnw=.08  

      bgh=.5*bg
      bgh2=.5*bg2
      bsh=.5*bs
      bwh=.5*bw
      bhh=.5*bh
      bgq=.25*bg
      bgq2=.25*bg2
      bsq=.25*bs
      bwq=.25*bw
      bhq=.25*bh          


       tslopes=0.11177209   
       tslopeg=0.05756866   

       draimax=0.0500 
       draimax=draimax**4.*roqr*cpi
       dsnomin=0.0100 
       dsnomin4=dsnomin**4.*0.900*cpi  

       dgrpmin=0.0135 
       dgrpmin4=dgrpmin**4.*roqg*cpi

       xs=0.97
       sno11=0.70   
       sno00=-0.24   
       dsno11=3.25   
       dsno00=0.40   
       sexp11=1.5    
       sexp00=0.9    
       stt=-35.      
       stexp=0.42    
       slim=1.00     
       sbase=0.04000 

       xg=0.98
       grp11=0.30    
       grp00=0.30    
       dgrp11=2.70   
       dgrp00=2.60
       gexp11=0.20   
       gexp00=0.20   
       gtt=-35.      
       gtexp=0.40
       glim=0.90
       gbase=0.0130  

       hai00=3.25     
       hai11=0.50     
       htt0=-5.00     
       htt1=-50.00    
       haixp=2.7      


      ga3=2.
      ga4=6.
      ga5=24.
      ga6=120.
      ga7=720.
      ga8=5040.
      ga9=40320.

      ga3b  = gammagce(3.+bw)
      ga4b  = gammagce(4.+bw)
      ga6b  = gammagce(6.+bw)
      ga5bh = gammagce((5.+bw)/2.)
      ga3g  = gammagce(3.+bg)
      ga4g  = gammagce(4.+bg)
      ga5gh = gammagce((5.+bg)/2.)
      ga3d  = gammagce(3.+bs)
      ga4d  = gammagce(4.+bs)
      ga5dh = gammagce((5.+bs)/2.)

        ga4g=11.63177
        ga3g=3.3233625          
        ga5gh=1.608355         
        if(bg.eq.0.37) ga4g=9.730877 
        if(bg.eq.0.37) ga3g=2.887512
        if(bg.eq.0.37) ga5gh=1.526425
        if(bg.eq.0.36) ga4g=9.599978
        if(bg.eq.0.36) ga3g=2.857136
        if(bg.eq.0.36) ga5gh=1.520402 
        if(bg2.eq.0.54) ga4g2=12.298653
        if(bg2.eq.0.54) ga3g2=3.474196
        if(bg2.eq.0.54) ga5gh2=1.635061 
          ga3d=2.54925           
          ga4d=8.285063         
          ga5dh=1.456943
          if(bs.eq.0.57) ga3d=3.59304
          if(bs.eq.0.57) ga4d=12.82715
          if(bs.eq.0.57) ga5dh=1.655588
          if(bs.eq.0.24) ga3d=2.523508
          if(bs.eq.0.24) ga4d=8.176166
          if(bs.eq.0.24) ga5dh=1.451396
          if(bs.eq.0.11) ga3d=2.218906
          if(bs.eq.0.11) ga4d=6.900796
          if(bs.eq.0.11) ga5dh=1.382792 

      ga6d=144.93124
        if(bs.eq.0.24) ga6d=181.654791
      ga3h=gammagce(3.+bh)                     
      ga4h=gammagce(4.+bh)                     
      ga5hh=gammagce((5.+bh)/2.)               


      ac1=aw
      ac2=ag            
      ac3=as            

      bc1=bw
      cc1=as
      dc1=bs

      zrc=(cpi*roqr*tnw)**0.25
      zsc=(cpi*roqs*tns)**0.25
      zgc=(cpi*roqg*tng)**0.25
      zgc2=(cpi*roqg2*tng)**0.25
      zhc=(cpi*roqh*tnh)**0.25               

      vrc=aw*ga4b/(6.*zrc**bw)

       vrc0=-26.7
       vrc1=20600./zrc
       vrc2=-204500./(zrc*zrc)
       vrc3=906000./(zrc*zrc*zrc)

      vsc=as*ga4d/(6.*zsc**bs)
      vgc=ag*ga4g/(6.*zgc**bg)
      vgc2=ag2*ga4g2/(6.*zgc2**bg2)
      vhc=ah*ga4h/(6.*zhc**bh)          

      rn1=9.4e-15
      rn2=1.e-3
      bnd2=2.0e-3                    

      esi=0.70
      rn3=.25*cpi*tns*as*esi*ga3d

      esc=0.45
      rn4=.25*cpi*esc*tns*as*ga3d

      eri=.1                        
      rn5=.25*cpi*eri*tnw
      rn50=-.267e2*ga3
      rn51=5.15e3*ga4
      rn52=-1.0225e4*ga5
      rn53=7.55e3*ga6

      ami=1./(24.*6.e-9)            
      rn6=cpi2*eri*tnw*roqr*ami
      rn60=-.267e2*ga6
      rn61=5.15e3*ga7
      rn62=-1.0225e4*ga8
      rn63=7.55e3*ga9

      ESR=1.                       
      rn7=cpi2*esr*tnw*tns*roqs
      rn8=cpi2*esr*tnw*tns*roqr

      egs=.1
      rn9=cpi2*egs*tns*tng*roqs

      rn10=4.*tns
      rn101=.65
      rn102=.44*sqrt(as/dva)*ga5dh
      rn10a=alv*als*amw/(tca*ars)
      rn10b=alv/tca
      rn10c=ars/(dwv*amw)

      rn11=2.*cpi*tns*tca/alf
      rn11a=cw/alf

      ami50=4.8e-7*(dsnomin*1.e4/2./50.)**3
      ami100=1.51e-7    
      ami40=2.46e-7*.875**3     

       eiw=1.
       ui50=100. 
       ri50=dsnomin/2.

      cmn=1.05e-15
      rn12=cpi*eiw*ui50*ri50*ri50
      do k=1,31
         y1=1.-aa2(k)
         rn13(k)=aa1(k)*y1/(ami50**y1-ami40**y1)
         rn12a(k)=rn13(k)/ami50
         rn12b(k)=aa1(k)*ami50**aa2(k)
         rn25a(k)=aa1(k)*cmn**aa2(k)

         BergCon1(k)=6.*aa1(k)*ami50**(aa2(k)-1.)
         BergCon2(k)=-2.*aa1(k)*ami50**aa2(k)*1.2

         BergCon3(k)=6.*aa2(k)/((aa2(k)+1.)*(aa2(k)+2.))        &
                   *aa1(k)*ami50**(aa2(k)-1.)
         BergCon4(k)=2.*(1.-aa2(k))/((aa2(k)+1.)*(aa2(k)+2.))   &
                   *aa1(k)*ami50**aa2(k)*1.2
      enddo

      egc=0.65
      rn14=.25*cpi*egc*ag*tng*ga3g
      rn142=.25*cpi*egc*ag2*tng*ga3g2     

      egi=.1
      rn15=.25*cpi*egi*tng*ag*ga3g
      rn15a=.25*cpi*egi*tng*ag*ga3g
      rn15a2=.25*cpi*egi*tng*ag2*ga3g2    
      rn152=.25*cpi*egi*tng*ag2*ga3g2     

      egr=1.
      rn16=cpi2*egr*tng*tnw*roqr
      rn17=2.*cpi*tng
      rn17a2=.31*ga5gh2*sqrt(ag2/dva)     
      rn17b=cw-ci
      rn17c=cw
      rn171=2.*cpi*tng*alv*dwv
      rn172=2.*cpi*tng*tca
      rn17a=.31*ga5gh*sqrt(ag/dva)

      apri=.66
      bpri=1.e-4
      bpri=0.5*bpri                        
      rn18=20.*cpi2*bpri*tnw*roqr
      rn18a=apri
      rn191=.78                     
      rn192=.31*ga5gh*sqrt(ag/dva)  
      rn192_2=.31*ga5gh2*sqrt(ag2/dva)    
      rn19b=cw/alf 
      rn19=2.*cpi*tng*tca/alf
      rn19a=cw/alf

      rn20=2.*cpi*tng
      rn20a=als*als*amw/(tca*ars)
      rn20b=als/tca
      rn30a=alv*alv*amw/(tca*ars)
      rn30=2.*cpi*tng

      bnd3=2.e-3
      rn21=1.e-3
      bnd21=1.5e-3

      erc=1.
      rn22=.25*cpi*erc*tnw

      rn23=2.*cpi*tnw
      rn23a=.31*ga5bh*sqrt(ac1)
      rn23b=alv*alv/rw
      rn231=.78
      rn232=.31*ga3*sqrt(3.e3/dva)







       itaobraun=0

         cn0=1.e-8
         beta=-.6


      rn25=cn0
      rn30b=alv/tca
      rn30c=ars/(dwv*amw)
      rn31=1.e-17

      rn32=4.*51.545e-4

      rn33=4.*tns
      rn331=.65
      rn332=.44*sqrt(as/dva)*ga5dh 

      amc=1./(24.*4.e-9)
      rn34=cpi2*esc*amc*as*roqs*tns*ga6d  
      rn35=alv*alv/(cp*rw)

      gn17=2.*cpi*tng                                                      
      gn17a=.31*ga5gh*sqrt(ag)                                             
      gn17a2=.31*ga5gh2*sqrt(ag2)                                          

      ehs=1.
      hn9=cpi2*ehs*tns*tnh*roqs
      ehg=0.3
      hn10=cpi2*ehg*tng*tnh*roqg
      ehw=1.
      hn14=.25*cpi*ehw*tnh*ga3h*ah
      ehi=1.
      hn15a=.25*cpi*ehi*tnh*ga3h*ah
      ehr=1.
      hn16=cpi2*ehr*tnh*tnw*roqr
      hn17=2.*cpi*tnh
      hn17a=.31*ga5hh*sqrt(ah)
      hn19=2.*cpi*tnh/alf
      hn19a=.31*ga5hh*sqrt(ah)
      hn10a=als*als/rw
      hn20=2.*cpi*tnh
      hn20b=.31*ga5hh*sqrt(ah)


      sc13    = 0.8420526
      rn102   = rn102 * sc13
      rn17a   = rn17a * sc13
      rn17a2  = rn17a2 * sc13
      rn192   = rn192 * sc13
      rn192_2 = rn192_2 * sc13
      rn232   = rn232 * sc13
      rn332   = rn332 * sc13

  END SUBROUTINE consat_s 









  real function gammagce (xx)

  implicit none
 
  real*8 cof(6),stp,half,one,fpf,x,tmp,ser
  data cof,stp /  76.18009173,-86.50532033,24.01409822, &
     -1.231739516,.120858003e-2,-.536382e-5, 2.50662827465 /
  data half,one,fpf / .5, 1., 5.5 /

  real xx
  real gammln
  integer j

      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do  j=1,6
         x=x+one
        ser=ser+cof(j)/x
      enddo 
      gammln=tmp+log(stp*ser)

      gammagce=exp(gammln)


 END FUNCTION gammagce

!DIR$ ATTRIBUTES FORCEINLINE :: sgmap
  SUBROUTINE sgmap(isg,qcs,qcg,qcg2,qch,qch2,r00,tairc,ftnsg)









  IMPLICIT NONE


  integer, intent(in)  :: isg



  real,    intent(in)  :: r00, tairc


  real,    intent(in)  :: qcs,qcg,qcg2,qch,qch2
  real,    intent(out) :: ftnsg

  









  real :: taird, qsg, qsg1, xx, fexp  
  real :: ftnsT, sno1, dsno1, sexp1, ftnsQ, tnsmax, densno
  real :: ftngT, grp1, dgrp1, gexp1, ftngQ, tngmax
  real :: hx, gx, hgx
  real :: kk
  real :: hai2, dhai1



  ftnsg=1.



  if (isg.eq.1.or.isg.eq.3) qsg=qcs

  if (isg.eq.2) qsg = qcg
  if (isg.eq.4) qsg = qch

  if (qsg .gt. cmin) then
      
     qsg1=qsg*r00*1.e6


     if(isg.eq.1.or.isg.eq.3)then  
        taird=min(0.,max(stt,tairc)+0.0)
        ftnsT=exp(-1.*tslopes*taird)
        sno1=sno11
        dsno1=dsno11
        sexp1=sexp11

        if (taird.gt.stt) then
           sno1=sno00-(sno00-sno11)*(taird/stt)**stexp
           dsno1=dsno00-(dsno00-dsno11)*(taird/stt)**stexp
           sexp1=sexp00-(sexp00-sexp11)*(taird/stt)**stexp
        endif 

          hx=0.
          gx=0.
          hgx=1.

           if(qch2*1.e6*r00.gt.0.008)    &
              hx=max(1.,qch2*1.e6*r00*125.)
           if(qcg2*1.e6*r00.gt.0.040)    &
              gx=max(1.,qcg2*1.e6*r00*25.)
           hgx=hx+gx
           hgx=max(1.,hgx)
 
        xx=xs-xs*min(slim,max(0.,(qsg1-sno1)/dsno1)**sexp1)
        ftnsT=ftnsT**xx
        fexp=xx
        ftnsQ=1.0
        ftnsQ=(qsg1/sbase)**fexp
        ftnsg=ftnsT*ftnsQ*hgx
        tnsmax=r00*qsg/dsnomin4

        if (ftnsg*tns.gt.tnsmax) ftnsg=tnsmax/tns               

        densno=0.001996*(ftnsg*tns/qsg/r00)**0.2995   
        if(isg.eq.3) ftnsg=min(densno,0.9)/roqs            

     else if (isg.eq.2) then                      
            taird=min(0.,max(gtt,tairc)+0.0)
             ftngT=exp(-1.*tslopeg*taird)
             grp1=grp11
             dgrp1=dgrp11
             gexp1=gexp11

             if (taird.gt.gtt) then
                grp1=grp00-(grp00-grp11)*(taird/gtt)**gtexp
                dgrp1=dgrp00-(dgrp00-dgrp11)*(taird/gtt)**gtexp
                gexp1=gexp00-(gexp00-gexp11)*(taird/gtt)**gtexp
             endif 

             xx=xg-xg*min(glim,max(0.0,(qsg1-grp1)/dgrp1)**gexp1)
             ftngT=ftngT**xx
             fexp=xx
             ftngQ=1.0
             ftngQ=(qsg1/gbase)**fexp
             ftnsg=ftngT*ftngQ
             tngmax=r00*qsg/dgrpmin4
             if (ftnsg*tng.gt.tngmax) ftnsg=tngmax/tng

	elseif(isg.eq.4)then  
             hai2=hai00
             if(tairc.le.htt0.and.tairc.ge.htt1) &  
                hai2=hai11+(hai00-hai11)*((tairc-htt1)/(htt0-htt1))**haixp   
             if(tairc.le.htt1)  hai2=hai11     
                dhai1=hai2          
             if(qsg1.ge.hai2)    &
                ftnsg=1.0-0.80*min(max((qsg1-hai2)/dhai1,0.),1.) 

     endif 

  endif 

  end subroutine sgmap


  SUBROUTINE vqrqi(isg,r00,fv,qri,ql,tair,ww1)





  implicit none

  integer, intent(in) :: isg
  real, intent(in) :: r00, fv,qri,ql,tair
  real, intent(inout) :: ww1 


  integer :: ic 
  real  :: y1,vr,vs,vg
  real  :: const_vt, const_d, const_m 
  real  :: bb1, bb2,ice_fall
  real  :: bin_factor, ftnw, ftnwmin
  real, dimension(7) ::  aice, vice
  data aice/1.e-6, 1.e-5, 1.e-4, 1.e-3, 0.01, 0.1, 1./
  data vice/5,15,30,35,40,45,50/



     ice_fall=0.
     const_vt=1.49e4
     const_d=11.9
     const_m=1./5.38e7

  y1=r00*qri
  ww1=0.

  if (y1 .gt. cmin) then

    if (isg.eq.1) then                             

       ftnw=1.                                                       
           if(ql.lt.cmin .and. tair .gt. t0)then                      
             bin_factor=0.11*(1000.*qri)**(-1.27) + 0.98      
             bin_factor=min(bin_factor, 1.30)       
             ftnw=1./bin_factor**3.35                             
             ftnwmin=r00*qri/draimax                        
             if(qri.le.0.001) ftnw=max(ftnw,ftnwmin/tnw)   
           endif                                               

               vs=sqrt( y1 )
               vg=sqrt( vs)

               vr=vrc0+vrc1*vg/ftnw**0.25+vrc2*vs/ftnw**0.50+vrc3*vg*vs/ftnw**0.75
               ww1=max(fv*vr, 0.e0)


    else if (isg.eq.2) then                         

            y1=1.e6*r00*qri                            

            if (y1 .gt. 1.e-6) then
                  y1=y1*1.e-3
                  bb1=const_m*y1**0.25
                  bb2=const_d*bb1**0.5
                  ww1=max(const_vt*bb2**1.31, 0.0)
                  ww1=ww1*100. 
                  if (ww1 .gt. 50.) ww1=50.               
            endif  
    endif  
  endif 

  end subroutine vqrqi

  SUBROUTINE saticel_s (dt, dx, itaobraun, istatmin,                   &
                       new_ice_sat, id, improve,                       &
                       ptwrf, qvwrf, qlwrf, qrwrf,                     &
                       qiwrf, qswrf, qgwrf, qhwrf,                     &
                       rho_mks, pi_mks, p0_mks, w_mks,                 &
                       itimestep, xland,                               &
                       refl_10cm, diagflag, do_radar_ref,              & 
                       ids,ide, jds,jde, kds,kde,                      &
                       ims,ime, jms,jme, kms,kme,                      &
                       its,ite, jts,jte, kts,kte,                      &

                       re_cloud_gsfc, re_rain_gsfc, re_ice_gsfc,       &
                       re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc,    & 
                       physc, physe, physd, physs, physm, physf,       &
                       acphysc, acphyse, acphysd, acphyss, acphysm, acphysf &
                       ,ii,j,irestrict)




  IMPLICIT NONE

















































































  INTEGER, INTENT(IN   ) :: itaobraun, improve, new_ice_sat
  integer, intent(in)  ::   id
  integer, intent(in)  ::   itimestep,istatmin  
  real, intent(in)     ::   dt  
  real, intent(in)     ::   dx  
  real     ::   thresh_evap



  integer, intent(in) :: ids,ide,jds,jde,kds,kde
  integer, intent(in) :: ims,ime,jms,jme,kms,kme
  integer, intent(in) :: its,ite,jts,jte,kts,kte
  integer, intent(in) :: ii,j,irestrict 
  integer i, k, kp     

  real, dimension(CHUNK) :: afcp, alvr, ascp, avcp, rp0, pi0, pir,  &
                            pr0, r00, rrs, rrq, fv0, fvs, cp409, &
                            rr0, zrr, zsr, zgr, zhr, &
                            cp580, cs580, cv409, vscf, vgcf, vgcf2, &
                            vhcr, dwvp, r3f, r4f, r5f, r6f, &
                            r12r, r14f ,r14f2, r15af, r15af2, r15f, r18r, &
                            r22f, r25rt, r32rt, r331r, r332rf, &
                            r34f

  real :: bg3, bg3_2, bgh5, bgh5_2, bs3 ,bs6, bsh5, bw3 ,bw6 ,bwh5, &
          cmin, cmin1, cmin2, d2t, del, f2 ,f3, ft, qb0, r25a, r_nci, &
          sccc, sddd, seee, sfff, smmm, ssss, tb0, temp, ucog ,ucog2, &
          ucor ,ucos, ucoh, uwet, rdt, bnd1, c_nci, &
          r10t, r20t, r23t

  real :: a_1, a_2, a_3, a_4
  real :: a_11, a_22, a_33, a_44
  real :: zdry, zwet, zwet0
  real :: vap_frac


  real, dimension (CHUNK, kts:kte) ::  fv
  real, dimension (CHUNK, kts:kte) ::  dpt, dqv
  real, dimension (CHUNK, kts:kte) ::  qcl, qrn,             &
                                       qci, qcs, qcg, qch
  real, dimension (CHUNK, kts:kte) ::  qsz, qgz,qhz

  real, dimension (CHUNK, kms:kme), INTENT(INOUT)                       &
                                               ::  ptwrf, qvwrf,       &
                                                   qlwrf, qrwrf,       &
                                                   qiwrf, qswrf,       &
                                                   qgwrf, qhwrf


  real, dimension (CHUNK, kms:kme), INTENT(IN   )                      &
                                              ::  rho_mks,            &
                                                  pi_mks,             &
                                                  p0_mks,             &
                                                  w_mks

  real, dimension (CHUNK) ::                                  &              
           vg,      zg,       &
           ps,      pg,       &
          prn,     psn,       &
        pwacs,   wgacr,       &
        pidep,    pint,       &
          qsi,     ssi,       &
          esi,     esw,       &
          qsw,      pr,       &
          ssw,   pihom,       &
         pidw,   pimlt,       &
        psaut,   qracs,       &
        psaci,   psacw,       &
        qsacw,   praci,       &
        pmlts,   pmltg,       &
        asss


  real, dimension (CHUNK) ::        &
        praut,   pracw,       &
         psfw,    psfi,       &
        dgacs,   dgacw,       &
        dgaci,   dgacr,       &
        pgacs,   wgacs,       &
        qgacw,   wgaci,       &
        qgacr,   pgwet,       &
        pgaut,   pracs,       &
        psacr,   qsacr,       &
         pgfr,   psmlt,       &
        pgmlt,   psdep,       &
        pgdep,   piacr,       &
          egs


  real, dimension (CHUNK) ::        &
           pt,      qv,       &
           qc,      qr,       &
           qi,      qs,       &
           qg,      qh,       &
                  tair,       &
        tairc,   rtair,       &
          dep,      dd,       &
          dd1,     qvs,       &
           dm,      rq,       &
        rsub1,     col,       &
          cnd,     ern,       &
         dlt1,    dlt2,       &
         dlt3,    dlt4,       &
           zr,      vr,       &
           zs,      vs
       


  real, dimension (CHUNK) ::        &
         phfr,phmlt,                          & 
         dhacw,qhacw,dhacr,qhacr,whacr,       & 
         dhaci,whaci,dhacs,phacs,whacs,       & 
         dhacg,whacg,phwet,phdep,phsub,       & 
         pvaph,primh,scv,dwv,tca                


  real, dimension (CHUNK,kts:kte) ::  rho 


  real, dimension (CHUNK, kts:kte) ::  p0, pi, f0, ww1
  real, dimension (CHUNK, kts:kte) ::    & 
           fd,      fe,           &
           st,      sv,           &
           sq,      sc,           &
           se,     sqa


  integer, dimension (CHUNK) ::        it  
  integer, dimension (CHUNK, 4) ::    ics 

  integer :: i24h
  real :: r2is, r2ig, r2ih
  


  real, dimension (CHUNK, kms:kme), INTENT(INOUT)  ::  &
          physc,   physe,   physd,                  &
          physs,   physm,   physf,                  &
          acphysc,   acphyse,   acphysd,            &
          acphyss,   acphysm,   acphysf


  real, dimension(CHUNK, kts:kte) :: dbz



  integer  ::  ihalmos
  real     ::  xnsplnt, xmsplnt
  real     ::  hmtemp1, hmtemp2, hmtemp3, hmtemp4
  real     ::  ftnw, ftnwmin
  real     ::  xssi, fssi, rssi, xsubi, wssi
  real     ::  dmicrons, dmicrong, dvair, alpha
  real,   dimension (CHUNK) :: tairN, tairI,    &
                                          ftns,  ftng,    &
                                         ftns0, ftng0,    &
                                         ftnh,  ftnh0,    &
                                         pihms, pihmg,    &
                                          pimm,  pcfr,    &
                                         pssub, pgsub,    &
                                          fros, fros0,    &
                                            vi,    zh,    &
                                            vh, pihmh,    &
                                          dda0, pvapg,    &
                                          pracg,qracg,    &
                                          qrimh,pg2h      

  real,   dimension (CHUNK) :: y1, y2, y3, y4,  &
                               y5, y6, y7, y8 

  real     ::  hfact, sfact, yy1
  real     ::  xncld, esat, rv, rlapse_m
  real     ::  delT, bhi  
  real     ::  rc, ra, cna
  real     ::  xccld, xknud, cunnf, diffar
  real     ::  qgz2, qhz2

  real :: r11t, r19t, r19at, r30t, r33t
  real, dimension(CHUNK) :: r7rf, r8rf, r9rf, r16rf
  real, dimension(CHUNK) :: r101r, r102rf, r191r, r192rf, r192rf2
  real, dimension(CHUNK) :: r231r, r232rf
  real, dimension(CHUNK) :: h9r, h10r, h14r, h15ar, h16r, h17r, h17aq,   &
              h19aq, h19rt, h10ar, h20t, h20bq
  real, dimension(CHUNK) :: bin_factor, rim_frac
  real     :: term1, term2, fdwv, dwv0  
  integer  :: iter





  real, dimension (CHUNK, kms:kme) , INTENT(INOUT   )                 &
                                              ::  re_cloud_gsfc, re_rain_gsfc,  &
                                                  re_ice_gsfc, re_snow_gsfc,    &
                                                  re_graupel_gsfc, re_hail_gsfc
  REAL , DIMENSION( CHUNK ) , INTENT(IN)   :: XLAND
  real, parameter :: roqi = 0.9179    
  real, parameter :: ccn_over_land = 1500  
  real, parameter :: ccn_over_water = 150  
  real :: L_cloud    
  real :: I_cloud    
  real :: mu, ccn_ref, lambda
  real :: gamfac1, gamfac3




  REAL, DIMENSION(CHUNK, kms:kme), INTENT(INOUT):: refl_10cm  

  LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
  INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref



      dwv0 =  0.226



    if (itimestep.eq.1) then
       do k = kts, kte
!dir$ vector aligned
         DO i=1,irestrict
             physc(i,k)=0.
             physe(i,k)=0.
             physd(i,k)=0.
             physs(i,k)=0.
             physf(i,k)=0.
             physm(i,k)=0.
             acphysc(i,k)=0.
             acphyse(i,k)=0.
             acphysd(i,k)=0.
             acphyss(i,k)=0.
             acphysf(i,k)=0.
             acphysm(i,k)=0.
       ENDDO
       enddo 
      if ( wrf_dm_on_monitor() .and. i.eq.its .and. j.eq.jts ) then
       write(6, *) '    latent heating variables have been initialized to 0. at timestep = ', itimestep
      endif
   endif


      do k=kts,kte
!dir$ vector aligned
        DO i=1,irestrict
         rho(i,k)=rho_mks(i,k)*0.001
         p0(i,k)=p0_mks(i,k)*10.0
         pi(i,k)=pi_mks(i,k)
         ww1(i,k)=w_mks(i,k)*100.
         dpt(i,k)=ptwrf(i,k)
         dqv(i,k)=qvwrf(i,k)
         qcl(i,k)=qlwrf(i,k)
         qrn(i,k)=qrwrf(i,k)
         qci(i,k)=qiwrf(i,k)
         qcs(i,k)=qswrf(i,k)
         qcg(i,k)=qgwrf(i,k)
         qch(i,k)=qhwrf(i,k)
        ENDDO
      enddo 

      do k=kts,kte
!dir$ vector aligned
        DO i=1,irestrict
            fv(i,k)=sqrt(rho(i,1)/rho(i,k))
        ENDDO
      enddo 





      d2t=dt

      r2ig=1.
      r2is=1.
      r2ih=1.



      cmin=1.e-20

      cmin1=1.e-20
      cmin2=1.e-35

      xssi=0.16  
      rssi=0.05  
      xsubi=0.70  


      ihalmos=1
      xnsplnt=370.     
      xmsplnt=4.4e-8   
      hmtemp1=-2.
      hmtemp2=-4.
      hmtemp3=-6.
      hmtemp4=-8.

      it(:)=1

      f2=rd1*d2t     
      f3=rd2*d2t

      ft=dt/d2t

      bw3=bw+3.
      bs3=bs+3.
      bg3=bg+3.
      bh3=bh+3
      bsh5=2.5+bsh
      bgh5=2.5+bgh
      bhh5=2.5+bhh
      bwh5=2.5+bwh
      bw6=bw+6.
      bs6=bs+6.


      r10t=rn10*d2t
      r25a=rn25

      rdt=1./d2t
      r11t=rn11*d2t
      r19t=rn19*d2t
      r19at=rn19a*d2t
      r20t=rn20*d2t
      r23t=rn23*d2t
      r30t=rn30*d2t
      r33t=rn33*d2t
      bg3_2=bg2+3
      bgh5_2=2.5+bgh2





      thresh_evap = -39.974 * exp(-1.194 * dx/1000.)

      if ( wrf_dm_on_monitor() .and. itimestep.eq.1 .and. &
           i.eq.its .and. j.eq.jts ) then
         print *,'GSFCGCE 4ice scheme inside satice improve=',improve
         print *,'dx, thresh_evap = ', dx, thresh_evap  
         print *,'no reduce suprious evaporation adjustment'
      endif

      Rc=1.e-3               
      Ra=1.e-5               
      Cna=500.               
      Bhi=1.01e-2            



  do 1000 k=kts,kte
       kp=k+1
       tb0=0.
       qb0=0.

!dir$ vector aligned
       DO i=1,irestrict




         rp0(i)=3.799052e3/p0(i,k)
         pi0(i)=pi(i,k)
         pir(i)=1./(pi(i,k))
         pr0(i)=1./p0(i,k)
         r00(i)=rho(i,k)
         rr0(i)=1./rho(i,k)
         rrs(i)=sqrt(rr0(i))
         rrq(i)=sqrt(rrs(i))
         f0(i,k)=al/cp/pi(i,k)
         fv0(i)=fv(i,k)
         fvs(i)=sqrt(fv(i,k))
         zrr(i)=1.e5*zrc*rrq(i)
         zsr(i)=1.e5*zsc*rrq(i)
         zgr(i)=1.e5*zgc*rrq(i)
         zhr(i)=1.e5*zhc*rrq(i)
         cp409(i)=c409*pi0(i)
         cv409(i)=c409*avc 
         cp580(i)=c580*pi0(i)
         cs580(i)=c580*asc 
         alvr(i)=r00(i)*alv
         afcp(i)=afc*pir(i)
         avcp(i)=avc*pir(i)
         ascp(i)=asc*pir(i)
         vscf(i)=vsc*fv0(i)
         vgcf(i)=vgc*fv0(i)
         vgcf2(i)=vgc2*fv0(i)
         vhcr(i)=vhc*rrs(i)
         dwvp(i)=c879*pr0(i)

         r3f(i)=rn3*fv0(i)
         r4f(i)=rn4*fv0(i)
         r5f(i)=rn5*fv0(i)
         r6f(i)=rn6*fv0(i)

         r12r(i)=rn12*r00(i)
         r14f(i)=rn14*fv0(i)
         r14f2(i)=rn142*fv0(i)
         r15f(i)=rn15*fv0(i)
         r15af(i)=rn15a*fv0(i) 
         r15af2(i)=rn15a2*fv0(i) 
         r18r(i)=rn18*rr0(i)
         r22f(i)=rn22*fv0(i)
         r25rt(i)=rn25*rr0(i)*d2t
         r32rt(i)=rn32*d2t*rrs(i)



         r7rf(i)=rn7*rr0(i)*fv0(i)
         r8rf(i)=rn8*rr0(i)*fv0(i)
         r9rf(i)=rn9*rr0(i)*fv0(i)
         r16rf(i)=rn16*rr0(i)*fv0(i)
         r101r(i)=rn101*rr0(i)
         r102rf(i)=rn102*rrs(i)*fvs(i)
         r191r(i)=rn191*rr0(i)
         r192rf(i)=rn192*rrs(i)*fvs(i)
         r192rf2(i)=rn192_2*rrs(i)*fvs(i)
         r331r(i)=rn331*rr0(i)
         r332rf(i)=rn332*rrs(i)*fvs(i)
         r34f(i)=rn34*fv0(i)

         r231r(i)=rn231*rr0(i)
         r232rf(i)=rn232*rrs(i)*fvs(i)

         h9r(i)=hn9*rr0(i)
         h10r(i)=hn10*rr0(i)
         h14r(i)=hn14*rrs(i)
         h15ar(i)=hn15a*rrs(i)
         h16r(i)=hn16*rr0(i)
         h17r(i)=hn17*rr0(i)
         h17aq(i)=hn17a*rrq(i)
         h19aq(i)=hn19a*rrq(i)
         h19rt(i)=hn19*rr0(i)*d2t
         h10ar(i)=hn10a*r00(i)
         h20t(i)=hn20*d2t 
         h20bq(i)=hn20b*rrq(i)

         pt(i)=dpt(i,k)
         qv(i)=dqv(i,k)
         qc(i)=qcl(i,k)
         qr(i)=qrn(i,k)
         qi(i)=qci(i,k)
         qs(i)=qcs(i,k)
         qg(i)=qcg(i,k)
         qh(i)=qch(i,k)
         if (qc(i) .le.  cmin) qc(i)=0.0
         if (qr(i) .le.  cmin) qr(i)=0.0
         if (qi(i) .le.  cmin) qi(i)=0.0
         if (qs(i) .le.  cmin) qs(i)=0.0
         if (qg(i) .le.  cmin) qg(i)=0.0
         if (qh(i) .le.  cmin) qh(i)=0.0
         tair(i)=(pt(i)+tb0)*pi0(i)
         tairc(i)=tair(i)-t0
         zr(i)=zrr(i)
         zs(i)=zsr(i)
         zg(i)=zgr(i)
         zh(i)=zhr(i)
         vr(i)=0.0 
         vs(i)=0.0
         vg(i)=0.0
         vi(i)=0.0
         vh(i)=0.0

         ftns(i)=1.
         ftng(i)=1.
         ftns0(i)=1.
         ftng0(i)=1.
         fros(i)=1.
         ftnh(i)=1.
         ftnh0(i)=1.

         cnd(i)=0.0
         dep(i)=0.
         ern(i)=0.0
         pint(i)=0.0
         pidep(i)=0.0

         psdep(i)=0.
         pgdep(i)=0.
         dd1(i)=0.
         dd(i)=0.
         pgsub(i)=0.
         psmlt(i)=0.
         pgmlt(i)=0.
         pimlt(i)=0.
         psacw(i)=0.
         piacr(i)=0.

         pssub(i)=0.0
         pgsub(i)=0.0

         psfw(i)=0.0
         psfi(i)=0.0
         pidep(i)=0.0

         pgfr(i)=0.
         psacr(i)=0.
         wgacr(i)=0.
         pihom(i)=0.
         pidw(i)=0.0
          
         psaut(i)=0.0
         psaci(i)=0.0
         praci(i)=0.0
         pwacs(i)=0.0
         qsacw(i)=0.0
          
         pracs(i)=0.0
         qracs(i)=0.0
         qsacr(i)=0.0
         pgaut(i)=0.0
 
         praut(i)=0.0
         pracw(i)=0.0
         pgfr(i)=0.0

         qracs(i)=0.0

         pgacs(i)=0.0
         qgacw(i)=0.0
         dgaci(i)=0.0
         dgacs(i)=0.0
         wgacs(i)=0.0
         wgaci(i)=0.0
         dgacw(i)=0.0
         dgacr(i)=0.
         pgwet(i)=0.0

         qgacr(i)=0.0

         pihom(i)=0.0
         pimlt(i)=0.0
         pidw(i)=0.0
         pimm(i)=0.0
         pcfr(i)=0.0

         pihms(i)=0.0 
         pihmg(i)=0.0
         ftns(i)=1.
         ftng(i)=1.
         pmlts(i)=0.0
         pmltg(i)=0.0

         phfr(i)=0.0
         phmlt(i)=0.0
         dhacw(i)=0.0
         qhacw(i)=0.0
         dhacr(i)=0.0
         qhacr(i)=0.0
         whacr(i)=0.0
         dhaci(i)=0.0
         whaci(i)=0.0
         dhacs(i)=0.0
         phacs(i)=0.0
         whacs(i)=0.0
         dhacg(i)=0.0
         whacg(i)=0.0
         phwet(i)=0.0
         phdep(i)=0.0
         phsub(i)=0.0
         pvapg(i)=0.0
         pvaph(i)=0.0
         primh(i)=0.0

         dlt4(i)=0.0
         dlt3(i)=0.0
         dlt2(i)=0.0







         y1(i)=c149*tair(i)**1.5/(tair(i)+120.)
         dwv(i)=dwvp(i)*tair(i)**1.81
         tca(i)=c141*y1(i)
         scv(i)=1./((rr0(i)*y1(i))**.1666667*dwv(i)**.3333333)

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict





            if (qr(i) .gt. cmin) then
	       dd(i)=r00(i)*qr(i)
	       y1(i)=sqrt(dd(i))
	       y2(i)=sqrt(y1(i))
	       zr(i)=zrc/y2(i)
            endif

            call vqrqi(1,r00(i),fv0(i),qr(i),qc(i),tair(i),vr(i))
            call vqrqi(2,r00(i),fv0(i),qi(i),qc(i),tair(i),vi(i))

            ftns(i)=1.
            ftns0(i)=1.

            qhz2=qhwrf(i,k)
            qgz2=qgwrf(i,k)
            if (k .lt. kte-2 .and. tairc(i) .ge. -5) then
               qhz2=qhwrf(i,k+1)
               qgz2=qgwrf(i,k+1)
            endif
            call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))
            call sgmap(3,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),fros0(i))

	    if (qs(i) .gt. cmin) then
	       dd(i)=r00(i)*qs(i)
	       y1(i)=dd(i)**.25
               ftns(i)=1.
               ftns(i)=ftns0(i)**0.25
               fros(i)=1                                        
               fros(i)=fros0(i)**0.25        
               ZS(i)=ZSC/Y1(i)*ftns(i)*fros(i)            
               ftns(i)=ftns0(i)**bsq
               fros(i)=fros0(i)**bsq         
               VS(i)=MAX(vscf(i)*DD(i)**BSQ/ftns(i)/fros(i), 0.)
            endif

            ftng(i)=1.
            ftng0(i)=1.
            call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))

	    if (qg(i) .gt. cmin) then
	       dd(i)=r00(i)*qg(i)
	       y1(i)=dd(i)**.25
               ftng(i)=1.
               ftng(i)=ftng0(i)**0.25

               zg(i)=zgc/y1(i)*ftng(i)
               if(dd(i).gt.qrog2) zg(i)=zgc2/y1(i)*ftng(i)

               ftng(i)=ftng0(i)**bgq
	       vg(i)=max(vgcf(i)*dd(i)**bgq/ftng(i), 0.0)
               if(dd(i).gt.qrog2)then
               ftng(i)=ftng0(i)**bgq2
               vg(i)=max(vgcf2(i)*dd(i)**bgq2/ftng(i), 0.e0)
              endif 
           endif 
           
           call sgmap(4,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftnh0(i))

           if (qh(i) .gt. cmin) then
              dd(i)=r00(i)*qh(i)
              y1(i)=dd(i)**.25
              ftnh(i)=ftnh0(i)**0.25
              zh(i)=zhc/y1(i)*ftnh(i)
              ftnh(i)=ftnh0(i)**bhq
              vh(i)=max(vhcr(i)*dd(i)**bhq/ftnh(i), 0.0)
            endif

            if (qr(i) .le. cmin1) vr(i)=0.0
            if (qs(i) .le. cmin1) vs(i)=0.0
            if (qg(i) .le. cmin1) vg(i)=0.0
            if (qi(i) .le. cmin1) vi(i)=0.0
            if (qh(i) .le. cmin1) vh(i)=0.0















          pihms(i)=0.0 
          pihmg(i)=0.0 
          psaut(i)=0.0
          psaci(i)=0.0
          praci(i)=0.0
          piacr(i)=0.0
          psacw(i)=0.0
          pwacs(i)=0.0
          qsacw(i)=0.0
          ftns(i)=1.
          ftng(i)=1.
          ftnh(i)=1.
	  ftns(i)=ftns0(i)
	  ftng(i)=ftng0(i)

          if (tair(i).lt.t0) then

             rn1=1./300.
             bnd1=6.e-5
             esi(i)=0.25
             psaut(i)=r2is*max(rn1*esi(i)*(qi(i)-bnd1*fv0(i)*fv0(i)) ,0.0) 
             ftns(i)=ftns0(i)
             ftng(i)=ftng0(i)
             fros(i)=fros0(i)
             ftnh(i)=ftnh0(i)
             esi(i)=1.0 
             dmicrons=(r00(i)*qs(i)/(roqs*fros(i))/cpi/(tns*ftns(i)))**.25*1.e4
             esi(i)=min(1.,(dmicrons/375.)**3.) 
 
             y1(i)=1.0
             if (vs(i).gt.0.) y1(i)=abs((vs(i)-vi(i))  &
                                                 /vs(i))
             psaci(i)=r2is*y1(i)*r3f(i)*qi(i)/zs(i)**bs3*ftns(i)*esi(i)
             if(qs(i).le.cmin) psaci(i)=0.
             psacw(i)=r2is*r4f(i)*qc(i)/zs(i)**bs3*ftns(i)
             if(qs(i).le.cmin) psacw(i)=0.
             if (ihalmos.eq.1)then
                y2(i)=0.
                if((tairc(i).le.hmtemp1).and.(tairc(i).ge.hmtemp4))  &
                                                         y2(i)=0.5
                if((tairc(i).le.hmtemp2).and.(tairc(i).ge.hmtemp3))  &
                                                         y2(i)=1.
                pihms(i)=r2ih*psacw(i)*y2(i)*xnsplnt*1000.*xmsplnt
                psacw(i)=psacw(i)-pihms(i)
             endif
             pwacs(i)=r2is*r34f(i)*qc(i)/zs(i)**bs6*ftns(i)*fros(i)      
             if(qs(i).le.cmin) pwacs(i)=0.
             y1(i)=1./zr(i)
             y2(i)=y1(i)*y1(i)
             y3(i)=y1(i)*y2(i)
             y5(i)=1.0
             if (vr(i).gt.0.) y5(i)=abs((vr(i)-vi(i))   &
                                                         /vr(i))
             dd(i)=y5(i)*r5f(i)*qi(i)*y3(i)*(rn50+rn51*y1(i)             &
                                        +rn52*y2(i)+rn53*y3(i))
             praci(i)=max(dd(i),0.0)
             if (qr(i) .le. cmin) praci(i)=0.
             y4(i)=y3(i)*y3(i)
             dd1(i)=y5(i)*r6f(i)*qi(i)*y4(i)*(rn60+rn61*y1(i)       &
                                     +rn62*y2(i)+rn63*y3(i))

             piacr(i)=max(dd1(i),0.0)
             if (qr(i) .le. cmin) piacr(i)=0.
          else
             qsacw(i)=r2is*r4f(i)*qc(i)/zs(i)**bs3*ftns(i)
             if (qs(i) .le. cmin) psacw(i)=0.
          endif   






             praut(i)=max(rn21*(qc(i)-bnd21),0.0)


             y1(i)=1./zr(i)
             y2(i)=y1(i)*y1(i)
             y3(i)=y1(i)*y2(i)
             y4(i)=r22f(i)*qc(i)*y3(i)*(rn50+rn51*y1(i)+  &
                     rn52*y2(i)+rn53*y3(i))
             pracw(i)=max(y4(i), 0.0) 
          if(qr(i) .le. cmin) pracw(i)=0.




          pidep(i)=0.0
          psfw(i)=0.0
          psfi(i)=0.0

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict

            if (tair(i) .lt. t0) then
               y1(i)=max( min(tairc(i), -1.), -31.)
               it(i)=int(abs(y1(i)))
               y1(i)=rn12a(it(i))
               y2(i)=rn12b(it(i))
               y3(i)=rn13(it(i))
               psfw(i)=r2is*max(d2t*y1(i)*(y2(i)+r12r(i)*qc(i))*  &
                            qi(i), 0.0)
               psfi(i)=r2is*y3(i)*qi(i)

               y4(i)=1./(tair(i)-c358)
               y5(i)=1./(tair(i)-c76)
               qsw(i)=rp0(i)*exp(c172-c409*y4(i))
               qsi(i)=rp0(i)*exp(c218-c580*y5(i))
               hfact=(qv(i)+qb0-qsi(i))/(qsw(i)-qsi(i)+cmin1)
                    
               if(hfact.gt.1.) hfact=1.
               sfact=1
               SSI(i)=(qv(i)+qb0)/qsi(i)-1.

               fssi=min(xssi,max(rssi,xssi*(tairc(i)+44.)/(44.0-38.0)))  
               fssi=rssi
               wssi=ww1(i,k)/100.-2.0
               if(wssi.gt.0.) fssi=rssi+min(xssi,wssi*0.01)
               fssi=min(ssi(i),fssi)


               if (tairc(i).le.-5.) then                         
                  r_nci=max(1.e-3*exp(-.639+12.96*fssi),0.528e-3)  
               else
                  r_nci=min(1.e-3*exp(-.639+12.96*fssi),0.528e-3)  
               endif  

               if (r_nci.gt.15.) r_nci=15.                  


               if( tairc(i) .lt. -40.0 ) then
                 c_nci = 5.0e-6*exp(0.304*40.0)
               else
                 c_nci = 5.0e-6*exp(0.304*abs(tairc(i)))
               end if
               r_nci = c_nci

               r_nci = max(r_nci,r00(i)*qi(i)/ami50)


               dd(i)=min((r00(i)*qi(i)/r_nci), ami40)   
               yy1=1.-aa2(it(i))                   
               sfact=(AMI50**YY1-AMI40**YY1)/(AMI50**YY1-dd(i)**YY1)


               esi(i)  = qsi(i)/rp0(i)*c610
               y3(i)   = 1./tair(i)

               term1     = y3(i)*(rn20a*y3(i)-rn20b)
               term2     = rn10c*tair(i)/esi(i)
               dd(i)   = term1+term2
               fdwv      = dd(i)/(term1+term2*dwv0/dwv(i))
               psfw(i) = max( d2t*y1(i)*fdwv*(y2(i)*fdwv   &
                               + r12r(i)*qc(i))*qi(i),0.0 )

               psfw(i)=0.0

               if (hfact.gt.0.) then
                  psfi(i)=r2is*psfi(i)*hfact*sfact*fdwv
               else
                  psfi(i)=0.
               endif 

               if(qi(i).le.1.e-5*fv0(i)*fv0(i)) psfi(i)=0.0

            endif   

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict











         y1(i)=abs( vg(i)-vs(i) )
         y2(i)=zs(i)*zg(i)
         y3(i)=5./y2(i)
         y4(i)=.08*y3(i)*y3(i)
         y5(i)=.05*y3(i)*y4(i)
         y2(i)=y1(i)*(y3(i)/zs(i)**5+y4(i)/zs(i)**3        &
                         +y5(i)/zs(i))

         pgacs(i)=r2ig*r2is*r9rf(i)*y2(i)*ftns(i)*ftng(i)*fros(i) 
        if(qs(i).le.cmin) pgacs(i)=0.
        if(qg(i).le.cmin) pgacs(i)=0.
         dgacs(i)=pgacs(i)
         dgacs(i)=0.0             

         wgacs(i)=0.0
         wgacs(i)=10.*r9rf(i)*y2(i)*ftns(i)*ftng(i)*fros(i)         



         if(qg(i).le.cmin) wgacs(i)=0.                
         if(qs(i).le.cmin) wgacs(i)=0.                

        dhacs(i)=0.0
        whacs(i)=0.0
        y1(i)=abs( vh(i)-vs(i) )
        y2(i)=zs(i)*zh(i)
        y3(i)=5./y2(i)
        y4(i)=.08*y3(i)*y3(i)
        y5(i)=.05*y3(i)*y4(i)
        dd(i)=Y1(i)*(Y3(i)/ZS(i)**5+Y4(i)/ZS(i)**3        &
               +Y5(i)/ZS(i))
        whacs(i)=r2ih*r2is*min(h9r(i)*dd(i)*ftnh(i)*ftns(i)*fros(i), &
				 qs(i)/d2t)
        if(qs(i).le.cmin) whacs(i)=0.
        if(qh(i).le.cmin) whacs(i)=0.

        dhacg(i)=0.0
        whacg(i)=0.0
        y1(i)=abs( vh(i)-vg(i) )
        y2(i)=zg(i)*zh(i)
        y3(i)=5./y2(i)
        y4(i)=.08*y3(i)*y3(i)
        y5(i)=.05*y3(i)*y4(i)
        dd(i)=Y1(i)*(Y3(i)/ZG(i)**5+Y4(i)/ZG(i)**3        &
               +Y5(i)/ZG(i))
        whacg(i)=r2ih*r2ig*min(h10r(i)*dd(i)*ftnh(i)*ftng(i),  &
				 qg(i)/d2t)
        if(r00(i)*qg(i).gt.qrog2)  &
	 whacg(i)=whacg(i)/roqg*roqg2*0.5        
        if(qg(i).le.cmin) whacg(i)=0.
        if(qh(i).le.cmin) whacg(i)=0.

        y1(i)=1./zg(i)**bg3
        esi(i)=1.0 
        dmicrong=(r00(i)*qg(i)/roqg/cpi/(tng*ftng(i)))**.25*1.e4
        if(r00(i)*qg(i).gt.qrog2)then
         y1(i)=1./zg(i)**bg3_2
         dmicrong=(r00(i)*qg(i)/roqg2/cpi/(tng*ftng(i)))**.25*1.e4
        endif
        esi(i)=min(1.,(dmicrong/500.)**1.1)       

        dgacw(i)= r2ig*esi(i)*r14f(i)*qc(i)*y1(i)*ftng(i)
        if(r00(i)*qg(i).gt.qrog2)  &
         dgacw(i)=r2ig*esi(i)*r14f2(i)*qc(i)*y1(i)*ftng(i)
        if(qg(i).le.cmin) dgacw(i)=0.

        dhacw(i)=0.0                                                     
        y2(i)=1./zh(i)**bh3                                           
        dhacw(i)=r2ih*max(h14r(i)*qc(i)*y2(i)*ftnh(i), 0.0)          
        if(qh(i).le.cmin) dhacw(i)=0.
        
        if(ihalmos.eq.1)then
         y2(i)=0.
         if ((tairc(i).le.hmtemp1).and.(tairc(i).ge.hmtemp4))  &
                                                    y2(i)=0.5
         if ((tairc(i).le.hmtemp2).and.(tairc(i).ge.hmtemp3))  &
                                                    y2(i)=1.
         pihmg(i)=r2ig*dgacw(i)*y2(i)*xnsplnt*1000.*xmsplnt
         dgacw(i)=r2ig*(dgacw(i)-pihmg(i))
         pihmh(i)=0.0                                                   
         pihmh(i)=r2ih*dhacw(i)*y2(i)*xnsplnt*1000.*xmsplnt            
         dhacw(i)=r2ih*(dhacw(i)-pihmh(i))                               
        endif 

        qgacw(i)=r2ig*dgacw(i)
        qhacw(i)=r2ih*dhacw(i)                                              
        y1(i)=1./zg(i)**bg3
        y5(i)=1.0
        if (vg(i).gt.0.) y5(i)=abs((vg(i)-vi(i))/vg(i))
        dgaci(i)= r2ig*y5(i)*r15f(i)*qi(i)*y1(i)*ftng(i)
        if(qg(i).le.cmin) dgaci(i)=0.
        dgaci(i)=0.0
        wgaci(i)=0.0
        wgaci(i)=y5(i)*r15af(i)*qi(i)*y1(i)*ftng(i)                
        if(r00(i)*qg(i).gt.qrog2)then                                      
          y1(i)=1./zg(i)**bg3_2
          wgaci(i)=y5(i)*r15af2(i)*qi(i)*y1(i)*ftng(i)              
        endif                                                             
        if(qg(i).le.cmin) wgaci(i)=0.                

        dhaci(i)=0.0                                                     
        whaci(i)=0.0                                                     
        y5(i)=1.0                                                      
        if(vh(i).gt.0.) y5(i)=abs((vh(i)-vi(i))/vh(i))         
        y2(i)=1./zh(i)**bh3                                          
        whaci(i)=r2ih*min(y5(i)*h15ar(i)*qi(i)*y2(i)*ftnh(i),  &
	qi(i)/d2t)   
        if(qh(i).le.cmin) whaci(i)=0.

        y1(i)=abs( vg(i)-vr(i) )
        y2(i)=zr(i)*zg(i)
        y3(i)=5./y2(i)
        y4(i)=.08*y3(i)*y3(i)
        y5(i)=.05*y3(i)*y4(i)
        dd(i)=r16rf(i)*y1(i)*(y3(i)/zr(i)**5+y4(i)/zr(i)**3 &
                 +y5(i)/zr(i))*ftng(i)
        dgacr(i)=r2ig*max(dd(i),0.0)
        if(qg(i).le.cmin) dgacr(i)=0.
        if(qr(i).le.cmin) dgacr(i)=0.
        qgacr(i)=dgacr(i)

        pracg(i)=0.
        qracg(i)=0.
        y2(i)=zr(i)*zg(i)
        y3(i)=5./y2(i)
        y4(i)=.08*y3(i)*y3(i)
        y5(i)=.05*y3(i)*y4(i)
        pracg(i)=r16rf(i)/roqr*roqg*y1(i)*(y3(i)/zg(i)**5+y4(i) &
                   /zg(i)**3+y5(i)/zg(i))*ftng(i)
        if(r00(i)*qg(i).gt.qrog2) pracg(i)=pracg(i)/roqg*roqg2
        if(qg(i).le.cmin) pracg(i)=0.
        if(qr(i).le.cmin) pracg(i)=0.
        qracg(i)=min(d2t*pracg(i), qg(i))

        y1(i)=abs( vh(i)-vr(i) )
        y2(i)=zr(i)*zh(i)
        y3(i)=5./y2(i)
        y4(i)=.08*y3(i)*y3(i)
        y5(i)=.05*y3(i)*y4(i)
        DD(i)=h16r(i)*Y1(i)*ftnh(i)*(Y3(i)/ZR(i)**5   &
		+Y4(i)/ZR(i)**3+Y5(i)/ZR(i))
        dhacr(i)=r2ih*max(dd(i), 0.0)
        if(qh(i).le.cmin) dhacr(i)=0.
        if(qr(i).le.cmin) dhacr(i)=0.
        qhacr(i)=dhacr(i)

        if (tair(i) .ge. t0) then
          dgacs(i)=0.0
          wgacs(i)=0.0   
          whacs(i)=0.0
          whacg(i)=0.0
          dgacw(i)=0.0
          dhacw(i)=0.0                                                   
          dgaci(i)=0.0
          wgaci(i)=0.0   
          whaci(i)=0.0                                                   
          dgacr(i)=0.0
          pracg(i)=0.0
          dhacr(i)=0.0
        else
          pgacs(i)=0.0
          qgacw(i)=0.0
          qhacw(i)=0.0                                                   
          qgacr(i)=0.0
          qracg(i)=0.0
          qhacr(i)=0.0
        endif
 
        PGWET(i)=0.0
        if(tair(i) .lt. t0)then                         


           y1(i)=1./(alf+rn17c*max(tairc(i),-75.0)) 
           y2(i)=r191r(i)/zg(i)**2+r192rf(i)/zg(i)**bgh5                   
           if(r00(i)*qg(i).gt.qrog2)      &                                 
            y2(i)=(r191r(i)/zg(i)**2+r192rf2(i)/zg(i)**bgh5_2)              
           Y4(i)=ALVR(i)*DWV(i)*(RP0(i)-(QV(i)+QB0))-TCA(i)*TAIRC(i)   
           DD(i)=Y1(i)*(rn20*ftng(i)*Y4(i)*Y2(i)+(WGACI(i) &    
                 +WGACS(i))*(ALF+RN17B*TAIRC(i)))                       
           PGWET(i)=max(DD(i), 0.0)                                    
           if(qg(i).le.cmin) pgwet(i)=0.                               
        endif                                                              



        phwet(i)=0.0
        if (tair(i) .lt. t0) then
           y1(i)=1./(alf+rn17c*tairc(i))
           y3(i)=.78/zh(i)**2+h17aq(i)*scv(i)/zh(i)**bhh5
           Y4(i)=ALVR(i)*DWV(i)*(RP0(i)-(QV(i)+QB0))-TCA(i)*TAIRC(i)
           DD(i)=Y1(i)*(h17r(i)*ftnh(i)*Y4(i)*Y3(i)+(WHACI(i)+WHACS(i)   &
                   +WHACG(i))*(ALF+RN17B*TAIRC(i)))
           phwet(i)=r2ih*max(DD(i), 0.0)
           if(qh(i).le.cmin) phwet(i)=0.
         endif 

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict

 
        y1(i)=qc(i)/d2t
          psacw(i)=min(y1(i), psacw(i))
          pihms(i)=min(y1(i), pihms(i))
          praut(i)=min(y1(i), praut(i))
          pracw(i)=min(y1(i), pracw(i))
          psfw(i)= min(y1(i), psfw(i))
          dgacw(i)=min(y1(i), dgacw(i))
          pihmg(i)=min(y1(i), pihmg(i))
          dhacw(i)=min(y1(i), dhacw(i))                              
          pihmh(i)=min(y1(i), pihmh(i))                              
          qsacw(i)=min(y1(i), qsacw(i))
          qgacw(i)=min(y1(i), qgacw(i))
          qhacw(i)=min(y1(i), qhacw(i))                              

        y1(i)=d2t*(psacw(i)+praut(i)+pracw(i)+psfw(i)            &
               +dgacw(i)+qsacw(i)+qgacw(i)+pihms(i)+pihmg(i)     &
               +dhacw(i)+qhacw(i)+pihmh(i))                          

        qc(i)=qc(i)-y1(i)

        if (qc(i) .lt. 0.0) then
           y2(i)=1.
           if (y1(i) .ne. 0.) y2(i)=qc(i)/y1(i)+1.
           psacw(i)=psacw(i)*y2(i)
           praut(i)=praut(i)*y2(i)
           pracw(i)=pracw(i)*y2(i)
           psfw(i)=psfw(i)*y2(i)
           dgacw(i)=dgacw(i)*y2(i)
           dhacw(i)=dhacw(i)*y2(i)
           qsacw(i)=qsacw(i)*y2(i)
           qgacw(i)=qgacw(i)*y2(i)
           qhacw(i)=qhacw(i)*y2(i)
           pihms(i)=pihms(i)*y2(i)
           pihmg(i)=pihmg(i)*y2(i)
           pihmh(i)=pihmh(i)*y2(i)                                   
           qc(i)=0.0
        endif




         y1(i)=dgacw(i)+dgacr(i)                                     
         if(Y1(i).lt..95*pgwet(i).or.y1(i).eq.0.)THEN                
           wgaci(i)=0.0                                                  
           wgacs(i)=0.0                                                  
         endif                                                             
         pg2h(i)=0.0                                                     
         if(y1(i).gt.0..and.pgwet(i).gt.0..and.  &                     
            Y1(i).gt.1.0*pgwet(i))THEN                                 

          pg2h(i)=qg(i)/d2t                                            

          pg2h(i)=min(pg2h(i), qg(i)/d2t)                            
         endif                                                             

         whacr(i)=phwet(i)-dhacw(i)-whaci(i)-whacs(i)-whacg(i)    
         y2(i)=dhacw(i)+dhacr(i)                                        
          if(y2(i).lt.0.95*phwet(i).or.y2(i).eq.0.) THEN
            whacr(i)=0.0                                                    
            whaci(i)=0.0                                                    
            whacg(i)=0.0
            whacs(i)=0.0
          endif
          
         primh(i)=0.0
         if(y2(i).gt.0..and.phwet(i).gt.0..and. Y2(i).lt..95*phwet(i)) THEN

          rim_frac(i)=2.0*min((1.-y2(i)/phwet(i))**2,1.0)
          rim_frac(i)=rim_frac(i)*min((tairc(i)/(t00-t0))**2,1.0)

          primh(i)=rim_frac(i)*dhacw(i)
          primh(i)=min(primh(i), qh(i)/d2t)
         endif

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict



        y1(i)=qi(i)/d2t
        psaut(i)=min(y1(i), psaut(i))
        psaci(i)=min(y1(i), psaci(i))
        praci(i)=min(y1(i), praci(i))
        psfi(i)= min(y1(i), psfi(i))
        dgaci(i)=min(y1(i), dgaci(i))
        wgaci(i)=min(y1(i), wgaci(i))
        whaci(i)=min(y1(i), whaci(i))                             

        qi(i)=qi(i)+d2t*(pihms(i)+pihmg(i)+pihmh(i))             

        y1(i)=d2t*(psaut(i)+psaci(i)+praci(i)+psfi(i)       &
                    +dgaci(i)+wgaci(i)+whaci(i))      

        qi(i)=qi(i)-y1(i)

        if (qi(i) .lt. 0.0) then
           y2(i)=1.
           if (y1(i) .ne. 0.0) y2(i)=qi(i)/y1(i)+1.
           psaut(i)=psaut(i)*y2(i)
           psaci(i)=psaci(i)*y2(i)
           praci(i)=praci(i)*y2(i)
           psfi(i)=psfi(i)*y2(i)
           dgaci(i)=dgaci(i)*y2(i)
           wgaci(i)=wgaci(i)*y2(i)
           whaci(i)=whaci(i)*y2(i)                                   
           qi(i)=0.0
        endif

            wgacr(i)=qgacr(i)+qgacw(i)
            dlt3(i)=0.0
              if (qr(i) .lt. 1.e-4) dlt3(i)=1.
            dlt4(i)=1.

          if (qc(i) .gt. 5.e-4) dlt4(i)=0.0

              if (qs(i) .le. 1.e-4) dlt4(i)=1.

            if (tair(i) .ge. t0) then
               dlt3(i)=0.0
               dlt4(i)=0.0
            endif
         
            pr(i)=d2t*(qsacw(i)+praut(i)+pracw(i)+wgacr(i)   &
                   +qhacw(i)-qgacr(i))
            ps(i)=d2t*(psaut(i)+psaci(i)+dlt4(i)*psacw(i)    &  
                   +psfw(i)+psfi(i))
            pg(i)=d2t*(dlt3(i)*praci(i)+dgaci(i)               &
                   +wgaci(i)+dgacw(i)+(1.-dlt4(i))*psacw(i)    &
                   +primh(i))    







       qracs(i)=0.0
       y1(i)=abs(vr(i)-vs(i))
       y2(i)=zr(i)*zs(i)
       y3(i)=5./y2(i)
       y4(i)=.08*y3(i)*y3(i)
       y5(i)=.05*y3(i)*y4(i)

       pracs(i)=r2ig*r2is*(r7rf(i)*y1(i)*(y3(i)/zs(i)**5 &                   
                  +y4(i)/zs(i)**3+y5(i)/zs(i))*ftns(i)*fros(i))       
        if(qs(i).le.cmin) pracs(i)=0.
        if(qr(i).le.cmin) pracs(i)=0.
       qracs(i)=r2ig*r2is*min(d2t*pracs(i), qs(i))
       psacr(i)=r2is*(r8rf(i)*y1(i)*(y3(i)/zr(i)**5 &
                  +y4(i)/zr(i)**3+y5(i)/zr(i))*ftns(i))
        if(qs(i).le.cmin) psacr(i)=0.
        if(qr(i).le.cmin) psacr(i)=0.
       qsacr(i)=psacr(i)

         if (tair(i) .ge. t0) then
          pracs(i)=0.0
          psacr(i)=0.0
         else
          qsacr(i)=0.0
          qracs(i)=0.0
         endif


          pgaut(i)=0.0
          pgfr(i)=0.0
          phfr(i)=0.0
       if (tair(i) .lt. t0) then
            y1(i)=exp(rn18a*(t0-tair(i)))
          if( qr(i).ge.0.)then
           temp = 1./zr(i)
           temp = temp*temp*temp*temp*temp*temp*temp
           phfr(i)=r2ih*max(r18r(i)*(y1(i)-1.)*temp, 0.0)

          else
           temp = 1./zr(i)
           temp = temp*temp*temp*temp*temp*temp*temp
           pgfr(i)=r2ig*max(r18r(i)*(y1(i)-1.)*temp, 0.0)

           if(qr(i).le.cmin) pgfr(i)=0.
          endif
       endif







          y1(i)=qr(i)/d2t
          y2(i)=-qh(i)/d2t
         piacr(i)=min(y1(i), piacr(i))
         dgacr(i)=min(y1(i), dgacr(i))
         dhacr(i)=min(y1(i), dhacr(i))


         psacr(i)=min(y1(i), psacr(i))
         pgfr(i)= min(y1(i), pgfr(i))
         phfr(i)= min(y1(i), phfr(i))
         del=0.
         IF(whacr(i) .LT. 0.) DEL=1.
         if(del.eq.1) whacr(i)=max(whacr(i),-dhacw(i))    
         dhacr(i)=min((1.-del)*whacr(i),dhacr(i))
         if(del.eq.1) dhacw(i)=dhacw(i)+del*whacr(i)      
          y1(i)=(piacr(i)+dgacr(i)+dhacr(i)+psacr(i)+pgfr(i) &
                 +phfr(i))*d2t
          qr(i)=qr(i)+pr(i)+qracs(i)-del*whacr(i)*d2t+qracg(i) 
          qr(i)=qr(i)-y1(i)                                          
          if (qr(i) .lt. 0.0) then
             y2(i)=1.
             if (y1(i) .ne. 0.0) y2(i)=qr(i)/y1(i)+1.
             piacr(i)=piacr(i)*y2(i)
             dgacr(i)=dgacr(i)*y2(i)
             dhacr(i)=dhacr(i)*y2(i)

             pgfr(i)=pgfr(i)*y2(i)
             phfr(i)=phfr(i)*y2(i)
             psacr(i)=psacr(i)*y2(i)
             qr(i)=0.0
          endif 
          dlt2(i)=1.
          if (qr(i) .gt. 1.e-4) dlt2(i)=0.
          if (tair(i) .ge. t0) dlt2(i)=0.
          y1(i)=qs(i)/d2t
          pgacs(i)=min(y1(i), pgacs(i))
          dgacs(i)=min(y1(i), dgacs(i))
          wgacs(i)=min(y1(i), wgacs(i))
          whacs(i)=min(y1(i), whacs(i))
          pracs(i)=min(y1(i), pracs(i))
          pwacs(i)=min(y1(i), pwacs(i))

          prn(i)=d2t*(dlt3(i)*piacr(i)+dlt2(i)*dgacr(i)+pgfr(i)     &
                  +dlt2(i)*psacr(i))
          pracs(i)=(1.-dlt2(i))*pracs(i)
          pwacs(i)=(1.-dlt4(i))*pwacs(i)
          pracg(i)=(1.-dlt2(i))*pracg(i)
          psn(i)=d2t*(pgacs(i)+dgacs(i)+wgacs(i)   &
                   +pracs(i)+pwacs(i)+whacs(i))
 
          qs(i)=qs(i)+ps(i)-qracs(i)-psn(i)
          if (qs(i) .lt. 0.0) then
             y2(i)=1.
             if (psn(i) .ne. 0.) y2(i)=qs(i)/psn(i)+1.
             pgacs(i)=pgacs(i)*y2(i)
             dgacs(i)=dgacs(i)*y2(i)
             wgacs(i)=wgacs(i)*y2(i)
             whacs(i)=whacs(i)*y2(i) 
             pracs(i)=pracs(i)*y2(i)
             pwacs(i)=pwacs(i)*y2(i)
             qs(i)=0.0
          endif
          psn(i)=d2t*(pgacs(i)+dgacs(i)+wgacs(i)    &
                    +pracs(i)+pwacs(i))
          qg(i)=qg(i)+pg(i)+prn(i)+psn(i)-qracg(i)
          qg(i)=qg(i)-d2t*(pracs(i)+whacg(i)  &
                                                       +pracg(i)  &   
                                                       +pg2h(i))      

          if (qg(i) .lt. 0.0) then                              
           y2(i)=1.                                             
            if (whacg(i)+pracg(i)+pg2h(i).ne. 0.)   &                 
                y2(i)=qg(i)/(d2t*(whacg(i)+pracg(i)+pg2h(i)))+1.  
            whacg(i)=whacg(i)*y2(i)                         
            pracg(i)=pracg(i)*y2(i)                         
            pg2h(i)=pg2h(i)*y2(i)                           
            qg(i)=0.0                                           
          endif        

            qh(i)=qh(i)+d2t*(phfr(i)+(1.-dlt3(i))*piacr(i)        &
                   +(1.-dlt3(i))*praci(i)+(1.-dlt2(i))*psacr(i)     &
                   +pracs(i)+(1.-dlt2(i))*dgacr(i)+pracg(i)         &
                   +dhacw(i)+dhacr(i)-primh(i)+whaci(i)             &
                   +whacs(i)+whacg(i)+pg2h(i))

          y1(i)=d2t*(psacw(i)+psfw(i)+dgacw(i)+piacr(i)             &
                 +dgacr(i)+psacr(i)+pgfr(i)+phfr(i)+pihms(i)        &
                 +pihmg(i)+dhacw(i)+dhacr(i)           +pihmh(i))     &
                 -qracs(i)
          pt(i)=pt(i)+afcp(i)*y1(i)
          tair(i)=(pt(i)+tb0)*pi0(i)




          psmlt(i)=0.0
          pgmlt(i)=0.0
          phmlt(i)=0.0

          qhz2=qhwrf(i,k)
          qgz2=qgwrf(i,k)
          if (k .lt. kte-2 .and. tairc(i) .ge. -5) then
             qhz2=qhwrf(i,k+1)
             qgz2=qgwrf(i,k+1)
          endif
          call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))
          call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))
          call sgmap(3,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),fros0(i))
          call sgmap(4,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftnh0(i))

         if (tair(i).ge.t0) then
           tairc(i)=tair(i)-t0
           ftns(i)=1.
           ftng(i)=1.
           ftnh(i)=1.
           ftns(i)=ftns0(i)
           ftng(i)=ftng0(i)
           ftnh(i)=ftnh0(i)
           dd(i)=r11t*tairc(i)*(r101r(i)/zs(i)**2+r102rf(i)  &
                               /zs(i)**bsh5)*ftns(i)
           psmlt(i)=r2is*min(qs(i),max(dd(i),0.0))
           if (r00(i)*qg(i).gt.qrog2) then
             y2(i)=(r191r(i)/zg(i)**2+r192rf2(i)/zg(i)**bgh5_2)*ftng(i)
           else
             y2(i)=(r191r(i)/zg(i)**2+r192rf(i)/zg(i)**bgh5)*ftng(i)
           endif 
           dd1(i)=tairc(i)*(r19t*y2(i)+r19at*(qgacw(i) &
                                             +qgacr(i)))
           pgmlt(i)=r2ig*min(qg(i),max(dd1(i),0.0))
       
           Y1(i)=TCA(i)*TAIRC(i)-ALVR(i)*DWV(i)*(RP0(i)-(QV(i)+QB0))
           Y3(i)=.78/ZH(i)**2+h19aq(i)*SCV(i)/ZH(i)**BHH5
           DDa0(i)=h19rt(i)*Y1(i)*Y3(i)*ftnh(i)+r19at*TAIRC(i)*  &
             (QHACW(i)+QHACR(i))
           PHMLT(i)=r2ih*max(0.0, min(DDa0(i), QH(i)))

           pt(i)=pt(i)-afcp(i)*(psmlt(i)+pgmlt(i)+phmlt(i))
           tair(i)=(pt(i)+tb0)*pi0(i)
           qr(i)=qr(i)+psmlt(i)+pgmlt(i)+phmlt(i)
           qs(i)=qs(i)-psmlt(i)
           qg(i)=qg(i)-pgmlt(i)
           qh(i)=qh(i)-phmlt(i)

         endif 








       if (qc(i).le.cmin1) qc(i)=0.0
       if (qi(i).le.cmin1) qi(i)=0.0

       ftns(i)=1.
       ftng(i)=1.
       ftns0(i)=1.
       ftng0(i)=1.

       qhz2=qhwrf(i,k)
       qgz2=qgwrf(i,k)
       if (k .lt. kte-2 .and. tairc(i) .ge. -5) then
         qhz2=qhwrf(i,k+1)
         qgz2=qgwrf(i,k+1)
       endif
       call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))
       call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))

       if (tair(i).le.t00) then
          pihom(i)=qc(i)
       else
          pihom(i)=0.0
       endif 
       if (tair(i).ge.t0) then
          pimlt(i)=qi(i)
       else
          pimlt(i)=0.0
       endif
       pidw(i)=0.0
           
          if (tair(i) .lt. t0 .and. tair(i) .gt. t00) then
            TAIRC(i)=TAIR(i)-T0
             y1(i) = max( min(tairc(i), -1.), -31.)
             it(i) = int(abs(y1(i)))
             y2(i)=aa1(it(i))
             y3(i) = aa2(it(i))
             if (tairc(i).le.-5.)then                       
                rtair(i)=1./(tair(i)-c76)
                y5(i)=exp(c218-c580*rtair(i))
                qsi(i)=rp0(i)*y5(i)
                SSI(i)=(qv(i)+qb0)/qsi(i)-1.
                fssi=min(xssi,max(rssi,xssi*(tairc(i)+44.)/(44.0-38.0))) 
                fssi=rssi
                wssi=ww1(i,k)/100.-2.0
                if(wssi.gt.0.) fssi=rssi+min(xssi,wssi*0.01)
                fssi=min(ssi(i),fssi)

                r_nci=max(1.e-3*exp(-.639+12.96*fssi), 0.528e-3)      
                                                                      
                if (r_nci.gt.15.) r_nci=15.                          


              if( tairc(i) .lt. -40.0 ) then
                c_nci = 5.0e-6*exp(0.304*40.0)
              else
                c_nci = 5.0e-6*exp(0.304*abs(tairc(i)))
              end if
              r_nci = c_nci

              r_nci = max(r_nci,r00(i)*qi(i)/ami50)


                dd(i)=(r00(i)*qi(i)/r_nci)**y3(i)                  
                PIDW(i)=min(rr0(i)*D2T*y2(i)*r_nci*dd(i), qc(i)) 


                 esi(i)  = qsi(i)/rp0(i)*c610
                 y4(i)   = 1./tair(i)

                 term1     = y4(i)*(rn20a*y4(i)-rn20b)
                 term2     = rn10c*tair(i)/esi(i)
                 fdwv      = (term1+term2)/(term1+term2*dwv0/dwv(i))
              PIDW(i)=min(rr0(i)*D2T*y2(i)*r_nci*dd(i)*fdwv,qc(i)) 
         endif  

             pimm(i)=0.0
             pcfr(i)=0.0

             if (qc(i) .gt. 0.0) then
                y4(i) = 1./(tair(i)-c358)
                qsw(i)=rp0(i)*exp(c172-c409*y4(i))
                xncld=qc(i)/4.e-9                         
                esat=0.6112*exp(17.67*tairc(i)/(tairc(i)+243.5))*10.
                rv=0.622*esat/(p0(i,k)/1000.-esat)
                rlapse_m=980.616*(1.+2.5e6*rv/287./tair(i))/          &     
                       (1004.67+2.5e6*2.5e6*rv*0.622/(287.*tair(i)*tair(i)))
                delT=rlapse_m*ww1(i,k)                     
                if (delT.lt.0.) delT=0.
                Bhi=1.01e-2 
                pimm(i)=xncld*Bhi*4.e-9*exp(-tairc(i))*delT*d2t*4.e-9
 
                xccld=xncld*r00(i)                           
                Xknud=7.37*tair(i)/(288.*Ra*p0(i,k))  
                alpha=1.257+0.400*exp(-1.10/Xknud)     
 
                cunnF=1.+alpha*Xknud                   

                if (tairc(i).ge.0.) then
                    dvair=(1.718+0.0049*tairc(i))*1.e-4   
                       
                else 
                    dvair=(1.718+0.0049*tairc(i)-1.2e-5*tairc(i)**2)*1.e-4  
                       
                endif 
                DIFFar=1.3804e-16*tair(i)/6./cpi/dvair/Ra*cunnF     
                if (qv(i)+qb0-qsw(i).lt.0.) then                       
                pcfr(i)=4.e-9*4.*cpi*Rc*DIFFar*xccld*Cna*rr0(i)*d2t    
                endif
             else  
                tairc(i)=tair(i)-t0
                y1(i)=max( min(tairc(i), -1.), -31.)
                it(i)=int(abs(y1(i)))
                y2(i)=aa1(it(i))
                y3(i)=aa2(it(i))
                y4(i)=exp(abs(.5*tairc(i)))
                dd(i)=(r00(i)*qi(i)/(r25a*y4(i)))**y3(i)
                pidw(i)=min(r25rt(i)*y2(i)*y4(i)*dd(i),qc(i))
             endif  
          endif  

        ENDDO
!dir$ vector aligned
        DO i=1,irestrict


       y1(i)=pihom(i)+pidw(i)+pimm(i)+pcfr(i)-pimlt(i)

         if (y1(i).gt.qc(i)) then
            y1(i)=qc(i)
            y2(i)=1.
            y3(i)=pihom(i)+pidw(i)+pimm(i)+pcfr(i)
            if(y3(i).ne.0.) y2(i)=(qc(i)+pimlt(i))/y3(i)
            pihom(i)=pihom(i)*y2(i)
            pidw(i)=pidw(i)*y2(i)
            pimm(i)=pimm(i)*y2(i)
            pcfr(i)=pcfr(i)*y2(i)
         endif  

         pt(i)=pt(i)+afcp(i)*y1(i)
         tair(i)=(pt(i)+tb0)*pi0(i)
         qc(i)=qc(i)-y1(i)
         qi(i)=qi(i)+y1(i)








         tair(i)=(pt(i)+tb0)*pi0(i)
         if (tair(i) .lt. t0) then
            if (qi(i) .le. cmin) qi(i)=0.
            tairc(i)=tair(i)-t0
            rtair(i)=1./(tair(i)-c76)
            y2(i)=exp(c218-c580*rtair(i))
            qsi(i)=rp0(i)*y2(i)
            esi(i)=c610*y2(i)
            ssi(i)=(qv(i)+qb0)/qsi(i)-1.
            y1(i)=1./tair(i)
            y3(i)=SQRT(qi(i))

            term1   = y1(i)*(rn20a*y1(i)-rn20b)
            term2   = rn10c*tair(i)/esi(i)
            dd(i) = term1+term2
            fdwv    = dd(i)/(term1+term2*dwv0/dwv(i))

            dm(i)=max(qv(i)+qb0-qsi(i),0.0)
            rsub1(i)=cs580(i)*qsi(i)*rtair(i)*rtair(i)
            dep(i)=dm(i)/(1.+rsub1(i))
            if (tairc(i).le.-5.) then     
                y4(i)=1./(tair(i)-c358)
                qsw(i)=rp0(i)*exp(c172-c409*y4(i))
                fssi=min(xssi,max(rssi,xssi*(tairc(i)+44.)/(44.-38.)))
                fssi=rssi
                wssi=ww1(i,k)/100.-2.0
                if(wssi.gt.0.) fssi=rssi+min(xssi,wssi*0.01)
                fssi=min(ssi(i),fssi)

                r_nci=max(1.e-3*exp(-.639+12.96*fssi),0.528e-3)
       	        if (r_nci.gt.15.) r_nci=15.   


                if( tairc(i) .lt. -40.0 ) then
                  c_nci = 5.0e-6*exp(0.304*40.0)
                else
                  c_nci = 5.0e-6*exp(0.304*abs(tairc(i)))
                end if
                r_nci = c_nci

                r_nci = max(r_nci,r00(i)*qi(i)/ami50)


                pidep(i)=max(R32RT(i)*1.e4*fssi*sqrt(r_nci)*y3(i)/     & 
                dd(i)*fdwv, -qi(i))                    
                if(qi(i).le.cmin) pidep(i)=0.
                dd(i)=max(1.e-9*r_nci/r00(i)-qci(i,k)*1.e-9/ami50, 0.) 
                pint(i)=max(min(dd(i),dm(i)),0.)
                pint(i)=min(pint(i)+pidep(i), dep(i))
                pt(i)=pt(i)+ascp(i)*pint(i)
                tair(i)=(pt(i)+tb0)*pi0(i)
                qv(i)=qv(i)-pint(i)
                qi(i)=qi(i)+pint(i)
            endif  
          endif  















         dep(i)=0.0
         cnd(i)=0.0
         tair(i)=(pt(i)+tb0)*pi0(i)
         if (tair(i).ge.t00) THEN
            y1(i)=1./(tair(i)-c358)
            qsw(i)=rp0(i)*exp(c172-c409*y1(i))
            dd(i)=cp409(i)*y1(i)*y1(i)
            dm(i)=qv(i)+qb0-qsw(i)
            cnd(i)=dm(i)/(1.+avcp(i)*dd(i)*qsw(i))

            cnd(i)=max(-qc(i),cnd(i))





            if(ww1(i,k) .ge. thresh_evap)  cnd(i)=max(0.,cnd(i)) 

            pt(i)=pt(i)+avcp(i)*cnd(i)
            tair(i)=(pt(i)+tb0)*pi0(i)
            qv(i)=qv(i)-cnd(i)
            qc(i)=qc(i)+cnd(i)
         endif
         if (tair(i).le.273.16) THEN

            y1(i)=1./(tair(i)-c358)
            qsw(i)=rp0(i)*exp(c172-c409*y1(i))
            y2(i)=1./(tair(i)-c76)
            qsi(i)=rp0(i)*exp(c218-c580*y2(i))
            fssi=min(xssi,max(rssi,xssi*(tair(i)-t0+44.0)/(44.0-38.0)))
            fssi=rssi
            wssi=ww1(i,k)/100.-2.0
            if(wssi.gt.0.) fssi=rssi+min(xssi,wssi*0.01)
            y3(i)=1.+min((qsw(i)-qsi(i))/qsi(i), fssi)
            y4(i)=qsi(i)*y3(i)
            if (tair(i).le.268.16.and.(qv(i)+qb0.gt.y4(i))) then
               dd1(i)=cp580(i)*y2(i)*y2(i)
               dep(i)=(qv(i)+qb0-y4(i))/(1.+ascp(i)*dd1(i)*y4(i))
            else if (qv(i)+qb0.lt.xsubi*qsi(i).and.qi(i).gt.cmin) then
               dd1(i)=cp580(i)*y2(i)*y2(i)
               dep(i)=(qv(i)+qb0-xsubi*qsi(i))/(1.+ascp(i)*dd1(i)*xsubi*qsi(i))
               dep(i)=max(-qi(i),dep(i))
            endif
            if(ww1(i,k).ge.0.)  &
              dep(i)=max(0.,dep(i))                                
            pt(i)=pt(i)+ascp(i)*dep(i)
            tair(i)=(pt(i)+tb0)*pi0(i)
            qv(i)=qv(i)-dep(i)
            qi(i)=qi(i)+dep(i)
         endif




        psdep(i)=0.0
        pgdep(i)=0.0
        phdep(i)=0.0
        pssub(i)=0.0
        pgsub(i)=0.0
        phsub(i)=0.0
        pvapg(i)=0.0
        pvaph(i)=0.0

        if (qc(i)+qi(i).gt.1.e-5) then
           dlt1(i)=1.
        else    
           dlt1(i)=0.
        endif

           if (tair(i) .lt. t0) then 
              rtair(i)=1./(tair(i)-c76)
              y2(i)=exp(c218-c580*rtair(i))
              qsi(i)=rp0(i)*y2(i)
              esi(i)=c610*y2(i)
              ftns(i)=1.
              ftng(i)=1. 
              ftnh(i)=1.             

              SSI(i)=(QV(i)+QB0)/QSI(i)-1.
                IF(SSI(i).GT.0.) DLT1(i)=1.
                IF(SSI(i).LE.0.) DLT1(i)=0.
              DM(i)=QV(i)+QB0-QSI(i)
              RSUB1(i)=cs580(i)*QSI(i)*RTAIR(i)*RTAIR(i)
              DD1(i)=DM(i)/(1.+RSUB1(i))
              Y3(i)=1./TAIR(i)

             term1   = y3(i)*(rn20a*y3(i)-rn20b)
             term2   = rn10c*tair(i)/esi(i)
             dd(i) = term1+term2
             fdwv    = dd(i)/(term1+term2*dwv0/dwv(i))
              TAIRC(i)=TAIR(i)-T0

            ftns(i)=1.
            ftng(i)=1.
            ftnh(i)=1.
            ftns0(i)=1.
            ftng0(i)=1.
            qhz2=qhwrf(i,k)
            qgz2=qgwrf(i,k)
            if (k .lt. kte-2 .and. tairc(i) .ge. -5) then
               qhz2=qhwrf(i,k+1)
               qgz2=qgwrf(i,k+1)
            endif
            call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))
            call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))
            call sgmap(3,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),fros0(i))
            call sgmap(4,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftnh0(i))

            ftns(i)=ftns0(i)
            ftng(i)=ftng0(i)
            ftnh(i)=ftnh0(i)

            Y4(i)=r10t*SSI(i)*(r101r(i)/ZS(i)**2+r102rf(i)/ZS(i)**BSH5)   &
                                /DD(i)*ftns(i)*fdwv
            PSDEP(i)=r2is*max(-QS(i), Y4(i))
            if(qs(i).le.cmin) psdep(i)=0.
            DD(i)=Y3(i)*(RN20A*Y3(i)-RN20B)+RN10C*TAIR(i)/ESI(i)
            Y2(i)=r191r(i)/ZG(i)**2+r192rf(i)/ZG(i)**BGH5
            if(r00(i)*qg(i).gt.qrog2) &
              y2(i)=(r191r(i)/zg(i)**2+r192rf2(i)/zg(i)**bgh5_2)
              PGDEP(i)=r2ig*MAX(-qg(i), R20T*SSI(i)*Y2(i)/DD(i)          &
                                     *ftng(i)*fdwv)
            if(qg(i).le.cmin) pgdep(i)=0.
           Y1(i)=h10ar(i)/(TCA(i)*TAIR(i)**2)+1./(DWV(i)*QSI(i))
           Y2(i)=.78/ZH(i)**2+h20bq(i)*SCV(i)/ZH(i)**BHH5
           PHDEP(i)=r2ih*MAX(-qh(i), h20t(i)*ftnh(i)*SSI(i)*Y2(i)/Y1(i))
                if(qh(i).le.cmin) phdep(i)=0.
          PSSUB(i)=min(PSDEP(i), 0.)
          PSDEP(i)=max(PSDEP(i), 0.)
          PGSUB(i)=min(PGDEP(i), 0.)
          PGDEP(i)=max(PGDEP(i), 0.)
          PHSUB(i)=min(PHDEP(i), 0.)
          PHDEP(i)=max(PHDEP(i), 0.)


              Y5(i)=min(0.,DD1(i))
              DD1(i)=max(0.,DD1(i))

         IF(DLT1(i).EQ.1.)THEN                              
           Y1(i)=PSDEP(i)+PGDEP(i)+PHDEP(i)
           IF(Y1(i).ge.DD1(i))THEN
            PSDEP(i)=PSDEP(i)/Y1(i)*DD1(i)            
            PGDEP(i)=PGDEP(i)/Y1(i)*DD1(i)
            PHDEP(i)=PHDEP(i)/Y1(i)*DD1(i)
           ENDIF
         ENDIF                                                

         if(qc(i).le.1.e-5) then
          vap_frac=2.0*min((tairc(i)/(t00-t0))**2,1.0)
          pvapg(i)=vap_frac*pgdep(i)
          pvaph(i)=vap_frac*phdep(i)
         endif
         if(pgdep(i).gt.0.)          &
           pvapg(i)=min(pvapg(i),qg(i)+pgdep(i))
         if(phdep(i).gt.0.)          &
           pvaph(i)=min(pvaph(i),qh(i)+phdep(i))


         IF(DLT1(i).EQ.0.)THEN
           Y1(i)=MAX(PSsub(i)+PGsub(i)+phsub(i), Y5(i))
           IF(Y5(i).gt.(PSsub(i)+PGsub(i)+phsub(i)))THEN
            Y3(i)=(PSsub(i)+PGsub(i)+phsub(i))
            IF(Y3(i).ne.0.0)THEN
             PSsub(i)=PSsub(i)/Y3(i)*Y5(i)
             PGsub(i)=PGsub(i)/Y3(i)*Y5(i)
             Phsub(i)=Phsub(i)/Y3(i)*Y5(i)
            ENDIF
           ENDIF
         ENDIF
          PSSUB(i)=-PSSUB(i)
          PGSUB(i)=-PGSUB(i)
          PHSUB(i)=-PHSUB(i)
          Y1(i)=PSDEP(i)+PGDEP(i)+PHDEP(i)  &
                 -PSsub(i)-PGsub(i)-PHsub(i)

           pt(i)=pt(i)+ascp(i)*y1(i)
           tair(i)=(pt(i)+tb0)*pi0(i)
           qv(i)=qv(i)-y1(i)
           qs(i)=qs(i)+psdep(i)-pssub(i)+pvapg(i)+pvaph(i)
           qg(i)=qg(i)+pgdep(i)-pgsub(i)-pvapg(i)
           qh(i)=qh(i)+phdep(i)-phsub(i)-pvaph(i)
           endif   



           ern(i)=0.0
           if (qr(i) .gt. 0.0) then
             tair(i)=(pt(i)+tb0)*pi0(i)
             rtair(i)=1./(tair(i)-c358)
	     y2(i)=exp( c172-c409*rtair(i) )
	     esw(i)=c610*y2(i)
             qsw(i)=rp0(i)*y2(i)
             ssw(i)=(qv(i)+qb0)/qsw(i)-1.
             dm(i)=qv(i)+qb0-qsw(i)
             rsub1(i)=cv409(i)*qsw(i)*rtair(i)*rtair(i)
             dd1(i)=max(-dm(i)/(1.+rsub1(i)),0.0)
             y3(i)=1./tair(i)

             term1   = y3(i)*(rn30a*y3(i)-rn10b)
             term2   = rn10c*tair(i)/esw(i)
             dd(i) = term1+term2
             fdwv    = dd(i)/(term1+term2*dwv0/dwv(i))
             ftnw=1.
             if (qr(i) .gt. cmin.and.tair(i).gt.t0) then   
                 bin_factor(i)=0.11*(1000.*qr(i))**(-1.27) + 0.98
                 bin_factor(i)=min(bin_factor(i),1.30)
                 ftnw=1./bin_factor(i)**3.35
                 ftnwmin=r00(i)*qr(i)/draimax
                 if(qr(i).le.0.001) ftnw=max(ftnw,ftnwmin/tnw)

                 y4(i)=r00(i)*qr(i)
                 y1(i)=sqrt(y4(i))
                 y2(i)=sqrt(y1(i))
                 zr(i)=zrc/y2(i)*ftnw**0.25
             endif 
             y1(i)=-r23t*ssw(i)*(r231r(i)/zr(i)**2+r232rf(i)/       &
                                      zr(i)**3)/dd(i)*ftnw*fdwv
             ern(i)=min(dd1(i),qr(i),max(y1(i),0.0))
             pt(i)=pt(i)-avcp(i)*ern(i)
             tair(i)=(pt(i)+tb0)*pi0(i)
             tairc(i)=tair(i)-t0
             qv(i)=qv(i)+ern(i)
             qr(i)=qr(i)-ern(i)
          endif 









          pmlts(i)=0.0
          pmltg(i)=0.0

          tair(i)=(pt(i)+tb0)*pi0(i)
          tairc(i)=tair(i)-t0

          ftns0(i)=1.
          ftng0(i)=1.

          qhz2=qhwrf(i,k)
          qgz2=qgwrf(i,k)
          if (k .lt. kte-2 .and. tairc(i) .ge. -5) then
            qhz2=qhwrf(i,k+1)
            qgz2=qgwrf(i,k+1)
          endif
          call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))
          call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))
          call sgmap(3,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),fros0(i))

          if (tair(i) .ge. t0) then
             ftns(i)=1.
             ftng(i)=1.
                ftns(i)=ftns0(i)
                ftng(i)=ftng0(i)
             rtair(i)=1./(t0-c358)
             y2(i)=exp( c172-c409*rtair(i) )
             esw(i)=c610*y2(i)
             qsw(i)=rp0(i)*y2(i)
             ssw(i)=1.-(qv(i)+qb0)/qsw(i)
             dm(i)=qsw(i)-qv(i)-qb0
             rsub1(i)=cv409(i)*qsw(i)*rtair(i)*rtair(i)
             dd1(i)=max(dm(i)/(1.+rsub1(i)),0.0)
             y3(i)=1./tair(i)

            term1   = y3(i)*(rn30a*y3(i)-rn10b)
            term2   = rn10c*tair(i)/esw(i)
            dd(i) = term1+term2
            fdwv    = dd(i)/(term1+term2*dwv0/dwv(i))
             y1(i)=ftng(i)*r30t*ssw(i)*(r191r(i)/zg(i)**2+r192rf(i)    &
                   /zg(i)**bgh5)/dd(i)*fdwv
             if(r00(i)*qg(i).gt.qrog2)                   &
               y1(i)=ftng(i)*r30t*ssw(i)*(r191r(i)/zg(i)**2+r192rf2(i) &
                      /zg(i)**bgh5_2)/dd(i)*fdwv
             pmltg(i)=r2ig*min(qg(i),max(y1(i),0.0))
             y1(i)=ftns(i)*r33t*ssw(i)*(r331r(i)/zs(i)**2+r332rf(i)    &
                                           /zs(i)**bsh5)/dd(i)*fdwv
             pmlts(i)=r2is*min(qs(i),max(y1(i),0.0))
             y1(i)=min(pmltg(i)+pmlts(i),dd1(i))
             pmltg(i)=y1(i)-pmlts(i)
             pt(i)=pt(i)-ascp(i)*y1(i)
             tair(i)=(pt(i)+tb0)*pi0(i)
             qv(i)=qv(i)+y1(i)
             qs(i)=qs(i)-pmlts(i)
             qg(i)=qg(i)-pmltg(i)
          endif  

  
            if (qc(i) .le. cmin) qc(i)=0.
            if (qr(i) .le. cmin) qr(i)=0.
            if (qi(i) .le. cmin) qi(i)=0.
            if (qs(i) .le. cmin) qs(i)=0.
            if (qg(i) .le. cmin) qg(i)=0.
            if (qh(i) .le. cmin) qh(i)=0.
            dpt(i,k)=pt(i)
            dqv(i,k)=qv(i)
            qcl(i,k)=qc(i)
            qrn(i,k)=qr(i)
            qci(i,k)=qi(i)
            qcs(i,k)=qs(i)
            qcg(i,k)=qg(i)
            qch(i,k)=qh(i)





            dd(i)=max(-cnd(i), 0.)
            cnd(i)=max(cnd(i), 0.)
            dd1(i)=max(-dep(i), 0.)  
            dep(i)=max(dep(i), 0.)
            del=0.
            IF(whacr(i) .LT. 0.) DEL=1.



            sccc=cnd(i)
            seee=dd(i)+ern(i)
            sddd=dep(i)+amax1(pint(i),0.0)+psdep(i)+pgdep(i)+phdep(i)
            ssss=dd1(i)-amin1(pint(i),0.0)+pssub(i)+pgsub(i)+phsub(i)+pmlts(i)+pmltg(i)
            smmm=psmlt(i)+pgmlt(i)+pimlt(i)+qracs(i)+phmlt(i)+qracg(i) &
                 -del*whacr(i)
            sfff=psacw(i)*d2t+piacr(i)*d2t+psfw(i)*d2t+pgfr(i)*d2t   &
                +dgacw(i)*d2t+dgacr(i)*d2t+psacr(i)*d2t+pihom(i) &
                +pidw(i)+pimm(i)+pcfr(i)+pihms(i)*d2t    &
                +pihmg(i)*d2t+phfr(i)*d2t+dhacw(i)*d2t+dhacr(i)*d2t+pihmh(i)*d2t


            physc(i,k) = avc * sccc / d2t       
            physe(i,k) = avc * seee / d2t       
            physd(i,k) = asc * sddd / d2t       
            physs(i,k) = asc * ssss / d2t       
            physf(i,k) = afc * sfff / d2t       
            physm(i,k) = afc * smmm / d2t       

            acphysc(i,k) = acphysc(i,k) + avcp(i) * sccc 
            acphyse(i,k) = acphyse(i,k) + avcp(i) * seee 
            acphysd(i,k) = acphysd(i,k) + ascp(i) * sddd 
            acphyss(i,k) = acphyss(i,k) + ascp(i) * ssss 
            acphysf(i,k) = acphysf(i,k) + afcp(i) * sfff 
            acphysm(i,k) = acphysm(i,k) + afcp(i) * smmm 






          call sgmap(1,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftns0(i))   
          call sgmap(2,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftng0(i))   
          call sgmap(3,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),fros0(i))   
          call sgmap(4,qs(i),qg(i),qgz2,qh(i),qhz2,r00(i),tairc(i),ftnh0(i))   

        a_1=1.e6*r00(i)*qr(i)
        a_2=1.e6*r00(i)*qs(i)
        a_3=1.e6*r00(i)*qg(i)
        a_4=1.e6*r00(i)*qh(i) 

        ucor=3071.29/tnw**0.75
        ucos=687.97*roqs**0.25/tns**0.75
        ucog=687.97*roqg**0.25/tng**0.75
        ucog2=687.97*roqg2**.25/tng**.75
        ucoh=687.97*roqh**0.25/tnh**0.75
        uwet=4.464**0.95

        ftnw=1.
        if(qr(i).gt.cmin .and. qc(i).lt.cmin)then
             bin_factor(i)=0.11*(1000.*qr(i))**(-1.27) + 0.98

             bin_factor(i)=min(bin_factor(i),1.30)
             ftnw=1./bin_factor(i)**3.50
             ftnwmin=r00(i)*qr(i)/draimax
             if(qr(i).le.0.001) ftnw=max(ftnw,ftnwmin/tnw)
        endif
        a_11=ucor*(max(0.,a_1))**1.75/ftnw**0.75

        ftns(i)=1.
        ftng(i)=1.
        ftns(i)=ftns0(i)**0.75
        ftng(i)=ftng0(i)**0.75
        ftnh(i)=ftnh0(i)**0.75  
        fros(i)=1.
        fros(i)=fros0(i)**.25
        a_22=ucos*(max(0.,a_2))**1.75/ftns(i)*fros(i)
        a_33=ucog*(max(0.,a_3))**1.75/ftng(i)
        if(a_3.ge.qrog2) a_33=ucog2*(max(0.,a_3))**1.75/ftng(i)
        a_44=ucoh*(max(0.,a_4))**1.75/ftnh(i) 

         IF (TAIR(i).LT.273.16) THEN
            ZDRY = MAX(1.e-9,A_11+A_22+A_33+A_44) 
            DBZ(i,k) = 10.*ALOG10(ZDRY)
         ELSE         
            ZWET0 = A_11+UWET*(A_22+A_33+A_44)**.95
            ZWET = MAX(1.e-9,ZWET0)
            DBZ(i,k) = 10.*ALOG10(ZWET)
         ENDIF



        ENDDO
!dir$ vector aligned
        DO i=1,irestrict
      




    
      if (qrn(i,k) .lt. cmin) then
         re_rain_gsfc(i,k) = 0.e0
      else
         re_rain_gsfc(i,k) = eff_rad(zr(i))
      endif
    
      if (qcs(i,k) .lt. cmin) then
         re_snow_gsfc(i,k) = 0.e0
      else
         re_snow_gsfc(i,k) = eff_rad(zs(i))
      endif
    
      if (qcg(i,k) .lt. cmin) then
         re_graupel_gsfc(i,k) = 0.e0
      else
         re_graupel_gsfc(i,k) = eff_rad(zg(i))
      endif
    
      if (qch(i,k) .lt. cmin) then
         re_hail_gsfc(i,k) = 0.e0
      else
         re_hail_gsfc(i,k) = eff_rad(zh(i))
      endif



   if (qcl(i,k) .lt. cmin) then
      re_cloud_gsfc(i,k) = 0.e0
   else
      L_cloud = qcl(i,k) * rho(i,k)             
   
            
            
            if (xland(i) .eq. 1.0) then
               ccn_ref = ccn_over_land
            else if (xland(i) .eq. 2.0) then
               ccn_ref = ccn_over_water
            else
               print *,' xland is not 1. or 2., run stopped'
               
               call wrf_error_fatal3("<stdin>",4199,&
' xland is not 1. or 2., run stopped')

            endif
           
                   mu = min(15.e0, (1000.E0/ccn_ref + 2.e0))
                   gamfac3 = ( gamma_toshi(mu+4.e0) / gamma_toshi(mu+3.e0) )
                   gamfac1 = ( gamma_toshi(mu+4.e0) / gamma_toshi(mu+1.e0) )
                   lambda = (4.e0/3.e0*cpi*roqr*ccn_ref/L_cloud*   &
                            gamfac1)**(1.e0/3.e0)  
                   re_cloud_gsfc(i,k) = 1.e0/lambda * gamfac3 * 1.e4  
   endif 
      


   if (qci(i,k) .lt. cmin) then
      re_ice_gsfc(i,k) = 0.e0
   else
  
     
      re_ice_gsfc(i,k) = 125.e0 +(tair(i)-243.16)*5.e0     
      if (tair(i) .gt. 243.16) re_ice_gsfc(i,k) = 125.e0
      if (tair(i) .lt. 223.16) re_ice_gsfc(i,k) = 25.e0
   endif 



 ENDDO

 1000 continue



      do k=kts,kte
!dir$ vector aligned
       DO i=1,irestrict
         ptwrf(i,k) = dpt(i,k)
         qvwrf(i,k) = dqv(i,k)
         qlwrf(i,k) = qcl(i,k)
         qrwrf(i,k) = qrn(i,k)
         qiwrf(i,k) = qci(i,k)
         qswrf(i,k) = qcs(i,k)
         qgwrf(i,k) = qcg(i,k)
         qhwrf(i,k) = qch(i,k)
       ENDDO
      enddo 






        IF ( PRESENT (diagflag) ) THEN
        if (diagflag .and. do_radar_ref == 1) then
          do k=kts,kte
!dir$ vector aligned
            DO i=1,irestrict
                    refl_10cm(i,k) = max(-35.,dbz(i,k))
            ENDDO
          end do

        endif
        ENDIF

     
  END SUBROUTINE saticel_s
  
  SUBROUTINE auto_conversion( L, N, P , re)
  implicit none













 real,intent(in) :: L    
 real,intent(in) :: N    
 real,intent(out) :: P   
 real,intent(out) :: re  

 real :: mu   
 real :: eta  
 real :: beta, beta1, beta2     
 real :: gamfac , gfac1 , gfac2 
 real :: R6_6power  
 real :: R6_thresh 
 real :: R6        
 real :: Heaviside_func  
 real :: lambda    


 real,parameter :: Rc = 10.e0 * 1.e-4 
 real,parameter :: const_pi    = 3.14159e0 
 real,parameter :: const_kappa = 1.9e11    
                                           



 real,parameter :: const_rho_liq = 1.e0    
 real,parameter :: eta_func = ((3.e0/(4.e0*const_pi*const_rho_liq))**2) * const_kappa  
                                                                                       

 logical,parameter :: no_thresh = .true.  






 if( N <= 0.e0 .or. L <= 1.0e-32 ) then
   P = 0.e0
   return
 endif


















 mu = MIN(15.e0, (1000.E0/N + 2.e0))





 gfac1 = gamma_toshi(mu+4.e0)
 gfac2 = gamma_toshi(mu+1.e0)
 gamfac = (gfac1/gfac2)




 lambda = (4.e0/3.e0*const_pi*const_rho_liq*N/L*gamfac)**(1.e0/3.e0)  


 THRESH: if( no_thresh ) then 




 gfac1 = gamma_toshi(mu+7.e0)
 gfac2 = gamma_toshi(mu+1.e0)
 gamfac = (gfac1/gfac2)

 R6_6power = (1.e0 / lambda)**6.e0 * gamfac   




 P = const_kappa *   N     * R6_6power *    L      



 else  




 beta1 = (6.e0+mu)*(5.e0+mu)*(4.e0+mu)
 beta2 = (3.e0+mu)*(2.e0+mu)*(1.e0+mu)
 beta  = beta1 / beta2

 eta = eta_func * beta  




 R6_thresh = beta * Rc  




 gfac1 = gamma_toshi(6.e0+mu+1.e0)
 gfac2 = gamma_toshi(1.e0+mu)
 gamfac = (gfac1/gfac2)**(1.e0/6.e0)

 R6 = (1.e0 / lambda) * gamfac   




 if    ( R6 - R6_thresh <= 0.e0 ) then
    Heaviside_func = 0.e0
 elseif( R6 - R6_thresh > 0.e0 ) then
    Heaviside_func = 1.e0
 else

    write(wrf_err_message,*)'MSG: auto_conversion: Strange value of R6= ',R6
    call wrf_error_fatal3("<stdin>",4406,&
trim(wrf_err_message))

 endif




 P = eta * (1.e0/N) * (L**3) *  Heaviside_func



 endif THRESH 







 gfac1 = gamma_toshi(mu+4.e0)
 gfac2 = gamma_toshi(mu+3.e0)
 gamfac = (gfac1/gfac2)

 re = 1.e0/lambda * gamfac * 1.e4  







 END subroutine auto_conversion

!DIR$ ATTRIBUTES FORCEINLINE :: gamma_toshi
 real function gamma_toshi(x)












 implicit double precision (a-h,o-z)
 dimension g(26)
 data g/1.0d0,0.5772156649015329d0, &
       -0.6558780715202538d0, -0.420026350340952d-1, &
        0.1665386113822915d0,-.421977345555443d-1, &
        -.96219715278770d-2, .72189432466630d-2, &
        -.11651675918591d-2, -.2152416741149d-3, &
        .1280502823882d-3, -.201348547807d-4, &
        -.12504934821d-5, .11330272320d-5, &
        -.2056338417d-6, .61160950d-8, &
         .50020075d-8, -.11812746d-8, &
        .1043427d-9, .77823d-11, &
        -.36968d-11, .51d-12, &
        -.206d-13, -.54d-14, .14d-14, .1d-15/
 real :: x

 pi=3.141592653589793d0
 if (x.eq.int(x)) then
     if (x.gt.0.0d0) then
         ga=1.0d0
         m1=int(x)-1
        do k=2,m1
           ga=ga*k
        enddo
     else
        ga=1.0d+300
     endif
  else
     if (dabs(dble(x)).gt.1.0d0) then
         z=dabs(dble(x))
         m=int(z)
         r=1.0d0
        do k=1,m
           r=r*(z-k)
        enddo
        z=z-m
     else
        z=dble(x)
     endif
     gr=g(26)
     do k=25,1,-1
        gr=gr*z+g(k)
     enddo
     ga=1.0d0/(gr*z)
     if (dabs(dble(x)).gt.1.0d0) then
         ga=ga*r
         if (x.lt.0.0d0) ga=-pi/(x*ga*dsin(pi*x))
     endif
  endif

  gamma_toshi = real(ga)

  end function gamma_toshi

 SUBROUTINE Find_NaN_Inf_Double(Warning_MSG, real_input, i_in,j_in,k_in)
 implicit none

 real,intent(inout) :: real_input  
 integer,intent(in) :: i_in, j_in, k_in
 character*(*),intent(in) :: Warning_MSG






 if( 1e+10/real_input == 0. ) then
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'Infinity at',i_in,j_in,k_in
    real_input = 0.
    return
 endif




 if( real_input==0. .or. real_input>0. .or. real_input<0. .or. real_input>=0. .or. real_input<=0. ) then
 else
    print*,'MSG Find_NaN_Inf: '//Warning_MSG//'NaN at',i_in,j_in,k_in
    real_input = 0.
    return
 endif

 END SUBROUTINE Find_NaN_Inf_Double



      subroutine refl10cm_gsfc (qv1d, qr1d, qs1d, qg1d,                 &
                       t1d, p1d, dBZ, kts, kte, ii, jj)

      IMPLICIT NONE


      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                      qv1d, qr1d, qs1d, qg1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ


      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho
      REAL, DIMENSION(kts:kte):: rr, rs, rg

      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilams, ilamg
      DOUBLE PRECISION, DIMENSION(kts:kte):: N0_r, N0_s, N0_g
      DOUBLE PRECISION:: lamr, lams, lamg
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg

      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel
      DOUBLE PRECISION:: fmelt_s, fmelt_g

      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti

      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL, PARAMETER:: R=287.
      REAL, PARAMETER:: PIx=3.1415926536



      do k = kts, kte
         dBZ(k) = -35.0
      enddo




      do k = kts, kte
         temp(k) = t1d(k)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))

         if (qr1d(k) .gt. 1.E-9) then
            rr(k) = qr1d(k)*rho(k)
            N0_r(k) = xnor
            lamr = (xam_r*xcrg(3)*N0_r(k)/rr(k))**(1./xcre(1))
            ilamr(k) = 1./lamr
            L_qr(k) = .true.
         else
            rr(k) = 1.E-12
            L_qr(k) = .false.
         endif

         if (qs1d(k) .gt. 1.E-9) then
            rs(k) = qs1d(k)*rho(k)
            N0_s(k) = xnos
            lams = (xam_s*xcsg(3)*N0_s(k)/rs(k))**(1./xcse(1))
            ilams(k) = 1./lams
            L_qs(k) = .true.
         else
            rs(k) = 1.E-12
            L_qs(k) = .false.
         endif

         if (qg1d(k) .gt. 1.E-9) then
            rg(k) = qg1d(k)*rho(k)
               N0_g(k) = xnog
            lamg = (xam_g*xcgg(3)*N0_g(k)/rg(k))**(1./xcge(1))
            ilamg(k) = 1./lamg
            L_qg(k) = .true.
         else
            rg(k) = 1.E-12
            L_qg(k) = .false.
         endif
      enddo




      melti = .false.
      k_0 = kts
      do k = kte-1, kts, -1
         if ( (temp(k).gt.273.15) .and. L_qr(k)                         &
                                  .and. (L_qs(k+1).or.L_qg(k+1)) ) then
            k_0 = MAX(k+1, k_0)
            melti=.true.
            goto 195
         endif
      enddo
 195  continue







      do k = kts, kte
         ze_rain(k) = 1.e-22
         ze_snow(k) = 1.e-22
         ze_graupel(k) = 1.e-22
         if (L_qr(k)) ze_rain(k) = N0_r(k)*xcrg(4)*ilamr(k)**xcre(4)
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PIx)*(6.0/PIx)     &
                                 * (xam_s/900.0)*(xam_s/900.0)          &
                                 * N0_s(k)*xcsg(4)*ilams(k)**xcse(4)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PIx)*(6.0/PIx)  &
                                    * (xam_g/900.0)*(xam_g/900.0)       &
                                    * N0_g(k)*xcgg(4)*ilamg(k)**xcge(4)
      enddo










      if (melti .and. k_0.ge.kts+1) then
       do k = k_0-1, kts, -1


          if (L_qs(k) .and. L_qs(k_0) ) then
           fmelt_s = MAX(0.005d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           eta = 0.d0
           lams = 1./ilams(k)
           do n = 1, nrbins
              x = xam_s * xxDs(n)**xbm_s
              call rayleigh_soak_wetgraupel (x,DBLE(xocms),DBLE(xobms), &
                    fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_s, matrixstring_s,          &
                    inclusionstring_s, hoststring_s,                    &
                    hostmatrixstring_s, hostinclusionstring_s)
              f_d = N0_s(k)*xxDs(n)**xmu_s * DEXP(-lams*xxDs(n))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif



          if (L_qg(k) .and. L_qg(k_0) ) then
           fmelt_g = MAX(0.005d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
           eta = 0.d0
           lamg = 1./ilamg(k)
           do n = 1, nrbins
              x = xam_g * xxDg(n)**xbm_g
              call rayleigh_soak_wetgraupel (x,DBLE(xocmg),DBLE(xobmg), &
                    fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_g, matrixstring_g,          &
                    inclusionstring_g, hoststring_g,                    &
                    hostmatrixstring_g, hostinclusionstring_g)
              f_d = N0_g(k)*xxDg(n)**xmu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif

       enddo
      endif

      do k = kte, kts, -1
         dBZ(k) = 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.d18)
      enddo

      end subroutine refl10cm_gsfc





   real function eff_rad(lambda)

      use, intrinsic :: ieee_arithmetic
      implicit none










      real,intent(in)  :: lambda   




       if ( lambda <= 0.e0  .or. ieee_is_nan(lambda) ) then
          eff_rad = 0.e0
          return
       endif




       eff_rad = 1.5e0 / (lambda*100.) * 1.0e+6  

   end function eff_rad

END MODULE  module_mp_gsfcgce_4ice_nuwrf


