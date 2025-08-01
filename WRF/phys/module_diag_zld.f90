
























MODULE module_diag_zld
CONTAINS

   SUBROUTINE zld ( u,v,w,t,qv,zp,zb,pp,pb,p,pw,                    &
                    msfux,msfuy,msfvx,msfvy,msftx,msfty,            &
                    f,e,ht,                                         &
                    use_tot_or_hyd_p,extrap_below_grnd,missing,     &  
                    num_z_levels,max_z_levels,z_levels,             &
                    z_zl,u_zl,v_zl,t_zl,rh_zl,ght_zl,s_zl,td_zl,    &
                    q_zl,                                           &
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE module_model_constants
   
      IMPLICIT NONE
   
   
      
   
      INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &
                                                                         ims,ime, jms,jme, kms,kme, &
                                                                         its,ite, jts,jte, kts,kte
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &
                                                                         f,e,ht
      INTEGER, INTENT(IN   )                                          :: use_tot_or_hyd_p
      INTEGER, INTENT(IN   )                                          :: extrap_below_grnd
      REAL   , INTENT(IN   )                                          :: missing
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,pw
      INTEGER, INTENT(IN   )                                          :: num_z_levels, max_z_levels
      REAL   , INTENT(IN   ) , DIMENSION(max_z_levels)                :: z_levels
   
      
   
      REAL   , INTENT(  OUT) ,  DIMENSION(num_z_levels)                     :: z_zl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_z_levels , jms:jme) :: u_zl,v_zl,t_zl,rh_zl,ght_zl,s_zl,td_zl,q_zl
   
      
   
      REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12
      REAL, PARAMETER :: zshul=75., tvshul=290.66
   
      INTEGER :: i, j, ke, kz, ke_h, ke_f
      REAL    :: zu, zd, zm , &
                 tu, td     , &
                 su, sd     , &
                 uu, ud     , &
                 vu, vd     , &
                 qu, qd     , &
                 eu, ed, em , &
                 pu, pd, pm , &
                 du, dd
      REAL    :: es, qs
      REAL    :: part, gammas, tvu, tvd
   
      
   
      DO kz = 1 , num_z_levels
         z_zl(kz) = z_levels(kz)
      END DO
   
      
   
      DO j = jts , jte
         DO kz = 1 , num_z_levels
            DO i = its , ite
               u_zl  (i,kz,j) = missing
               v_zl  (i,kz,j) = missing
               t_zl  (i,kz,j) = missing
               rh_zl (i,kz,j) = missing
               ght_zl(i,kz,j) = missing
               s_zl  (i,kz,j) = missing
               td_zl (i,kz,j) = missing
            END DO
         END DO
      END DO
   
      
   
      j_loop : DO j = jts , MIN(jte,jde-1)
         i_loop : DO i = its , MIN(ite,ide-1)
   
            
            
   
            ke_h = kts
            ke_f = kts
            kz_loop : DO kz = 1 , num_z_levels
   
               
               
               
   
               ke_loop_half : DO ke = ke_h , kte-2
   
                  zu = ( zp(i,ke+1,j)+zb(i,ke+1,j) + zp(i,ke+2,j)+zb(i,ke+2,j) ) / 2.0 / g
                  zd = ( zp(i,ke  ,j)+zb(i,ke  ,j) + zp(i,ke+1,j)+zb(i,ke+1,j) ) / 2.0 / g
                  IF ( z_zl(kz) .LT. 1 ) THEN
                     zm = ABS(z_zl(kz)) + ht(i,j)
                  ELSE 
                     zm = z_zl(kz)
                  END IF
                 
                  IF ( ( zd .LE. zm ) .AND. ( zu .GT. zm ) ) THEN

                     pu = pp(i,ke+1,j)+pb(i,ke+1,j) 
                     pd = pp(i,ke  ,j)+pb(i,ke  ,j)
                     pm = ( pu * (zm-zd) + pd * (zu-zm) ) / (zu-zd)
   
                     
                     
                     
   
                     
   
                     tu = (t(i,ke+1,j)+t0)*(pu/p1000mb)**rcp
                     td = (t(i,ke  ,j)+t0)*(pd/p1000mb)**rcp
                     t_zl(i,kz,j) = ( tu * (zm-zd) + td * (zu-zm) ) / (zu-zd)
   
                     
   
                     su = 0.5 * SQRT ( ( u(i,ke+1,j)+u(i+1,ke+1,j) )**2 + &
                                       ( v(i,ke+1,j)+v(i,ke+1,j+1) )**2 ) 
                     sd = 0.5 * SQRT ( ( u(i,ke  ,j)+u(i+1,ke  ,j) )**2 + &
                                       ( v(i,ke  ,j)+v(i,ke  ,j+1) )**2 ) 
                     s_zl(i,kz,j) = ( su * (zm-zd) + sd * (zu-zm) ) / (zu-zd)
   
                     
   
                     uu = 0.5 * ( u(i,ke+1,j)+u(i+1,ke+1,j) )
                     ud = 0.5 * ( u(i,ke  ,j)+u(i+1,ke  ,j) )
                     u_zl(i,kz,j) = ( uu * (zm-zd) + ud * (zu-zm) ) / (zu-zd)
   
                     vu = 0.5 * ( v(i,ke+1,j)+v(i,ke+1,j+1) )
                     vd = 0.5 * ( v(i,ke  ,j)+v(i,ke  ,j+1) )
                     v_zl(i,kz,j) = ( vu * (zm-zd) + vd * (zu-zm) ) / (zu-zd)
   
                     

                     qu = MAX(qv(i,ke+1,j),0.)
                     qd = MAX(qv(i,ke  ,j),0.)
                     q_zl(i,kz,j) = ( qu * (zm-zd) + qd * (zu-zm) ) / (zu-zd)


   
                     eu = qu * pu * 0.01 / ( eps + qu ) 
                     ed = qd * pd * 0.01 / ( eps + qd ) 
                     eu = max(eu, 0.001)
                     ed = max(ed, 0.001)
   
                     du = t_kelvin + ( s1 / ((s2 / log(eu/s3)) - 1.0) )
                     dd = t_kelvin + ( s1 / ((s2 / log(ed/s3)) - 1.0) )
                     td_zl(i,kz,j) = ( du * (zm-zd) + dd * (zu-zm) ) / (zu-zd)
   

                     
   
                     es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_zl(i,kz,j)))
                     qs = eps * es / (pm - es)
                     rh_zl(i,kz,j)   = q_zl(i,kz,j) / qs * 100.
   
                     ke_h = ke
                     EXIT ke_loop_half
                  END IF
               END DO ke_loop_half
   
               ke_loop_full : DO ke = ke_f , kte-1

                  zu = ( zp(i,ke+1,j)+zb(i,ke+1,j) ) / g
                  zd = ( zp(i,ke  ,j)+zb(i,ke  ,j) ) / g
                  IF ( z_zl(kz) .LT. 1 ) THEN
                     zm = ABS(z_zl(kz)) + ht(i,j)
                  ELSE 
                     zm = z_zl(kz)
                  END IF

                  IF ( ( zd .LE. zm ) .AND. &
                       ( zu .GT. zm) ) THEN
   
                     
                     
   
                     
   
                     
   
                     ght_zl(i,kz,j) = ( zu * (zm-zd) + zd * (zu-zm) ) / (zu-zd)
   
                     ke_f = ke
                     EXIT ke_loop_full
                  END IF
               END DO ke_loop_full
   
            END DO kz_loop
         END DO i_loop
      END DO j_loop

   END SUBROUTINE zld

END MODULE module_diag_zld
