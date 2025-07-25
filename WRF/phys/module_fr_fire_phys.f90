






















module module_fr_fire_phys

use module_model_constants, only: cp,xlv
use module_fr_fire_util

PRIVATE


PUBLIC:: init_fuel_cats,fire_ros,heat_fluxes,set_nfuel_cat,set_fire_params,write_fuels_m
PUBLIC:: fuel_moisture,advance_moisture,read_namelist_fire


PUBLIC:: moisture_classes
PUBLIC::fuelmc_g

PUBLIC::fire_params


type fire_params
real,pointer,dimension(:,:):: vx,vy                
real,pointer,dimension(:,:):: zsf                  
real,pointer,dimension(:,:):: dzdxf,dzdyf          
real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0 
real,pointer,dimension(:,:):: fgip                 
real,pointer,dimension(:,:):: ischap               
real,pointer,dimension(:,:):: iboros               
real,pointer,dimension(:,:):: fuel_time            
real,pointer,dimension(:,:):: fmc_g                
end type fire_params






























  INTEGER, PARAMETER :: mfuelcats = 30     
  INTEGER, PARAMETER ::max_moisture_classes=5


  integer, save:: moisture_classes=5
  real, dimension(max_moisture_classes), save:: drying_lag,wetting_lag,saturation_moisture,saturation_rain, &
         rain_threshold,rec_drying_lag_sec,rec_wetting_lag_sec,fmc_gc_initial_value
  integer, dimension(max_moisture_classes), save:: drying_model,wetting_model,fmc_gc_initialization
   
   integer::itmp
   CHARACTER (len=80), DIMENSION(max_moisture_classes), save :: moisture_class_name
  REAL, save:: fmc_1h, fmc_10h, fmc_100h, fmc_1000h, fmc_live
    
  data moisture_class_name /'1-h','10-h','100-h','1000-h','Live'/
  data drying_lag          /1., 10., 100., 1000.,1e9/  
  data wetting_lag         /1.4,14., 140., 1400.,1e9/  
  data saturation_moisture /2.5, 2.5, 2.5 ,2.5, 2.5/  
  data saturation_rain     /8.0, 8.0, 8.0, 8.0, 8.0/  
  data rain_threshold      /0.05,0.05,0.05,0.05,0.05/ 
  data drying_model        /1,   1,   1,   1,   1 / 
  data wetting_model       /1,   1,   1,   1,   1 / 
  data fmc_gc_initialization/2,  2,   2,   2,   3 / 
                                                    
  data fmc_gc_initial_value/0.,  0.,  0.,  0.,  0./ 
  data fmc_1h /0.08/, fmc_10h/0.08/, fmc_100h/0.08/, fmc_1000h/0.08/, fmc_live/0.3/
  
  
  
  
  
  
  


























































   REAL, SAVE:: cmbcnst,hfgl,fuelmc_g,fuelmc_c

   REAL, SAVE:: fuelheat


   DATA cmbcnst  / 17.433e+06/             
   DATA hfgl     / 17.e4 /                
   DATA fuelmc_g / 0.08  /                
   DATA fuelmc_c / 1.00  /                







   INTEGER, PARAMETER :: nf=14              
   INTEGER, SAVE      :: nfuelcats = 13     
   INTEGER, PARAMETER :: zf = mfuelcats-nf  
   INTEGER, SAVE      :: no_fuel_cat = 14   
   CHARACTER (len=80), DIMENSION(mfuelcats ), save :: fuel_name
   INTEGER, DIMENSION( mfuelcats ), save :: ichap
   REAL   , DIMENSION( mfuelcats ), save :: windrf,weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se, &
                                            fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live, &
                                            fgi_t,fmc_gwt
   REAL,   DIMENSION(mfuelcats,max_moisture_classes), save :: fgi_c, fmc_gw 
   DATA fuel_name /'1: Short grass (1 ft)', &
     '2: Timber (grass and understory)', &
     '3: Tall grass (2.5 ft)', &
     '4: Chaparral (6 ft)', &
     '5: Brush (2 ft) ', &
     '6: Dormant brush, hardwood slash', &
     '7: Southern rough', &
     '8: Closed timber litter', &
     '9: Hardwood litter', &
     '10: Timber (litter + understory)', &
     '11: Light logging slash', &
     '12: Medium logging slash', &
     '13: Heavy logging slash', &
     '14: no fuel', zf* ' '/

   DATA windrf /0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44, &     
                0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1e-7, zf*0 /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305,zf*0. /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500., zf*0. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 , zf*0. / 
   DATA fueldens / nf * 32., zf*0. /   
   DATA st / nf* 0.0555 , zf*0./
   DATA se / nf* 0.010 , zf*0./



   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. , zf*0./ 

   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0., zf*0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60. , zf*0.   /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 , zf*0/




  DATA fgi_1h    / 0.74,   2.00,  3.01,  5.01,  1.00,  1.50,  1.13,  1.50,  2.92,  3.01,  1.50,  4.01,  7.01,   0.0,   zf*0./
  DATA fgi_10h   / 0.00,   1.00,  0.00,  4.01,  0.50,  2.50,  1.87,  1.00,  0.41,  2.00,  4.51, 14.03, 23.04,   0.0,   zf*0./
  DATA fgi_100h  / 0.00,   0.50,  0.00,  2.00,  0.00,  2.00,  1.50,  2.50,  0.15,  5.01,  5.51, 16.53, 28.05,   0.0,   zf*0./
  DATA fgi_1000h / 0.0,    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,    0.0,   zf*0./
  DATA fgi_live  / 0.00,   0.50,  0.000, 5.01,  2.00,  0.00,  0.37,  0.00,  0.00,  2.00,  0.00,  2.3,   0.00,   0.0,   zf*0./


  DATA fgi       / 0.166,  0.896, 0.674, 3.591, 0.784, 1.344, 1.091, 1.120, 0.780, 2.692, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /


contains

subroutine fuel_moisture(                &
    id,                                  & 
    nfmc,                                &
    ids,ide, jds,jde,                    & 
    ims,ime, jms,jme,                    &
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & 
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    ir,jr,                               & 
    nfuel_cat,                           & 
    fmc_gc,                              & 
    fmc_g                                & 
    )

implicit none


integer, intent(in)::                    &
    id,nfmc,                             &
    ids,ide, jds,jde,                    & 
    ims,ime, jms,jme,                    &
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & 
    ifms, ifme, jfms, jfme,              &
    ifts,ifte,jfts,jfte,                 &
    ir,jr                                  


real,intent(in),dimension(ifms:ifme,jfms:jfme):: nfuel_cat 
real,intent(inout),dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real,intent(out),dimension(ifms:ifme,jfms:jfme):: fmc_g 


real, dimension(its-1:ite+1,jts-1:jte+1):: fmc_k  
real, dimension(ifts-1:ifte+1,jfts-1:jfte+1):: fmc_f      
integer::i,j,k,n
integer::ibs,ibe,jbs,jbe
character(len=128)::msg

call check_mesh_2dim(ifts,ifte,jfts,jfte,ifds,ifde,jfds,jfde) 
call check_mesh_2dim(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme) 

do j=jfts,jfte
    do i=ifts,ifte
        fmc_g(i,j)=0.               
    enddo
enddo


ibs=max(ids,its-1)
ibe=min(ide,ite+1)
jbs=max(jds,jts-1)
jbe=min(jde,jte+1)

call check_mesh_2dim(ibs,ibe,jbs,jbe,ims,ime,jms,jme) 

do k=1,moisture_classes

    
    do j=jbs,jbe
        do i=ibs,ibe
            fmc_k(i,j)=fmc_gc(i,k,j)      
        enddo
    enddo

    call print_2d_stats(ibs,ibe,jbs,jbe,its-1,ite+1,jts-1,jte+1,fmc_k,'fuel_moisture: fmc_k')

    
    call interpolate_z2fire(id,        & 
    ids,ide,jds,jde,                     & 
    its-1,ite+1,jts-1,jte+1,             & 
    ips,ipe,jps,jpe,                     &
    its,ite,jts,jte,                     &
    ifds, ifde, jfds, jfde,              & 
    ifts-1,ifte+1,jfts-1,jfte+1,         & 
    ifts,ifte,  jfts,jfte,               &
    ir,jr,                               & 
    fmc_k,                               & 
    fmc_f,0)                                 

    call print_2d_stats(ifts,ifte,jfts,jfte,ifts-1,ifte+1,jfts-1,jfte+1,fmc_f,'fuel_moisture: fmc_f')

    
    do j=jfts,jfte
        do i=ifts,ifte
            n = nfuel_cat(i,j)
            if(n > 0)then
                fmc_g(i,j)=fmc_g(i,j)+fmc_gw(n,k)*fmc_f(i,j)      
            endif
        enddo
    enddo

    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fmc_g,'fuel_moisture: fmc_g')

enddo


end subroutine fuel_moisture

subroutine advance_moisture(    &
    initialize,                 & 
    ims,ime,  jms,jme,          & 
    its,ite,  jts,jte,          & 
    nfmc,                       & 
    moisture_dt,                & 
    fmep_decay_tlag,            & 
    rainc, rainnc,              & 
    t2, q2, psfc,               & 
    rain_old,                   & 
    t2_old, q2_old, psfc_old,   & 
    rh_fire,                    & 
    fmc_gc,                     & 
    fmep,                       & 
    fmc_equi,                   & 
    fmc_lag                     & 
    )

implicit none


logical, intent(in):: initialize
integer, intent(in)::           &
    ims,ime,  jms,jme,          & 
    its,ite,  jts,jte,          & 
    nfmc                          
real, intent(in):: moisture_dt, fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi, fmc_lag
real, intent(out), dimension(ims:ime,jms:jme)::rh_fire 





integer:: i,j,k
real::rain_int, T, P, Q, QRS, ES, RH, tend, EMC_d, EMC_w, EMC, R, rain_diff, fmc, rlag, equi, &
    d, w, rhmax, rhmin, change, rainmax,rainmin, fmc_old, H, deltaS, deltaE
real, parameter::tol=1e-2 
character(len=256)::msg
integer::msglevel=2
logical, parameter::check_data=.true.,check_rh=.false.
real::epsilon,Pws,Pw





if(msglevel>1)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,f10.2,a,i4,a,i4)')'advance moisture dt=',moisture_dt,'s using ',moisture_classes,' classes from possible ',nfmc
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg)
endif

if(moisture_classes > nfmc .or. moisture_classes > max_moisture_classes)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,*)'advance_moisture: moisture_classes=',moisture_classes, &
       ' > nfmc=',nfmc,' or >  max_moisture_classes=',max_moisture_classes
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call crash(msg)
endif

call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,t2,'T2')
call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,q2,'Q2')
call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,psfc,'PSFC')

if(initialize) then 
    call message('advance_moisture: initializing, copying surface variables to old')
    call copy2old
else
    call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_gc,'before advance fmc_gc')
endif

if(check_data)then
    do j=jts,jte
        do i=its,ite
            if( .not.(t2(i,j)>0.0 .and. psfc(i,j)>0.0 .and. .not. q2(i,j) < 0.0 ))then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                 write(msg,'(a,2i4,a,3e12.2)')'At i j',i,j,' t2 psfc q2 are ',t2(i,j),psfc(i,j),q2(i,j)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                 call message(msg) 
                 call crash('invalid data passed from WRF, must have t2 psfc>0, q2 >= 0')
            endif
        enddo
    enddo
endif



rhmax=-huge(rhmax)
rhmin=huge(rhmin)
rainmax=-huge(rainmax)
rainmin= huge(rainmin)
do j=jts,jte
    do k=1,moisture_classes
        do i=its,ite
            
            
            rain_diff = ((rainc(i,j) + rainnc(i,j)) - rain_old(i,j))
            if(moisture_dt > 0.)then
                rain_int  = 3600. * rain_diff / moisture_dt 
            else
                rain_int  = 0.
            endif
            rainmax = max(rainmax,rain_int)
            rainmin = min(rainmin,rain_int)
            R = rain_int - rain_threshold(k)

            
            T = 0.5 * (t2_old(i,j) + t2(i,j))
            P = 0.5 * (psfc_old(i,j) + psfc(i,j))
            Q = 0.5 * (q2_old(i,j) + q2(i,j))

            
            
            
            
            
            epsilon = 0.622 
            
            Pw=q*P/(epsilon+(1-epsilon)*q); 
            
            Pws= exp( 54.842763 - 6763.22/T - 4.210 * log(T) + 0.000367*T + &
                tanh(0.0415*(T - 218.8)) * (53.878 - 1331.22/T - 9.44523 * log(T) + 0.014025*T))
            
            RH = Pw/Pws
            rh_fire(i,j)=RH
            rhmax=max(RH,rhmax)         
            rhmin=min(RH,rhmin)         

            deltaE = fmep(i,1,j)
            deltaS = fmep(i,2,j)

            if(.not.check_rh)then
                RH = min(RH,1.0)
            else
                if(RH < 0.0 .or. RH > 1.0 .or. RH .ne. RH )then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
                    write(msg,'(a,2i6,5(a,f10.2))')'At i,j ',i,j,' RH=',RH, &
                        ' from T=',T,' P=',P,' Q=',Q
                    call message(msg) 
                    call crash('Relative humidity must be between 0 and 1, saturated water contents must be >0')
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif
            endif 


            if (R > 0.) then
                select case(wetting_model(k))
                case(1) 
                    EMC_w=saturation_moisture(k) + deltaS
                    EMC_d=saturation_moisture(k) + deltaS
                    rlag=rec_wetting_lag_sec(k) * (1. - exp(-R/saturation_rain(k)))
                end select
            else 
                select case(drying_model(k))
                case(1) 
                    H = RH * 100.
                    d=0.942*H**0.679 + 0.000499*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) 
                    w=0.618*H**0.753 + 0.000454*exp(0.1*H) + 0.18*(21.1+273.15-T)*(1-exp(-0.115*H)) 
                    if(d.ne.d.or.w.ne.w)call crash('equilibrium moisture calculation failed, result is NaN')
                    d = d*0.01
                    w = w*0.01
                    EMC_d = max(max(d,w)+deltaE,0.0)
                    EMC_w = max(min(d,w)+deltaE,0.0)
                    rlag=rec_drying_lag_sec(k)
                end select
            endif
            
            
            
            
            if(rlag > 0.0)then

                if(.not.initialize .or. fmc_gc_initialization(k).eq.0)then 
                    fmc_old = fmc_gc(i,k,j)
                elseif(fmc_gc_initialization(k).eq.1)then 
                    fmc_old = fuelmc_g
                elseif(fmc_gc_initialization(k).eq.2)then 
                    fmc_old=0.5*(EMC_d+EMC_w)
                elseif(fmc_gc_initialization(k).eq.3)then 
                    fmc_old = fmc_gc_initial_value(k)
                else
                    call crash('bad value of fmc_gc_initialization(k), must be between 0 and 2')
                endif
                equi = max(min(fmc_old, EMC_d),EMC_w) 

                change = moisture_dt * rlag 

                if(change  < tol)then
                     if(fire_print_msg.ge.3)call message('midpoint method')
                     fmc = fmc_old + (equi - fmc_old)*change*(1.0 - 0.5*change)  
                else
                     if(fire_print_msg.ge.3)call message('exponential method')
                     fmc = fmc_old + (equi - fmc_old)*(1 - exp(-change))
                endif
                fmc_gc(i,k,j) = fmc

                
                fmc_equi(i,k,j)=equi
                fmc_lag(i,k,j)=1.0/(3600.0*rlag)
                 
                
                if(fire_print_msg.ge.3)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
		    write(msg,*)'i=',i,' j=',j,'EMC_w=',EMC_w,' EMC_d=',EMC_d
                    call message(msg)
                    write(msg,*)'fmc_old=',fmc,' equi=',equi,' change=',change,' fmc=',fmc
                    call message(msg)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
                endif

            endif
      enddo
   enddo
enddo



do j=jts,jte
  do k=1,2
    do i=its,ite
      change = moisture_dt / (fmep_decay_tlag * 3600.)
      if(change < tol) then
        fmep(i,k,j) = fmep(i,k,j)*(1.0 - change * (1.0 - 0.5 * change))
      else
        fmep(i,k,j) = fmep(i,k,j)*exp(-change)
      endif
    enddo
  enddo
enddo


if(fire_print_msg.ge.2)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,2)'Rain intensity    min',rainmin,  ' max',rainmax,' mm/h'
    call message(msg) 
    if(rainmin <0.)then
        call message('WARNING rain accumulation must increase')
    endif
    write(msg,2)'Relative humidity min',100*rhmin,' max',100*rhmax,'%'
    call message(msg) 
    if(.not.(rhmax<=1.0 .and. rhmin>=0))then
        call message('WARNING Relative humidity must be between 0 and 100%')
    endif
2   format(2(a,f10.2),a)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
endif

call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_equi,'equilibrium fmc_equi')
call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_lag,'time lag')
call print_3d_stats_by_slice(its,ite,1,moisture_classes,jts,jte,ims,ime,1,nfmc,jms,jme,fmc_gc,'after advance fmc_gc')

call copy2old

return

contains

subroutine copy2old

do j=jts,jte
    do i=its,ite
        rain_old(i,j) = rainc(i,j) + rainnc(i,j)
        t2_old(i,j) = t2(i,j)
        q2_old(i,j) = q2(i,j)
        psfc_old(i,j) = psfc(i,j)
    enddo
enddo

end subroutine copy2old

subroutine get_equi_moist
end subroutine get_equi_moist

end subroutine advance_moisture


subroutine read_namelist_fire(init_fuel_moisture)
implicit none


logical, intent(in)::init_fuel_moisture
logical, external:: wrf_dm_on_monitor

integer:: iounit, i, k, io
character(len=128):: msg
real:: rat



namelist /fuel_scalars/ cmbcnst,hfgl,fuelmc_g,fuelmc_c,nfuelcats,no_fuel_cat
namelist /fuel_categories/ fuel_name,windrf,fgi,fueldepthm,savr, &
    fuelmce,fueldens,st,se,weight,fci_d,fct,ichap,fgi_1h,fgi_10h,fgi_100h,fgi_1000h,fgi_live
namelist /fuel_moisture/ moisture_classes,drying_lag,wetting_lag,saturation_moisture,saturation_rain,rain_threshold, &
    drying_model,wetting_model, moisture_class_name,fmc_gc_initialization, fmc_1h,fmc_10h,fmc_100h,fmc_1000h,fmc_live


IF ( wrf_dm_on_monitor() ) THEN
    

    
    call message('Reading file namelist.fire',level=0)
    iounit=open_text_file('namelist.fire','read',allow_fail=.true.)
    if(iounit < 0)then
       call message('File namelist.fire not found, using defaults',level=0)
    else  
       read(iounit,fuel_scalars,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_scalars not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_scalars')
       endif
       rewind(iounit)
       read(iounit,fuel_categories,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_categories not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_categories')
       endif
       rewind(iounit)
       read(iounit,fuel_moisture,iostat=io)
       if(io < 0)then
           call message('Namelist fuel_moisture not found, using defaults',level=0)
       elseif(io > 0)then
           call crash('Error in file namelist.fire, namelist fuel_moisture')
       endif
       close(iounit)
    endif

    
    iounit=open_text_file('namelist.fire.output','write')
    write(iounit,fuel_scalars)
    write(iounit,fuel_categories)
    write(iounit,fuel_moisture)
    close(iounit)

    if (nfuelcats>mfuelcats) then
        write(msg,*)'nfuelcats=',nfuelcats,' is too large, increase mfuelcats'
        call crash(msg)
    endif
    if (no_fuel_cat >= 1 .and. no_fuel_cat <= nfuelcats)then
        write(msg,*)'no_fuel_cat=',no_fuel_cat,' may not be between 1 and nfuelcats=',nfuelcats
        call crash(msg)
    endif

    
    
    

    
    if (max_moisture_classes.ne.5)then
        call crash('Must have 5 fuel classes, modify source code if not')
    endif
    
    
    
    
    fgi_c(1:mfuelcats,1)=fgi_1h
    fgi_c(1:mfuelcats,2)=fgi_10h
    fgi_c(1:mfuelcats,3)=fgi_100h
    fgi_c(1:mfuelcats,4)=fgi_1000h
    fgi_c(1:mfuelcats,5)=fgi_live
    fmc_gc_initial_value(1)=fmc_1h
    fmc_gc_initial_value(2)=fmc_10h
    fmc_gc_initial_value(3)=fmc_100h
    fmc_gc_initial_value(3)=fmc_1000h
    fmc_gc_initial_value(5)=fmc_live
    
    
    
    

    call message('Scaling fuel loads within each fuel category to averaging weights of fuel moisture classes')
    do i=1,mfuelcats
            fgi_t(i) = 0.
            do k=1,max_moisture_classes
                if(fgi_c(i,k).ge.0.)then
                    fgi_t(i) = fgi_t(i) + fgi_c(i,k)
                else 
                    write(msg,*)'fuel load in category',i,' fuel class ',k,' is ',fgi_c(i,k),',must be nonegative.'
                    call crash(msg)
                endif
            enddo
            if (fgi_t(i)>0. .or. fgi(i)>0.)then
                if (fgi_t(i)>0.) then
                     rat = fgi(i)/fgi_t(i)
                else
                     rat = 0.
                endif
                write(msg,'(a,i4,1x,a,g13.6,1x,a,g13.6,1x,a,g13.6)') &
                    'fuel category',i,'fuel load',fgi(i),'total by class',fgi_t(i), 'ratio',rat
                call message(msg)
            endif
            
            fmc_gwt(i)=0.
            do k=1,max_moisture_classes
               if (fgi_t(i) > 0.) then
                   fmc_gw(i,k) = fgi_c(i,k) / fgi_t(i)
                   fmc_gwt(i) = fmc_gwt(i) + fmc_gw(i,k)
               else
                   fmc_gw(i,k) = 0.
               endif
            enddo
    enddo

    
ENDIF

end subroutine read_namelist_fire


subroutine init_fuel_cats(init_fuel_moisture)
implicit none


logical, intent(in)::init_fuel_moisture
logical, external:: wrf_dm_on_monitor

integer:: i,j,k,ii
integer:: kk
character(len=128):: msg



call read_namelist_fire(init_fuel_moisture)


call wrf_dm_bcast_real(cmbcnst,1)
call wrf_dm_bcast_real(hfgl,1)
call wrf_dm_bcast_real(fuelmc_g,1)
call wrf_dm_bcast_real(fuelmc_c,1)
call wrf_dm_bcast_integer(nfuelcats,1)
call wrf_dm_bcast_integer(no_fuel_cat,1)
call wrf_dm_bcast_real(windrf,    nfuelcats)
call wrf_dm_bcast_real(fgi,       nfuelcats)
call wrf_dm_bcast_real(fueldepthm,nfuelcats)
call wrf_dm_bcast_real(savr,      nfuelcats)
call wrf_dm_bcast_real(fuelmce,   nfuelcats)
call wrf_dm_bcast_real(fueldens,  nfuelcats)
call wrf_dm_bcast_real(st,        nfuelcats)
call wrf_dm_bcast_real(se,        nfuelcats)
call wrf_dm_bcast_real(weight,    nfuelcats)
call wrf_dm_bcast_real(fci_d,     nfuelcats)
call wrf_dm_bcast_real(fct,       nfuelcats)
call wrf_dm_bcast_integer(ichap,  nfuelcats)

call wrf_dm_bcast_integer(moisture_classes,1)
call wrf_dm_bcast_real(drying_lag,     max_moisture_classes)
call wrf_dm_bcast_real(wetting_lag,     max_moisture_classes)
call wrf_dm_bcast_real(saturation_moisture,     max_moisture_classes)
call wrf_dm_bcast_real(saturation_rain,     max_moisture_classes)
call wrf_dm_bcast_real(rain_threshold,     max_moisture_classes)
call wrf_dm_bcast_integer(drying_model,     max_moisture_classes)
call wrf_dm_bcast_integer(wetting_model,     max_moisture_classes)
call wrf_dm_bcast_integer(fmc_gc_initialization,     max_moisture_classes)
call wrf_dm_bcast_real(fmc_gc_initial_value,     max_moisture_classes)
call wrf_dm_bcast_real(fmc_gw,     mfuelcats*max_moisture_classes)


do i=1,moisture_classes
    rec_drying_lag_sec(i)  = 1.0/(3600.0*drying_lag(i))
    rec_wetting_lag_sec(i) = 1.0/(3600.0*wetting_lag(i))
enddo



fuelheat = cmbcnst * 4.30e-04     



DO i = 1,nfuelcats
    fci(i) = (1.+fuelmc_c)*fci_d(i)
    if(fct(i) .ne.  0.)then
        fcbr(i) = fci_d(i)/fct(i) 
    else
        fcbr(i) = 0
    endif
END DO



call message('**********************************************************')
call message('FUEL COEFFICIENTS')
write(msg,8)'cmbcnst    ',cmbcnst
call message(msg)
write(msg,8)'hfgl       ',hfgl
call message(msg)
write(msg,8)'fuelmc_g   ',fuelmc_g
call message(msg)
write(msg,8)'fuelmc_c   ',fuelmc_c
call message(msg)
write(msg,8)'fuelheat   ',fuelheat
call message(msg)
write(msg,7)'nfuelcats  ',nfuelcats
call message(msg)
write(msg,7)'no_fuel_cat',no_fuel_cat
call message(msg)
if(init_fuel_moisture)then
    write(msg,7)'moisture_classes',moisture_classes
    call message(msg)
endif

j=1    
7 format(a,(1x,i8,4x))
8 format(a,5(1x,g12.5e2))
9 format(a,5(1x,a))
10 format(a,2x,99(1x,f12.5))
11 format(a,2x,99(1x,a12))
do i=1,nfuelcats,j
    k=min(i+j-1,nfuelcats)
    call message(' ')
    write(msg,7)'CATEGORY  ',(ii,ii=i,k)
    call message(msg)
    write(msg,9)'fuel name ',(fuel_name(ii),ii=i,k)
    call message(msg)


    write(msg,8)'fgi       ',(fgi(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldepthm',(fueldepthm(ii),ii=i,k)
    call message(msg)
    write(msg,8)'savr      ',(savr(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fuelmce   ',(fuelmce(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldens  ',(fueldens(ii),ii=i,k)
    call message(msg)
    write(msg,8)'st        ',(st(ii),ii=i,k)
    call message(msg)
    write(msg,8)'se        ',(se(ii),ii=i,k)
    call message(msg)
    write(msg,8)'weight    ',(weight(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci_d     ',(fci_d(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fct       ',(fct(ii),ii=i,k)
    call message(msg)
    write(msg,7)'ichap     ',(ichap(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci       ',(fci(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fcbr      ',(fcbr(ii),ii=i,k)
    call message(msg)
    if(init_fuel_moisture)then
        write(msg,11)'fuel class name',(trim(moisture_class_name(kk)),kk=1,moisture_classes),'Total','Total/fgi'
        call message(msg)
        write(msg,10)'fuel load fgi_c',(fgi_c(i,kk),kk=1,moisture_classes),fgi_t(i),fgi_t(i)/fgi(i)
        call message(msg)
        write(msg,10)'fraction fmc_gw',(fmc_gw(i,kk),kk=1,moisture_classes),fmc_gwt(i)
        call message(msg)
    endif
enddo
call message(' ')
call message('**********************************************************')

if(init_fuel_moisture)then
    j=1
    do i=1,moisture_classes,j
        k=min(i+j-1,nfuelcats)
        call message(' ')
        write(msg,7)'FUEL MOISTURE CLASS',(ii,ii=i,k)
        call message(msg)
        write(msg,9)'moisture class name    ',(trim(moisture_class_name(ii)),ii=i,k)
        call message(msg)
        write(msg,7)'drying_model           ',(drying_model(ii),ii=i,k)
        call message(msg)
        write(msg,8)'drying_lag (h)         ',(drying_lag(ii),ii=i,k)
        call message(msg)
        write(msg,7)'wetting_model          ',(wetting_model(ii),ii=i,k)
        call message(msg)
        write(msg,7)'fmc_gc_initialization  ',(fmc_gc_initialization(ii),ii=i,k)
        call message(msg)
        write(msg,8)'wetting_lag (h)        ',(wetting_lag(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'saturation_moisture (1)',(saturation_moisture(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'saturation_rain (mm/h) ',(saturation_rain(ii),ii=i,k)
        call message(msg)    
        write(msg,8)'rain_threshold (mm/h)  ',(rain_threshold(ii),ii=i,k)
        call message(msg)    
    enddo
    call message(' ')
    call message('**********************************************************')
    call message(' ')
endif


IF ( wrf_dm_on_monitor() ) THEN

ENDIF
end subroutine init_fuel_cats


subroutine write_fuels_m(nsteps,maxwind,maxslope)
implicit none
integer, intent(in):: nsteps   
real, intent(in):: maxwind,maxslope 

integer:: iounit,k,j,i
type(fire_params)::fp








real, dimension(1:2,1:nsteps), target::vx,vy,zsf,dzdxf,dzdyf,bbb,betafl,phiwc,r_0,fgip,ischap,fmc_g
real, dimension(1:2,1:nsteps), target::iboros
real, dimension(1:2,1:nsteps)::nfuel_cat,fuel_time,ros
real::ros_base,ros_wind,ros_slope,propx,propy,r

fp%vx=>vx
fp%vy=>vy
fp%dzdxf=>dzdxf
fp%dzdyf=>dzdyf
fp%bbb=>bbb
fp%betafl=>betafl
fp%phiwc=>phiwc
fp%r_0=>r_0
fp%fgip=>fgip
fp%ischap=>ischap
fp%iboros=>iboros 
fp%fmc_g=>fmc_g

iounit = open_text_file('fuels.m','write')

10 format('fuel(',i3,').',a,'=',"'",a,"'",';% ',a)
do k=1,nfuelcats
    write(iounit,10)k,'fuel_name',trim(fuel_name(k)),'FUEL MODEL NAME'
    call write_var(k,'windrf',windrf(k),'WIND REDUCTION FACTOR FROM 20ft TO MIDFLAME HEIGHT' )
    call write_var(k,'fgi',fgi(k),'INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)' )
    call write_var(k,'fueldepthm',fueldepthm(k),'FUEL DEPTH (M)')
    call write_var(k,'savr',savr(k),'FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT')
    call write_var(k,'fuelmce',fuelmce(k),'MOISTURE CONTENT OF EXTINCTION')
    call write_var(k,'fueldens',fueldens(k),'OVENDRY PARTICLE DENSITY, LB/FT^3')
    call write_var(k,'st',st(k),'FUEL PARTICLE TOTAL MINERAL CONTENT')
    call write_var(k,'se',se(k),'FUEL PARTICLE EFFECTIVE MINERAL CONTENT')
    call write_var(k,'weight',weight(k),'WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE')
    call write_var(k,'fci_d',fci_d(k),'INITIAL DRY MASS OF CANOPY FUEL')
    call write_var(k,'fct',fct(k),'BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)')
    call write_var(k,'ichap',float(ichap(k)),'1 if chaparral, 0 if not')
    call write_var(k,'fci',fci(k),'INITIAL TOTAL MASS OF CANOPY FUEL')
    call write_var(k,'fcbr',fcbr(k),'FUEL CANOPY BURN RATE (KG/M**2/S)')
    call write_var(k,'hfgl',hfgl,'SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)')
    call write_var(k,'cmbcnst',cmbcnst,'JOULES PER KG OF DRY FUEL')
    call write_var(k,'fuelheat',fuelheat,'FUEL PARTICLE LOW HEAT CONTENT, BTU/LB')
    call write_var(k,'fuelmc_g',fuelmc_g,'FUEL PARTICLE (SURFACE) MOISTURE CONTENT')
    call write_var(k,'fuelmc_c',fuelmc_c,'FUEL PARTICLE (CANOPY) MOISTURE CONTENT')
    
    
    
    
    
    
    
    
    nfuel_cat = k
    call set_fire_params( &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           0.,0.,k,  &
                           nfuel_cat,fuel_time, &
                           fp ) 
    
    propx=1.
    propy=0.
    do j=1,nsteps
       r=float(j-1)/float(nsteps-1)
       
       vx(1,j)=maxwind*r
       vy(1,j)=0.
       dzdxf(1,j)=0.
       dzdyf(1,j)=0.
       
       vx(2,j)=0.
       vy(2,j)=0.
       dzdxf(2,j)=maxslope*r
       dzdyf(2,j)=0.
    enddo
    do j=1,nsteps
       do i=1,2
          call fire_ros(ros_base,ros_wind,ros_slope, &
             propx,propy,i,j,fp)
          ros(i,j)=ros_base+ros_wind+ros_slope
       enddo
       write(iounit,13)k,'wind',j,vx(1,j),'wind speed'
       write(iounit,13)k,'ros_wind',j,ros(1,j),'rate of spread for the wind speed'
       write(iounit,13)k,'slope',j,dzdxf(2,j),'slope'
       write(iounit,13)k,'ros_slope',j,ros(2,j),'rate of spread for the slope'
    enddo
enddo
13 format('fuel(',i3,').',a,'(',i3,')=',g12.5e2,';% ',a)
 
close(iounit)


contains

subroutine write_var(k,name,value,descr)

integer, intent(in)::k
character(len=*), intent(in)::name,descr
real, intent(in)::value
write(iounit,11)k,name,value
write(iounit,12)k,name,descr
11 format('fuel(',i3,').',a,'=',g12.5e2,  ';')
12 format('fuel(',i3,').',a,"_descr='",a,"';")
end subroutine write_var

end subroutine write_fuels_m





subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,nfuel_cat0,  &
                           nfuel_cat,fuel_time, &
                           fp ) 

implicit none




integer, intent(in)::ifds,ifde,jfds,jfde                        
integer, intent(in)::ifts,ifte,jfts,jfte                        
integer, intent(in)::ifms,ifme,jfms,jfme                        
real, intent(in):: fdx,fdy                                      
integer,intent(in)::nfuel_cat0                                  
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   
type(fire_params),intent(inout)::fp



real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
real:: bmst
integer:: i,j,k
integer::nerr
character(len=128)::msg

integer :: kk
integer,parameter :: nf_sb = 204 
integer,dimension(1:nf_sb) :: ksb 





do kk=1,nf_sb
   ksb(kk)=14
enddo

ksb(1)=1
ksb(2)=2
ksb(3)=3
ksb(4)=4
ksb(5)=5
ksb(6)=6
ksb(7)=7
ksb(8)=8
ksb(9)=9
ksb(10)=10
ksb(11)=11
ksb(12)=12
ksb(13)=13


ksb(101)=1
ksb(104)=1
ksb(107)=1

ksb(102)=2
ksb(121)=2
ksb(122)=2
ksb(123)=2
ksb(124)=2

ksb(103)=3
ksb(105)=3
ksb(106)=3
ksb(108)=3
ksb(109)=3

ksb(145)=4
ksb(147)=4

ksb(142)=5

ksb(141)=6
ksb(146)=6

ksb(143)=7
ksb(144)=7
ksb(148)=7
ksb(149)=7

ksb(181)=8
ksb(183)=8
ksb(184)=8
ksb(187)=8

ksb(182)=9
ksb(186)=9
ksb(188)=9
ksb(189)=9

ksb(161)=10
ksb(162)=10
ksb(163)=10
ksb(164)=10
ksb(165)=10

ksb(185)=11
ksb(201)=11

ksb(202)=12

ksb(203)=13
ksb(204)=13



nerr=0
do j=jfts,jfte
   do i=ifts,ifte
     
     k=ksb(int(nfuel_cat(i,j))) 
     if(k.eq.no_fuel_cat)then   
        fp%fgip(i,j)=0.            
        fp%ischap(i,j)=0.
        fp%betafl(i,j)=1.          
        fp%bbb(i,j)=1.             
        fuel_time(i,j)=7./0.85  
        fp%phiwc(i,j)=0.
        fp%r_0(i,j)=0.             
        fp%iboros(i,j)=0.   
     else
        if(k.eq.0.and.nfuel_cat0.ge.1.and.nfuel_cat0.le.nfuelcats)then
            
            k=nfuel_cat0
            nerr=nerr+1
        endif
   
        if(k.lt.1.or.k.gt.nfuelcats)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
            call message(msg)
            call crash('set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=weight(k)/0.85 
        
        
        
        

        fp%ischap(i,j)=ichap(k)
        fp%fgip(i,j)=fgi(k)
        if(fire_fmc_read.eq.1)then
           fp%fmc_g(i,j)=fuelmc_g
        endif

        

        
        bmst     = fp%fmc_g(i,j) / (1.+fp%fmc_g(i,j))
        fuelloadm= (1.-bmst) * fgi(k)  
        fuelload = fuelloadm * (.3048)**2 * 2.205    
        fueldepth = fueldepthm(k)/0.3048               
        fp%betafl(i,j) = fuelload/(fueldepth * fueldens(k))
        betaop = 3.348 * savr(k)**(-0.8189)     
        qig = 250. + 1116.*fp%fmc_g(i,j)            
        epsilon = exp(-138./savr(k) )    
        rhob = fuelload/fueldepth    

        c = 7.47 * exp( -0.133 * savr(k)**0.55)    
        fp%bbb(i,j) = 0.02526 * savr(k)**0.54      
        e = 0.715 * exp( -3.59e-4 * savr(k))       
        fp%phiwc(i,j) = c * (fp%betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              
        a = 1./(4.774 * savr(k)**0.1 - 7.27)   
        ratio = fp%betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) 

        wn = fuelload/(1 + st(k))       
        rtemp1 = fp%fmc_g(i,j)/fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  
        etas = 0.174* se(k)**(-0.19)                
        ir = gamma * wn * fuelheat * etam * etas  
        
        fp%iboros(i,j) = ir * 1055./( 0.3048**2 * 60.) * 1.e-3 * (60.*12.6/savr(k))     

        xifr = exp( (0.792 + 0.681*savr(k)**0.5) &
            * (fp%betafl(i,j)+0.1)) /(192. + 0.2595*savr(k)) 


        fp%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    

     endif
  enddo
enddo

if(nerr.gt.1)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i6,a)')'set_fire_params: WARNING: fuel category 0 replaced in',nerr,' cells'
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
endif

end subroutine set_fire_params





subroutine heat_fluxes(dt,fp,                     &
        ifms,ifme,jfms,jfme,                      &  
        ifts,ifte,jfts,jfte,                      &  
        iffs,iffe,jffs,jffe,                      &  
        fgip,fuel_frac_burnt,                     & 
        grnhft,grnqft)                              
implicit none





type(fire_params), intent(in)::fp
real, intent(in)::dt          
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft


integer::i,j
real:: dmass,bmst
logical::latent


do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     
             fgip(i,j)               &     
             * fuel_frac_burnt(i,j)        
         bmst     = fp%fmc_g(i,j)/(1.+fp%fmc_g(i,j))        
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  
         
    enddo
enddo

end subroutine heat_fluxes






subroutine set_nfuel_cat(   &
    ifms,ifme,jfms,jfme,               &
    ifts,ifte,jfts,jfte,               &
    ifuelread,nfuel_cat0,zsf,nfuel_cat)

implicit none


integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread,nfuel_cat0
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
real, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat




integer:: i,j,iu1
real:: t1
character(len=128)msg

!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: ifuelread=',ifuelread 
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)

if (ifuelread .eq. -1 .or. ifuelread .eq. 2) then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    call message('set_nfuel_cat: assuming nfuel_cat initialized already') 
    call message(msg)
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
else if (ifuelread .eq. 0) then

    do j=jfts,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=real(nfuel_cat0)
        enddo
    enddo
!$OMP CRITICAL(FIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: fuel initialized with category',nfuel_cat0
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
    call message(msg)
         
else if (ifuelread .eq. 1) then





    do j=jfts,jfte
        do  i=ifts,ifte
            
            
            t1 = zsf(i,j)  
            if(t1.le.1524.)then   
                nfuel_cat(i,j)= 3  
            else if(t1.ge.1524. .and. t1.le.2073.)then  
                nfuel_cat(i,j)= 2  
            else if(t1.ge.2073..and.t1.le.2438.)then  
                nfuel_cat(i,j)= 8  
            else if(t1.gt.2438. .and. t1.le. 3354.) then 

                nfuel_cat(i,j)= 10 
            else if(t1.gt.3354. .and. t1.le. 3658.) then 
                nfuel_cat(i,j)= 1  
            else if(t1.gt.3658. ) then  
                nfuel_cat(i,j)= 14 
            endif
        enddo
    enddo

    call message('set_nfuel_cat: fuel initialized by altitude')
else

    call crash('set_nfuel_cat: bad ifuelread')
endif


end subroutine set_nfuel_cat            





subroutine fire_ros(ros_base,ros_wind,ros_slope, &
propx,propy,i,j,fp)

implicit none




















real, intent(out)::ros_base,ros_wind,ros_slope 
real, intent(in)::propx,propy
integer, intent(in)::i,j         
type(fire_params),intent(in)::fp


real:: speed, tanphi 
real:: umid, phis, phiw, spdms, umidm, excess
real:: ros_back
integer, parameter::ibeh=1
real, parameter::ros_max=6.
character(len=128)msg
real::cor_wind,cor_slope,nvx,nvy,scale






scale=1.
nvx=propx/scale
nvy=propy/scale
if (fire_advection.ne.0) then 
    
    speed =  sqrt(fp%vx(i,j)*fp%vx(i,j)+ fp%vy(i,j)*fp%vy(i,j))+tiny(speed)
    
    tanphi = sqrt(fp%dzdxf(i,j)*fp%dzdxf(i,j) + fp%dzdyf(i,j)*fp%dzdyf(i,j))+tiny(tanphi)
    
    cor_wind =  max(0.,(fp%vx(i,j)*nvx + fp%vy(i,j)*nvy)/speed)
    
    cor_slope = max(0., (fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy)/tanphi)
else
    
    speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
    
    tanphi = fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
    cor_wind=1.
    cor_slope=1.
endif

if (.not. fp%ischap(i,j) > 0.) then      
    if (ibeh .eq. 1) then                

        spdms = max(speed,0.)            
        umidm = min(spdms,30.)           
        umid = umidm * 196.850           
        
        phiw = umid**fp%bbb(i,j) * fp%phiwc(i,j) 
        phis=0.
        if (tanphi .gt. 0.) then
            phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphi**2   
        endif
        
        ros_base = fp%r_0(i,j) * .00508
        ros_wind = ros_base*phiw
        ros_slope= ros_base*phis
        

    else                                   
        
        ros_base = 0.18*exp(0.8424)
        ros_wind = 0.18*exp(0.8424*max(speed,0.))
        ros_slope =0.
    endif

else   

    spdms = max(speed,0.)      
    
    
    
    
    

    ros_back=.03333    
    ros_wind = 1.2974 * spdms**1.41       
    ros_wind = max(ros_wind, ros_back)
    ros_slope =0.

endif

ros_wind=ros_wind*cor_wind
ros_slope=ros_slope*cor_slope




excess = ros_base + ros_wind + ros_slope - ros_max

if (excess > 0.)then
    
    ros_wind = ros_wind - excess*ros_wind/(ros_wind+ros_slope)
    ros_slope = ros_slope - excess*ros_slope/(ros_wind+ros_slope)
endif




      return

contains
real function nrm2(u,v)
real, intent(in)::u,v
nrm2=sqrt(u*u+v*v)
end function nrm2

end subroutine fire_ros 

end module module_fr_fire_phys
