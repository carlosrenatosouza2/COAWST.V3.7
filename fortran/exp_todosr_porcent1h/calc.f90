program porcent

   implicit none
   
   
   integer                    :: rank, unid=1000, ranki, nlinha
   real*8                     :: ref, tempi, tempf
   real*8                     :: deltainitatm_med = 0.0, deltarunatm_med = 0.0, deltafinaatm_med = 0.0, deltatotalatm_med = 0.0
   real*8                     :: deltainitatm_desv = 0.0, deltarunatm_desv = 0.0, deltafinaatm_desv = 0.0, deltatotalatm_desv = 0.0
   real*8                     :: deltainitocn_med = 0.0, deltarunocn_med = 0.0, deltafinaocn_med = 0.0, deltatotalocn_med = 0.0
   real*8                     :: deltainitocn_desv = 0.0, deltarunocn_desv = 0.0, deltafinaocn_desv = 0.0, deltatotalocn_desv = 0.0
   real*8                     :: deltawaitratm_med = 0.0, deltawaitsatm_med = 0.0, deltatodoswaitatm_med = 0.0
   real*8                     :: deltawaitratm_desv = 0.0, deltawaitsatm_desv = 0.0, deltatodoswaitatm_desv = 0.0
   real*8                     :: deltawaitrocn_med = 0.0, deltawaitsocn_med = 0.0, deltatodoswaitocn_med = 0.0
   real*8                     :: deltawaitrocn_desv = 0.0, deltawaitsocn_desv = 0.0, deltatodoswaitocn_desv = 0.0
   real*8, dimension(640)     :: deltainitatm = 0.0, deltarunatm = 0.0, deltafinaatm = 0.0, deltatotalatm = 0.0 
   real*8, dimension(640)     :: deltainitocn = 0.0, deltarunocn = 0.0, deltafinaocn = 0.0
   real*8, dimension(640)     :: deltawaitratm = 0.0, deltawaitsatm = 0.0
   real*8, dimension(640)     :: deltawaitrocn = 0.0, deltawaitsocn = 0.0, deltatotalocn = 0.0
   real*8, dimension(640, 2)  :: temporunatm = 0.0, tempoinitatm = 0.0, tempofinaatm = 0.0
   real*8, dimension(640, 2)  :: temporunocn = 0.0, tempoinitocn = 0.0, tempofinaocn = 0.0
   character(len=4)           :: cunid
   character(len=9)           :: func
   character                  :: pos


   !Output:
   open(10, file='outputATM.txt')
   open(11, file='outputOCN.txt')
   open(12, file='outputATM_media.txt')
   open(13, file='outputOCN_media.txt')

   
   
   do rank = 320, 639



      !lendo os tempos principais: init fort.1320~1639(ATM): -----------------
      write(cunid, '(i4)') unid+rank
      !print*, cunid
      !Init:
      open(unid+rank, file='datain/soinit/fort.'//cunid)
         read(unid+rank, *) tempoinitatm(rank, 1), ranki, func, pos  
         read(unid+rank, *) tempoinitatm(rank, 2), ranki, func, pos  
      close(unid+rank)
      !Run:
      open(unid+rank, file='datain/sorun/fort.'//cunid)
         read(unid+rank, *) temporunatm(rank, 1), ranki, func, pos  
         read(unid+rank, *) temporunatm(rank, 2), ranki, func, pos  
      close(unid+rank) 
      !Finalize:
      open(unid+rank, file='datain/sofinalize/fort.'//cunid)
         read(unid+rank, *) tempofinaatm(rank, 1), ranki, func, pos  
         read(unid+rank, *) tempofinaatm(rank, 2), ranki, func, pos  
      close(unid+rank) 
      !------------------------------------------------------------------
   

      !lendo os tempos Waits: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sowaits/fort.'//cunid)
      do
         read(unid+rank, *, end=10) tempi, ranki, func, pos   
         read(unid+rank, *, end=10) tempf, ranki, func, pos 
         deltawaitsatm(rank) = deltawaitsatm(rank) + (tempf-tempi)
      enddo
10    continue   
      close(unid+rank)
      
      !lendo os tempos Waitr: fort.1320~1639(ATM): ----------------------
      open(unid+rank, file='datain/sowaitr/fort.'//cunid)
      do
         read(unid+rank, *, end=11) tempi, ranki, func, pos   
         read(unid+rank, *, end=11) tempf, ranki, func, pos 
         deltawaitratm(rank) = deltawaitratm(rank) + (tempf-tempi)
      enddo
11    continue    
      close(unid+rank)  
   
   
      deltainitatm(rank) = tempoinitatm(rank, 2) - tempoinitatm(rank, 1)
      deltarunatm(rank) = temporunatm(rank, 2) - temporunatm(rank, 1)
      deltafinaatm(rank) = tempofinaatm(rank, 2) - tempofinaatm(rank, 1)
      deltatotalatm(rank) = tempofinaatm(rank, 1) - tempoinitatm(rank, 2)
  
   
   
      write(10, "('ATM - Rank:', i4, ' DInit: ', f7.2, ' s | DRun: ', f7.2, ' s | DFina: ', f7.2, ' s | DTOTAL: ', f7.2, ' s')") &
      rank, deltainitatm(rank), deltarunatm(rank), deltafinaatm(rank), deltatotalatm(rank)
      
      write(10, "('ATM - Rank:', i4, ' DTWaits: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsatm(rank), deltawaitsatm(rank)/deltatotalatm(rank)*100 
      
      write(10, "('ATM - Rank:', i4, ' DTWaitr: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitratm(rank), deltawaitratm(rank)/deltatotalatm(rank)*100 
      
      write(10, "('ATM - Rank:', i4, ' DTTodosWait: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsatm(rank)+deltawaitratm(rank), (deltawaitsatm(rank)+deltawaitratm(rank))/deltatotalatm(rank)*100 
   
      write(10, *)''
      
      !media:
      deltainitatm_med = deltainitatm_med + deltainitatm(rank)
      deltarunatm_med = deltarunatm_med + deltarunatm(rank)
      deltafinaatm_med = deltafinaatm_med + deltafinaatm(rank)
      deltatotalatm_med = deltatotalatm_med + deltatotalatm(rank)
      deltawaitsatm_med = deltawaitsatm_med + deltawaitsatm(rank)
      deltawaitratm_med = deltawaitratm_med + deltawaitratm(rank)
      deltatodoswaitatm_med = deltatodoswaitatm_med + ( deltawaitsatm(rank)+deltawaitratm(rank) )
   
   enddo
   deltainitatm_med = deltainitatm_med / 320
   deltarunatm_med = deltarunatm_med / 320
   deltafinaatm_med = deltafinaatm_med / 320
   deltatotalatm_med = deltatotalatm_med / 320
   deltawaitsatm_med = deltawaitsatm_med / 320
   deltawaitratm_med = deltawaitratm_med / 320
   deltatodoswaitatm_med = deltatodoswaitatm_med / 320
   
   !Desvio padrao:
   do rank = 320, 639
      deltainitatm_desv = deltainitatm_desv + ( deltainitatm(rank) - deltainitatm_med )**2
      deltarunatm_desv = deltarunatm_desv + ( deltarunatm(rank) - deltarunatm_med )**2
      deltafinaatm_desv = deltafinaatm_desv + ( deltafinaatm(rank) - deltafinaatm_med )**2
      deltatotalatm_desv = deltatotalatm_desv + ( deltatotalatm(rank) - deltatotalatm_med )**2
      deltawaitsatm_desv = deltawaitsatm_desv + ( deltawaitsatm(rank) - deltawaitsatm_med )**2
      deltawaitratm_desv = deltawaitratm_desv + ( deltawaitratm(rank) - deltawaitratm_med )**2
      deltatodoswaitatm_desv = deltatodoswaitatm_desv + ( ( deltawaitsatm(rank)+deltawaitratm(rank) ) - deltatodoswaitatm_med )**2
   enddo
   deltainitatm_desv = sqrt(deltainitatm_desv / 320 )
   deltarunatm_desv = sqrt(deltarunatm_desv / 320 )
   deltafinaatm_desv = sqrt(deltafinaatm_desv / 320 )
   deltatotalatm_desv = sqrt(deltatotalatm_desv / 320 )
   deltawaitsatm_desv = sqrt(deltawaitsatm_desv / 320 )
   deltawaitratm_desv = sqrt(deltawaitratm_desv / 320 )
   deltatodoswaitatm_desv = sqrt(deltatodoswaitatm_desv / 320 )
   
   
   
   write(12, "('ATM - Media dos ranks: DInit: ', f7.2, ' s (DP: ', f7.2, 's) | DRun: ', f7.2, ' s (DP: ', f7.2, 's) | DFina: ', f7.2, ' s (DP: ', f7.2, 's) | DTOTAL: ', f7.2, ' s (DP: ', f7.2, 's)')") &
   deltainitatm_med, deltainitatm_desv, deltarunatm_med, deltarunatm_desv, deltafinaatm_med, deltafinaatm_desv, deltatotalatm_med, deltatotalatm_desv
   
   write(12, "('ATM - Media dos ranks: DTWaits: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitsatm_med, deltawaitsatm_desv, deltawaitsatm_med/deltatotalatm_med*100 
   
   write(12, "('ATM - Media dos ranks: DTWaitr: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltawaitratm_med, deltawaitratm_desv, deltawaitratm_med/deltatotalatm_med*100 
   
   write(12, "('ATM - Media dos ranks: DTTodosWait: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
   deltatodoswaitatm_med, deltatodoswaitatm_desv, deltatodoswaitatm_med/deltatotalatm_med*100 
   
   
   
   
   
   
   
   
   
   
   do rank = 0, 319
   
      !lendo os tempos principais: init fort.1000~1319(OCN): -----------------
      write(cunid, '(i4)') unid+rank
      !print*, cunid
      open(unid+rank, file='datain/soinit/fort.'//cunid)
         read(unid+rank, *) tempoinitocn(rank, 1), ranki, func, pos  
         read(unid+rank, *) tempoinitocn(rank, 2), ranki, func, pos  
      close(unid+rank)
      !Run:
      open(unid+rank, file='datain/sorun/fort.'//cunid)
         read(unid+rank, *) temporunocn(rank, 1), ranki, func, pos  
         read(unid+rank, *) temporunocn(rank, 2), ranki, func, pos  
      close(unid+rank) 
      !Finalize:
      open(unid+rank, file='datain/sofinalize/fort.'//cunid)
         read(unid+rank, *) tempofinaocn(rank, 1), ranki, func, pos  
         read(unid+rank, *) tempofinaocn(rank, 2), ranki, func, pos  
      close(unid+rank) 
      !------------------------------------------------------------------
   
   
      !lendo os tempos Waits: fort.1000~1319(OCN): ----------------------
      open(unid+rank, file='datain/sowaits/fort.'//cunid)
      do
         read(unid+rank, *, end=20) tempi, ranki, func, pos   
         read(unid+rank, *, end=20) tempf, ranki, func, pos 
         deltawaitsocn(rank) = deltawaitsocn(rank) + (tempf-tempi)
      enddo
20    continue   
      close(unid+rank)

      !lendo os tempos Waitr: fort.1000~1319(OCN): ----------------------
      open(unid+rank, file='datain/sowaitr/fort.'//cunid)
      do
         read(unid+rank, *, end=21) tempi, ranki, func, pos   
         read(unid+rank, *, end=21) tempf, ranki, func, pos 
         deltawaitrocn(rank) = deltawaitrocn(rank) + (tempf-tempi)
         !print*, rank,tempf,tempi, (tempf-tempi), deltawaitrocn(rank)
      enddo
21    continue    
      close(unid+rank)  
   
   
      deltainitocn(rank) = tempoinitocn(rank, 2) - tempoinitocn(rank, 1)
      deltarunocn(rank) = temporunocn(rank, 2) - temporunocn(rank, 1)
      deltafinaocn(rank) = tempofinaocn(rank, 2) - tempofinaocn(rank, 1)
      deltatotalocn(rank) = tempofinaocn(rank, 1) - tempoinitocn(rank, 2)
   
      write(11, "('OCN - Rank:', i4, ' DInit: ', f7.2, ' s | DRun: ', f7.2, ' s | DFina: ', f7.2, ' s | DTOTAL: ', f7.2, ' s')") &
      rank, deltainitocn(rank), deltarunocn(rank), deltafinaocn(rank), deltatotalocn(rank)
      
      write(11, "('OCN - Rank:', i4, ' DTWaits: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsocn(rank), deltawaitsocn(rank)/deltatotalocn(rank)*100 
      
      write(11, "('OCN - Rank:', i4, ' DTWaitr: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitrocn(rank), deltawaitrocn(rank)/deltatotalocn(rank)*100 
      
      write(11, "('OCN - Rank:', i4, ' DTTodosWait: ', f7.2, ' s, ', f6.2, ' %')") &
      rank, deltawaitsocn(rank)+deltawaitrocn(rank), (deltawaitsocn(rank)+deltawaitrocn(rank))/deltatotalocn(rank)*100 

      write(11, *)''

      !media:
      deltainitocn_med = deltainitocn_med + deltainitocn(rank)
      deltarunocn_med = deltarunocn_med + deltarunocn(rank)
      deltafinaocn_med = deltafinaocn_med + deltafinaocn(rank)
      deltatotalocn_med = deltatotalocn_med + deltatotalocn(rank)
      deltawaitsocn_med = deltawaitsocn_med + deltawaitsocn(rank)
      deltawaitrocn_med = deltawaitrocn_med + deltawaitrocn(rank)
      deltatodoswaitocn_med = deltatodoswaitocn_med + ( deltawaitsocn(rank)+deltawaitrocn(rank) )

   enddo
   deltainitocn_med = deltainitocn_med / 320
   deltarunocn_med = deltarunocn_med / 320
   deltafinaocn_med = deltafinaocn_med / 320
   deltatotalocn_med = deltatotalocn_med / 320
   deltawaitsocn_med = deltawaitsocn_med / 320
   deltawaitrocn_med = deltawaitrocn_med / 320
   deltatodoswaitocn_med = deltatodoswaitocn_med / 320
   
   
   !Desvio padrao:
   do rank = 0, 319
      deltainitocn_desv = deltainitocn_desv + ( deltainitocn(rank) - deltainitocn_med )**2
      deltarunocn_desv = deltarunocn_desv + ( deltarunocn(rank) - deltarunocn_med )**2
      deltafinaocn_desv = deltafinaocn_desv + ( deltafinaocn(rank) - deltafinaocn_med )**2
      deltatotalocn_desv = deltatotalocn_desv + ( deltatotalocn(rank) - deltatotalocn_med )**2
      deltawaitsocn_desv = deltawaitsocn_desv + ( deltawaitsocn(rank) - deltawaitsocn_med )**2
      deltawaitrocn_desv = deltawaitrocn_desv + ( deltawaitrocn(rank) - deltawaitrocn_med )**2
      deltatodoswaitocn_desv = deltatodoswaitocn_desv + ( ( deltawaitsocn(rank)+deltawaitrocn(rank) ) - deltatodoswaitocn_med )**2
   
   enddo
   deltainitocn_desv = sqrt(deltainitocn_desv / 320 )
   deltarunocn_desv = sqrt(deltarunocn_desv / 320 )
   deltafinaocn_desv = sqrt(deltafinaocn_desv / 320 )
   deltatotalocn_desv = sqrt(deltatotalocn_desv / 320 )
   deltawaitsocn_desv = sqrt(deltawaitsocn_desv / 320 )
   deltawaitrocn_desv = sqrt(deltawaitrocn_desv / 320 )
   deltatodoswaitocn_desv = sqrt(deltatodoswaitocn_desv / 320 )
   
   
      write(13, "('OCN - Media dos ranks: DInit: ', f7.2, ' s (DP: ', f7.2, 's) | DRun: ', f7.2, ' s (DP: ', f7.2, 's) | DFina: ', f7.2, ' s (DP: ', f7.2, 's) | DTOTAL: ', f7.2, ' s (DP: ', f7.2, 's)')") &
      deltainitocn_med, deltainitocn_desv, deltarunocn_med, deltarunocn_desv, deltafinaocn_med, deltafinaocn_desv, deltatotalocn_med, deltatotalocn_desv
      
      write(13, "('OCN - Media dos ranks: DTWaits: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
      deltawaitsocn_med, deltawaitsocn_desv, deltawaitsocn_med/deltatotalocn_med*100 
   
      write(13, "('OCN - Media dos ranks: DTWaitr: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
      deltawaitrocn_med, deltawaitrocn_desv, deltawaitrocn_med/deltatotalocn_med*100 
   
      write(13, "('OCN - Media dos ranks: DTTodosWait: ', f7.2, ' s (DP: ', f7.2, 's), ', f6.2, ' %')") &
      deltatodoswaitocn_med, deltatodoswaitocn_desv, deltatodoswaitocn_med/deltatotalocn_med*100 
   
end program
