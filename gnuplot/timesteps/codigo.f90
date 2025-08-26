program media
   implicit none

   integer, parameter               :: dp = kind(1.0d0), nciclos=49
   integer                          :: exp, i, ios, iletra, r, temp, k, unit
   integer, dimension(9)            :: ATM_ncores=0, OCN_ncores=0
   real(dp)                         :: tempo=0.0_dp, med_tempo_total=0.0_dp
   real(dp), dimension(nciclos)     :: t_ciclos_atm
   character(len=1)                 :: letra
   character(len=200)               :: linha, dirname, filename
   character(len=1), dimension(9)   :: expletras

   ! ==============================
   ! Definicao dos experimentos
   ! ==============================
   expletras = (/'e','h','i','j','k','l','m','n','o'/)
   ATM_ncores = (/256,384,384,512,512,768,768,1024,1024/)
   OCN_ncores = (/128,128,256,128,256,128,256,128,256/)

   ! ==============================
   ! Exemplo: so a letra J (iletra=4) e exp=0
   ! ==============================
   iletra = 4
   letra  = expletras(iletra)

   do exp = 0, 0
      print *, "=== Exp ", letra, exp, "==="

      ! ------------------------------
      ! Lendo fort.1000 (tempo total)
      ! ------------------------------
      write(dirname,"('/mnt/beegfs/carlos.souza/Doutorado/COAWST.V3.7/Work/ATLSW12/wr_17022023-',A1,I1,'/')") letra, exp

      open(newunit=unit, file=trim(dirname)//'fort.1000', status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print *, "Erro ao abrir ", trim(dirname)//"fort.1000"
         stop
      end if

      read(unit,'(A)', iostat=ios) linha
      if (ios == 0) then
         read(linha(index(linha,'=')+1:), *, iostat=ios) tempo
         if (ios == 0) then
            med_tempo_total = med_tempo_total + tempo
            print *, "Tempo total = ", tempo
         else
            print *, "Erro lendo tempo em fort.1000"
            stop
         end if
      else
         print *, "Erro lendo linha de fort.1000"
         stop
      end if
      close(unit)

      ! ------------------------------
      ! Lendo fort.20000 ... fort.(20000+ATM_ncores-1)
      ! ------------------------------
      do r = 0, ATM_ncores(iletra)-1
         write(filename,'("fort.",I0)') 20000 + r

         open(newunit=unit, file=trim(dirname)//trim(filename), status='old', action='read', iostat=ios)
         if (ios /= 0) then
            print *, "Erro ao abrir ", trim(dirname)//trim(filename)
            cycle
         end if

         do k = 1, nciclos
            read(unit, *, iostat=ios) t_ciclos_atm(k), temp
            if (ios /= 0) exit
            print *, "Rank=", r, " Ciclo=", k, " Tempo=", t_ciclos_atm(k), " Temp=", temp
         end do

         close(unit)
      end do
   end do

   med_tempo_total = med_tempo_total/3.0_dp   
   print *, 'Media =', med_tempo_total

end program media

