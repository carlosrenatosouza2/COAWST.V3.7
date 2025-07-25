1)- Intalar o MCT:
usar o file Makefile.conf_pgi, como base para atualizar o Makefile.conf,
que é gerado com o ./configure. Depois e rodar  
> make
> installs

2) Atualizar o file Config_new.pl, que fica no diretorio:
WRF/arch. Substituir pelas opceos de maquina e compilaçao:
V3.3 e a 43 e 1.

3)



-----------MUDIFICACOES NOS CODIGOS--------

Para adicionar o efeito das correntes no vento e incluir a ativação do LOESS:
1) ROMS/Modules/mod_param.F
   declara a variável ADDCUR e LOESSK
2) ROMS/Utility/read_phypar.F
   adiciona a linha para ler ADDCUR e LOESSK
3) ROMS/Nonlinear/bulk_flux.F
   adicona o calculo de ADDCUR
4) Inclui em ocean.in a variavel ADDCUR que ira dizer se o efeito da corrente
sera incluido ou nao. Mesmo caso para o LOESS

-------------------------------------------
Para incluir o filtro LOESS2
1) copiar a rotina 
    Master/mct_roms_wrf.h

-------------------------------------------
Para incluir os termos da equacao da tendencia de movimento no output
"Extracting Horizontal Momentum Tendency Terms 
from WRF 3.4.1 - Nadya Moisseeva - November 2013"
Document:  /Users/luciano/Luciano/Models/COAWST3.3/WRF-ExtractingMomentumTerms.pdf  

1) WRF/Registry/Registry.EM_COMMON
Acresecentar as linhas 173 a 189 
# new tendency variables for dynamical analysis - (NM 2013)

2) WRF/dyn_em/module_em.F 
acrescentar linhas 216 e 217
acrescentar linhas 291 e 300
acrescentar linha  819
acrescentar linhas 909 e 910

3) WRF/dyn_em/solve_em.F 
acrescentar linhas 842 e 843
acrescentar linhas 963

-------------------------------------------
Para incluir os fluxos de calor no output

For averages (the Aout's) you will need to edit the files
1) edit ROMS/Utility/def_avg.F and:
change line 1897,   line  3920(#LPP 3919), and line 3323(#LPP 4705)  from
#  ifdef BULK_FLUXES
to
# if defined BULK_FLUXES || defined AIR_OCEAN
(these all stop the next call to latent etc.)

2) edit ROMS/Utility/wrt_avg.F and:
change line 1897 (#LPP 3323) from
#  ifdef BULK_FLUXES
to
# if defined BULK_FLUXES || defined AIR_OCEAN


For the History files , same kind of thing..
3) edit ROMS/Utility/def_his.F and:
change line 4415   from
#  ifdef BULK_FLUXES
to
# if defined BULK_FLUXES || defined AIR_OCEAN

LPP - Has touched also /ROMS/Modules/mod_average.F
# if defined BULK_FLUXES || defined AIR_OCEAN

81 !LPP#  ifdef BULK_FLUXES
82 if defined BULK_FLUXES || defined AIR_OCEAN

285 !LPP#  ifdef BULK_FLUXES
286 # if defined BULK_FLUXES || defined AIR_OCEAN

708 !LPP#  ifdef BULK_FLUXES
709 # if defined BULK_FLUXES || defined AIR_OCEAN

1541 !LPP#  ifdef BULK_FLUXES
1542 # if defined BULK_FLUXES || defined AIR_OCEAN

LPP - Has touched also /ROMS/Nonlinear/set_avg.F
946 !LPP#  ifdef BULK_FLUXES
947 #  if defined BULK_FLUXES || defined AIR_OCEAN

2867 !LPP#  ifdef BULK_FLUXES
2868 #  if defined BULK_FLUXES || defined AIR_OCEAN

5276 !LPP#  ifdef BULK_FLUXES
5277 #  if defined BULK_FLUXES || defined AIR_OCEAN

