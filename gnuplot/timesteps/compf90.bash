#!/bin/bash

#codename="graftimestep_egeon_24h_media"
#codename="graftimestep_egeon_24h_media_comWait"
#codename="graftimestep_egeon_24h_soRank0_comWait"
codename="graftimestep_egeon_24h_soRank0_comWait_somaRanks"


rm -f ${codename}.x

echo "compilando: ${codename}.f90"

gfortran -ffree-line-length-none ${codename}.f90 -o ${codename}.x

echo "compilado"
sleep 3

if [ -s ${codename}.x ] 
then
   echo "executando: ${codename}.x"
   time ./${codename}.x
   echo "executado"

   
else
   echo "deu ruim"
fi



exit
