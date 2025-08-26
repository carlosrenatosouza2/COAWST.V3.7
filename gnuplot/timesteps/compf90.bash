#!/bin/bash

codename="graftimestep_egeon_24h_media"

rm -f ${codename}.x

echo "compilando"

gfortran -ffree-line-length-none ${codename}.f90 -o ${codename}.x

echo "compilado"
sleep 3

if [ -s ${codename}.x ] 
then
   echo "executando"
   time ./${codename}.x
   echo "executado"

   
else
   echo "deu ruim"
fi

exit
