#!/bin/ksh

codename="make_media"

echo "compilando"
rm -fr ${codename}.x

gfortran -ffree-line-length-none -fcheck=bounds -g -O0 ${codename}.f90 -o ${codename}.x

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

