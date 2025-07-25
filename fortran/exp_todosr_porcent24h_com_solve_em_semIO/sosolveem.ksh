#!/bin/ksh


cd datain
rm -fr sosolveem
mkdir sosolveem
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "solve_em i" > ${i}.u
   mv ${i}.u sosolveem/${i}
   
done

