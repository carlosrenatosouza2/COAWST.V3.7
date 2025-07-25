#!/bin/ksh


cd datain
rm -fr soinit
mkdir soinit
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Init" > ${i}.u
   mv ${i}.u soinit/${i}
   
done

rm -fr sorun
mkdir sorun
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Run" > ${i}.u
   mv ${i}.u  sorun/${i}
   
done

rm -fr sofinalize
mkdir sofinalize

for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Final" > ${i}.u
   mv ${i}.u  sofinalize/${i}
   
done
