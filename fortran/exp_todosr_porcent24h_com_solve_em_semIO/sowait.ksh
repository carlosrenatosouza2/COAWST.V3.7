#!/bin/ksh


cd datain
rm -fr sowaits
mkdir sowaits
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Waits" > ${i}.u
   mv ${i}.u sowaits/${i}
   
done

rm -fr sowaitr
mkdir sowaitr
 
for i in $(ls fort.*)
do
   echo $i
   
   cat ${i} | grep "Waitr" > ${i}.u
   mv ${i}.u sowaitr/${i}
   
done
