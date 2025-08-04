# My COAWST notes

## To install:

1. Compile MCT libraries:

~~~
source config_env_gnu_egeon_gcc+mpichspack_libsJP.ksh
cd Lib/MCT
./configure
~~~

alterar o makefile.conf:
~~~
   libdir          = /mnt/beegfs/carlos.souza/COAWST.V3.7/Lib/MCT/gnu/lib
   includedir      = /mnt/beegfs/carlos.souza/COAWST.V3.7/Lib/MCT/gnu/include
~~~

Make:
~~~
make clean
make install


