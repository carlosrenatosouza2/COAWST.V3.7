WRF Model GRAPHIC TOOLS : ARWpost

    Purpose

    - Generate GrADS and/or Vis5D input files from WRF ARW output files
    - All WRF ARW geogrid, metgrid, input and output files (real and idealized data) can be converted
    - This package used WRF IO API, so file format is not restricted to netCDF files
       (netCDF had been tested extensively, while some limited tested has been with GRIB1 files.
        Binary files does not currently work)
    - Create the corresponding .ctl and .dat files (GrADS) or .v5d file (Vis5D)
    - Version 2.0 release April 4, 2008
    Necessary Software to run Scripts

    - Obtain the ARWpost TAR file from the WRF Download page
    - It you plan to convert WRF ARW data to GrADS, you will also need the GrADS software:
          You can download and install GrADS from 
          http://grads.iges.org/grads/
          GrADS libraries are not needed to compile and run ARWpost, but is needed to display the graphics
    - It you plan to convert WRF ARW data to Vis5D, you will also need the Vis5D software:
          You can download and install Vis5D from 
          http://www.ssec.wisc.edu/~billh/vis5d.html
          Vis5D libraries ARE needed to compile and run ARWpost, as well as to display the graphics

    - If you do not plan to convert data to Vis5D format, you do NOT need the Vis5D libraries.
    Hardware

    Compiler flags are available for the following machine (not all has been tested fully):

    - DEC Alpha
    - Linux (PGI & INTEL compilers)
    - Sun
    - SGI
    - IBM
    - Mac (xlf compilers)
    Steps to Compile and Run the Code

    - Unzip and untar the ARWpost TAR file - you will have the following files:

        -rw-r--r-- 1 wrfhelp users 5534 Dec 18 12:23 README
        drwxr-xr-x 2 wrfhelp users 512 Dec 18 12:23 arch/
        -rwxr-xr-x 1 wrfhelp users 966 Dec 18 12:23 clean*
        -rwxr-xr-x 1 wrfhelp users 733 Dec 18 12:23 compile*
        -rwxr-xr-x 1 wrfhelp users 4257 Dec 18 12:23 configure*
        -rw-r--r-- 1 wrfhelp users 12 Dec 18 12:23 fields.plt
        -rw-r--r-- 1 wrfhelp users 15626 Dec 18 12:23 gribinfo.txt
        -rw-r--r-- 1 wrfhelp users 54446 Dec 18 12:23 gribmap.txt
        -rw-r--r-- 1 wrfhelp users 19 Dec 18 12:23 myLIST
        -rw-r--r-- 1 wrfhelp users 1653 Dec 18 12:23 namelist.ARWpost
        drwxr-xr-x 2 wrfhelp users 512 Dec 18 12:23 scripts/
        drwxr-xr-x 2 wrfhelp users 1536 Dec 18 12:23 src/
        drwxr-xr-x 2 wrfhelp users 512 Dec 18 12:23 util/

    - Configure

        The code has been written to conform to the configure/compile patterns or WRFV2 and WPS

        WRFV2 MUST be installed somewhere on your system

        Type:

            ./configure

            You will see a list of options for your computer (below is an example for a Linux machine)

            Will use NETCDF in dir: /usr/local/netcdf-pgi
            ------------------------------------------------------------------------
            Please select from among the following supported platforms.
            1. PC Linux i486 i586 i686, PGI compiler (no vis5d)
            2. PC Linux i486 i586 i686, PGI compiler (vis5d)
            3. PC Linux i486 i586 i686, Intel compiler (no vis5d)
            4. PC Linux i486 i586 i686, Intel compiler (vis5d)
             
            Enter selection [1-4] :

            - Make sure the netCDF path is correct.
            - Pick compile options for your machine (if you do not have Vis5D, or if you do not plan on using it,
              pick an option without Vis5D libraries)

    - Compile

        If your WRFV2 code is not compiled under ../WRFV2, edit configure.arwp, and set "WRF_DIR"
        to the correct location of your WRFV2 code

        Type:

            ./compile

            This will create the ARWpost.exe executable

    - Edit the namelist.ARWpost:

        - set times to be processed (&datetime)
        - set input and output file names and variables to be processed (&io)
             io_form_input: 2=netCDF, 5=GRIB1
             input_root_name: Path and root name of files to use as input.
                                      All files starting with the root name will be processed.
             output_root_name: Output root name
             plot: Which fields to process.
                     Options are - all, basic, list, file, basic_file, basic_list, list_file, all_file, all_list, basic_list_file, all_list_file
                     If "list" is used, a list of variables must be supplied under "fields"
                     If "file" is used, a list of variables must be added to a file, and the filename supplied under "fields_file
             fields: fields to plot
             fields_file: file name which contain list of fields to plot
             output_type: Options are 'grads' (default) or 'v5d'
             mercator_defs: Set to true if mercator plots are distorted

             Available diagnostics: height ,theta ,tc, tk, td, rh, umet, vmet, pressure, dbz, max_dbz,
                                           u10m, v10m, slp, mcape, mcin, lcl, lfc, cape, cin, clfr, wdir, wspd, wd10, ws10

        - set levels to interpolate too (&interp)
             interp_method: 0=sigma levels, -1=code defined "nice" height levels, 1=user defined height or pressure levels
             interp_levels: Only used if interp_method=1
                                 Supply levels to interpolate to, in hPa (pressure) or km (height)
                                 Supply levels bottom to top

        - for extra information regarding the namelist.ARWpost file, refer to the README file

    - Run

        Type

            ./ARWpost.exe

        - This will create output_root_name.dat and output_root_name.ctl files if creating GrADS input file, and
          output_root_name.v5d, if creating Vis5D files.

         

    NOW YOU ARE READY TO VIEW THE OUTPUT

    GrADS

    For general information about working with GrADS, view the GrADS home page: http://grads.iges.org/grads/

    To help users get started a number of grads scripts have been provided:

        - The scripts are all available in the scripts/ directory.
        - The scripts provided are only examples of the type of plots one can generate with GrADS data.
        - The user will need to modify these script to suit their data (Example, if you did not specify 0.25 km and 2 km
           as levels to interpolate to when you run the "bwave" data through the converter, the "bwave.gs" script will
           not display any plots, since it will specifically look for these to levels).
        - Scripts must be copied to the location of the input data.

        - GENERAL SCRIPTS:

            cbar.gs
            Plot color bar on shaded plots (from GrADS home page)

            rgbset.gs
            Some extra colors (Users can add/change colors from color number 20 to 99)

            skew.gs
            Program to plot skewT (modified version of function as found on
            http://www.ems.psu.edu/~bryan/mm5/grads/)
            TO RUN TYPE: run skew.gs (needs pressure level TC,TD,U,V as input)
            User will be prompted if a hardcopy of the plot must be create - 1 for yes and 0 for no.
            If 1 is entered a GIF image will be created.
            Need to enter lon/lat of point you are interested in
            Need to enter time you are interested in
            Can overlay 2 different times

            plotALL.gs
            Once you have opened a GrADS window, all one needs to do is run this script.
            It will automatically find all .ctl files in the current directory and list them so one can pick which file to open.
            Then the script will loop through all available fields and plot the ones a user requests.

        - SCRIPTS FOR REAL DATA:

            real_surf.gs
            Plot some surface data
            Need input data on model levels

            plevels.gs
            Plot some pressure level fields
            Need model output on pressure levels

            rain.gs
            Plot total rainfall
            Need a model output data set (any vertical coordinate), that contain fields "RAINC" and "RAINNC"

            cross_z.gs
            Need z level data as input
            Will plot a NS and EW cross section of RH and T (C)
            Plots will run through middle of the domain

            zlevels.gs
            Plot some height level fields
            Need input data on height levels
            Will plot data on 2, 5, 10 and 16km levels

            input.gs
            Need WRF INPUT data on height levels

        - SCRIPTS FOR IDEALIZED DATA:

            bwave.gs
            Need height level data as input
            Will look for 0.25 and 2 km data to plot

            grav2d.gs
            Need normal model level data

            hill2d.gs
            Need normal model level data

            qss.gs
            Need height level data as input.
            Will look for heights 0.75, 1.5, 4 and 8 km to plot

            sqx.gs
            Need normal model level data a input

            sqy.gs
            Need normal model level data a input

        Trouble Shooting

        The code executes correctly, but you get "NaN" or "Undefined Grid" for all fields
        when displaying the data.

        Look in the .ctl file.

        a) If the second line is:
             options byteswapped
        Remove this line from your .ctl file and try to display the data again.
        If this SOLVES the problem, you need to remove the -Dbytesw option from the configure.arwp file.

        b) If the line below does NOT appear in your .ctl file:
             options byteswapped
        ADD this lines as the second line in the .ctl file.
        Try to display the data again.
        If this SOLVES the problem, you need to ADD the -Dbytesw option for the configure.arwp file.

        The line "options byteswapped" is often needed on some computers (DEC alpha as an example).
        It is also often needed if you run the converter on one computer and use another
        to display the data.



    Vis5D

    For general information about working with Vis5D, view the Vis5D home page: http://www.ssec.wisc.edu/~billh/vis5d.html
     

updated 04/04/2008 19:21:23

  	  	
