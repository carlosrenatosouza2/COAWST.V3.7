                                                         ww3_grid.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       PROGRAM W3GRID
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |           J. H. Alves             |
    9 !/                  |            F. Ardhuin             |
   10 !/                  |                        FORTRAN 90 |
   11 !/                  | Last update :         15-Apr-2020 |
   12 !/                  +-----------------------------------+
   13 !/
   14 !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
   15 !/    27-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   16 !/                  Add UNFORMATTED bath file option.
   17 !/                  Read options with namelists.
   18 !/    14-Feb-2000 : Adding exact Snl                    ( version 2.01 )
   19 !/    04-May-2000 : Non central source term int.        ( version 2.03 )
   20 !/    24-Jan-2001 : Flat grid option.                   ( version 2.06 )
   21 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
   22 !/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
   23 !/    27-Feb-2001 : O0 output switch added.             ( version 2.08 )
   24 !/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
   25 !/    29-Mar-2001 : Sub-grid island treatment.          ( version 2.10 )
   26 !/    20-Jul-2001 : Clean up.                           ( version 2.11 )
   27 !/    12-Sep-2001 : Clean up.                           ( version 2.13 )
   28 !/    09-Nov-2001 : Clean up.                           ( version 2.14 )
   29 !/    11-Jan-2002 : Sub-grid ice treatment.             ( version 2.15 )
   30 !/    17-Jan-2002 : DSII bug fix.                       ( version 2.16 )
   31 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
   32 !/    26-Nov-2002 : Adding first version of NL-3/4.     ( version 3.01 )
   33 !/                  Removed before distribution in 3.12.
   34 !/    26-Dec-2002 : Relaxing CFL time step.             ( version 3.02 )
   35 !/    01-Aug-2003 : Modify GSE correction for moving gr.( version 3.03 )
   36 !/                  Add offset option for first direction.
   37 !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
   38 !/    04-May-2005 : Allow active points at edge.        ( version 3.07 )
   39 !/    07-Jul-2005 : Add MAPST2 and map processing.      ( version 3.07 )
   40 !/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
   41 !/    23-Jun-2006 : Adding alternative source terms.    ( version 3.09 )
   42 !/                  Module W3SLN1MD, dummy for others.
   43 !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   44 !/    28-Oct-2006 : Spectral partitioning.              ( version 3.09 )
   45 !/    09-Jan-2007 : Correct edges of read mask.         ( version 3.10 )
   46 !/    26-Mar-2007 : Add to spectral partitioning.       ( version 3.11 )
   47 !/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
   48 !/                  ( J. H. Alves )
   49 !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
   50 !/                  ( J. H. Alves )
   51 !/    18-Sep-2007 : Adding WAM4 physics option.         ( version 3.13 )
   52 !/                  ( F. Ardhuin )
   53 !/    09-Oct-2007 : Adding bottom scattering SBS1.      ( version 3.13 )
   54 !/                  ( F. Ardhuin )
   55 !/    22-Feb-2008 : Initialize TRNX-Y properly.         ( version 3.13 )
 
 
                                                         ww3_grid.F90  page   2
 
 
   56 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   57 !/    23-Jul-2009 : Modification of ST3 namelist  .     ( version 3.14-SH>
   58 !/    31-Mar-2010 : Addition of shoreline reflection    ( version 3.14-IF>
   59 !/    29-Jun-2010 : Adding Stokes drift profile output  ( version 3.14-IF>
   60 !/    30-Aug-2010 : Adding ST4 option                   ( version 3.14-IF>
   61                                                                           
   62 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   63 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   64 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   65 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   66 !/    29-Oct-2010 : Clean up of unstructured grids      ( version 3.14.4 )
   67 !/                  (A. Roland and F. Ardhuin)
   68 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   69 !/                  specify index closure for a grid. Change GLOBAL
   70 !/                  input in ww3_grid.inp to CSTRG.     ( version 3.14 )
   71 !/                  (T. J. Campbell, NRL)
   72 !/    25-Jun-2011 : Adding movable bed friction         ( version 4.01 )
   73 !/    16-Sep-2011 : Clean up.                           ( version 4.05 )
   74 !/    01-Dec-2011 : New namelist for reflection         ( version 4.05 )
   75 !/    01-Mar-2012 : Bug correction for NLPROP in ST2    ( version 4.05 )
   76 !/    12-Jun-2012 : Add /RTD rotated grid option. JGLi  ( version 4.06 )
   77 !/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
   78 !/                  filter (SNLS) from 3.15 (HLT).      ( version 4.07 )
   79 !/    02-Sep-2012 : Clean up of reflection and UG grids ( version 4.08 )
   80 !/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
   81 !/    19-Dec-2012 : Add NOSWLL as namelist variable.    ( version 4.OF )
   82 !/    05-Mar-2013 : Adjusted default roughness for rocks( version 4.09 )
   83 !/    01-Jun-2013 : Adding namelist for spectral output ( version 4.10 )
   84 !/    12-Sep-2013 : Adding Arctic part for SMC grid.    ( version 4.11 )
   85 !/    01-Nov-2013 : Changed UG list name to UNST        ( version 4.12 )
   86 !/    11-Nov-2013 : Make SMC and RTD option compatible. ( version 4.13 )
   87 !/    13-Nov-2013 : Moved out reflection to W3UPDTMD    ( version 4.12 )
   88 !/    27-Jul-2013 : Adding free infragravity waves      ( version 4.15 )
   89 !/    02-Dec-2013 : Update of ST4                       ( version 4.16 )
   90 !/    16-Feb-2014 : Adds wind bias correction: WCOR     ( version 5.00 )
   91 !/    10-Mar-2014 : Adding namelist for IC2             ( version 5.01 )
   92 !/    29-May-2014 : Adding namelist for IC3             ( version 5.01 )
   93 !/    15 Oct-2015 : Change SMC grid input files. JGLi   ( version 5.09 )
   94 !/    10-Jan-2017 : Changes for US3D and USSP           ( version 6.01 )
   95 !/    20-Jan-2017 : Bug fix for mask input from file.   ( version 6.02 )
   96 !/    01-Mar-2018 : RTD poles info read from namelist   ( version 6.02 )
   97 !/    14-Mar-2018 : Option to read UNST boundary file   ( version 6.02 )
   98 !/    26-Mar-2018 : Sea-point only Wnd/Cur input. JGLi  ( version 6.02 )
   99 !/    15-May-2018 : Dry sea points over zlim            ( version 6.04 )
  100 !/    06-Jun-2018 : add Implicit grid parameters for unstructured grids
  101 !/                  add DEBUGGRID/DEBUGSTP              ( version 6.04 )
  102 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
  103 !/    20-Jun-2018 : Update of ST6  (Q. Liu)             ( version 6.06 )
  104 !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
  105 !/    27-Aug-2018 : Add WBT parameter                   ( version 6.06 )
  106 !/    22-Jan-2020 : Update default values for IS2       ( version 7.05 )
  107 !/    20-Feb-2020 : Include Romero's dissipation in ST4 ( version 7.06 )
  108 !/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
  109 !/    18-Jun-2020 : Adds 360-day calendar option        ( version 7.08 )
  110 !/    24-Jun-2020 : RTD output b. c. to rotated grid.   ( version 7.11 )
 
 
                                                         ww3_grid.F90  page   3
 
 
  111 !/
  112 !/    Copyright 2009-2013 National Weather Service (NWS),
  113 !/       National Oceanic and Atmospheric Administration.  All rights
  114 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  115 !/       No unauthorized use without permission.
  116 !/
  117 !  1. Purpose :
  118 !
  119 !     "Grid" preprocessing program, which writes a model definition
  120 !     file containing the model parameter settigs and grid data.
  121 !
  122 !  2. Method :
  123 !
  124 !     Information is read from the file ww3_grid.inp (NDSI), or
  125 !     preset in this program. A model definition file mod_def.ww3 is
  126 !     then produced by W3IOGR. Note that the name of the model
  127 !     definition file is set in W3IOGR.
  128 !
  129 !  3. Parameters :
  130 !
  131 !     Local parameters.
  132 !     ----------------------------------------------------------------
  133 !       NDSI    Int.  Input unit number ("ww3_grid.inp").
  134 !       NDSS    Int.  Scratch file.
  135 !       NDSG    Int.  Grid unit ( may be NDSI )
  136 !       NDSTR   Int.  Sub-grid unit ( may be NDSI or NDSG )
  137 !       VSC     Real  Scale factor.
  138 !       VOF     Real  Add offset.
  139 !       ZLIM    Real  Limiting bottom depth, used to define land.
  140 !       IDLA    Int.  Layout indicator used by INA2R.
  141 !       IDFM    Int.  Id. FORMAT indicator.
  142 !       RFORM   C*16  Id. FORMAT.
  143 !       FNAME   C*60  File name with bottom level data.
  144 !       FROM    C*4   Test string for open, 'UNIT' or 'FILE'
  145 !     ----------------------------------------------------------------
  146 !
  147 !  4. Subroutines used :
  148 !
  149 !      Name      Type  Module   Description
  150 !     ----------------------------------------------------------------
  151 !      W3NMOD    Subr. W3GDATMD Set number of model.
  152 !      W3SETG    Subr.   Id.    Point to selected model.
  153 !      W3DIMS    Subr.   Id.    Set array dims for a spectral grid.
  154 !      W3DIMX    Subr.   Id.    Set array dims for a spatial grid.
  155 !      W3GRMP    Subr. W3GSRUMD Compute bilinear interpolation for point
  156 !      W3NOUT    Subr. W3ODATMD Set number of model for output.
  157 !      W3SETO    Subr.   Id.    Point to selected model for output.
  158 !      W3DMO5    Subr.   Id.    Set array dims for output type 5.
  159 !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  160 !      STRACE    Subr.   Id.    Subroutine tracing.
  161 !      NEXTLN    Subr.   Id.    Get next line from input file
  162 !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  163 !      DISTAB    Subr. W3DISPMD Make tables for solution of the
  164 !                               dispersion relation.
  165 !      READNL    Subr. Internal Read namelist.
 
 
                                                         ww3_grid.F90  page   4
 
 
  166 !      INAR2R    Subr. W3ARRYMD Read in an REAL array.
  167 !      PRTBLK    Subr.   Id.    Print plot of array.
  168 !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  169 !     ----------------------------------------------------------------
  170 !
  171 !  5. Called by :
  172 !
  173 !     None, stand-alone program.
  174 !
  175 !  6. Error messages :
  176 !
  177 !  7. Remarks :
  178 !
  179 !      Physical grid :
  180 !     -----------------
  181 !
  182 !     The physical grid is defined by a grid counter IX defining the
  183 !     discrete longitude and IY defining the discrete latitude as shown
  184 !     below. For mathemathical convenience, these grid axes will
  185 !     generally be denoted as the X and Y axes. Two-dimensional arrays
  186 !     describing parameters on this grid are given as A(IY,IX).
  187 !
  188 !           IY=NY
  189 !             ^  |      |      |      |      |      |            ^ N
  190 !             |  |------|------|------|------|------|----        |
  191 !             |  |  ::  |  25  |  26  |  27  |  28  |          --|--
  192 !                |------|------|------|------|------|----        |
  193 !           IY=3 |  ::  |  ::  |  9   |  10  |  11  |            |
  194 !                |------|------|------|------|------|----
  195 !           IY=2 |  ::  |   1  |   2  |  ::  |   3  |
  196 !                |------|------|------|------|------|----
  197 !           IY=1 |  ::  |  ::  |  ::  |  ::  |  ::  |
  198 !                +------+------+------+------+------+----
  199 !                  IX=1   IX=2   IX=3   IX=4   IX=5   ---> IX=NX
  200 !
  201 !                                        :: is a land point.
  202 !
  203 !     To reduce memory usage of the model, spectra are stored for sea
  204 !     points only, in a one-dimensional grid with the length NSEA. This
  205 !     grid is called the storage grid. The definition of the counter
  206 !     in the storage grid is graphically depicted above. To transfer
  207 !     data between the two grids, the maps MAPFS and MAPSF are
  208 !     determined. MAPFS gives the counter of the storage grid ISEA
  209 !     for every physical grid point (IY,IX), such that
  210 !
  211 !             MAPFS(IY,IX) = ISEA
  212 !
  213 !     ISEA = 0 corresponds to land points. The map MAPSF gives the grid
  214 !     counters (IY,IX) for a given storage point ISEA.
  215 !
  216 !             MAPSF(ISEA,1) = IX
  217 !             MAPSF(ISEA,2) = IY
  218 !             MAPSF(ISEA,3) = IY+(IX-1)*NY  ( filled during reading )
  219 !
  220 !     Finally, a status maps MAPSTA and MAPST2 are determined, where
 
 
                                                         ww3_grid.F90  page   5
 
 
  221 !     the status indicator ISTAT = MAPSTA(IY,IX) determines the type
  222 !     of the grid point.
  223 !
  224 !         ISTAT  Means
  225 !       ---------------------------------------------------
  226 !           0    Point excluded from grid.
  227 !        (-)1    Sea point
  228 !        (-)2    "Active" boundary point (data prescribed)
  229 !
  230 !     For ISTAT=0, the secondary status counter ISTA2 is defined as
  231 !
  232 !         ISTA2  Means
  233 !       ---------------------------------------------------
  234 !           0    Land point.
  235 !           1    Point excluded from grid.
  236 !
  237 !     Negative values of ISTAT identify points that are temporarily
  238 !     taken out of the computation. For these points ISTA2 are
  239 !     defined per bit
  240 !
  241 !         BIT    Means
  242 !       ---------------------------------------------------
  243 !          1     Ice flag (1 = ice coverage)
  244 !          2     Dry flag (1 = dry point with depth 0)
  245 !          3     Inferred land in multi-grid model.
  246 !          4     Masking in multi-grid model.
  247 !          5     land point flag for relocatable grid.
  248 !
  249 !      Thus ISTA2=0 for ISTAT<0 is in error, ISTA2=1 means ice cover,
  250 !      ISTA2=3 means ice on dry point, etc.
  251 !
  252 !      Spectral grid :
  253 !     -----------------
  254 !
  255 !     In the spectral grid (and in physical space in general),
  256 !     the cartesian convention for directions is used, i.e., the
  257 !     direction 0 corresponds to waves propagating in the positive
  258 !     X-direction and 90 degr. corresponds to waves propagating in
  259 !     the positive Y-direction. Similar definitions are used for the
  260 !     internal description of winds and currents. Output can obviously
  261 !     be transformed according to any preferred convention.
  262 !
  263 !          ITH=NTH
  264 !             ^  |      |      |      |      |
  265 !             |  |------|------|------|------|----
  266 !             |  |      |      |      |      |      TH(3) = DTH*2.
  267 !                |------|------|------|------|----
  268 !          ITH=2 |      |      |      |      |      TH(2) = DTH
  269 !                |------|------|------|------|----
  270 !          ITH=1 |      |      |      |      |      TH(1) = 0.
  271 !                +------+------+------+------+----
  272 !                  IK=1   IK=2   IK=3   IK=4   ---> IK=NK
  273 !
  274 !     The spectral grid consists of NK wavenumbers. The first
  275 !     wavenumber IK=1 corresponds to the longest wave. The wavenumber
 
 
                                                         ww3_grid.F90  page   6
 
 
  276 !     grid varies in space, as given by an invariant relative freq.
  277 !     grid and the local depth. The spectral grid furthermore contains
  278 !     NTH directions, equally spaced over a full circle. the first
  279 !     direction corresponds to the direction 0, etc.
  280 !
  281 ! (Begin SMC description)
  282 !
  283 !      Spherical Multiple-Cell (SMC) grid
  284 !     -----------------------------------
  285 !
  286 !     SMC grid is a multi-resolution grid using cells of multiple times
  287 !     of each other.  It is similar to the lat-lon grid using rectangular
  288 !     cells but only cells at sea points are retained.  All land points
  289 !     have been removed from the model.  At high latitudes, cells are
  290 !     merged longitudinally to relax the CFL resctiction on time steps.
  291 !     Near coastlines, cells are divided into quarters in a few steps so
  292 !     that high resolution is achieved to refine coastlines and resolve
  293 !     small islands.  At present, three tiers of quarter cells are used.
  294 !     For locating purpose, a usual x-y counter is setup by the smallest
  295 !     cell size and starting from the south-west corner of the usual
  296 !     rectuangular domain.  Each sea cell is then given a pair of x-y
  297 !     index, plus a pair of increments.  These four index are stored in
  298 !     the cell array IJKCel(NCel, 5), each row holds i, j, di, dj, ndps
  299 !     where ndps is an integer depth in metre.  If precision higher than
  300 !     a metre is required, it may use other unit (cm for instance) with a
  301 !     conversion factor.
  302 !
  303 !     For transport calculation, two face arrays, IJKUFc(NUFc, 7) and
  304 !     IJKVFc(NVFc,8), are also created to store the neighbouring cell
  305 !     sequential numbers and the face location and size.  The 3 arrays
  306 !     are calculated outside the wave model and input from text files.
  307 !
  308 !     Boundary condition is added for SMC grid so that it can be used for
  309 !     regional model as well.  Most of the original boundary settings
  310 !     are reclaimed as long as the boundary condition file is provided
  311 !     by a lat-lon grid WW3 model, which will set the interpolation
  312 !     parameters in the boundary condition file.  The NBI number is
  313 !     reset with an input value because the NX-Y double loop overcount
  314 !     the boundary cells for merged cells in the SMC grid.  ISBPI
  315 !     boundary cell mapping array is fine as MAPFS uses duplicated cell
  316 !     number in any merged cell.  From there, all original NBI loops are
  317 !     reusable.
  318 !
  319 !     The whole Arctic can be included in the SMC grid if another option
  320 !     ARC is activated along with the SMC option.  ARC option appends
  321 !     the polar Arctic part above 86N to the existing SMC grid and uses
  322 !     a map-east reference direction for this extra polar region.
  323 !     Because the map-east direction changes with latitude and longitude
  324 !     the wave spectra defined to the map-east direction could not be
  325 !     mixed up with the conventional spectra defined to the local east
  326 !     direction.  A rotation sub is provided for convertion from one to
  327 !     another.  Propagation part will be calculated together, including
  328 !     the boundary cells.  The boundary cells are then updated by
  329 !     assigning the corresponding inner cells to them after conversion.
  330 !     Boundary cells are duplicated northmost 4 rows of the global part
 
 
                                                         ww3_grid.F90  page   7
 
 
  331 !     and they can be excluded for source term and output if required.
  332 !     For convenience, Arctic cellls are all base level cells and are
  333 !     appended to the end of the global cells.  If refined cells were
  334 !     used in the Arctic part, it would not be kept all together, making
  335 !     the sub-loops much more complicated. If refined resolution cells
  336 !     are required for a Arctic regional model, users may consider use
  337 !     the rotated SMC grid options (RTD and SMC).
  338 !
  339 !     For more information about the SMC grid, please refer to
  340 !     Li, J.G. (2012) Propagation of Ocean Surface Waves on a Spherical
  341 !     Multiple-Cell Grid.  J. Comput. Phys., 231, 8262-8277.  online at
  342 !     http://dx.doi.org/10.1016/j.jcp.2012.08.007
  343 !
  344 ! (End SMC description)
  345 !
  346 !     ICEWIND is the scale factor for reduction of wind input by ice
  347 !     concentration. Value specified corresponds to the fractional
  348 !     input for 100% ice concentration. Default is 1.0, meaning that
  349 !     100% ice concentration result in zero wind input.
  350 !     Sin_in_ice=Sin_in_open_water * (1-ICE*ICEWIND)
  351                                                     
  352 !     -----------------------------------------------------------------*
  353 !  8. Structure :
  354 !
  355 !     ----------------------------------------------------------------
  356 !        1.   Set up grid storage structure.
  357 !                               ( W3NMOD , W3NOUT , W3SETG , W3SETO )
  358 !        2.a  I-O setup.
  359 !          b  Print heading(s).
  360 !        3.   Prepare int. table for dispersion relation   ( DISTAB )
  361 !        4.   Read and process input file up to spectrum.
  362 !          a  Get comment character
  363 !          b  Name of grid
  364 !          c  Define spectrum                              ( W3DIMS )
  365 !        5.   Set-up discrete spectrum.
  366 !          a  Directions.
  367 !          b  Frequency for spectrum.
  368 !        6.   Read and process input file up to numerical parameters
  369 !          a  Set model flags and time steps
  370 !          b  Set / select source term package
  371 !          c  Pre-process namelists.
  372 !          d  Wind input source term.
  373 !          e  Nonlinear interactions.
  374 !          f  Whitecapping term.
  375 !          g  Bottom friction source term.
  376 !          h  Depth indiced breaking source term.
  377 !          i  Triad interaction source term.
  378 !          j  Bottom scattering source term.
  379 !          k  Undefined source term.
  380 !          l  Set / select propagaton scheme
  381 !          m  Parameters for propagation scheme.
  382 !          n  Set misc. parameters (ice, seeding, ...)
  383 !          o  End of namelist processing
  384 !          p  Set various other variables
  385 !        7.   Read and prepare grid.
 
 
                                                         ww3_grid.F90  page   8
 
 
  386 !          a  Layout of grid
  387 !          b  Storage of grid of grid
  388 !          c  Read bottom depths
  389 !          d  Set up temp map
  390 !          e  Subgrid information
  391 !            1 Info from input file
  392 !            2 Open file and check if necessary
  393 !            3 Read the data
  394 !            4 Limit
  395 !        8    Finalize status maps
  396 !          a  Determine where to get the data
  397 !             Get data in parts from input file
  398 !             ----------------------------------------------------
  399 !          b  Read and update TMPSTA with bound. and excl. points.
  400 !          c  Finalize excluded points
  401 !             ----------------------------------------------------
  402 !             Read data from file
  403 !             ----------------------------------------------------
  404 !          d  Read data from file
  405 !             ----------------------------------------------------
  406 !          e  Get NSEA and other counters
  407 !          f  Set up all maps                              ( W3DIMX )
  408 !        9.   Prepare output boundary points.
  409 !          a  Read
  410 !          b  Update
  411 !       10.   Write model definition file.                 ( W3IOGR )
  412 !     ----------------------------------------------------------------
  413 !
  414 !  9. Switches :
  415 !
  416 !     !/FLX1  Stresses according to Wu (1980).
  417 !     !/FLX2  Stresses according to T&C (1996).
  418 !     !/FLX3  Stresses according to T&C (1996) with cap on Cd.
  419 !     !/FLX4  Stresses according to Hwang (2011).
  420 !
  421 !     !/LN0   No linear input source term.
  422 !     !/SEED  'Seeding' of lowest frequency for sufficiently strong
  423 !             winds. Proxi for linear input.
  424 !     !/LN1   Cavaleri and Melanotte-Rizzoli with Tolman filter.
  425 !     !/LNX   Open slot.
  426 !
  427 !     !/ST0   No source terms included (input/dissipation)
  428 !     !/ST1   WAM-3 physics package.
  429 !     !/ST2   Tolman and Chalikov (1996) physics package.
  430 !     !/ST3   WAM 4+ source terms from P.A.E.M. Janssen and J-R. Bidlot
  431 !     !/ST4   Input and dissipation using saturation following Ardhuin et>
  432 !             Filipot & Ardhuin (2010) or Romero (2019)
  433 !     !/ST6   BYDRZ source term package featuring Donelan et al.
  434 !             (2006) input and Babanin et al. (2001,2010) dissipation.
  435 !     !/STX   Open slot.
  436 !
  437 !     !/NL0   No nonlinear interactions.
  438 !     !/NL1   Discrete interaction approximation (DIA).
  439 !     !/NL2   Exact interactions (WRT).
  440 !     !/NL3   Generalized Multiple DIA (GMD).
 
 
                                                         ww3_grid.F90  page   9
 
 
  441 !     !/NL4   Two Scale Approximation
  442 !     !/NLX   Open slot.
  443 !     !/NLS   Snl based HF filter.
  444 !
  445 !     !/BT0   No bottom friction included.
  446 !     !/BT1   JONSWAP bottom friction package.
  447 !     !/BT4   SHOWEX bottom friction using movable bed roughness
  448 !                  (Tolman 1994, Ardhuin & al. 2003)
  449 !     !/BTX   Open slot.
  450 !
  451 !     !/IC1   Sink term for interaction with ice (uniform k_i)
  452 !     !/IC2   Sink term for under-ice boundary layer friction
  453 !                  (Liu et al.    1991: JGR 96 (C3), 4605-4621)
  454 !                  (Liu and Mollo 1988: JPO 18       1720-1712)
  455 !     !/IC3   Sink term for interaction with ice (Wang and Shen method)
  456 !                  (Wang and Shen JGR 2010)
  457 !     !/IC4   Sink term for empirical, frequency-dependent attenuation
  458 !                   in ice (Wadhams et al. 1988: JGR 93 (C6) 6799-6818)
  459 !     !/IC5   Sink term for interaction with ice (Mosig et al. method)
  460 !                  (Mosig et al. 2015: JGR)
  461 !
  462 !     !/UOST  Unresolved Obstacles Source Term (UOST), Mentaschi et al. 2>
  463 !
  464 !     !/DB0   No depth-induced breaking included.
  465 !     !/DB1   Battjes-Janssen depth-limited breaking.
  466 !     !/DBX   Open slot.
  467 !     !/MLIM  Mich-style limiter.
  468 !
  469 !     !/TR0   No triad interactions included.
  470 !     !/TRX   Open slot.
  471 !
  472 !     !/BS0   No bottom scattering included.
  473 !     !/BS1   Routines from F. Ardhuin.
  474 !     !/BSX   Open slot.
  475 !
  476 !     !/XX0   No unclasified source term included.
  477 !     !/XXX   Open slot.
  478 !
  479 !     !/PR1   First order propagation scheme.
  480 !     !/PR2   QUICKEST scheme with ULTIMATE limite and diffusion
  481 !             correction for swell dispersion.
  482 !     !/PR3   Averaging ULTIMATE QUICKEST scheme.
  483 !
  484 !     !/RTD   Rotated regular lat-lon grid. Special case is standard Pola>
  485 !     !/SMC   UNO2 scheme on Spherical Multiple-Cell grid.
  486 !     !/ARC   Append the Arctic part to the SMC grid.
  487 !
  488 !     !/MGG   GSE correction for moving grid.
  489 !
  490 !     !/S     Enable subroutine tracing.
  491 !     !/T     Enable test output.
  492 !     !/T0    Enable test output tables for boundary output.
  493 !
  494 !     !/O0    Print equivalent namelist setting to std out.
  495 !     !/O1    Print tables with boundary points as part of output.
 
 
                                                         ww3_grid.F90  page  10
 
 
  496 !     !/O2    Print MAPSTA as part of output.
  497 !     !/O2a   Print land-sea mask in mask.ww3.
  498 !     !/O2b   Print obstruction data.
  499 !     !/O2c   Print extended status map.
  500 !
  501 ! 10. Source code :
  502 !
  503 !/ ------------------------------------------------------------------- /
  504       USE CONSTANTS
  505 !/
  506       USE W3TRIAMD
  507       USE W3GSRUMD, ONLY: W3GRMP
  508       USE W3ODATMD, ONLY: W3NOUT, W3SETO, W3DMO5
  509       USE W3IOGRMD, ONLY: W3IOGR
  510       USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  511       USE W3ARRYMD, ONLY: INA2R, INA2I
  512       USE W3DISPMD, ONLY: DISTAB
  513 !/
  514       USE W3GDATMD
  515       USE W3ODATMD, ONLY: NDSE, NDST, NDSO
  516       USE W3ODATMD, ONLY: NBI, NBI2, NFBPO, NBO, NBO2, FLBPI, FLBPO,  &
  517                           IPBPO, ISBPO, XBPO, YBPO, RDBPO, FNMPRE,    &
  518                           IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB,       &
  519                           NOSWLL, PTMETH, PTFCUT
  520       USE W3TIMEMD, ONLY: CALTYPE
  521       USE W3NMLGRIDMD
  522 !
  523       IMPLICIT NONE
  524 !/
  525 !/ ------------------------------------------------------------------- /
  526 !/ Local parameters
  527 !/
  528       TYPE(NML_SPECTRUM_T)     :: NML_SPECTRUM
  529       TYPE(NML_RUN_T)          :: NML_RUN
  530       TYPE(NML_TIMESTEPS_T)    :: NML_TIMESTEPS
  531       TYPE(NML_GRID_T)         :: NML_GRID
  532       TYPE(NML_RECT_T)         :: NML_RECT
  533       TYPE(NML_CURV_T)         :: NML_CURV
  534       TYPE(NML_UNST_T)         :: NML_UNST
  535       TYPE(NML_SMC_T)          :: NML_SMC
  536       TYPE(NML_DEPTH_T)        :: NML_DEPTH
  537       TYPE(NML_MASK_T)         :: NML_MASK
  538       TYPE(NML_OBST_T)         :: NML_OBST
  539       TYPE(NML_SLOPE_T)        :: NML_SLOPE
  540       TYPE(NML_SED_T)          :: NML_SED
  541       TYPE(NML_INBND_COUNT_T)  :: NML_INBND_COUNT
  542       TYPE(NML_INBND_POINT_T), ALLOCATABLE  :: NML_INBND_POINT(:)
  543       TYPE(NML_EXCL_COUNT_T)   :: NML_EXCL_COUNT
  544       TYPE(NML_EXCL_POINT_T), ALLOCATABLE   :: NML_EXCL_POINT(:)
  545       TYPE(NML_EXCL_BODY_T), ALLOCATABLE    :: NML_EXCL_BODY(:)
  546       TYPE(NML_OUTBND_COUNT_T) :: NML_OUTBND_COUNT
  547       TYPE(NML_OUTBND_LINE_T), ALLOCATABLE  :: NML_OUTBND_LINE(:)
  548 !
  549       INTEGER, PARAMETER      :: NFL = 6
  550       INTEGER                 :: NDSI, NDSI2, NDSS, NDSM, NDSG, NDSTR,&
 
 
                                                         ww3_grid.F90  page  11
 
 
  551                                  IERR, NDSTRC, NTRACE, ITH, IK, ITH0, &
  552                                  ISP, IYN(NFL), NRLIN, NRSRCE, NRNL,  &
  553                                  NRBT, NRDB, NRTR, NRBS, NRXX, NRPROP,&
  554                                  IDLA, IDFM, IX0, IXN, IX, IY, ISEA,  &
  555                                  IDX, IXO, IDY, IYO, IBA, NBA, ILOOP, &
  556                                  IFL, NBOTOT, NPO, IP, IX1, IX2, IY1, &
  557                                  IY2, J, JJ, IXR(4), IYR(4), ISEAI(4),&
  558                                  IST, NKI, NTHI, NRIC, NRIS, I, IDFT, &
  559                                  NSTAT, NBT, NLAND, NOSW, NMAPB, IMAPB
  560       INTEGER             :: IBI, IP0, IPN, IPH, IPI
  561       INTEGER                 :: NCOL =  78
  562 !
  563       INTEGER                 :: NMAP, IMAP
  564 !
  565       INTEGER, ALLOCATABLE    :: TMPSTA(:,:), TMPMAP(:,:), READMP(:,:)
  566 !
  567       REAL                    :: RXFR, RFR1, SIGMA, SXFR, FACHF,      &
  568                                  VSC, VSC0, VOF,                      &
  569                                  ZLIM, X, Y, XP,  XO0, YO0, DXO, DYO, &
  570                                  XO, YO, RD(4), RDTOT,                &
  571                                  FACTOR, RTH0, FMICHE, RWNDC,         &
  572                                  WCOR1, WCOR2
  573 !
  574       CHARACTER(LEN=4)        :: GSTRG, CSTRG
  575 !
  576 ! Variables used to allow spectral output on full grid
  577 !
  578       INTEGER                 :: P2SF,I1P2SF,I2P2SF
  579       INTEGER                 :: E3D,I1E3D,I2E3D
  580       INTEGER                 :: US3D,I1US3D,I2US3D,                  &
  581                                  USSP, IUSSP,                         &
  582                                  TH1MF, I1TH1M, I2TH1M,               &
  583                                  STH1MF, I1STH1M, I2STH1M,            &
  584                                  TH2MF, I1TH2M, I2TH2M,               &
  585                                  STH2MF, I1STH2M, I2STH2M
  586       ! STK_WN are the decays for Stokes drift partitions
  587       REAL                    :: STK_WN(25)
  588                                            
  589 !
  590       REAL                    :: CLIN, RFPM, RFHF
  591       REAL                    :: NLPROP
  592 !
  593       REAL, ALLOCATABLE       :: XGRDIN(:,:), YGRDIN(:,:)
  594       REAL, ALLOCATABLE       :: ZBIN(:,:), OBSX(:,:), OBSY(:,:)
  595       REAL, ALLOCATABLE       :: REFD(:,:), REFD2(:,:), REFS(:,:)
  596 !
  597       LOGICAL                 :: FLLIN, FLINDS, FLNL, FLBT, FLDB,     &
  598                                  FLTR, FLBS, FLXX, FLPROP, FLREF,     &
  599                                  FIRST, CONNCT, FLNEW, INGRID,FLIC,   &
  600                                  FLIS, FLGNML
  601       LOGICAL                 :: FLTC96 = .FALSE.
  602       LOGICAL                 :: FLNMLO = .FALSE.
  603       LOGICAL                 :: FLSTB2 = .FALSE.
  604       LOGICAL                 :: FLST4  = .FALSE.
  605       LOGICAL                 :: FLST6  = .FALSE.
 
 
                                                         ww3_grid.F90  page  12
 
 
  606                                                  
  607 !!Li  Add a logical variable to shelter regular grid lines from SMC grid.
  608       LOGICAL                 :: RGLGRD = .TRUE.
  609 !!Li
  610       REAL                    :: FACBERG, REFSLOPE
  611 !
  612       REAL                    :: REFCOAST, REFFREQ, REFMAP,     &
  613                                  REFSUBGRID, REFRMAX, REFMAPD,  &
  614                                  REFICEBERG, REFCOSP_STRAIGHT,  &
  615                                  REFFREQPOW, REFUNSTSOURCE
  616 !
  617   LOGICAL                 :: IGSWELLMAX, IGBCOVERWRITE
  618   INTEGER                 :: IGMETHOD, IGADDOUTP, IGSOURCE,      &
  619                              IGSOURCEATBP, IGSTERMS
  620   REAL                    :: IGMAXFREQ, IGMINDEP, IGMAXDEP,      &
  621                              IGKDMIN, IGFIXEDDEPTH, IGEMPIRICAL
  622 !
  623  
  624  
  625 !
  626  
  627       CHARACTER               :: COMSTR*1, PNAME*30, RFORM*16,        &
  628                                  FROM*4, FNAME*60, TNAME*60, LINE*80, &
  629                                  STATUS*20,FNAME2*60, PNAME2*40
  630       CHARACTER(LEN=6)        :: YESXNO(2)
  631                                           
  632                                           
  633 !/ ------------------------------------------------------------------- /
  634 !/ Namelists
  635 !/
  636       INTEGER                 :: FLAGTR, IHM
  637       REAL                    :: CFLTM, CICE0, CICEN, PMOVE, XFILT,    &
  638                                  LICE, XSEED, XR, HSPM, WSM, WSC, STDX,&
  639                                  STDY, STDT, ICEHMIN, ICEHFAC, ICEHINIT, &
  640                                  ICESLN, ICEWIND, ICESNL, ICESDS,        &
  641                                  ICEHDISP, ICEFDISP, ICEDDISP, BTBET
  642 !
  643       REAL(8)                 :: GSHIFT ! see notes in WMGHGH
  644       LOGICAL                 :: FLC, ICEDISP, TRCKCMPR
  645       INTEGER                 :: PTM   ! Partitioning method
  646       REAL                    :: PTFC  ! Part. cut off freq (for method 5)
  647       REAL                    :: AIRCMIN, AIRGB
  648       CHARACTER               :: PMNAME*45, PMNAM2*45  ! Part. method des>
  649 !
  650       INTEGER                 :: SWELLFPAR, SDSISO, SDSBRFDF
  651       REAL 		   :: SDSBCHOICE
  652       REAL                    :: ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP,&
  653                                  ZALP, Z0RAT, TAUWSHELTER, SWELLF,    &
  654                                  SWELLF2,SWELLF3,SWELLF4, SWELLF5,    &
  655                                  SWELLF6, SWELLF7, FXPM3, FXFM3,      &
  656                                  WNMEANPTAIL, WNMEANP, STXFTF, STXFTFTAIL,
  657                                  STXFTWN, SINBR, FXFMAGE,             &
  658                                  SDSC2, SDSCUM, SDSC4, SDSC5, SDSC6, WHIT>
  659                                  SDSSTRAIN, SDSSTRAINA, SDSSTRAIN2,   &
  660                                  SDSBR, SDSP, SDSBT, SDS4A, SDKOF,    &
 
 
                                                         ww3_grid.F90  page  13
 
 
  661                                  SDSCOS, SDSDTH, SDSBCK, SDSABK,      &
  662                                  SDSPBK, SDSBINT, SDSHCK,             &
  663                                  SDSBRF1,                             &
  664                                  SDSBM0, SDSBM1, SDSBM2, SDSBM3,      &
  665                                  SDSBM4, SDSFACMTF, SDSCUMP,  SDSNUW, &
  666                                  SDSL, SDSMWD, SDSMWPOW, SPMSS, SDSNMTF
  667 !
  668       REAL                    :: LAMBDA, KDCONV, KDMIN,               &
  669                                  SNLCS1, SNLCS2, SNLCS3
  670 !
  671       REAL                    :: WDTHCG, WDTHTH
  672            LOGICAL :: JGS_TERMINATE_MAXITER = .TRUE.
  673            LOGICAL :: JGS_TERMINATE_DIFFERENCE = .TRUE.
  674            LOGICAL :: JGS_TERMINATE_NORM = .TRUE.
  675            LOGICAL :: JGS_LIMITER = .FALSE.
  676            LOGICAL :: JGS_BLOCK_GAUSS_SEIDEL = .TRUE.
  677            LOGICAL :: JGS_USE_JACOBI = .TRUE.
  678            LOGICAL :: JGS_SOURCE_NONLINEAR = .FALSE.
  679            LOGICAL :: UGOBCAUTO = .FALSE.
  680            LOGICAL :: UGBCCFL   = .FALSE.
  681            LOGICAL :: EXPFSN    = .TRUE.
  682            LOGICAL :: EXPFSPSI  = .FALSE.
  683            LOGICAL :: EXPFSFCT  = .FALSE.
  684            LOGICAL :: IMPFSN    = .FALSE.
  685            LOGICAL :: EXPTOTAL  = .FALSE.
  686            LOGICAL :: IMPTOTAL  = .FALSE.
  687            LOGICAL :: IMPREFRACTION = .FALSE.
  688            LOGICAL :: IMPFREQSHIFT = .FALSE.
  689            LOGICAL :: IMPSOURCE = .FALSE.
  690            LOGICAL :: SETUP_APPLY_WLV = .FALSE.
  691            INTEGER :: JGS_MAXITER=100
  692            INTEGER :: nbSel
  693            INTEGER :: UNSTSCHEMES(4)
  694            INTEGER :: UNSTSCHEME
  695            INTEGER :: JGS_NLEVEL = 0
  696            REAL*8  :: JGS_PMIN = 0.
  697            REAL*8  :: JGS_DIFF_THR = 1.E-10
  698            REAL*8  :: JGS_NORM_THR = 1.E-20
  699            REAL*8  :: SOLVERTHR_SETUP = 1.E-20
  700            REAL*8  :: CRIT_DEP_SETUP = 0.
  701 !
  702            CHARACTER               :: UGOBCFILE*60
  703            REAL                    :: UGOBCDEPTH
  704            LOGICAL                 :: UGOBCOK
  705                                              
  706 !
  707       NAMELIST /SIG1/  IGMETHOD, IGADDOUTP, IGSOURCE, IGBCOVERWRITE,   &
  708                       IGMAXFREQ, IGSTERMS, IGSWELLMAX,                &
  709                       IGSOURCEATBP, IGKDMIN, IGFIXEDDEPTH, IGEMPIRICAL
  710       NAMELIST /SLN1/ CLIN, RFPM, RFHF
  711       NAMELIST /SIN4/ ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP, &
  712                       TAUWSHELTER, SWELLFPAR, SWELLF,                 &
  713                       SWELLF2, SWELLF3, SWELLF4, SWELLF5, SWELLF6,    &
  714                       SWELLF7, Z0RAT, SINBR
  715       NAMELIST /SNL1/ LAMBDA, NLPROP, KDCONV, KDMIN,                  &
 
 
                                                         ww3_grid.F90  page  14
 
 
  716                       SNLCS1, SNLCS2, SNLCS3
  717       NAMELIST /SDS4/ SDSBCHOICE, WNMEANP, WNMEANPTAIL, FXPM3, FXFM3, &
  718                       FXFMAGE, SDSC2, SDSCUM, SDSSTRAIN, SDSSTRAINA,  &
  719                       SDSSTRAIN2, SDSC4, SDSFACMTF, SDSNMTF,SDSCUMP,  &
  720                       SDSC5, SDSC6, SDSBR, SDSBT, SDSP, SDSISO,       &
  721                       SDSBCK, SDSABK, SDSPBK, SDSBINT, SDSHCK,        &
  722                       SDSDTH, SDSCOS, SDSBRF1, SDSBRFDF,  SDSNUW,     &
  723                       SDSBM0, SDSBM1, SDSBM2, SDSBM3, SDSBM4,         &
  724                       WHITECAPWIDTH, WHITECAPDUR, SDSMWD, SDSMWPOW, SDKOF
  725                                                                          
  726 !
  727       NAMELIST /PRO3/ CFLTM, WDTHCG, WDTHTH
  728            NAMELIST /UNST/ UGOBCAUTO, UGOBCDEPTH, UGOBCFILE,          &
  729                            UGBCCFL, EXPFSN, EXPFSPSI, EXPFSFCT,       &
  730                            IMPFSN, IMPTOTAL, EXPTOTAL,                &
  731                            IMPREFRACTION, IMPFREQSHIFT,               &
  732                            IMPSOURCE,                                 &
  733                            JGS_TERMINATE_MAXITER,                     &
  734                            JGS_TERMINATE_DIFFERENCE,                  &
  735                            JGS_TERMINATE_NORM,                        &
  736                            JGS_LIMITER,                               &
  737                            JGS_USE_JACOBI,                            &
  738                            JGS_BLOCK_GAUSS_SEIDEL,                    &
  739                            JGS_MAXITER,                               &
  740                            JGS_PMIN,                                  &
  741                            JGS_DIFF_THR,                              &
  742                            JGS_NORM_THR,                              &
  743                            JGS_NLEVEL,                                &
  744                            JGS_SOURCE_NONLINEAR,                      &
  745                            SETUP_APPLY_WLV, SOLVERTHR_SETUP,          &
  746                            CRIT_DEP_SETUP
  747            NAMELIST /MISC/ CICE0, CICEN, LICE, XSEED, FLAGTR, XP, XR, &
  748                       XFILT, PMOVE, IHM, HSPM, WSM, WSC, FLC, FMICHE, &
  749                       RWNDC, FACBERG, NOSW, GSHIFT, WCOR1, WCOR2,     &
  750                       STDX, STDY, STDT, ICEHMIN, ICEHINIT, ICEDISP,   &
  751                       ICESLN, ICEWIND, ICESNL, ICESDS, ICEHFAC,       &
  752                       ICEHDISP, ICEDDISP, ICEFDISP, CALTYPE,          &
  753                       TRCKCMPR, PTM, PTFC, BTBET
  754            NAMELIST /OUTS/ P2SF, I1P2SF, I2P2SF,                      &
  755                              US3D, I1US3D, I2US3D,                    &
  756                              USSP, IUSSP, STK_WN,                     &
  757                              E3D, I1E3D, I2E3D,                       &
  758                              TH1MF, I1TH1M, I2TH1M,                   &
  759                              STH1MF, I1STH1M, I2STH1M,                &
  760                              TH2MF, I1TH2M, I2TH2M,                   &
  761                              STH2MF, I1STH2M, I2STH2M
  762            NAMELIST /REF1/ REFCOAST, REFFREQ, REFMAP,  REFMAPD,       &
  763                           REFSUBGRID, REFICEBERG,                     &
  764                           REFCOSP_STRAIGHT, REFSLOPE, REFRMAX,        &
  765                           REFFREQPOW, REFUNSTSOURCE
  766 !/
  767 !/
  768 !/ ------------------------------------------------------------------- /
  769 !/
  770       DATA YESXNO / 'YES/--' , '---/NO' /
 
 
                                                         ww3_grid.F90  page  15
 
 
  771       FLNMLO = .TRUE.
  772 !
  773 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  774 ! 1.  Set up grid storage structure
  775 !
  776       CALL W3NMOD ( 1, 6, 6 )
  777       CALL W3SETG ( 1, 6, 6 )
  778       CALL W3NOUT (    6, 6 )
  779       CALL W3SETO ( 1, 6, 6 )
  780 !
  781 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  782 ! 2.  IO set-up.
  783 !
  784       NDSI   = 10
  785       NDSS   = 99
  786       NDSM   = 20
  787 !
  788       INQUIRE(FILE=TRIM(FNMPRE)//"ww3_grid.nml", EXIST=FLGNML)
  789       IF (FLGNML) THEN
  790          ! Read namelist
  791          CALL W3NMLGRID (NDSI, TRIM(FNMPRE)//'ww3_grid.nml', NML_SPECTRUM>
  792                          NML_TIMESTEPS, NML_GRID, NML_RECT, NML_CURV,   &
  793                          NML_UNST, NML_SMC, NML_DEPTH, NML_MASK,        &
  794                          NML_OBST, NML_SLOPE, NML_SED, NML_INBND_COUNT, &
  795                          NML_INBND_POINT, NML_EXCL_COUNT,               &
  796                          NML_EXCL_POINT, NML_EXCL_BODY,                 &
  797                          NML_OUTBND_COUNT, NML_OUTBND_LINE, IERR)
  798       ELSE
  799         OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_grid.inp',STATUS='OLD',       >
  800               ERR=2000,IOSTAT=IERR)
  801       END IF
  802 !
  803       NDSTRC =  6
  804       NTRACE =  10
  805       CALL ITRACE ( NDSTRC, NTRACE )
  806 !
  807       WRITE (NDSO,900)
  808 !
  809 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  810 ! 3.a Interpolation table for dispersion relation.
  811 !
  812       CALL DISTAB
  813 !
  814 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  815 ! 3.b Table for friction factors
  816 !
  817       CALL TABU_FW
  818 !
  819 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  820 ! 4   Read and process input file up to spectrum
  821 !
  822  
  823       IF (FLGNML) THEN
  824          ! grid name
  825          GNAME=TRIM(NML_GRID%NAME)
 
 
                                                         ww3_grid.F90  page  16
 
 
  826          WRITE (NDSO,902) GNAME
  827                                
  828          ! spectrum parameters
  829          RXFR=NML_SPECTRUM%XFR
  830          RFR1=NML_SPECTRUM%FREQ1
  831          NKI=NML_SPECTRUM%NK
  832          NTHI=NML_SPECTRUM%NTH
  833          RTH0=NML_SPECTRUM%THOFF
  834                                 
  835       ELSE
  836           
  837         READ (NDSI,'(A)',END=2001,ERR=2002,IOSTAT=IERR) COMSTR
  838         IF (COMSTR.EQ.' ') COMSTR = '$'
  839         WRITE (NDSO,901) COMSTR
  840         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  841 !
  842         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  843         READ (NDSI,*,END=2001,ERR=2002) GNAME
  844         WRITE (NDSO,902) GNAME
  845 !
  846         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  847         READ (NDSI,*,END=2001,ERR=2002) RXFR, RFR1, NKI, NTHI, RTH0
  848       END IF
  849             
  850             
  851       NK     = NKI
  852       NK2    = NKI + 2
  853       NTH    = NTHI
  854       NSPEC  = NK * NTH
  855       XFR    = MAX ( RXFR , 1.00001 )
  856       FR1    = MAX ( RFR1 , 1.E-6 )
  857       DTH    = TPI / REAL(NTH)
  858       RTH0   = MAX ( -0.5 , MIN ( 0.5 , RTH0 ) )
  859       WRITE (NDSO,903) NTH, DTH*RADE
  860       WRITE (NDSO,904) 360./REAL(NTH)*RTH0
  861       WRITE (NDSO,905) NK, FR1, FR1*XFR**(NK-1), XFR
  862 !
  863       CALL W3DIMS ( 1, NK, NTH, NDSE, NDST )
  864 !
  865 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  866 ! 5.  Initialize spectral parameters.
  867 ! 5.a Directions :
  868 !
  869       DO ITH=1, NTH
  870         TH  (ITH) = DTH * ( RTH0 + REAL(ITH-1) )
  871         ESIN(ITH) = SIN ( TH(ITH) )
  872         ECOS(ITH) = COS ( TH(ITH) )
  873         IF ( ABS(ESIN(ITH)) .LT. 1.E-5 ) THEN
  874           ESIN(ITH) = 0.
  875           IF ( ECOS(ITH) .GT. 0.5 ) THEN
  876             ECOS(ITH) =  1.
  877           ELSE
  878             ECOS(ITH) = -1.
  879             END IF
  880           END IF
 
 
                                                         ww3_grid.F90  page  17
 
 
  881         IF ( ABS(ECOS(ITH)) .LT. 1.E-5 ) THEN
  882           ECOS(ITH) = 0.
  883           IF ( ESIN(ITH) .GT. 0.5 ) THEN
  884             ESIN(ITH) =  1.
  885           ELSE
  886             ESIN(ITH) = -1.
  887             END IF
  888           END IF
  889         ES2 (ITH) = ESIN(ITH)**2
  890         EC2 (ITH) = ECOS(ITH)**2
  891         ESC (ITH) = ESIN(ITH)*ECOS(ITH)
  892         END DO
  893 !
  894       DO IK=2, NK+1
  895         ITH0   = (IK-1)*NTH
  896         DO ITH=1, NTH
  897           ESIN(ITH0+ITH) = ESIN(ITH)
  898           ECOS(ITH0+ITH) = ECOS(ITH)
  899           ES2 (ITH0+ITH) = ES2 (ITH)
  900           EC2 (ITH0+ITH) = EC2 (ITH)
  901           ESC (ITH0+ITH) = ESC (ITH)
  902           END DO
  903         END DO
  904 !
  905 !   b Frequencies :
  906 !
  907       SIGMA   = FR1 * TPI / XFR**2
  908       SXFR    = 0.5 * (XFR-1./XFR)
  909 !
  910       DO IK=0, NK+1
  911         SIGMA    = SIGMA * XFR
  912         SIG (IK) = SIGMA
  913         DSIP(IK) = SIGMA * SXFR
  914         END DO
  915 !
  916       DSII( 1) = 0.5 * SIG( 1) * (XFR-1.)
  917       DO IK=2, NK-1
  918         DSII(IK) = DSIP(IK)
  919         END DO
  920       DSII(NK) = 0.5 * SIG(NK) * (XFR-1.) / XFR
  921 !
  922       DO IK=1, NK
  923         DDEN(IK) = DTH * DSII(IK) * SIG(IK)
  924         END DO
  925 !
  926       DO ISP=1, NSPEC
  927         IK         = 1 + (ISP-1)/NTH
  928         SIG2 (ISP) = SIG (IK)
  929         DDEN2(ISP) = DDEN(IK)
  930         END DO
  931 !
  932 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  933 ! 6   Read and process input file up to numerical parameters
  934 ! 6.a Set model flags and time steps
  935 !
 
 
                                                         ww3_grid.F90  page  18
 
 
  936       WRITE (NDSO,910)
  937       IF (FLGNML) THEN
  938         FLDRY=NML_RUN%FLDRY
  939         FLCX=NML_RUN%FLCX
  940         FLCY=NML_RUN%FLCY
  941         FLCTH=NML_RUN%FLCTH
  942         FLCK=NML_RUN%FLCK
  943         FLSOU=NML_RUN%FLSOU
  944       ELSE
  945         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  946         READ (NDSI,*,END=2001,ERR=2002)                                 &
  947           FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU
  948       END IF
  949 !
  950       IYN = 2
  951       IF ( FLDRY ) IYN(1) = 1
  952       IF ( FLCX  ) IYN(2) = 1
  953       IF ( FLCY  ) IYN(3) = 1
  954       IF ( FLCTH ) IYN(4) = 1
  955       IF ( FLCK  ) IYN(5) = 1
  956       IF ( FLSOU ) IYN(6) = 1
  957 !
  958       WRITE (NDSO,911) (YESXNO(IYN(IFL)),IFL=1,NFL)
  959 !
  960       IF ( .NOT. (FLDRY.OR.FLCX.OR.FLCY.OR.FLCK.OR.FLCTH.OR.FLSOU) ) THEN
  961           WRITE (NDSE,1010)
  962           CALL EXTCDE ( 2 )
  963         END IF
  964 !
  965       IF (FLGNML) THEN
  966         DTMAX=NML_TIMESTEPS%DTMAX
  967         DTCFL=NML_TIMESTEPS%DTXY
  968         DTCFLI=NML_TIMESTEPS%DTKTH
  969         DTMIN=NML_TIMESTEPS%DTMIN
  970       ELSE
  971         CALL NEXTLN ( COMSTR , NDSI , NDSE )
  972         READ (NDSI,*,END=2001,ERR=2002) DTMAX, DTCFL, DTCFLI, DTMIN
  973       END IF
  974             
  975       DTMAX  = MAX ( 1. , DTMAX )
  976 !
  977 ! Commented to allow very high resolution zooms
  978 !
  979 !      DTCFL  = MAX ( 1. , DTCFL  )
  980 !      DTCFLI = MIN ( DTMAX , MAX ( 1. , DTCFLI ) )
  981       DTMIN  = MIN ( DTMAX , MAX ( 0. , DTMIN  ) )
  982       WRITE (NDSO,912) DTMAX, DTCFL, DTCFLI, DTMIN
  983 !
  984 ! 6.b Set / select source term package
  985 !
  986       NRLIN  = 0
  987       NRSRCE = 0
  988       NRNL   = 0
  989       NRBT   = 0
  990       NRIC   = 0
 
 
                                                         ww3_grid.F90  page  19
 
 
  991       NRIS   = 0
  992       NRDB   = 0
  993       NRTR   = 0
  994       NRBS   = 0
  995       NRXX   = 0
  996 !
  997       FLLIN  = .TRUE.
  998       FLINDS = .TRUE.
  999       FLNL   = .TRUE.
 1000       FLBT   = .TRUE.
 1001       FLIC   = .FALSE.
 1002       FLIS   = .FALSE.
 1003       FLDB   = .TRUE.
 1004       FLTR   = .TRUE.
 1005       FLBS   = .TRUE.
 1006       FLREF  = .FALSE.
 1007       FLXX   = .TRUE.
 1008 !
 1009       NRLIN  = NRLIN + 1
 1010 !
 1011       NRSRCE = NRSRCE + 1
 1012       FLST4  = .TRUE.
 1013 !
 1014       NRNL   = NRNL + 1
 1015 !
 1016       NRBT   = NRBT + 1
 1017       FLBT   = .FALSE.
 1018 !
 1019       NRDB   = NRDB + 1
 1020       FLDB   = .FALSE.
 1021 !
 1022       NRTR   = NRTR + 1
 1023       FLTR   = .FALSE.
 1024 !
 1025       NRBS   = NRBS + 1
 1026       FLBS   = .FALSE.
 1027 !
 1028       FLREF   = .TRUE.
 1029 !
 1030       NRXX   = NRXX + 1
 1031       FLXX   = .FALSE.
 1032 !
 1033       IF ( .NOT.FLLIN .AND.  .NOT.FLINDS .AND.  .NOT.FLNL .AND.        &
 1034            .NOT.FLBT  .AND.  .NOT.FLIC   .AND.  .NOT.FLIS .AND.        &
 1035            .NOT.FLDB  .AND.  .NOT.FLTR   .AND.  .NOT.FLBS .AND.        &
 1036            .NOT.FLXX  .AND.  .NOT.FLREF  .AND.  FLSOU ) THEN
 1037           WRITE (NDSE,1020)
 1038           CALL EXTCDE ( 10 )
 1039         END IF
 1040 !
 1041       IF ( ( FLLIN .OR. FLINDS .OR. FLNL .OR. FLBT .OR. FLDB .OR.     &
 1042              FLTR .OR. FLBS .OR. FLREF .OR. FLXX .OR. FLIC )          &
 1043              .AND. .NOT.FLSOU ) THEN
 1044           WRITE (NDSE,1021)
 1045         END IF
 
 
                                                         ww3_grid.F90  page  20
 
 
 1046 !
 1047       IF ( NRLIN .NE. 1 ) THEN
 1048           WRITE (NDSE,1022) NRLIN
 1049           CALL EXTCDE ( 11 )
 1050         END IF
 1051 !
 1052       IF ( NRSRCE .NE. 1 ) THEN
 1053           WRITE (NDSE,1023) NRSRCE
 1054           CALL EXTCDE ( 12 )
 1055         END IF
 1056 !
 1057       IF ( NRNL .NE. 1 ) THEN
 1058           WRITE (NDSE,1024) NRNL
 1059           CALL EXTCDE ( 13 )
 1060         END IF
 1061 !
 1062       IF ( NRBT .NE. 1 ) THEN
 1063           WRITE (NDSE,1025) NRBT
 1064           CALL EXTCDE ( 14 )
 1065         END IF
 1066 !
 1067       IF ( NRDB .NE. 1 ) THEN
 1068           WRITE (NDSE,1026) NRDB
 1069           CALL EXTCDE ( 15 )
 1070         END IF
 1071 !
 1072       IF ( NRTR .NE. 1 ) THEN
 1073           WRITE (NDSE,1027) NRTR
 1074           CALL EXTCDE ( 16 )
 1075         END IF
 1076 !
 1077       IF ( NRBS .NE. 1 ) THEN
 1078           WRITE (NDSE,1028) NRBS
 1079           CALL EXTCDE ( 17 )
 1080         END IF
 1081 !
 1082       IF ( NRXX .NE. 1 ) THEN
 1083           WRITE (NDSE,1029) NRXX
 1084           CALL EXTCDE ( 18 )
 1085         END IF
 1086 !
 1087       IF ( NRIC .GT. 1 ) THEN
 1088           WRITE (NDSE,1034) NRIC
 1089           CALL EXTCDE ( 19 )
 1090         END IF
 1091 !
 1092       IF ( NRIS .GT. 1 ) THEN
 1093           WRITE (NDSE,1036) NRIS
 1094           CALL EXTCDE ( 26 )
 1095         END IF
 1096               
 1097               
 1098 !
 1099 ! 6.c Read namelist file or Pre-process namelists into scratch file
 1100 !
 
 
                                                         ww3_grid.F90  page  21
 
 
 1101       WRITE (NDSO,915)
 1102       IF (FLGNML) THEN
 1103         OPEN (NDSS,FILE=TRIM(FNMPRE)//TRIM(NML_GRID%NML),STATUS='OLD',FOR>
 1104       ELSE
 1105         OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_grid.scratch',FORM='FORMATTED')
 1106         DO
 1107           CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1108           READ (NDSI,'(A)',END=2001,ERR=2002) LINE
 1109           IF ( LINE(1:16) .EQ. 'END OF NAMELISTS' ) THEN
 1110             EXIT
 1111           ELSE
 1112             WRITE (NDSS,'(A)') LINE
 1113           ENDIF
 1114         END DO
 1115       END IF
 1116       WRITE (NDSO,916)
 1117 !
 1118 ! 6.d Define Sin.
 1119 ! 6.d.1 Stresses
 1120 !
 1121 ! 6.d.2 Linear input
 1122 !
 1123       CLIN   = 80.
 1124       RFPM   =  1.
 1125       RFHF   =  0.5
 1126 !
 1127       CALL READNL ( NDSS, 'SLN1', STATUS )
 1128       WRITE (NDSO,820) STATUS
 1129       CLIN   = MAX (0.,CLIN)
 1130       RFPM   = MAX (0.,RFPM)
 1131       RFHF   = MAX(0.,MIN (1.,RFHF))
 1132       WRITE (NDSO,821) CLIN, RFPM, RFHF
 1133       SLNC1  = CLIN * (DAIR/DWAT)**2 / GRAV**2
 1134       FSPM   = RFPM
 1135       FSHF   = RFHF
 1136 !
 1137 ! 6.d.3 Exponential input
 1138 !
 1139       ZWND   =   10.
 1140       ALPHA0 = 0.0095
 1141       Z0MAX = 0.0
 1142       Z0RAT = 0.04
 1143       BETAMAX   = 1.43
 1144       SINTHP    = 2.
 1145       SWELLF    = 0.66
 1146       SWELLFPAR = 1
 1147       SWELLF2 = -0.018
 1148       SWELLF3 = 0.022
 1149       SWELLF4 = 1.5E5
 1150       SWELLF5 = 1.2
 1151       SWELLF6 = 0.
 1152       SWELLF7 = 360000.
 1153       TAUWSHELTER = 0.3
 1154       ZALP   = 0.006
 1155       SINBR   = 0.
 
 
                                                         ww3_grid.F90  page  22
 
 
 1156 !
 1157       CALL READNL ( NDSS, 'SIN4', STATUS )
 1158       WRITE (NDSO,920) STATUS
 1159       WRITE (NDSO,921) ALPHA0, BETAMAX, SINTHP, Z0MAX, ZALP, ZWND, TAUWSH>
 1160            SWELLFPAR, SWELLF, SWELLF2, SWELLF3, SWELLF4, SWELLF5, &
 1161            SWELLF6, SWELLF7, Z0RAT
 1162       ZZWND  = ZWND
 1163       AALPHA = ALPHA0
 1164       BBETA  = BETAMAX
 1165       SSINBR  = SINBR
 1166       SSINTHP  = SINTHP
 1167       ZZ0MAX  = Z0MAX
 1168       ZZ0RAT  = Z0RAT
 1169       ZZALP  = ZALP
 1170       TTAUWSHELTER = TAUWSHELTER
 1171       SSWELLF(1) = SWELLF
 1172       SSWELLF(2) = SWELLF2
 1173       SSWELLF(3) = SWELLF3
 1174       SSWELLF(4) = SWELLF4
 1175       SSWELLF(5) = SWELLF5
 1176       SSWELLF(6) = SWELLF6
 1177       SSWELLF(7) = SWELLF7
 1178       SSWELLFPAR = SWELLFPAR
 1179 !
 1180 ! 6.e Define Snl.
 1181 !
 1182       LAMBDA =  0.25
 1183       IF ( FLTC96 ) THEN
 1184           NLPROP =  1.00E7
 1185         ELSE IF ( FLST4 ) THEN
 1186           NLPROP =  2.50E7
 1187         ELSE IF ( FLST6 ) THEN
 1188           NLPROP =  3.00E7
 1189         ELSE
 1190           NLPROP =  2.78E7
 1191         END IF
 1192 !
 1193       KDCONV =  0.75
 1194       KDMIN  =  0.50
 1195       SNLCS1 =  5.5
 1196       SNLCS2 =  0.833
 1197       SNLCS3 = -1.25
 1198 !
 1199       CALL READNL ( NDSS, 'SNL1', STATUS )
 1200       WRITE (NDSO,922) STATUS
 1201       WRITE (NDSO,923) LAMBDA, NLPROP, KDCONV, KDMIN,            &
 1202                        SNLCS1, SNLCS2, SNLCS3
 1203       SNLC1  = NLPROP / GRAV**4
 1204       LAM    = LAMBDA
 1205       KDCON  = KDCONV
 1206       KDMN   = KDMIN
 1207       SNLS1  = SNLCS1
 1208       SNLS2  = SNLCS2
 1209       SNLS3  = SNLCS3
 1210 !
 
 
                                                         ww3_grid.F90  page  23
 
 
 1211       FACHF  = 5.
 1212 !
 1213 !!/NL3      MSC    = MAX ( 0. , MIN ( 8. , MSC ) )  ! Disabled HLT ca. 20>
 1214 !
 1215 ! 6.f Define Sds.
 1216 !
 1217       WNMEANP = 0.5    ! taken from Bidlot et al. 2005
 1218       FXFM3 = 2.5
 1219       FXFMAGE = 0.
 1220       FXPM3 = 4.
 1221       WNMEANPTAIL = -0.5
 1222       SDSBCHOICE =1 ! 1: Ardhuin et al., 2: Filipot & Ardhuin, 3: Romero
 1223       SDSC2     = -2.2E-5     ! -3.8 for Romero
 1224       SDSCUM    = -0.40344
 1225       SDSC4     = 1.
 1226       SDSC5     = 0.
 1227       SDSNUW    = 0.
 1228       SDSC6     = 0.3
 1229       SDSBR     = 0.90E-3     ! 0.005 for Romero
 1230       SDSBRFDF  = 0
 1231       SDSBRF1   = 0.5
 1232       SDSP      = 2.   ! this is now fixed in w3sds4, should be cleaned up
 1233       SDSDTH    = 80.
 1234       SDSCOS    = 2.
 1235       SDSISO    = 2
 1236       SDSBM0    = 1.
 1237       SDSBM1    = 0.
 1238       SDSBM2    = 0.
 1239       SDSBM3    = 0.
 1240       SDSBM4    = 0.
 1241       SDSBCK    = 0.
 1242       SDSABK    = 1.5
 1243       SDSPBK    = 4.
 1244       SDSBINT   = 0.3
 1245       SDSHCK    = 1.5
 1246       WHITECAPWIDTH = 0.3
 1247       SDSSTRAIN = 0.
 1248       SDSFACMTF =  400    ! MTF factor for Lambda , Romero (2019)
 1249       SDSSTRAINA = 15.
 1250       SDSSTRAIN2 = 0.
 1251       WHITECAPDUR   = 0.56 ! breaking duration factor
 1252       SDSBT     = 1.100E-3 ! B_T (sturation threshold for dissipation rat>
 1253       SDSL     = 3.5000e-05  ! L scaling
 1254       SPMSS     = 0.5    ! cmss^SPMSS
 1255       SDSNMTF   = 1.5    ! MTF power
 1256       SDSCUMP   = 2.
 1257       SDSMWD    = .9  ! new AFo
 1258       SDSMWPOW  = 1.  ! (k )^pow
 1259       SDKOF     = 3.  !  ko factor such that ko= g (SDKOF/(28 us))^2
 1260 !
 1261       CALL READNL ( NDSS, 'SDS4', STATUS )
 1262       WRITE (NDSO,924) STATUS
 1263       WRITE (NDSO,925) SDSC2, SDSBCK, SDSCUM, WNMEANP
 1264       SSDSC(1)   = REAL(SDSBCHOICE)
 1265       SSDSC(2)   = SDSC2
 
 
                                                         ww3_grid.F90  page  24
 
 
 1266       SSDSC(3)   = SDSCUM
 1267       SSDSC(4)   = SDSC4
 1268       SSDSC(5)   = SDSC5
 1269       SSDSC(6)   = SDSC6
 1270       SSDSC(7)   = WHITECAPWIDTH
 1271       SSDSC(8)   = SDSSTRAIN   ! Straining constant ...
 1272       SSDSC(9)   = SDSL
 1273       SSDSC(10)  = SDSSTRAINA*NTH/360. ! angle Aor enhanced straining
 1274       SSDSC(11)  = SDSSTRAIN2  ! straining constant for directional part
 1275       SSDSC(12)  = SDSBT
 1276       SSDSC(13)  = SDSMWD
 1277       SSDSC(14)  = SPMSS
 1278       SSDSC(15)  = SDSMWPOW
 1279       SSDSC(16)  = SDKOF
 1280       SSDSC(17)  = WHITECAPDUR
 1281       SSDSC(18)  = SDSFACMTF
 1282       SSDSC(19)  = SDSNMTF
 1283       SSDSC(20)  = SDSCUMP
 1284       SSDSC(21)  = SDSNUW
 1285 !
 1286       SSDSBR   = SDSBR
 1287       SSDSBRF1 = SDSBRF1
 1288       SSDSBRFDF= SDSBRFDF
 1289       SSDSBM(0)   = SDSBM0
 1290       SSDSBM(1)   = SDSBM1
 1291       SSDSBM(2)   = SDSBM2
 1292       SSDSBM(3)   = SDSBM3
 1293       SSDSBM(4)   = SDSBM4
 1294       SSDSBT   = SDSBT
 1295       SSDSISO  = SDSISO
 1296       SSDSCOS  = SDSCOS
 1297       SSDSP    = SDSP
 1298       SSDSDTH  = SDSDTH
 1299       WWNMEANP   = WNMEANP
 1300       FFXFM = FXFM3 * TPI
 1301       FFXFA = FXFMAGE * TPI
 1302       FFXPM = FXPM3 * GRAV / 28.
 1303       WWNMEANPTAIL   = WNMEANPTAIL
 1304       SSDSBCK   = SDSBCK
 1305       SSDSABK   = SDSABK
 1306       SSDSPBK   = SDSPBK
 1307       SSDSBINT  = SDSBINT
 1308       SSDSHCK   = SDSHCK
 1309 !
 1310 ! 6.g Define Sbt.
 1311 !
 1312       WRITE (NDSO,926)
 1313 !
 1314 ! 6.h Define Sdb.
 1315 !
 1316       WRITE (NDSO,928)
 1317 !
 1318 ! 6.i Define Str.
 1319 !
 1320       WRITE (NDSO,930)
 
 
                                                         ww3_grid.F90  page  25
 
 
 1321 !
 1322 ! 6.j Define Sbs.
 1323 !
 1324       WRITE (NDSO,932)
 1325 !
 1326 ! 6.k Define Sxx and Sic.
 1327 !
 1328 ! !/XX0      WRITE (NDSO,934)
 1329 !
 1330 ! 6.l Read unstructured data
 1331 ! initialisation of logical related to unstructured grid
 1332        UGOBCAUTO = .TRUE.
 1333        UGBCCFL = .TRUE.
 1334        UGOBCDEPTH= -10.
 1335        UGOBCOK = .FALSE.
 1336        UGOBCFILE = 'unset'
 1337        EXPFSN    = .TRUE.
 1338        EXPFSPSI  = .FALSE.
 1339        EXPFSFCT  = .FALSE.
 1340        IMPFSN    = .FALSE.
 1341        IMPTOTAL  = .FALSE.
 1342        EXPTOTAL  = .FALSE.
 1343        IMPREFRACTION = .FALSE.
 1344        IMPFREQSHIFT = .FALSE.
 1345        IMPSOURCE = .FALSE.
 1346        SETUP_APPLY_WLV = .FALSE.
 1347        SOLVERTHR_SETUP=1E-14
 1348        CRIT_DEP_SETUP=0.1
 1349        JGS_TERMINATE_MAXITER = .TRUE.
 1350        JGS_TERMINATE_DIFFERENCE = .TRUE.
 1351        JGS_TERMINATE_NORM = .FALSE.
 1352        JGS_LIMITER = .FALSE.
 1353        JGS_BLOCK_GAUSS_SEIDEL = .TRUE.
 1354        JGS_USE_JACOBI = .TRUE.
 1355        JGS_MAXITER=100
 1356        JGS_PMIN = 1
 1357        JGS_DIFF_THR = 1.E-10
 1358        JGS_NORM_THR = 1.E-20
 1359        JGS_NLEVEL = 0
 1360        JGS_SOURCE_NONLINEAR = .FALSE.
 1361 ! read data from the unstructured devoted namelist
 1362        CALL READNL ( NDSS, 'UNST', STATUS )
 1363                                            
 1364        B_JGS_USE_JACOBI = JGS_USE_JACOBI
 1365        B_JGS_TERMINATE_MAXITER = JGS_TERMINATE_MAXITER
 1366        B_JGS_TERMINATE_DIFFERENCE = JGS_TERMINATE_DIFFERENCE
 1367        B_JGS_TERMINATE_NORM = JGS_TERMINATE_NORM
 1368        B_JGS_LIMITER = JGS_LIMITER
 1369        B_JGS_BLOCK_GAUSS_SEIDEL = JGS_BLOCK_GAUSS_SEIDEL
 1370        B_JGS_MAXITER = JGS_MAXITER
 1371        B_JGS_PMIN = JGS_PMIN
 1372        B_JGS_DIFF_THR = JGS_DIFF_THR
 1373        B_JGS_NORM_THR = JGS_NORM_THR
 1374        B_JGS_NLEVEL = JGS_NLEVEL
 1375        B_JGS_SOURCE_NONLINEAR = JGS_SOURCE_NONLINEAR
 
 
                                                         ww3_grid.F90  page  26
 
 
 1376                                                     
 1377        IF ((EXPFSN .eqv. .FALSE.).and.(EXPFSPSI .eqv. .FALSE.)     &
 1378            .and.(EXPFSFCT .eqv. .FALSE.)                           &
 1379            .and.(IMPFSN .eqv. .FALSE.)                             &
 1380            .and.(EXPTOTAL .eqv. .FALSE.)                           &
 1381            .and.(IMPTOTAL .eqv. .FALSE.)) THEN
 1382          EXPFSN=.TRUE. ! This is the default scheme ...
 1383        END IF
 1384        nbSel=0
 1385               
 1386        IF (EXPFSN) nbSel=nbSel+1
 1387        IF (EXPFSPSI) nbSel=nbSel+1
 1388        IF (EXPFSFCT) nbSel=nbSel+1
 1389        IF (IMPFSN) nbSel=nbSel+1
 1390        IF (IMPTOTAL) nbSel=nbSel+1
 1391        IF (EXPTOTAL) nbSel=nbSel+1
 1392                                   
 1393        IF (GTYPE .EQ. UNGTYPE) THEN
 1394          IF (nbSel .ne. 1) THEN
 1395            WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN WW3_GRID:'
 1396            IF (nbSel .gt. 1) THEN
 1397              WRITE (NDSE,*) 'More than one scheme selected'
 1398            ELSE IF (nbSel .eq. 0) THEN
 1399              WRITE (NDSE,*) 'no scheme selected'
 1400            END IF
 1401            WRITE (NDSE,*)'Select only one of EXPFSN, EXPFSFCT, EXPFSPSI'
 1402            WRITE (NDSE,*)'IMPFSN, IMPTOTAL'
 1403            CALL EXTCDE ( 30 )
 1404          END IF
 1405        END IF
 1406 !
 1407 ! 6.m Select propagation scheme
 1408 !
 1409       WRITE (NDSO,950)
 1410 !
 1411       NRPROP = 0
 1412       FLPROP = .TRUE.
 1413       PNAME  = '                              '
 1414       PNAME  = '3rd order UQ'
 1415        J = LEN_TRIM(PNAME)
 1416       PNAME  = PNAME(1:J)//' + GSE averaging '
 1417       NRPROP = NRPROP + 1
 1418 !
 1419       IF ( (FLCX.OR.FLCY.OR.FLCTH.OR.FLCK) .AND. .NOT. FLPROP ) THEN
 1420           WRITE (NDSE,1030)
 1421           CALL EXTCDE ( 20 )
 1422         END IF
 1423 !
 1424       IF ( .NOT.(FLCX.OR.FLCY.OR.FLCTH.OR.FLCK) .AND. FLPROP ) THEN
 1425           WRITE (NDSE,1031)
 1426         END IF
 1427 !
 1428       IF ( NRPROP.EQ.0 ) THEN
 1429           WRITE (NDSE,1032)
 1430           CALL EXTCDE ( 21 )
 
 
                                                         ww3_grid.F90  page  27
 
 
 1431         END IF
 1432 !
 1433       IF ( NRPROP .GT. 1 ) THEN
 1434           WRITE (NDSE,1033) NRPROP
 1435           CALL EXTCDE ( 22 )
 1436         END IF
 1437 !
 1438 ! 6.m Parameters for propagation scheme
 1439 !
 1440       WRITE (NDSO,951) PNAME
 1441 !
 1442       CFLTM  =  0.7
 1443 !
 1444       WDTHCG = 1.5
 1445       WDTHTH = WDTHCG
 1446 !
 1447       CALL READNL ( NDSS, 'PRO3', STATUS )
 1448       IF ( STATUS(18:18) .EQ. ':' ) STATUS(18:18) = ' '
 1449        IF (GTYPE.NE.UNGTYPE) THEN
 1450           WRITE (NDSO,952) STATUS(1:18)
 1451       CFLTM  = MAX ( 0. , CFLTM )
 1452           WRITE (NDSO,953) CFLTM, WDTHCG
 1453       IF ( WDTHCG*(XFR-1.) .GT. 1. ) WRITE (NDSO,955) 1./(XFR-1.)
 1454           WRITE (NDSO,954) WDTHTH
 1455       IF ( WDTHTH*DTH .GT. 1. ) WRITE (NDSO,955) 1./DTH
 1456           WRITE (NDSO,*)
 1457        ENDIF
 1458       WDCG   = WDTHCG
 1459       WDTH   = WDTHTH
 1460 !
 1461       CTMAX  = CFLTM
 1462 !
 1463 ! 6.n Set miscellaneous parameters (ice, seeding, numerics ... )
 1464 !
 1465       CICE0  = 0.5
 1466       CICEN  = 0.5
 1467       LICE   = 0.
 1468       ICEHFAC= 1.0
 1469       ICEHMIN= 0.2  ! the 0.2 value is arbitrary and needs to be tuned.
 1470       ICEHINIT= 0.5
 1471       ICESLN = 1.0
 1472       ICEWIND= 1.0
 1473       ICESNL = 1.0
 1474       ICESDS = 1.0
 1475       ICEHDISP= 0.6 ! Prevent from convergence crash in w3dispmd in the p>
 1476       ICEDDISP= 80
 1477       ICEFDISP= 2
 1478       GSHIFT = 0.0D0
 1479       PMOVE  = 0.5
 1480       XSEED  = 1.
 1481       FLAGTR = 0
 1482       XP     = 0.15
 1483       XR     = 0.10
 1484       XFILT  = 0.05
 1485       IHM    = 100
 
 
                                                         ww3_grid.F90  page  28
 
 
 1486       HSPM   = 0.05
 1487       WSM    = 1.7
 1488       WSC    = 0.333
 1489       FLC    = .TRUE.
 1490       TRCKCMPR = .TRUE.
 1491       NOSW   = 5
 1492 !
 1493 ! Gas fluxes
 1494 !
 1495       AIRCMIN   = 2.0  ! cmin for whitecap coverage and entrained air
 1496       AIRGB     = 0.2  ! volume of entrained air constant (Deike et al. 2>
 1497 !
 1498       PTM    = 1    ! Default to standard WW3 partitioning. C. Bunney
 1499       PTFC   = 0.1  ! Part. method 5 cutoff freq default. C. Bunney
 1500       FMICHE = 1.6
 1501       RWNDC  = 1.
 1502       WCOR1  = 99.
 1503       WCOR2  = 0.
 1504       BTBET  = 1.2 ! β for c / [U cos(θ - φ)] < β
 1505 ! Variables for Space-Time Extremes
 1506 !  Default negative values make w3iogomd switch off space-time extremes
 1507 !  forces user to provide NAMELIST if wanting to compute STE parameters
 1508       STDX = -1.
 1509       STDY = -1.
 1510       STDT = -1.
 1511       ICEDISP = .FALSE.
 1512       CALTYPE = 'standard'
 1513 ! Variables for 3D array output
 1514       E3D=0
 1515       I1E3D=1
 1516       I2E3D=NK
 1517       P2SF   = 0
 1518       I1P2SF = 1	
 1519       I2P2SF = 15
 1520       US3D   = 0
 1521       I1US3D = 1
 1522       I2US3D = NK
 1523       USSP=0
 1524       IUSSP=1
 1525       STK_WN(:)=0.0
 1526       STK_WN(1)=TPI/100. !Set default decay of 100 m for Stokes drift
 1527       TH1MF=0
 1528       I1TH1M=1
 1529       I2TH1M=NK
 1530       STH1MF=0
 1531       I1STH1M=1
 1532       I2STH1M=NK
 1533       TH2MF=0
 1534       I1TH2M=1
 1535       I2TH2M=NK
 1536       STH2MF=0
 1537       I1STH2M=1
 1538       I2STH2M=NK
 1539 !
 1540       FACBERG=1.
 
 
                                                         ww3_grid.F90  page  29
 
 
 1541       WRITE (NDSO,944)
 1542 !                            ! compared to the 0.2 value in Boutin et al.>
 1543 !
 1544       REFCOAST=0.
 1545       REFMAP=0.
 1546       REFMAPD=0.
 1547       REFRMAX=1.
 1548       REFFREQPOW=2.
 1549       REFFREQ=0.
 1550       REFCOSP_STRAIGHT=4.
 1551       REFSLOPE=0.22
 1552       REFSUBGRID=0.
 1553       REFICEBERG=0.
 1554       REFUNSTSOURCE=0.
 1555 !
 1556       CALL READNL ( NDSS, 'REF1', STATUS )
 1557       WRITE (NDSO,969) STATUS
 1558 !
 1559        IGMETHOD = 2
 1560        IGADDOUTP= 0
 1561        IGSOURCE = 2
 1562        IGSTERMS = 0
 1563        IGMAXFREQ=0.03
 1564        IGSOURCEATBP = 0
 1565        IGBCOVERWRITE = .TRUE.
 1566        IGSWELLMAX = .TRUE.
 1567        IGKDMIN = 1.1
 1568        IGFIXEDDEPTH = 0.
 1569        IGEMPIRICAL = 0.00125
 1570 !
 1571       CALL READNL ( NDSS, 'SIG1 ', STATUS )
 1572       WRITE (NDSO,970) STATUS
 1573 !
 1574 !fixme: if USECGICE = .TRUE., don't allow use of IC3MAXTHK<100.0
 1575                                                                 
 1576 !
 1577       CALL READNL ( NDSS, 'OUTS', STATUS )
 1578       WRITE (NDSO,4970) STATUS
 1579 !
 1580 ! output of frequency spectra, th1m ...
 1581 !
 1582       E3DF(1,1) = E3D
 1583       E3DF(2,1) = MIN(MAX(1,I1E3D),NK)
 1584       E3DF(3,1) = MIN(MAX(1,I2E3D),NK)
 1585       E3DF(1,2) = TH1MF
 1586       E3DF(2,2) = MIN(MAX(1,I1TH1M),NK)
 1587       E3DF(3,2) = MIN(MAX(1,I2TH1M),NK)
 1588       E3DF(1,3) = STH1MF
 1589       E3DF(2,3) = MIN(MAX(1,I1STH1M),NK)
 1590       E3DF(3,3) = MIN(MAX(1,I2STH1M),NK)
 1591       E3DF(1,4) = TH2MF
 1592       E3DF(2,4) = MIN(MAX(1,I1TH2M),NK)
 1593       E3DF(3,4) = MIN(MAX(1,I2TH2M),NK)
 1594       E3DF(1,5) = STH2MF
 1595       E3DF(2,5) = MIN(MAX(1,I1STH2M),NK)
 
 
                                                         ww3_grid.F90  page  30
 
 
 1596       E3DF(3,5) = MIN(MAX(1,I2STH2M),NK)
 1597 !
 1598 ! output of microseismic source spectra
 1599 !
 1600       P2MSF(1) = P2SF
 1601       P2MSF(2) = MIN(MAX(1,I1P2SF),NK)
 1602       P2MSF(3) = MIN(MAX(1,I2P2SF),NK)
 1603 !
 1604 ! output of Stokes drift profile
 1605 !
 1606       US3DF(1) = US3D
 1607       US3DF(2) = MAX( 1 , MIN( NK, I1US3D) )
 1608       US3DF(3) = MAX( 1 , MIN( NK, I2US3D) )
 1609 !
 1610 ! output of Stokes drift partitions
 1611 !
 1612       USSPF(1) = USSP
 1613       USSPF(2) = MAX( 1 , MIN(25, IUSSP ) )
 1614       IF (IUSSP.GT.25) THEN
 1615          WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN ww3_grid:'
 1616          WRITE(NDSE,*) "  Stokes drift partition outputs not    "
 1617          WRITE(NDSE,*) "   intended for use with more than 25   "
 1618          WRITE(NDSE,*) "   partitions.  Please reduce IUSSP     "
 1619          WRITE(NDSE,*) "   specified in ww3_grid.inp to proceed "
 1620          CALL EXTCDE( 31)
 1621       ENDIF
 1622            
 1623       DO J=1,USSPF(2)
 1624          USSP_WN(j) = STK_WN(J)
 1625       ENDDO
 1626            
 1627 !
 1628       WRITE (NDSO,4971) P2MSF(1:3)
 1629       WRITE (NDSO,4972) US3DF(1:3)
 1630       WRITE (NDSO,4973) E3DF(1:3,1)
 1631       WRITE (NDSO,4974) USSPF(1:2)
 1632       DO J=1,USSPF(2)
 1633          WRITE(NDSO,4975) J,USSP_WN(J)
 1634       ENDDO
 1635 !
 1636       CALL READNL ( NDSS, 'MISC', STATUS )
 1637       WRITE (NDSO,960) STATUS
 1638 !
 1639       IF ( FLAGTR.LT.0 .OR. FLAGTR.GT.6 ) FLAGTR = 0
 1640       CICEN  = MIN ( 1. , MAX ( 0. , CICEN ) )
 1641       ICESLN  = MIN ( 1. , MAX ( 0. , ICESLN ) )
 1642       ICEWIND = MIN ( 1. , MAX ( 0. , ICEWIND ) )
 1643       ICESDS  = MIN ( 1. , MAX ( 0. , ICESDS ) )
 1644       ICESNL  = MIN ( 1. , MAX ( 0. , ICESNL ) )
 1645       FICEN  = CICEN
 1646       GRIDSHIFT=GSHIFT
 1647       ICESCALES(1)=ICESLN
 1648       ICESCALES(2)=ICEWIND
 1649       ICESCALES(3)=ICESNL
 1650       ICESCALES(4)=ICESDS
 
 
                                                         ww3_grid.F90  page  31
 
 
 1651       CMPRTRCK=TRCKCMPR
 1652       CICE0  = MIN ( CICEN , MAX ( 0. , CICE0 ) )
 1653       FICEL  = LICE
 1654       IICEHMIN  = ICEHMIN
 1655       IICEHFAC  = ICEHFAC
 1656       IICEHINIT  = ICEHINIT
 1657       IICEDISP= ICEDISP
 1658       IICEHDISP  = ICEHDISP
 1659       IICEDDISP  = ICEDDISP
 1660       IICEFDISP  = ICEFDISP
 1661       PMOVE  = MAX ( 0. , PMOVE )
 1662       PFMOVE = PMOVE
 1663 !
 1664       BTBETA = MIN(MAX (1., BTBET), 2.)
 1665       AAIRCMIN = ALOG(GRAV/AIRCMIN/SIG(1))/ALOG(XFR)+1 ! goes from phase >
 1666       AAIRGB = AIRGB
 1667 !
 1668 ! Notes: Presently, if we select CICE0.ne.CICEN requires an obstruction
 1669 !     grid, that is initialized with zeros as default.
 1670       IF ( FLAGTR .LT. 3 ) THEN
 1671         IF (CICE0.NE.CICEN) THEN
 1672           CICE0 = CICEN
 1673           IF (STATUS=='(user def. values) :')  WRITE (NDSO,2961)
 1674           END IF
 1675         END IF
 1676       IF ( CICE0.EQ.CICEN .AND. FLAGTR.GE.3 ) FLAGTR = FLAGTR - 2
 1677       WRITE (NDSO,961) CICE0, CICEN
 1678       WRITE (NDSO,8972) ICEWIND
 1679       FICE0  = CICE0
 1680 ! Variables for Space-Time Extremes
 1681       STEXU = STDX
 1682       IF ( STDY .LE. 0. ) THEN
 1683         STDY = STDX
 1684       END IF
 1685       STEYU = STDY
 1686       STEDU = STDT
 1687       IF ( STDX .GT. 0 ) THEN
 1688          WRITE (NDSO,1040) STDX
 1689          WRITE (NDSO,1041) STDY
 1690       ELSE
 1691          WRITE (NDSO,1042)
 1692       END IF
 1693       IF ( STDT .GT. 0 ) THEN
 1694          WRITE (NDSO,1043) STDT
 1695       ELSE
 1696          WRITE (NDSO,1044)
 1697       END IF
 1698 !
 1699       WRITE (NDSO,1972) TRCKCMPR
 1700       FACSD  = XSEED
 1701  RWINDC = RWNDC
 1702 !
 1703       XP     = MAX ( 1.E-6 , XP )
 1704       XR     = MAX ( 1.E-6 , XR )
 1705       XREL   = XR
 
 
                                                         ww3_grid.F90  page  32
 
 
 1706       XFILT  = MAX ( 0. , XFILT )
 1707       XFLT   = XFILT
 1708       WRITE (NDSO,965) XP, XR, XFILT
 1709       FACP   = XP / PI * 0.62E-3 * TPI**4 / GRAV**2
 1710 !
 1711       IHMAX  = MAX ( 50, IHM )
 1712       HSPMIN = MAX ( 0.0001 , HSPM )
 1713       WSMULT = MAX ( 1. , WSM )
 1714       WSCUT  = MIN ( 1.0001 , MAX ( 0. , WSC ) )
 1715       FLCOMB = FLC
 1716       NOSWLL = MAX ( 1 , NOSW )
 1717       PTMETH = PTM  ! Partitioning method. Chris Bunney (Jan 2016)
 1718       PTFCUT = PTFC ! Freq cutoff for partitiong method 5
 1719       PMNAM2 = ""
 1720       IF( PTMETH .EQ. 1 ) THEN
 1721         PMNAME = "WW3 default"
 1722       ELSE IF( PTMETH .EQ. 2 ) THEN
 1723         PMNAME = "Watershedding plus wind cut-off"
 1724       ELSE IF( PTMETH .EQ. 3 ) THEN
 1725         PMNAME = "Watershedding only"
 1726         WSCUT = 0.0 ! We don't want to classify by ws frac
 1727         PMNAM2 = "WSC set to 0.0"
 1728       ELSE IF( PTMETH .EQ. 4 ) THEN
 1729         PMNAME = "Wind speed cut-off only"
 1730         PMNAM2 = "WSC set to 0.0, NOSW set to 1"
 1731         WSCUT = 0.0 ! We don't want to classify by ws frac
 1732         NOSWLL = 1  ! Only ever one swell
 1733       ELSE IF( PTMETH .EQ. 5 ) THEN
 1734         WRITE(PMNAME, '("2-Band hi/low cutoff at ", F4.2,"Hz")') PTFCUT
 1735         PMNAM2 = "WSC set to 0.0, NOSW set to 1"
 1736         WSCUT = 0.0 ! We don't want to classify by ws frac
 1737         NOSWLL = 1  ! Only ever one swell
 1738       ELSE
 1739         WRITE( NDSE, * )                                                &
 1740           "*** Error - unknown partitioing method (PTM)! ***"
 1741         CALL EXIT(1)
 1742       ENDIF
 1743            
 1744         IF ( FLCOMB ) THEN
 1745           J      = 1
 1746         ELSE
 1747           J      = 2
 1748         END IF
 1749       WRITE (NDSO,966) IHMAX, HSPMIN, WSMULT, WSCUT, YESXNO(J), NOSWLL
 1750       WRITE (NDSO,5971) PMNAME
 1751       IF( PMNAM2 .NE. "" ) WRITE (NDSO,5972) PMNAM2
 1752 !!    WRITE (NDSO,966) IHMAX, HSPMIN, WSMULT, WSCUT, YESXNO(J)
 1753 !
 1754       FHMAX  = MAX ( 0.01 , FMICHE )
 1755       J      = 2
 1756       J      = 1
 1757       WRITE (NDSO,967) FHMAX, FHMAX/SQRT(2.), YESXNO(J)
 1758       IF ( FHMAX.LT.0.50 .AND. J.EQ.1 ) WRITE (NDST,968)
 1759 !
 1760       IF (TRIM(CALTYPE) .NE. 'standard' .AND.                           &
 
 
                                                         ww3_grid.F90  page  33
 
 
 1761           TRIM(CALTYPE) .NE. '360_day'  .AND.                           &
 1762           TRIM(CALTYPE) .NE. '365_day' ) GOTO 2003
 1763       WRITE (NDST,1973) CALTYPE
 1764       WRITE (NDSO,*)
 1765 !
 1766 ! 6.x Read values for FLD stress calculation
 1767 !
 1768 ! 6.o End of namelist processing
 1769 !
 1770       IF (FLGNML) THEN
 1771         CLOSE (NDSS)
 1772       ELSE
 1773         CLOSE (NDSS,STATUS='DELETE')
 1774       END IF
 1775 !
 1776       IF ( FLNMLO ) THEN
 1777         WRITE (NDSO,917)
 1778           WRITE (NDSO,2820) CLIN, RFPM, RFHF
 1779         IF ( .NOT. FLSTB2 ) THEN
 1780         ELSE
 1781           END IF
 1782 !
 1783           WRITE (NDSO,2920) ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP,  >
 1784             TAUWSHELTER, SWELLFPAR, SWELLF, SWELLF2, SWELLF3, SWELLF4, &
 1785             SWELLF5, SWELLF6, SWELLF7, Z0RAT, SINBR
 1786           WRITE (NDSO,2922) LAMBDA, NLPROP, KDCONV, KDMIN,       &
 1787                             SNLCS1, SNLCS2, SNLCS3
 1788                                                   
 1789           WRITE (NDSO,2924) SDSBCHOICE, SDSC2, SDSCUM, SDSC4,         &
 1790                             SDSC5, SDSC6, &
 1791                     WNMEANP, FXPM3, FXFM3, FXFMAGE,                   &
 1792                     SDSBINT, SDSBCK, SDSABK, SDSPBK, SDSHCK,          &
 1793                     SDSBR, SDSSTRAIN,  SDSSTRAINA,  SDSSTRAIN2,       &
 1794                     SDSBT, SDSP, SDSISO, SDSCOS, SDSDTH, SDSBRF1,     &
 1795                     SDSBRFDF, SDSBM0, SDSBM1, SDSBM2, SDSBM3, SDSBM4, &
 1796                     SPMSS, SDKOF, SDSMWD, SDSFACMTF, SDSNMTF,SDSMWPOW,&
 1797                     SDSCUMP, SDSNUW, WHITECAPWIDTH, WHITECAPDUR
 1798           WRITE (NDSO,2953) CFLTM, WDTHCG, WDTHTH
 1799 !
 1800         WRITE (NDSO,2956) UGBCCFL, UGOBCAUTO, UGOBCDEPTH,TRIM(UGOBCFILE),>
 1801                           EXPFSN, EXPFSPSI, EXPFSFCT, IMPFSN, EXPTOTAL,&
 1802                           IMPTOTAL, IMPREFRACTION, IMPFREQSHIFT,      &
 1803                           IMPSOURCE, SETUP_APPLY_WLV,                 &
 1804                           JGS_TERMINATE_MAXITER,                      &
 1805                           JGS_TERMINATE_DIFFERENCE,                   &
 1806                           JGS_TERMINATE_NORM,                         &
 1807                           JGS_LIMITER,                                &
 1808                           JGS_USE_JACOBI,                             &
 1809                           JGS_BLOCK_GAUSS_SEIDEL,                     &
 1810                           JGS_MAXITER,                                &
 1811                           JGS_PMIN,                                   &
 1812                           JGS_DIFF_THR,                               &
 1813                           JGS_NORM_THR,                               &
 1814                           JGS_NLEVEL,                                 &
 1815                           JGS_SOURCE_NONLINEAR
 
 
                                                         ww3_grid.F90  page  34
 
 
 1816 !
 1817         WRITE (NDSO,2976)    P2SF, I1P2SF, I2P2SF,                    &
 1818                              US3D, I1US3D, I2US3D,                    &
 1819                              USSP, IUSSP,                             &
 1820                              E3D, I1E3D, I2E3D,                       &
 1821                              TH1MF, I1TH1M, I2TH1M,                   &
 1822                              STH1MF, I1STH1M, I2STH1M,                &
 1823                              TH2MF, I1TH2M, I2TH2M,                   &
 1824                              STH2MF, I1STH2M, I2STH2M
 1825 !
 1826          WRITE(NDSO,2986) REFCOAST, REFFREQ, REFSLOPE, REFMAP,  &
 1827                     REFMAPD, REFSUBGRID , REFRMAX, REFFREQPOW,  &
 1828                     REFICEBERG, REFCOSP_STRAIGHT, REFUNSTSOURCE
 1829 !
 1830          WRITE(NDSO,2977) IGMETHOD, IGADDOUTP, IGSOURCE,         &
 1831                     IGSTERMS, IGBCOVERWRITE, IGSWELLMAX,         &
 1832                     IGMAXFREQ, IGSOURCEATBP, IGKDMIN,            &
 1833                     IGFIXEDDEPTH, IGEMPIRICAL
 1834 !
 1835  
 1836 !
 1837         IF ( FLCOMB ) THEN
 1838           WRITE (NDSO,2966) CICE0, CICEN, LICE, PMOVE, XSEED, FLAGTR, &
 1839                                 XP, XR, XFILT, IHMAX, HSPMIN, WSMULT, &
 1840                                 WSCUT, '.TRUE.', NOSWLL, FHMAX,       &
 1841                                 RWNDC, WCOR1, WCOR2, FACBERG, GSHIFT, &
 1842                                 STDX, STDY, STDT, ICEHMIN, ICEHFAC,   &
 1843                                 ICEHINIT, ICEDISP, ICEHDISP,          &
 1844                                 ICESLN, ICEWIND, ICESNL, ICESDS,      &
 1845                                 ICEDDISP,ICEFDISP, CALTYPE, TRCKCMPR, &
 1846                                 BTBETA
 1847         ELSE
 1848           WRITE (NDSO,2966) CICE0, CICEN, LICE, PMOVE, XSEED, FLAGTR, &
 1849                                 XP, XR, XFILT, IHMAX, HSPMIN, WSMULT, &
 1850                                 WSCUT, '.FALSE.', NOSWLL, FHMAX,      &
 1851                                 RWNDC, WCOR1, WCOR2, FACBERG, GSHIFT, &
 1852                                 STDX, STDY, STDT,  ICEHMIN, ICEHFAC,  &
 1853                                 ICEHINIT, ICEDISP, ICEHDISP,          &
 1854                                 ICESLN, ICEWIND, ICESNL, ICESDS,      &
 1855                                 ICEDDISP, ICEFDISP, CALTYPE, TRCKCMPR,&
 1856                                 BTBETA
 1857           END IF
 1858 !
 1859         WRITE (NDSO,918)
 1860         END IF
 1861 !
 1862 ! 6.p Set various other values ...
 1863 ! ... Tail in integration       --> scale factor for A to E conv
 1864 !
 1865       FTE    = 0.25 * SIG(NK)      * DTH * SIG(NK)
 1866       FTF    = 0.20                * DTH * SIG(NK)
 1867       FTWN   = 0.20 * SQRT(GRAV)   * DTH * SIG(NK)
 1868       FTTR   = FTF
 1869       FTWL   = GRAV / 6. / SIG(NK) * DTH * SIG(NK)
 1870 !
 
 
                                                         ww3_grid.F90  page  35
 
 
 1871       STXFTF      = 1/(FACHF-1.-WNMEANP*2)                       &
 1872                            * SIG(NK)**(2+WNMEANP*2) * DTH
 1873       STXFTFTAIL  = 1/(FACHF-1.-WNMEANPTAIL*2)                   &
 1874                            * SIG(NK)**(2+WNMEANPTAIL*2) * DTH
 1875       STXFTWN = 1/(FACHF-1.-WNMEANP*2) * SIG(NK)**(2)            &
 1876                  * (SIG(NK)/SQRT(GRAV))**(WNMEANP*2)   * DTH
 1877       SSTXFTF     = STXFTF
 1878       SSTXFTFTAIL = STXFTFTAIL
 1879       SSTXFTWN    = STXFTWN
 1880 !
 1881 ! ... High frequency cut-off
 1882 !
 1883       FXFM   = 2.5
 1884       FXPM   = 4.0
 1885       FXPM   = FXPM * GRAV / 28.
 1886       FXFM   = FXFM * TPI
 1887       XFC    = 3.0
 1888 !
 1889       FACTI1 = 1. / LOG(XFR)
 1890       FACTI2 = 1. - LOG(TPI*FR1) * FACTI1
 1891 !
 1892 ! Setting of FACHF moved to before !/NL2 set-up for consistency
 1893 !
 1894       FACHFA = XFR**(-FACHF-2)
 1895       FACHFE = XFR**(-FACHF)
 1896 !
 1897 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 1898 ! 7.  Read and prepare the grid.
 1899 ! 7.a Type of grid
 1900 !
 1901       IF (FLGNML) THEN
 1902         GSTRG=TRIM(NML_GRID%TYPE)
 1903         IF (TRIM(NML_GRID%COORD).EQ.'SPHE') FLAGLL=.TRUE.
 1904         IF (TRIM(NML_GRID%COORD).EQ.'CART') FLAGLL=.FALSE.
 1905         CSTRG=TRIM(NML_GRID%CLOS)
 1906       ELSE
 1907         CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1908         READ (NDSI,*,END=2001,ERR=2002) GSTRG, FLAGLL, CSTRG
 1909         CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1910       END IF
 1911             
 1912       SELECT CASE (TRIM(GSTRG))
 1913         CASE ('RECT')
 1914           GTYPE = RLGTYPE
 1915           WRITE (NDSO,3000) 'rectilinear'
 1916         CASE ('CURV')
 1917           GTYPE = CLGTYPE
 1918           WRITE (NDSO,3000) 'curvilinear'
 1919         CASE ('UNST')
 1920           GTYPE = UNGTYPE
 1921           WRITE (NDSO,3000) 'unstructured'
 1922         CASE DEFAULT
 1923           WRITE (NDSE,1007) TRIM(GSTRG)
 1924           CALL EXTCDE ( 25 )
 1925         END SELECT
 
 
                                                         ww3_grid.F90  page  36
 
 
 1926 !
 1927       IF ( FLAGLL ) THEN
 1928           FACTOR = 1.
 1929           WRITE (NDSO,3001) 'spherical'
 1930         ELSE
 1931           FACTOR = 1.E-3
 1932           WRITE (NDSO,3001) 'Cartesian'
 1933         END IF
 1934 !
 1935 !     Only process grid closure string for logically rectangular grids.
 1936 !     Closure setting for unstructured grids is NONE.
 1937       ICLOSE = ICLOSE_NONE
 1938       IF ( GTYPE.NE.UNGTYPE ) THEN
 1939           SELECT CASE (TRIM(CSTRG))
 1940             CASE ('NONE')
 1941               ICLOSE = ICLOSE_NONE
 1942               WRITE (NDSO,3002) 'none'
 1943             CASE ('SMPL')
 1944               ICLOSE = ICLOSE_SMPL
 1945               WRITE (NDSO,3002) 'simple'
 1946             CASE ('TRPL')
 1947               WRITE (NDSE,'(/2A)') ' *** WARNING WW3_GRID: TRIPOLE ',  &
 1948               'GRID CLOSURE IMPLEMENTATION IS INCOMPLETE ***'
 1949               ICLOSE = ICLOSE_TRPL
 1950               WRITE (NDSO,3002) 'tripole'
 1951               IF ( GTYPE.EQ.RLGTYPE ) THEN
 1952                   WRITE (NDSE,1009)
 1953                   CALL EXTCDE ( 25 )
 1954                 END IF
 1955             CASE DEFAULT
 1956               ! Check for old style GLOBAL input
 1957               SELECT CASE (TRIM(CSTRG))
 1958                 CASE ('T','t','.TRU','.tru')
 1959                   ICLOSE = ICLOSE_SMPL
 1960                   WRITE (NDSO,3002) 'simple'
 1961                   WRITE (NDSE,1013)
 1962                 CASE ('F','f','.FAL','.fal')
 1963                   ICLOSE = ICLOSE_NONE
 1964                   WRITE (NDSO,3002) 'none'
 1965                   WRITE (NDSE,1013)
 1966                 CASE DEFAULT
 1967                   WRITE (NDSE,1012) TRIM(CSTRG)
 1968                   CALL EXTCDE ( 25 )
 1969                 END SELECT
 1970             END SELECT
 1971           IF ( ICLOSE.NE.ICLOSE_NONE .AND. .NOT.FLAGLL ) THEN
 1972               WRITE (NDSE,1008)
 1973               CALL EXTCDE ( 25 )
 1974             END IF
 1975         END IF !GTYPE.NE.UNGTYPE
 1976 !
 1977 ! 7.b Size of grid
 1978 !
 1979       IF (FLGNML) THEN
 1980         SELECT CASE ( GTYPE )
 
 
                                                         ww3_grid.F90  page  37
 
 
 1981         CASE ( RLGTYPE )
 1982           NX = NML_RECT%NX
 1983           NY = NML_RECT%NY
 1984           NX = MAX ( 3 , NX )
 1985           NY = MAX ( 3 , NY )
 1986           WRITE (NDSO,3003) NX, NY
 1987         CASE ( CLGTYPE )
 1988           NX = NML_CURV%NX
 1989           NY = NML_CURV%NY
 1990           NX = MAX ( 3 , NX )
 1991           NY = MAX ( 3 , NY )
 1992           WRITE (NDSO,3003) NX, NY
 1993         CASE ( UNGTYPE )
 1994           NY=1
 1995         END SELECT
 1996       ELSE
 1997         IF ( GTYPE.NE.UNGTYPE) THEN
 1998           CALL NEXTLN ( COMSTR , NDSI , NDSE )
 1999           READ (NDSI,*,END=2001,ERR=2002) NX, NY
 2000           NX     = MAX ( 3 , NX )
 2001           NY     = MAX ( 3 , NY )
 2002           WRITE (NDSO,3003) NX, NY
 2003         ELSE
 2004           NY =1
 2005         END IF
 2006       END IF
 2007 !
 2008 ! Propagation specific to unstructured grids
 2009 !
 2010       DO_CHANGE_WLV=.FALSE.
 2011       IF ( GTYPE.EQ.UNGTYPE) THEN
 2012         UNSTSCHEMES(:)=0
 2013         IF (EXPFSN)   UNSTSCHEMES(1)=1
 2014         IF (EXPFSPSI) UNSTSCHEMES(2)=1
 2015         IF (EXPFSFCT) UNSTSCHEMES(3)=1
 2016         IF (IMPFSN)   UNSTSCHEMES(4)=1
 2017         UNSTSCHEME=-1
 2018         DO IX=1,4
 2019           IF (UNSTSCHEMES(IX).EQ.1) THEN
 2020             UNSTSCHEME=IX
 2021             EXIT
 2022           END IF
 2023         END DO
 2024               
 2025         FSBCCFL = UGBCCFL
 2026         SELECT CASE (UNSTSCHEME)
 2027         CASE (1)
 2028           FSN = EXPFSN
 2029           PNAME2 = 'N Explicit (Fluctuation Splitting) '
 2030         CASE (2)
 2031           FSPSI = EXPFSPSI
 2032           PNAME2 = 'PSI Explicit (Fluctuation Splitting)  '
 2033         CASE (3)
 2034           FSFCT = EXPFSFCT
 2035           PNAME2 = ' Flux Corrected Transport Explicit'
 
 
                                                         ww3_grid.F90  page  38
 
 
 2036         CASE (4)
 2037           FSNIMP = IMPFSN
 2038           PNAME2 = 'N Implicit (Fluctuation Splitting) '
 2039           END SELECT
 2040 !
 2041         IF (SUM(UNSTSCHEMES).GT.1) WRITE(NDSO,1035)
 2042         WRITE (NDSO,2951) PNAME2
 2043         IF (IMPTOTAL) THEN
 2044           FSTOTALIMP = IMPTOTAL
 2045           PNAME2 = 'N Implicit (Fluctuation Splitting) for total implicit'
 2046         END IF
 2047         IF (EXPTOTAL) THEN
 2048           FSTOTALEXP = EXPTOTAL
 2049           PNAME2 = 'N Explicit (Fluctuation Splitting) for one exchange e>
 2050         END IF
 2051         IF (IMPREFRACTION .and. IMPTOTAL .AND. FLCTH) THEN
 2052           FSREFRACTION = .TRUE.
 2053           PNAME2 = 'Refraction done implicitly'
 2054           WRITE (NDSO,2951) PNAME2
 2055         ELSE
 2056           FSREFRACTION = .FALSE.
 2057         END IF
 2058         IF (IMPFREQSHIFT .and. IMPTOTAL .AND. FLCK) THEN
 2059           FSFREQSHIFT = .TRUE.
 2060           PNAME2 = 'Frequency shifting done implicitly'
 2061           WRITE (NDSO,2951) PNAME2
 2062         ELSE
 2063           FSFREQSHIFT = .FALSE.
 2064         END IF
 2065         IF (IMPSOURCE .and. IMPTOTAL .AND. FLSOU) THEN
 2066           FSSOURCE = .TRUE.
 2067           PNAME2 = 'Source terms integrated implicitly'
 2068           WRITE (NDSO,2951) PNAME2
 2069         ELSE
 2070           FSSOURCE = .FALSE.
 2071         END IF
 2072         IF (SETUP_APPLY_WLV) THEN
 2073           DO_CHANGE_WLV = SETUP_APPLY_WLV
 2074           PNAME2 = ' we change WLV'
 2075           WRITE (NDSO,2952) PNAME2
 2076         END IF
 2077         SOLVERTHR_STP = SOLVERTHR_SETUP
 2078         CRIT_DEP_STP  = CRIT_DEP_SETUP
 2079       END IF
 2080             
 2081 !
 2082 ! 7.c Grid coordinates (branch here based on grid type)
 2083 !
 2084       IF ( GTYPE.NE.UNGTYPE) ALLOCATE ( XGRDIN(NX,NY), YGRDIN(NX,NY) )
 2085       SELECT CASE ( GTYPE )
 2086 !
 2087 ! 7.c.1 Rectilinear grid
 2088 !
 2089         CASE ( RLGTYPE )
 2090 !
 
 
                                                         ww3_grid.F90  page  39
 
 
 2091           IF (FLGNML) THEN
 2092             SX = NML_RECT%SX
 2093             SY = NML_RECT%SY
 2094             VSC = NML_RECT%SF
 2095             X0 = NML_RECT%X0
 2096             Y0 = NML_RECT%Y0
 2097             VSC0 = NML_RECT%SF0
 2098           ELSE
 2099             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2100             READ (NDSI,*,END=2001,ERR=2002) SX, SY, VSC
 2101             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2102             READ (NDSI,*,END=2001,ERR=2002) X0, Y0, VSC0
 2103           END IF
 2104 !
 2105           VSC    = MAX ( 1.E-7 , VSC )
 2106           SX     = SX / VSC
 2107           SY     = SY / VSC
 2108           SX     = MAX ( 1.E-7 , SX )
 2109           SY     = MAX ( 1.E-7 , SY )
 2110           IF ( ICLOSE.EQ.ICLOSE_SMPL ) SX = 360. / REAL(NX)
 2111 !
 2112           VSC0    = MAX ( 1.E-7 , VSC0 )
 2113           X0     = X0 / VSC0
 2114           Y0     = Y0 / VSC0
 2115 !
 2116           IF ( FLAGLL ) THEN
 2117               WRITE (NDSO,3004) FACTOR*SX, FACTOR*SY,         &
 2118                      FACTOR*X0, FACTOR*(X0+REAL(NX-1)*SX),    &
 2119                      FACTOR*Y0, FACTOR*(Y0+REAL(NY-1)*SY)
 2120             ELSE
 2121               WRITE (NDSO,3005) FACTOR*SX, FACTOR*SY,         &
 2122                      FACTOR*X0, FACTOR*(X0+REAL(NX-1)*SX),    &
 2123                      FACTOR*Y0, FACTOR*(Y0+REAL(NY-1)*SY)
 2124             END IF
 2125 !
 2126           DO IY=1, NY
 2127             DO IX=1, NX
 2128               XGRDIN(IX,IY) = X0 + REAL(IX-1)*SX
 2129               YGRDIN(IX,IY) = Y0 + REAL(IY-1)*SY
 2130               END DO
 2131             END DO
 2132 !
 2133 ! 7.c.2 Curvilinear grid
 2134 !
 2135         CASE ( CLGTYPE )
 2136 !
 2137 ! 7.c.2.a Process x-coordinates
 2138 !
 2139           IF (FLGNML) THEN
 2140             NDSG = NML_CURV%XCOORD%IDF
 2141             VSC = NML_CURV%XCOORD%SF
 2142             VOF = NML_CURV%XCOORD%OFF
 2143             IDLA = NML_CURV%XCOORD%IDLA
 2144             IDFM = NML_CURV%XCOORD%IDFM
 2145             RFORM = TRIM(NML_CURV%XCOORD%FORMAT)
 
 
                                                         ww3_grid.F90  page  40
 
 
 2146             FROM = TRIM(NML_CURV%XCOORD%FROM)
 2147             FNAME = TRIM(NML_CURV%XCOORD%FILENAME)
 2148           ELSE
 2149             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2150             READ (NDSI,*,END=2001,ERR=2002) NDSG, VSC, VOF, &
 2151                                             IDLA, IDFM, RFORM, FROM, FNAME
 2152           END IF
 2153 !
 2154           IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 2155           IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
 2156 !
 2157           WRITE (NDSO,3006) NDSG, VSC, VOF, IDLA, IDFM
 2158           IF (IDFM.EQ.2) WRITE (NDSO,3008) TRIM(RFORM)
 2159           IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
 2160               WRITE (NDSO,3009) TRIM(FNAME)
 2161 !
 2162           IF ( NDSG .EQ. NDSI ) THEN
 2163             IF ( IDFM .EQ. 3 ) THEN
 2164               WRITE (NDSE,1004) NDSG
 2165               CALL EXTCDE (23)
 2166             ELSE
 2167               IF (.NOT.FLGNML) THEN
 2168               CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2169               END IF
 2170             END IF
 2171           ELSE
 2172             IF ( IDFM .EQ. 3 ) THEN
 2173               IF (FROM.EQ.'NAME') THEN
 2174                 OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
 2175                       FORM='UNFORMATTED',                 &
 2176                       STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2177               ELSE
 2178                 OPEN (NDSG,                               &
 2179                       FORM='UNFORMATTED',                 &
 2180                       STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2181               END IF
 2182             ELSE
 2183               IF (FROM.EQ.'NAME') THEN
 2184                   OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
 2185                   STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2186               ELSE
 2187                   OPEN (NDSG,                               &
 2188                         STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2189               END IF
 2190             END IF !IDFM
 2191           END IF !NDSG
 2192 !
 2193           CALL INA2R ( XGRDIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE, &
 2194                        IDFM, RFORM, IDLA, VSC, VOF)
 2195 !
 2196 ! 7.c.2.b Process y-coordinates
 2197 !
 2198           IF (FLGNML) THEN
 2199             NDSG = NML_CURV%YCOORD%IDF
 2200             VSC = NML_CURV%YCOORD%SF
 
 
                                                         ww3_grid.F90  page  41
 
 
 2201             VOF = NML_CURV%YCOORD%OFF
 2202             IDLA = NML_CURV%YCOORD%IDLA
 2203             IDFM = NML_CURV%YCOORD%IDFM
 2204             RFORM = TRIM(NML_CURV%YCOORD%FORMAT)
 2205             FROM = TRIM(NML_CURV%YCOORD%FROM)
 2206             FNAME = TRIM(NML_CURV%YCOORD%FILENAME)
 2207           ELSE
 2208             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2209             READ (NDSI,*,END=2001,ERR=2002) NDSG, VSC, VOF, &
 2210                                             IDLA, IDFM, RFORM, FROM, FNAME
 2211           END IF
 2212 !
 2213           IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 2214           IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
 2215 !
 2216           WRITE (NDSO,3007) NDSG, VSC, VOF, IDLA, IDFM
 2217           IF (IDFM.EQ.2) WRITE (NDSO,3008) TRIM(RFORM)
 2218           IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
 2219               WRITE (NDSO,3009) TRIM(FNAME)
 2220 !
 2221           IF ( NDSG .EQ. NDSI ) THEN
 2222             IF ( IDFM .EQ. 3 ) THEN
 2223               WRITE (NDSE,1004) NDSG
 2224               CALL EXTCDE (23)
 2225             ELSE
 2226               IF (.NOT.FLGNML) THEN
 2227                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2228               END IF
 2229             END IF
 2230           ELSE
 2231             IF ( IDFM .EQ. 3 ) THEN
 2232               IF (FROM.EQ.'NAME') THEN
 2233                 OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
 2234                       FORM='UNFORMATTED',                 &
 2235                       STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2236               ELSE
 2237                 OPEN (NDSG,                               &
 2238                       FORM='UNFORMATTED',                 &
 2239                       STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2240               END IF
 2241             ELSE
 2242               IF (FROM.EQ.'NAME') THEN
 2243                   OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
 2244                   STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2245               ELSE
 2246                 OPEN (NDSG,                               &
 2247                       STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2248               END IF
 2249             END IF !IDFM
 2250           END IF !NDSG
 2251 !
 2252           CALL INA2R ( YGRDIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE, &
 2253                        IDFM, RFORM, IDLA, VSC, VOF)
 2254 !
 2255 ! 7.c.2.c Check for obvious errors in grid definition or input
 
 
                                                         ww3_grid.F90  page  42
 
 
 2256 !
 2257 ! ....... Check for inverted grid (can result from wrong IDLA)
 2258           IF ( (XGRDIN(2,1)-XGRDIN(1,1))*(YGRDIN(1,2)-YGRDIN(1,1)) .LT. &
 2259                (YGRDIN(2,1)-YGRDIN(1,1))*(XGRDIN(1,2)-XGRDIN(1,1)) ) THEN
 2260              WRITE (NDSE,1011) IDLA
 2261 !.........Notes: here, we are checking to make sure that the j axis is ~9>
 2262 !................counter-clockwise from the i axis (the standard cartesia>
 2263 !................So, it is a check on the handedness of the grid.
 2264 !................We have confirmed for one case that a left-handed grid p>
 2265 !................errors in SCRIP. We have not confirmed that left-handed >
 2266 !................produce errors in single-grid simulations, or that they >
 2267 !................produce errors in all multi-grid simulations.
 2268 !................Note that transposing or flipping a grid will generally >
 2269              CALL EXTCDE (25)
 2270           END IF
 2271 !
 2272 ! 7.c.3 Unstructured grid
 2273 !
 2274         CASE ( UNGTYPE )
 2275 !
 2276           MAXX = 0.
 2277           MAXY = 0.
 2278           DXYMAX = 0.
 2279           WRITE (NDSO,1150)
 2280                            
 2281           IF (FLGNML) THEN
 2282             ZLIM = NML_GRID%ZLIM
 2283             DMIN = NML_GRID%DMIN
 2284             NDSG = NML_UNST%IDF
 2285             VSC = NML_UNST%SF
 2286             IDLA = NML_UNST%IDLA
 2287             IDFM = NML_UNST%IDFM
 2288             RFORM = TRIM(NML_UNST%FORMAT)
 2289             FROM = 'NAME'
 2290             FNAME = TRIM(NML_UNST%FILENAME)
 2291             UGOBCFILE = TRIM(NML_UNST%UGOBCFILE)
 2292           END IF
 2293       END SELECT !GTYPE
 2294 !
 2295 ! 7.d Depth information for grid
 2296 !
 2297       IF (FLGNML) THEN
 2298         IF (GTYPE.NE.UNGTYPE) THEN
 2299           ZLIM = NML_GRID%ZLIM
 2300           DMIN = NML_GRID%DMIN
 2301           NDSG = NML_DEPTH%IDF
 2302           VSC = NML_DEPTH%SF
 2303           IDLA = NML_DEPTH%IDLA
 2304           IDFM = NML_DEPTH%IDFM
 2305           RFORM = TRIM(NML_DEPTH%FORMAT)
 2306           FROM = TRIM(NML_DEPTH%FROM)
 2307           FNAME = TRIM(NML_DEPTH%FILENAME)
 2308         END IF
 2309       ELSE
 2310         CALL NEXTLN ( COMSTR , NDSI , NDSE )
 
 
                                                         ww3_grid.F90  page  43
 
 
 2311         READ (NDSI,*,END=2001,ERR=2002) ZLIM, DMIN, NDSG, VSC, IDLA,    &
 2312                                         IDFM, RFORM, FROM, FNAME
 2313       END IF
 2314 !
 2315       DMIN    = MAX ( 1.E-3 , DMIN )
 2316       IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
 2317       IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 2318       IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
 2319 !
 2320       WRITE (NDSO,972) NDSG, ZLIM, DMIN, VSC, IDLA, IDFM
 2321       IF (IDFM.EQ.2) WRITE (NDSO,973) TRIM(RFORM)
 2322       IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
 2323           WRITE (NDSO,974) TRIM(FNAME)
 2324 !
 2325 ! 7.e Read bottom depths
 2326 !
 2327       IF ( GTYPE.NE.UNGTYPE ) THEN
 2328 !
 2329 ! Reading depths on structured grid
 2330 !
 2331 !Li Suspended for SMC grid, which uses depth stored in its cell array.
 2332 !Li               JGLi15Oct2014
 2333         IF( RGLGRD ) THEN
 2334 !Li
 2335           IF ( NDSG .EQ. NDSI ) THEN
 2336               IF ( IDFM .EQ. 3 ) THEN
 2337                   WRITE (NDSE,1004) NDSG
 2338                   CALL EXTCDE (23)
 2339                 ELSE
 2340                   CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2341                 END IF
 2342             ELSE  ! NDSG.NE.NDSI
 2343               IF ( IDFM .EQ. 3 ) THEN
 2344                   IF (FROM.EQ.'NAME') THEN
 2345                       OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME), &
 2346                             FORM='UNFORMATTED',&
 2347                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2348                     ELSE
 2349                       OPEN (NDSG, FORM='UNFORMATTED',                &
 2350                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2351                     END IF
 2352                 ELSE
 2353                   IF (FROM.EQ.'NAME') THEN
 2354                       OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),  &
 2355                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2356                     ELSE
 2357                       OPEN (NDSG,                                     &
 2358                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2359                     END IF
 2360                 END IF
 2361             END IF  !( NDSG .EQ. NDSI )
 2362 !
 2363 !Li     End of RGLGRD block
 2364         ENDIF
 2365 !Li
 
 
                                                         ww3_grid.F90  page  44
 
 
 2366 !
 2367         ALLOCATE ( ZBIN(NX,NY), OBSX(NX,NY), OBSY(NX,NY) )
 2368 !
 2369 !       Initialize subgrid obstructions with zeros.
 2370         ZBIN(:,:)=0.
 2371         OBSX(:,:)=0.
 2372         OBSY(:,:)=0.
 2373                     
 2374 !Li   Suspend read depth file.     JGLi15Oct2014
 2375         IF( RGLGRD ) THEN
 2376 !Li
 2377           CALL INA2R ( ZBIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE,     >
 2378                        IDFM, RFORM, IDLA, VSC, 0.0)
 2379 !Li     End of RGLGRD block
 2380         ENDIF
 2381 !Li
 2382 !
 2383       ELSE
 2384 !
 2385 ! Reading depths on unstructured grid (this also sets number of mesh poin>
 2386 !
 2387         CALL READMSH(NDSG,FNAME)
 2388         ALLOCATE(ZBIN(NX, NY),OBSX(NX,NY),OBSY(NX,NY))
 2389         ZBIN(:,1) = VSC*XYB(:,3)
 2390 !
 2391 ! subgrid obstructions are not yet handled in unstructured grids
 2392 !
 2393         OBSX(:,:)=0.
 2394         OBSY(:,:)=0.
 2395                     
 2396       END IF
 2397 !
 2398 ! 7.f Set up temporary map
 2399 !
 2400       ALLOCATE ( TMPSTA(NY,NX), TMPMAP(NY,NX) )
 2401       TMPSTA = 0
 2402 !
 2403       IF (GTYPE .EQ. UNGTYPE) THEN
 2404         TMPSTA = 1
 2405       ELSE
 2406         DO IY=1, NY
 2407           DO IX=1, NX
 2408             IF ( ZBIN(IX,IY) .LE. ZLIM ) TMPSTA(IY,IX) = 1
 2409           END DO
 2410         END DO
 2411       ENDIF
 2412 !
 2413 !Li   Suspended for SMC grid.  JGLi15Oct2014
 2414       IF( RGLGRD ) THEN
 2415 !Li
 2416 !
 2417 ! 7.g Subgrid information
 2418 !
 2419       TRFLAG = FLAGTR
 2420       IF ( TRFLAG.GT.6 .OR. TRFLAG.LT.0 ) TRFLAG = 0
 
 
                                                         ww3_grid.F90  page  45
 
 
 2421 !
 2422       IF ( TRFLAG .EQ. 0 ) THEN
 2423         WRITE (NDSO,976) 'Not available.'
 2424       ELSE IF ( TRFLAG.EQ.1 .OR. TRFLAG.EQ.3 .OR. TRFLAG.EQ.5 ) THEN
 2425         WRITE (NDSO,976) 'In between grid points.'
 2426       ELSE
 2427         WRITE (NDSO,976) 'At grid points.'
 2428       END IF
 2429 !
 2430       IF ( TRFLAG .NE. 0 ) THEN
 2431 !
 2432 ! 7.g.1 Info from input file
 2433 !
 2434         IF (FLGNML) THEN
 2435           NDSTR = NML_OBST%IDF
 2436           VSC = NML_OBST%SF
 2437           IDLA = NML_OBST%IDLA
 2438           IDFT = NML_OBST%IDFM
 2439           RFORM = TRIM(NML_OBST%FORMAT)
 2440           FROM = TRIM(NML_OBST%FROM)
 2441           TNAME = TRIM(NML_OBST%FILENAME)
 2442         ELSE
 2443           CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2444           READ (NDSI,*,END=2001,ERR=2002) NDSTR, VSC, IDLA, IDFT, RFORM, &
 2445                                           FROM, TNAME
 2446         END IF
 2447 !
 2448         IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
 2449         IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 2450         IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
 2451 !
 2452         WRITE (NDSO,977) NDSTR, VSC, IDLA, IDFT
 2453         IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
 2454         IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSTR) WRITE (NDSO,974) TNAME
 2455 !
 2456 ! 7.g.2 Open file and check if necessary
 2457 !
 2458         IF ( NDSTR .EQ. NDSI ) THEN
 2459           IF ( IDFT .EQ. 3 ) THEN
 2460             WRITE (NDSE,1004) NDSTR
 2461             CALL EXTCDE (23)
 2462           ELSE
 2463             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2464           END IF
 2465         ELSE IF ( NDSTR .EQ. NDSG ) THEN
 2466           IF ( ( IDFM.EQ.3 .AND. IDFT.NE.3 ) .OR.                 &
 2467                ( IDFM.NE.3 .AND. IDFT.EQ.3 ) ) THEN
 2468             WRITE (NDSE,1005) IDFM, IDFT
 2469             CALL EXTCDE (24)
 2470           END IF
 2471         ELSE
 2472           IF ( IDFT .EQ. 3 ) THEN
 2473             IF (FROM.EQ.'NAME') THEN
 2474               OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
 2475                     FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
 
 
                                                         ww3_grid.F90  page  46
 
 
 2476                     IOSTAT=IERR)
 2477             ELSE
 2478               OPEN (NDSTR,           FORM='UNFORMATTED',      &
 2479                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2480             END IF
 2481           ELSE
 2482             IF (FROM.EQ.'NAME') THEN
 2483               OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
 2484                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2485             ELSE
 2486               OPEN (NDSTR,                                    &
 2487                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2488             END IF
 2489           END IF
 2490         END IF
 2491 !
 2492 ! 7.g.3 Read the data
 2493 !
 2494         CALL INA2R ( OBSX, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
 2495                      IDFT, RFORM, IDLA, VSC, 0.0)
 2496 !
 2497         IF ( NDSTR .EQ. NDSI ) CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2498 !
 2499         CALL INA2R ( OBSY, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
 2500                      IDFT, RFORM, IDLA, VSC, 0.0)
 2501 !
 2502 ! 7.g.4 Limit
 2503 !
 2504         DO IX=1, NX
 2505           DO IY=1, NY
 2506             OBSX(IX,IY) = MAX( 0. , MIN(1.,OBSX(IX,IY)) )
 2507             OBSY(IX,IY) = MAX( 0. , MIN(1.,OBSY(IX,IY)) )
 2508           END DO
 2509         END DO
 2510 !
 2511         WRITE (NDSO,*)
 2512 !
 2513       END IF ! TRFLAG
 2514 !
 2515 !Li     End of RGLGRD block
 2516       END IF
 2517 !Li
 2518 !
 2519 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 2520 ! 8.  Finalize status maps
 2521 ! 8.a Defines open boundary conditions for UNST grids
 2522 !
 2523        J = LEN_TRIM(UGOBCFILE)
 2524        IF (GTYPE.EQ.UNGTYPE.AND.UGOBCFILE(:J).NE.'unset')  &
 2525         CALL READMSHOBC(NDSG,UGOBCFILE,TMPSTA,UGOBCOK)
 2526        IF ((GTYPE.EQ.UNGTYPE).AND.UGOBCAUTO.AND.(.NOT.UGOBCOK))  &
 2527           CALL UG_GETOPENBOUNDARY(TMPSTA,ZBIN,UGOBCDEPTH)
 2528 !
 2529 ! 8.b Determine where to get the data
 2530 !
 
 
                                                         ww3_grid.F90  page  47
 
 
 2531       IF (FLGNML) THEN
 2532         NDSTR = NML_MASK%IDF
 2533         IDLA = NML_MASK%IDLA
 2534         IDFT = NML_MASK%IDFM
 2535         RFORM = TRIM(NML_MASK%FORMAT)
 2536         FROM = TRIM(NML_MASK%FROM)
 2537         TNAME = TRIM(NML_MASK%FILENAME)
 2538         IF (TNAME.EQ.'unset' .OR. TNAME.EQ.'UNSET') FROM='PART'
 2539       ELSE
 2540         CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2541         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFT, RFORM,     &
 2542                                         FROM, TNAME
 2543       END IF
 2544 !
 2545 ! ... Data to be read in parts
 2546 !
 2547       IF ( FROM .EQ. 'PART' ) THEN
 2548 !
 2549 ! 8.b Update TMPSTA with input boundary data (ILOOP=1)
 2550 !                        and excluded points (ILOOP=2)
 2551 !
 2552         IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
 2553           WRITE(NDSE,*)'PROGRAM W3GRID STATUS MAP CALCULATION IS '//   &
 2554           'NOT TESTED FOR TRIPOLE GRIDS FOR CASE WHERE USER OPTS '//   &
 2555           'TO READ DATA IN PARTS. STOPPING NOW (107).'
 2556           CALL EXTCDE ( 107 )
 2557         END IF
 2558         DO ILOOP=1, 2
 2559 !
 2560           I = 1
 2561           IF ( ILOOP .EQ. 1 ) THEN
 2562             WRITE (NDSO,979) 'boundary points'
 2563             NSTAT  = 2
 2564           ELSE
 2565             WRITE (NDSO,979) 'excluded points'
 2566             NSTAT  = -1
 2567           END IF
 2568           FIRST  = .TRUE.
 2569 !
 2570           DO
 2571             IF (FLGNML) THEN
 2572               ! inbound points
 2573               IF (ILOOP.EQ.1) THEN
 2574                 IF (NML_INBND_COUNT%N_POINT.GT.0 .AND. I.LE.NML_INBND_COU>
 2575                   IX = NML_INBND_POINT(I)%X_INDEX
 2576                   IY = NML_INBND_POINT(I)%Y_INDEX
 2577                   CONNCT = NML_INBND_POINT(I)%CONNECT
 2578                   I=I+1
 2579                 ELSE
 2580                   EXIT
 2581                 END IF
 2582               ! excluded points
 2583               ELSE IF (ILOOP.EQ.2) THEN
 2584                 IF (NML_EXCL_COUNT%N_POINT.GT.0 .AND. I.LE.NML_EXCL_COUNT>
 2585                   IX = NML_EXCL_POINT(I)%X_INDEX
 
 
                                                         ww3_grid.F90  page  48
 
 
 2586                   IY = NML_EXCL_POINT(I)%Y_INDEX
 2587                   CONNCT = NML_EXCL_POINT(I)%CONNECT
 2588                   I=I+1
 2589                 ELSE
 2590                   EXIT
 2591                 END IF
 2592               END IF
 2593             ELSE
 2594               CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2595               READ (NDSI,*,END=2001,ERR=2002) IX, IY, CONNCT
 2596             END IF
 2597                   
 2598 !
 2599 ! ... Check if last point reached.
 2600 !
 2601             IF (IX.EQ.0 .AND. IY.EQ.0) EXIT
 2602 !
 2603 ! ... Check if point in grid.
 2604 !
 2605             IF (GTYPE.EQ.UNGTYPE.AND.(UGOBCAUTO.OR.UGOBCOK)) CYCLE
 2606             IF (IX.LT.1 .OR. IX.GT.NX .OR.  IY.LT.1 .OR. IY.GT.NY) THEN
 2607               WRITE (NDSO,981)
 2608               WRITE (NDSO,*) '       ', IX, IY
 2609               CYCLE
 2610             END IF
 2611 !
 2612 ! ... Check if intermediate points are to be added.
 2613 !
 2614             IF ( CONNCT .AND. .NOT.FIRST ) THEN
 2615                 IDX    = IX - IXO
 2616                 IDY    = IY - IYO
 2617                 IF ( IDX.EQ.0 .OR. IDY.EQ.0 .OR.                      &
 2618                     ABS(IDX).EQ.ABS(IDY) ) THEN
 2619                     NBA    = MAX ( MAX(ABS(IDX),ABS(IDY))-1 , 0 )
 2620                     IF (IDX.NE.0) IDX = SIGN(1,IDX)
 2621                     IF (IDY.NE.0) IDY = SIGN(1,IDY)
 2622                     IX     = IXO
 2623                     IY     = IYO
 2624                     DO IBA=1, NBA
 2625                       IX     = IX + IDX
 2626                       IY     = IY + IDY
 2627                       IF ( TMPSTA(IY,IX).EQ.1 .OR. J.EQ.2 ) THEN
 2628                         TMPSTA(IY,IX) = NSTAT
 2629                       ELSE
 2630                         WRITE(NDSO,*) 'WARNING: POINT (',IX,',',IY,  &
 2631                                    ') CANNOT BE GIVEN THE STATUS ',NSTAT
 2632                       END IF
 2633                     END DO
 2634                   IX     = IX + IDX
 2635                   IY     = IY + IDY
 2636                 ELSE
 2637                   WRITE (NDSO,982)
 2638                   WRITE (NDSO,*) '       ', IX , IY
 2639                   WRITE (NDSO,*) '       ', IXO, IYO
 2640                 END IF
 
 
                                                         ww3_grid.F90  page  49
 
 
 2641               END IF
 2642 !
 2643 ! ... Check if point itself is to be added
 2644 !
 2645             IF ( TMPSTA(IY,IX).EQ.1 .OR. J.EQ.2 ) THEN
 2646               TMPSTA(IY,IX) = NSTAT
 2647             END IF
 2648 !
 2649 ! ... Save data of previous point
 2650 !
 2651             IXO    = IX
 2652             IYO    = IY
 2653             FIRST  = .FALSE.
 2654 !
 2655 ! ... Branch back to read.
 2656 !
 2657           END DO
 2658 !
 2659 ! 8.c Final processing excluded points
 2660 !
 2661           IF ( ILOOP .EQ. 2 ) THEN
 2662 !
 2663             I = 1
 2664             DO
 2665               IF (FLGNML) THEN
 2666                 ! excluded bodies
 2667                 IF (NML_EXCL_COUNT%N_BODY.GT.0 .AND. I.LE.NML_EXCL_COUNT%>
 2668                   IX = NML_EXCL_BODY(I)%X_INDEX
 2669                   IY = NML_EXCL_BODY(I)%Y_INDEX
 2670                   I=I+1
 2671                 ELSE
 2672                   EXIT
 2673                 END IF
 2674               ELSE
 2675                 CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2676                 READ (NDSI,*,END=2001,ERR=2002) IX, IY
 2677               END IF
 2678 !
 2679 ! ... Check if last point reached.
 2680 !
 2681               IF (IX.EQ.0 .AND. IY.EQ.0) EXIT
 2682 !
 2683 ! ... Check if point in grid.
 2684 !
 2685               IF (IX.LT.1 .OR. IX.GT.NX .OR. IY.LT.1 .OR. IY.GT.NY) THEN
 2686                 WRITE (NDSO,981)
 2687                 WRITE (NDSO,*) '       ', IX, IY
 2688                 CYCLE
 2689               END IF
 2690 !
 2691 ! ... Check if point already excluded
 2692 !
 2693               IF ( TMPSTA(IY,IX) .EQ. NSTAT ) THEN
 2694                 WRITE (NDSO,1981)
 2695                 WRITE (NDSO,*) '       ', IX, IY
 
 
                                                         ww3_grid.F90  page  50
 
 
 2696                 CYCLE
 2697               END IF
 2698 !
 2699 ! ... Search for points to exclude
 2700 !
 2701               TMPMAP = TMPSTA
 2702               J      = 1
 2703               IX1    = IX
 2704               IY1    = IY
 2705 !
 2706               JJ     = TMPSTA(IY,IX)
 2707               TMPSTA(IY,IX) = NSTAT
 2708               DO
 2709                 NBT    = 0
 2710                 DO IX=MAX(1,IX1-J), MIN(IX1+J,NX)
 2711                   DO IY=MAX(1,IY1-J), MIN(IY1+J,NY)
 2712                     IF ( TMPSTA(IY,IX) .EQ. JJ ) THEN
 2713                       IF (IX.GT.1) THEN
 2714                         IF (TMPSTA(IY  ,IX-1).EQ.NSTAT           &
 2715                             .AND. TMPMAP(IY  ,IX-1).EQ.JJ ) THEN
 2716                           TMPSTA(IY,IX) = NSTAT
 2717                         END IF
 2718                       END IF
 2719                       IF (IX.LT.NX) THEN
 2720                         IF (TMPSTA(IY  ,IX+1).EQ.NSTAT           &
 2721                             .AND. TMPMAP(IY  ,IX+1).EQ.JJ ) THEN
 2722                           TMPSTA(IY,IX) = NSTAT
 2723                         END IF
 2724                       END IF
 2725                       IF (IY.LT.NY) THEN
 2726                         IF (TMPSTA(IY+1,IX  ).EQ.NSTAT           &
 2727                             .AND. TMPMAP(IY+1,IX  ).EQ.JJ ) THEN
 2728                           TMPSTA(IY,IX) = NSTAT
 2729                         END IF
 2730                       END IF
 2731                       IF (IY.GT.1) THEN
 2732                         IF (TMPSTA(IY-1,IX  ).EQ.NSTAT           &
 2733                            .AND. TMPMAP(IY-1,IX  ).EQ.JJ ) THEN
 2734                           TMPSTA(IY,IX) = NSTAT
 2735                         END IF
 2736                       END IF
 2737                       IF (TMPSTA(IY,IX).EQ.NSTAT) NBT = NBT + 1
 2738                     END IF
 2739                   END DO
 2740                 END DO
 2741 !
 2742                 IF ( NBT .NE. 0 ) THEN
 2743                   J = J + 1
 2744                 ELSE
 2745                   EXIT
 2746                 END IF
 2747               END DO
 2748             END DO
 2749 !
 2750 ! ... Outer boundary excluded points
 
 
                                                         ww3_grid.F90  page  51
 
 
 2751 !
 2752             IF ( GTYPE.NE.UNGTYPE ) THEN
 2753                                         
 2754               DO IX=1, NX
 2755                 IF ( TMPSTA( 1,IX) .EQ. 1 ) TMPSTA( 1,IX) = NSTAT
 2756                 IF ( TMPSTA(NY,IX) .EQ. 1 ) TMPSTA(NY,IX) = NSTAT
 2757               END DO
 2758 !
 2759               IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
 2760                 DO IY=2, NY-1
 2761                   IF ( TMPSTA(IY, 1) .EQ. 1 ) TMPSTA(IY, 1) = NSTAT
 2762                   IF ( TMPSTA(IY,NX) .EQ. 1 ) TMPSTA(IY,NX) = NSTAT
 2763                 END DO
 2764               END IF
 2765                     
 2766             END IF ! GTYPE
 2767 !
 2768           END IF ! ILOOP .EQ. 2
 2769 !
 2770 ! ... Branch back input / excluded points ( ILOOP in 8.b )
 2771 !
 2772         END DO
 2773 !
 2774         ELSE ! FROM .EQ. PART
 2775 !
 2776 ! 8.d Read the map from file instead
 2777 !
 2778           NSTAT  = -1
 2779           IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 2780           IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
 2781                                                   
 2782 !!Li  Suspended for SMC grid though the file input line in  ww3_grid.inp
 2783 !!Li  is kept to divert the program into this block.  JGLi15Oct2014
 2784 !!Li
 2785           IF( RGLGRD ) THEN
 2786 !!Li
 2787 !
 2788           WRITE (NDSO,978) NDSTR, IDLA, IDFT
 2789           IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
 2790           IF (FROM.EQ.'NAME') WRITE (NDSO,974) TNAME
 2791 !
 2792           IF ( NDSTR .EQ. NDSI ) THEN
 2793               IF ( IDFT .EQ. 3 ) THEN
 2794                   WRITE (NDSE,1004) NDSTR
 2795                   CALL EXTCDE (23)
 2796                 ELSE
 2797                   CALL NEXTLN ( COMSTR , NDSI , NDSE )
 2798                 END IF
 2799             ELSE
 2800               IF ( IDFT .EQ. 3 ) THEN
 2801                   IF (FROM.EQ.'NAME') THEN
 2802                       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
 2803                             FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
 2804                             IOSTAT=IERR)
 2805                     ELSE
 
 
                                                         ww3_grid.F90  page  52
 
 
 2806                       OPEN (NDSTR,           FORM='UNFORMATTED',      &
 2807                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2808                     END IF
 2809                 ELSE
 2810                   IF (FROM.EQ.'NAME') THEN
 2811                       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
 2812                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2813                     ELSE
 2814                       OPEN (NDSTR,                                    &
 2815                             STATUS='OLD',ERR=2000,IOSTAT=IERR)
 2816                     END IF
 2817                 END IF
 2818             END IF
 2819 !
 2820           ALLOCATE ( READMP(NX,NY) )
 2821           CALL INA2I ( READMP, NX, NY, 1, NX, 1, NY, NDSTR, NDST,    &
 2822                        NDSE, IDFT, RFORM, IDLA, 1, 0 )
 2823 !
 2824           IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
 2825               DO IY=2, NY-1
 2826                 IF ( READMP( 1,IY) .EQ. 1 ) READMP( 1,IY) = 3
 2827                 IF ( READMP(NX,IY) .EQ. 1 ) READMP(NX,IY) = 3
 2828                 END DO
 2829             END IF
 2830 !
 2831           DO IX=1, NX
 2832             IF ( READMP(IX, 1) .EQ. 1 ) READMP(IX, 1) = 3
 2833             IF ( READMP(IX,NY) .EQ. 1 .AND. ICLOSE .NE. ICLOSE_TRPL)   &
 2834                READMP(IX,NY) = 3
 2835             END DO
 2836 !
 2837           DO IY=1, NY
 2838             DO IX=1, NX
 2839               IF ( READMP(IX,IY) .EQ. 3 ) THEN
 2840                   TMPSTA(IY,IX) = NSTAT
 2841                 ELSE
 2842                   TMPSTA(IY,IX) = READMP(IX,IY)
 2843                   ! force to dry the sea points over zlim
 2844                   IF ( ZBIN(IX,IY) .GT. ZLIM ) TMPSTA(IY,IX) = 0
 2845                 END IF
 2846               END DO
 2847             END DO
 2848           DEALLOCATE ( READMP )
 2849 !!Li
 2850           ENDIF   !! RGLGRD
 2851 !!Li
 2852 !
 2853         END IF !FROM .NE. 'PART'
 2854 !
 2855 ! 8.e Get NSEA and other counters
 2856 !
 2857       NSEA   = 0
 2858       NLAND  = 0
 2859       NBI    = 0
 2860       NBT    = 0
 
 
                                                         ww3_grid.F90  page  53
 
 
 2861 !
 2862       DO IX=1, NX
 2863         DO IY=1, NY
 2864           IF ( TMPSTA(IY,IX) .GT. 0 ) NSEA   = NSEA + 1
 2865           IF ( TMPSTA(IY,IX) .EQ. 0 ) NLAND  = NLAND + 1
 2866           IF ( TMPSTA(IY,IX) .LT. 0 ) NBT    = NBT + 1
 2867           IF ( TMPSTA(IY,IX) .EQ. 2 ) NBI    = NBI + 1
 2868           END DO
 2869         END DO
 2870 !
 2871       WRITE (NDSO,980)
 2872       FLBPI  = NBI .GT. 0
 2873       IF ( .NOT. FLBPI ) THEN
 2874           WRITE (NDSO,985)
 2875         ELSE
 2876           WRITE (NDSO,986) NBI
 2877           IF ( FLAGLL ) THEN
 2878               WRITE (NDSO, 987)
 2879             ELSE
 2880               WRITE (NDSO,1987)
 2881             END IF
 2882           IBI    = 1
 2883           DO IY=1, NY
 2884             DO IX=1, NX
 2885               IF (GTYPE.NE.UNGTYPE) THEN
 2886                 X = FACTOR * ( XGRDIN(IX,IY) )
 2887                 Y = FACTOR * ( YGRDIN(IX,IY) )
 2888               ELSE
 2889                 X = FACTOR * XYB(IX,1)
 2890                 Y = FACTOR * XYB(IX,2)
 2891                 END IF
 2892             IF ( TMPSTA(IY,IX).EQ.2 ) THEN
 2893                   IF ( FLAGLL ) THEN
 2894                       WRITE (NDSO, 988) IBI, IX, IY, X, Y
 2895                     ELSE
 2896                       WRITE (NDSO,1988) IBI, IX, IY, X, Y
 2897                     END IF
 2898                   IBI    = IBI + 1
 2899                 END IF
 2900               END DO
 2901             END DO
 2902         END IF
 2903 !
 2904       WRITE (NDSO,1980)
 2905       IF ( NBT .EQ. 0 ) THEN
 2906           WRITE (NDSO,1985)
 2907         ELSE
 2908           WRITE (NDSO,1986) NBT
 2909         END IF
 2910 !
 2911 ! 8.f Set up all maps
 2912 !
 2913 !!Li  CALL W3DIMX ( 1, NX, NY, NSEA, NDSE, NDST )
 2914       CALL W3DIMX ( 1, NX, NY, NSEA, NDSE, NDST  &
 2915                   )
 
 
                                                         ww3_grid.F90  page  54
 
 
 2916 !
 2917 ! 8.g Activation of reflections and scattering
 2918       FFACBERG=FACBERG
 2919       REFPARS(1)=REFCOAST
 2920       REFPARS(2)=REFSUBGRID
 2921       REFPARS(3)=REFUNSTSOURCE
 2922       REFPARS(4)=REFICEBERG
 2923       REFPARS(6)=REFFREQ
 2924       REFPARS(7)=REFSLOPE
 2925       REFPARS(8)=REFCOSP_STRAIGHT
 2926       REFPARS(9)=REFRMAX
 2927       REFPARS(10)=REFFREQPOW
 2928       IF (GTYPE.EQ.UNGTYPE) REFPARS(2:5)=0.
 2929       IF (REFMAP.EQ.0) THEN
 2930         REFLC(3,:)=REFPARS(7)
 2931         END IF
 2932               
 2933               
 2934       IF (GTYPE.NE.UNGTYPE) THEN
 2935         DO IY=1, NY
 2936           DO IX=1, NX
 2937             XGRD(IY,IX) = XGRDIN(IX,IY)
 2938             YGRD(IY,IX) = YGRDIN(IX,IY)
 2939             END DO
 2940           END DO
 2941           DEALLOCATE ( XGRDIN, YGRDIN )
 2942           CALL W3GNTX ( 1, 6, 6 )
 2943       ELSE
 2944 !
 2945 !FA:  This distinction  between structured and unstructured
 2946 ! should be removed when XYB is replaced by XGRD and YGRD
 2947 !
 2948         DO IX=1, NX
 2949           XGRD(:,IX) = XYB(IX,1)
 2950           YGRD(:,IX) = XYB(IX,2)
 2951           END DO
 2952         END IF   ! GTYPE
 2953 !
 2954 !!Li  MAPSTA = TMPSTA
 2955 !!Li  Shelter MAPSTA LLG definition for SMC by RGLGRD.
 2956       IF( RGLGRD ) MAPSTA = TMPSTA
 2957       MAPFS  = 0
 2958 !
 2959       TRNX   = 0.
 2960       TRNY   = 0.
 2961 !
 2962 !Li  Shelter MAPSTA etc LLG definitions for SMC by logical RGLGRD !
 2963 !AR  This is only .FALSE. for SMC ..
 2964       IF( RGLGRD ) THEN
 2965       ISEA   = 0
 2966       DO IY=1, NY
 2967         DO IX=1, NX
 2968           IF ( TMPSTA(IY,IX) .EQ. NSTAT ) THEN
 2969               MAPSTA(IY,IX) = 0
 2970               MAPST2(IY,IX) = 1
 
 
                                                         ww3_grid.F90  page  55
 
 
 2971               TMPSTA(IY,IX) = 3
 2972             ELSE
 2973               MAPSTA(IY,IX) = TMPSTA(IY,IX)
 2974               MAPST2(IY,IX) = 0
 2975             END IF
 2976           IF ( MAPSTA(IY,IX) .NE. 0 ) THEN
 2977               ISEA           = ISEA + 1
 2978               MAPFS (IY,IX)  = ISEA
 2979               ZB(ISEA)       = ZBIN(IX,IY)
 2980               MAPSF(ISEA,1)  = IX
 2981               MAPSF(ISEA,2)  = IY
 2982               IF ( FLAGLL ) THEN
 2983                   Y              = YGRD(IY,IX)
 2984                   CLATS(ISEA)    = COS(Y*DERA)
 2985                   CLATIS(ISEA)   = 1. / CLATS(ISEA)
 2986                   CTHG0S(ISEA)   = - TAN(DERA*Y) / RADIUS
 2987                 ELSE
 2988                   CLATS(ISEA)    = 1.
 2989                   CLATIS(ISEA)   = 1.
 2990                   CTHG0S(ISEA)   = 0.
 2991                 END IF
 2992             END IF
 2993                   
 2994 !/ ------------------------------------------------------------------- /
 2995                                                                         
 2996 ! notes: Oct 22 2012: I moved the following "if-then" statement from
 2997 ! inside the  "IF ( MAPSTA(IY,IX) .NE. 0 )" statement to outside that
 2998 ! statement. This is needed since later on, ATRNX is computed from
 2999 ! TRNX(ix-1) , TRNX(ix) etc. which causes boundary effects if the
 3000 ! MAPSTA=0 values are set to TRNX=0
 3001                                    
 3002               IF ( TRFLAG .NE. 0 ) THEN
 3003                   TRNX(IY,IX) = 1. - OBSX(IX,IY)
 3004                   TRNY(IY,IX) = 1. - OBSY(IX,IY)
 3005                 END IF
 3006                       
 3007           END DO
 3008         END DO
 3009       ENDIF
 3010 !!Li End of RGLGRD IF block
 3011 !
 3012       DO ISP=1, NSPEC+NTH
 3013         MAPWN(ISP) = 1 + (ISP-1)/NTH
 3014         MAPTH(ISP) = 1 + MOD(ISP-1,NTH)
 3015         END DO
 3016 !
 3017       NMAP   = 1 + (NX-1)/NCOL
 3018       WRITE (NDSO,1100) NMAP
 3019       DO IMAP=1, NMAP
 3020         IX0    = 1 + (IMAP-1)*NCOL
 3021         IXN    = MIN ( NX , IMAP*NCOL )
 3022         DO IY=NY,1,-1
 3023           WRITE (NDSO,1101) (TMPSTA(IY,IX),IX=IX0,IXN)
 3024           END DO
 3025         WRITE (NDSO,*) ' '
 
 
                                                         ww3_grid.F90  page  56
 
 
 3026         END DO
 3027       WRITE (NDSO,1102)
 3028                        
 3029       OPEN (NDSM,FILE=TRIM(FNMPRE)//'mask.ww3')
 3030       DO IY=1, NY
 3031         WRITE (NDSM,998) MIN(1,MAPSTA(IY,:))
 3032         END DO
 3033       CLOSE (NDSM)
 3034 !
 3035       IF ( TRFLAG .GT. 0 ) THEN
 3036           NMAPB  = 1 + (NX-1)/NCOL
 3037           WRITE (NDSO,1103) 'X', NMAPB
 3038           DO IMAPB=1, NMAPB
 3039             IX0    = 1 + (IMAPB-1)*NCOL
 3040             IXN    = MIN ( NX , IMAPB*NCOL )
 3041             DO IY=NY,1,-1
 3042               WRITE (NDSO,1101) (NINT(10.*OBSX(IX,IY)),IX=IX0,IXN)
 3043               END DO
 3044             WRITE (NDSO,*) ' '
 3045             END DO
 3046           WRITE (NDSO,1104)
 3047           WRITE (NDSO,1103) 'Y', NMAPB
 3048           DO IMAPB=1, NMAPB
 3049             IX0    = 1 + (IMAPB-1)*NCOL
 3050             IXN    = MIN ( NX , IMAPB*NCOL )
 3051             DO IY=NY,1,-1
 3052               WRITE (NDSO,1101) (NINT(10.*OBSY(IX,IY)),IX=IX0,IXN)
 3053               END DO
 3054             WRITE (NDSO,*) ' '
 3055             END DO
 3056           WRITE (NDSO,1104)
 3057         END IF
 3058 !
 3059       OPEN (NDSM,FILE=TRIM(FNMPRE)//'mapsta.ww3', RECL=2*NX*NY*50+1)
 3060       DO IY=NY,1, -1
 3061         DO IX=1,NX
 3062           DO I=1,50
 3063             WRITE (NDSM,1998,ADVANCE='NO') (TMPSTA(IY,IX))
 3064            END DO
 3065          END DO
 3066         END DO
 3067       CLOSE (NDSM)
 3068 !
 3069  
 3070        IGPARS(1)=IGMETHOD
 3071        IGPARS(2)=IGADDOUTP
 3072        IGPARS(3)=IGSOURCE
 3073        IGPARS(4)=0
 3074        IF (IGBCOVERWRITE) IGPARS(4)=IGPARS(4)+1
 3075        IF (IGSWELLMAX) IGPARS(4)=IGPARS(4)+2
 3076        IGPARS(5)=1
 3077        DO IK=1,NK
 3078          IF (SIG(IK)*TPIINV.LT.IGMAXFREQ) IGPARS(5)=IK
 3079          END DO
 3080        IGMINDEP=MINVAL(ZB*(-1.)-2)  ! -2 / +2 is there for water level ch>
 
 
                                                         ww3_grid.F90  page  57
 
 
 3081        IGMAXDEP=MAXVAL(ZB*(-1.)+2)
 3082        IF (IGSOURCEATBP.EQ.1)  IGMINDEP=1.   ! should use true minimum de>
 3083        IGPARS(6)=1+NINT(LOG(MAX(IGMAXDEP,1.0)/MAX(IGMINDEP,1.0))/LOG(1.1))
 3084        IGPARS(7)=MAX(IGMINDEP,1.0)
 3085        IGPARS(8)=IGSOURCEATBP
 3086        IGPARS(9)=IGKDMIN
 3087        IGPARS(10)=IGFIXEDDEPTH
 3088        IGPARS(11)=IGEMPIRICAL**2
 3089        IGPARS(12)=IGSTERMS
 3090 !
 3091 ! 9.d Estimates shoreline direction for reflection
 3092 !     and shoreline treatment in general for UNST grids.
 3093 ! NB: this is updated with moving water levels in W3ULEV
 3094 ! AR: this is not anymore needed and will be deleted ...
 3095 !
 3096       IF (GTYPE.EQ.UNGTYPE) THEN
 3097         CALL SETUGIOBP
 3098       ELSE
 3099         CALL W3SETREF
 3100         END IF
 3101       ALLOCATE ( REFD(NX,NY), REFD2(NX,NY), REFS(NX,NY) )
 3102       IF (REFMAP.EQ.0) THEN
 3103         REFS(:,:)=1.
 3104       ELSE
 3105         IF (FLGNML) THEN
 3106           NDSTR = NML_SLOPE%IDF
 3107           VSC = NML_SLOPE%SF
 3108           IDLA = NML_SLOPE%IDLA
 3109           IDFT = NML_SLOPE%IDFM
 3110           RFORM = TRIM(NML_SLOPE%FORMAT)
 3111           FROM = TRIM(NML_SLOPE%FROM)
 3112           TNAME = TRIM(NML_SLOPE%FILENAME)
 3113         ELSE
 3114           CALL NEXTLN ( COMSTR , NDSI , NDSE )
 3115           READ (NDSI,*,END=2001,ERR=2002) NDSTR, VSC, IDLA, IDFT, RFORM, &
 3116                                             FROM, TNAME
 3117         END IF
 3118         IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
 3119         IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
 3120         IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
 3121         WRITE (NDSO,1977) NDSTR, VSC, IDLA, IDFT
 3122         IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
 3123         IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSTR) WRITE (NDSO,974) TNAME
 3124         IF ( NDSTR .EQ. NDSI ) THEN
 3125           IF ( IDFT .EQ. 3 ) THEN
 3126             WRITE (NDSE,1004) NDSTR
 3127             CALL EXTCDE (23)
 3128           ELSE
 3129             CALL NEXTLN ( COMSTR , NDSI , NDSE )
 3130             END IF
 3131         ELSE IF ( NDSTR .EQ. NDSG ) THEN
 3132           IF ( ( IDFM.EQ.3 .AND. IDFT.NE.3 ) .OR.                 &
 3133                ( IDFM.NE.3 .AND. IDFT.EQ.3 ) ) THEN
 3134             WRITE (NDSE,1005) IDFM, IDFT
 3135             CALL EXTCDE (24)
 
 
                                                         ww3_grid.F90  page  58
 
 
 3136             END IF
 3137           ELSE
 3138             IF ( IDFT .EQ. 3 ) THEN
 3139               IF (FROM.EQ.'NAME') THEN
 3140                 OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,                 &
 3141                         FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
 3142                         IOSTAT=IERR)
 3143               ELSE
 3144                 OPEN (NDSTR,           FORM='UNFORMATTED',      &
 3145                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 3146                 END IF
 3147             ELSE
 3148               IF (FROM.EQ.'NAME') THEN
 3149                 OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
 3150                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 3151               ELSE
 3152                 OPEN (NDSTR,                                    &
 3153                     STATUS='OLD',ERR=2000,IOSTAT=IERR)
 3154                 END IF   !end  of (FROM.EQ.'NAME')
 3155               END IF     !end of ( IDFT .EQ. 3 )
 3156             END IF       !end of ( NDSTR .EQ. NDSG )
 3157           IF ( NDSTR .EQ. NDSI ) CALL NEXTLN ( COMSTR , NDSI , NDSE )
 3158           CALL INA2R ( REFS, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
 3159                        IDFM, RFORM, IDLA, VSC, 0.0)
 3160           DO ISEA=1,NSEA
 3161             IX = MAPSF(ISEA,1)
 3162             IY = MAPSF(ISEA,2)
 3163             REFLC(3,ISEA) = REFS(IX,IY)*REFMAP
 3164             END DO
 3165 !
 3166           NMAPB  = 1 + (NX-1)/NCOL
 3167           WRITE (NDSO,1105) NMAPB
 3168 !
 3169           DO IMAPB=1, NMAPB
 3170             IX0    = 1 + (IMAPB-1)*NCOL
 3171             IXN    = MIN ( NX , IMAPB*NCOL )
 3172             WRITE (NDSO,*) ' '
 3173             END DO
 3174           WRITE (NDSO,1106)
 3175           WRITE (NDSO,*)
 3176           END IF         !end of (REFMAP.EQ.0)
 3177 !
 3178       DEALLOCATE ( ZBIN, TMPSTA, TMPMAP )
 3179 !
 3180 ! 9.e Reads bottom information from file
 3181 !
 3182  
 3183  
 3184 !
 3185 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 3186 ! 10.  Prepare output boundary points.
 3187 !     ILOOP = 1 to count NFBPO and NBO
 3188 !     ILOOP = 2 to fill data arrays
 3189 !
 3190       WRITE (NDSO,990)
 
 
                                                         ww3_grid.F90  page  59
 
 
 3191       IF ( .NOT. FLGNML ) &
 3192         OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_grid.scratch',FORM='FORMATTED')
 3193 !
 3194       DO ILOOP = 1, 2
 3195 !
 3196         IF ( ILOOP.EQ.2 ) CALL W3DMO5 ( 1, NDST, NDSE, 2 )
 3197 !
 3198         I = 1
 3199         NBOTOT = 0
 3200         NFBPO  = 0
 3201         NBO(0) = 0
 3202         NBO2(0)= 0
 3203         FIRST  = .TRUE.
 3204         IF ( .NOT. FLGNML ) THEN
 3205           REWIND (NDSS)
 3206           IF ( ILOOP .EQ. 1 ) THEN
 3207             NDSI2 = NDSI
 3208           ELSE
 3209             NDSI2 = NDSS
 3210           END IF
 3211         END IF
 3212 !
 3213         DO
 3214           IF (FLGNML) THEN
 3215             ! outbound lines
 3216             IF (NML_OUTBND_COUNT%N_LINE.GT.0 .AND. I.LE.NML_OUTBND_COUNT%>
 3217               XO0 = NML_OUTBND_LINE(I)%X0
 3218               YO0 = NML_OUTBND_LINE(I)%Y0
 3219               DXO = NML_OUTBND_LINE(I)%DX
 3220               DYO = NML_OUTBND_LINE(I)%DY
 3221               NPO = NML_OUTBND_LINE(I)%NP
 3222               I=I+1
 3223             ELSE
 3224               NPO=0
 3225             END IF
 3226           ELSE
 3227             CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
 3228             READ (NDSI2,*,END=2001,ERR=2002) XO0, YO0, DXO, DYO, NPO
 3229           END IF
 3230 !
 3231           IF ( .NOT. FLGNML .AND. ILOOP .EQ. 1 ) THEN
 3232             BACKSPACE (NDSI)
 3233             READ (NDSI,'(A)') LINE
 3234             WRITE (NDSS,'(A)') LINE
 3235           END IF
 3236 !
 3237 ! ... Check if new file to be used
 3238 !
 3239           FIRST  = FIRST .OR. NPO.LE.0
 3240           NPO    = ABS(NPO)
 3241 !
 3242 ! ... Preparations for new output file including end check
 3243 !     and output for last output file
 3244 !
 3245           IF ( FIRST ) THEN
 
 
                                                         ww3_grid.F90  page  60
 
 
 3246 !
 3247               FIRST  = .FALSE.
 3248 !
 3249               IF ( NFBPO.GE.1 .AND. ILOOP.EQ.2 ) THEN
 3250                   WRITE (NDSO,991)  NFBPO, NBO(NFBPO) - NBO(NFBPO-1), &
 3251                                           NBO2(NFBPO) - NBO2(NFBPO-1)
 3252                   IF ( NBO(NFBPO) - NBO(NFBPO-1) .EQ. 1 ) THEN
 3253                       IF ( FLAGLL ) THEN
 3254                           WRITE (NDSO,992)
 3255                         ELSE
 3256                           WRITE (NDSO,2992)
 3257                         END IF
 3258                     ELSE
 3259                       IF ( FLAGLL ) THEN
 3260                           WRITE (NDSO,1992)
 3261                         ELSE
 3262                           WRITE (NDSO,3992)
 3263                         END IF
 3264                     END IF
 3265                   IP0    = NBO(NFBPO-1)+1
 3266                   IPN    = NBO(NFBPO)
 3267                   IPH    = IP0 + (IPN-IP0-1)/2
 3268                   IPI    = IPH -IP0 + 1 + MOD(IPN-IP0+1,2)
 3269                   DO IP=IP0, IPH
 3270                     IF ( FLAGLL ) THEN
 3271                         WRITE (NDSO,1993) IP-NBO(NFBPO-1),     &
 3272                                           FACTOR*XBPO(IP),     &
 3273                                           FACTOR*YBPO(IP),     &
 3274                                           IP+IPI-NBO(NFBPO-1), &
 3275                                           FACTOR*XBPO(IP+IPI), &
 3276                                           FACTOR*YBPO(IP+IPI)
 3277                       ELSE
 3278                         WRITE (NDSO,3993) IP-NBO(NFBPO-1),     &
 3279                                           FACTOR*XBPO(IP),     &
 3280                                           FACTOR*YBPO(IP),     &
 3281                                           IP+IPI-NBO(NFBPO-1), &
 3282                                           FACTOR*XBPO(IP+IPI), &
 3283                                           FACTOR*YBPO(IP+IPI)
 3284                       END IF
 3285                     END DO
 3286                   IF ( MOD(IPN-IP0+1,2) .EQ. 1 ) THEN
 3287                       IF ( FLAGLL ) THEN
 3288                           WRITE (NDSO, 993) IPH+1-NBO(NFBPO-1), &
 3289                                             FACTOR*XBPO(IPH+1), &
 3290                                             FACTOR*YBPO(IPH+1)
 3291                         ELSE
 3292                           WRITE (NDSO,2993) IPH+1-NBO(NFBPO-1), &
 3293                                             FACTOR*XBPO(IPH+1), &
 3294                                             FACTOR*YBPO(IPH+1)
 3295                         END IF
 3296                     END IF
 3297                   WRITE (NDSO,*)
 3298                 END IF
 3299 !
 3300               IF ( NPO .EQ. 0 ) EXIT
 
 
                                                         ww3_grid.F90  page  61
 
 
 3301 !
 3302               NFBPO  = NFBPO + 1
 3303               IF ( NFBPO .GT. 9 ) THEN
 3304                   WRITE (NDSE,1006)
 3305                   CALL EXTCDE ( 50 )
 3306                 END IF
 3307               NBO2(NFBPO) = NBO2(NFBPO-1)
 3308               NBO(NFBPO) = NBOTOT
 3309 !
 3310             END IF
 3311 !
 3312 ! ... Loop over line segment - - - - - - - - - - - - - - - - - - - - -
 3313 !
 3314           DO IP=1, NPO
 3315 !
 3316             XO     = XO0 + REAL(IP-1)*DXO
 3317             YO     = YO0 + REAL(IP-1)*DYO
 3318 !
 3319 ! ... Compute bilinear remapping weights
 3320 !
 3321             INGRID = W3GRMP( GSU, XO, YO, IXR, IYR, RD )
 3322 !
 3323 !           Change cell-corners from counter-clockwise to column-major or>
 3324             IX     = IXR(3);  IY     = IYR(3);  X     = RD(3);
 3325             IXR(3) = IXR(4);  IYR(3) = IYR(4);  RD(3) = RD(4);
 3326             IXR(4) = IX    ;  IYR(4) = IY    ;  RD(4) = X    ;
 3327 !
 3328 ! ... Check if point in grid
 3329 !
 3330             IF ( INGRID ) THEN
 3331 !
 3332 ! ... Check if point not on land
 3333 !
 3334               IF ( ( MAPSTA(IYR(1),IXR(1)).GT.0 .AND.                 &
 3335                                         RD(1).GT.0.05 ) .OR.          &
 3336                    ( MAPSTA(IYR(2),IXR(2)).GT.0 .AND.                 &
 3337                                         RD(2).GT.0.05 ) .OR.          &
 3338                    ( MAPSTA(IYR(3),IXR(3)).GT.0 .AND.                 &
 3339                                         RD(3).GT.0.05 ) .OR.          &
 3340                    ( MAPSTA(IYR(4),IXR(4)).GT.0 .AND.                 &
 3341                                         RD(4).GT.0.05 ) ) THEN
 3342 !
 3343 ! ... Check storage and store coordinates
 3344 !
 3345                 NBOTOT = NBOTOT + 1
 3346                 IF ( ILOOP .EQ. 1 ) CYCLE
 3347 !
 3348                 XBPO(NBOTOT) = XO
 3349                 YBPO(NBOTOT) = YO
 3350 !
 3351 ! ... Interpolation factors
 3352 !
 3353                 RDTOT = 0.
 3354                 DO J=1, 4
 3355                   IF ( MAPSTA(IYR(J),IXR(J)).GT.0 .AND.               &
 
 
                                                         ww3_grid.F90  page  62
 
 
 3356                                             RD(J).GT.0.05 ) THEN
 3357                       RDBPO(NBOTOT,J) = RD(J)
 3358                     ELSE
 3359                       RDBPO(NBOTOT,J) = 0.
 3360                     END IF
 3361                     RDTOT = RDTOT + RDBPO(NBOTOT,J)
 3362                   END DO
 3363 !
 3364                 DO J=1, 4
 3365                   RDBPO(NBOTOT,J) = RDBPO(NBOTOT,J) / RDTOT
 3366                   END DO
 3367 !
 3368 ! ... Determine sea and interpolation point counters
 3369 !
 3370                 DO J=1, 4
 3371                   ISEAI(J) = MAPFS(IYR(J),IXR(J))
 3372                   END DO
 3373 !
 3374                 DO J=1, 4
 3375                   IF ( ISEAI(J).EQ.0 .OR. RDBPO(NBOTOT,J).EQ. 0. ) THEN
 3376                       IPBPO(NBOTOT,J) = 0
 3377                     ELSE
 3378                       FLNEW   = .TRUE.
 3379                       DO IST=NBO2(NFBPO-1)+1, NBO2(NFBPO)
 3380                         IF ( ISEAI(J) .EQ. ISBPO(IST) ) THEN
 3381                             FLNEW  = .FALSE.
 3382                             IPBPO(NBOTOT,J) = IST - NBO2(NFBPO-1)
 3383                           END IF
 3384                         END DO
 3385                       IF ( FLNEW ) THEN
 3386                           NBO2(NFBPO)        = NBO2(NFBPO) + 1
 3387                           IPBPO(NBOTOT,J)    = NBO2(NFBPO) - NBO2(NFBPO-1)
 3388                           ISBPO(NBO2(NFBPO)) = ISEAI(J)
 3389                         END IF
 3390                     END IF
 3391                   END DO
 3392 !
 3393 ! ... Error output
 3394 !
 3395                 ELSE
 3396                   IF ( FLAGLL ) THEN
 3397                       WRITE (NDSE,2995) FACTOR*XO, FACTOR*YO
 3398                     ELSE
 3399                       WRITE (NDSE,995) FACTOR*XO, FACTOR*YO
 3400                     END IF
 3401                 END IF
 3402               ELSE
 3403                 IF ( FLAGLL ) THEN
 3404                     WRITE (NDSE,2994) FACTOR*XO, FACTOR*YO
 3405                   ELSE
 3406                     WRITE (NDSE,994) FACTOR*XO, FACTOR*YO
 3407                   END IF
 3408               END IF
 3409 !
 3410             END DO
 
 
                                                         ww3_grid.F90  page  63
 
 
 3411 !
 3412           NBO(NFBPO) = NBOTOT
 3413 !
 3414 ! ... Branch back to read.
 3415 !
 3416           END DO
 3417 !
 3418 ! ... End of ILOOP loop
 3419 !
 3420         END DO
 3421 !
 3422       IF ( .NOT. FLGNML ) CLOSE ( NDSS, STATUS='DELETE' )
 3423 !
 3424       FLBPO  = NBOTOT .GT. 0
 3425       IF ( .NOT. FLBPO ) THEN
 3426           WRITE (NDSO,996)
 3427         ELSE
 3428           WRITE (NDSO,997) NBOTOT, NBO2(NFBPO)
 3429         END IF
 3430 !
 3431 !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 3432 !10.  Write model definition file.
 3433 !
 3434       WRITE (NDSO,999)
 3435       CALL W3IOGR ( 'WRITE', NDSM )
 3436 !
 3437       CLOSE (NDSM)
 3438 !
 3439       GOTO 2222
 3440 !
 3441 ! Escape locations read errors :
 3442 !
 3443  2000 CONTINUE
 3444       WRITE (NDSE,1000) IERR
 3445       CALL EXTCDE ( 60 )
 3446 !
 3447  2001 CONTINUE
 3448       WRITE (NDSE,1001)
 3449       CALL EXTCDE ( 61 )
 3450 !
 3451  2002 CONTINUE
 3452       WRITE (NDSE,1002) IERR
 3453       CALL EXTCDE ( 62 )
 3454 !
 3455  2003 CONTINUE
 3456       WRITE (NDSE,1003)
 3457       CALL EXTCDE ( 64 )
 3458 !
 3459  2222 CONTINUE
 3460       IF ( GTYPE .NE. UNGTYPE) THEN
 3461           IF ( NX*NY .NE. NSEA ) THEN
 3462               WRITE (NDSO,9997) NX, NY, NX*NY, NSEA,                     >
 3463                                 100.*REAL(NSEA)/REAL(NX*NY), NBI, NLAND, >
 3464             ELSE
 3465               WRITE (NDSO,9998) NX, NY, NX*NY, NSEA, NBI, NLAND, NBT
 
 
                                                         ww3_grid.F90  page  64
 
 
 3466             END IF
 3467         ELSE IF ( GTYPE .EQ. UNGTYPE ) THEN
 3468           IF ( NX*NY .NE. NSEA ) THEN
 3469               WRITE (NDSO,9997)  0,  0, NX*NY, NSEA,                     >
 3470                                 100.*REAL(NSEA)/REAL(NX*NY), NBI, NLAND, >
 3471             ELSE
 3472               WRITE (NDSO,9998)  0,  0, NX*NY, NSEA, NBI, NLAND, NBT
 3473             END IF
 3474         ENDIF ! GTYPE .EQ. UNGTYPE
 3475                                   
 3476      WRITE (NDSO,9999)
 3477                       
 3478                       
 3479                       
 3480                       
 3481                       
 3482                       
 3483                       
 3484                       
 3485                       
 3486                       
 3487 !
 3488 ! Formats
 3489 !
 3490   900 FORMAT (/15X,'    *** WAVEWATCH III Grid preprocessor ***    '/ &
 3491                15X,'==============================================='/)
 3492   901 FORMAT ( '  Comment character is ''',A,''''/)
 3493   902 FORMAT ( '  Grid name : ',A/)
 3494   903 FORMAT (/'  Spectral discretization : '/                        &
 3495                ' --------------------------------------------------'/ &
 3496                '       Number of directions        :',I4/             &
 3497                '       Directional increment (deg.):',F6.1)
 3498   904 FORMAT ( '       First direction       (deg.):',F6.1)
 3499   905 FORMAT ( '       Number of frequencies       :',I4/             &
 3500                '       Frequency range        (Hz) :',F9.4,'-',F6.4/  &
 3501                '       Increment factor            :',F8.3/)
 3502 !
 3503   910 FORMAT (/'  Model definition :'/                                &
 3504                ' --------------------------------------------------')
 3505   911 FORMAT ( '       Dry run (no calculations)   :  ',A/            &
 3506                '       Propagation in X-direction  :  ',A/            &
 3507                '       Propagation in Y-direction  :  ',A/            &
 3508                '       Refraction                  :  ',A/            &
 3509                '       Current-induced k-shift     :  ',A/            &
 3510                '       Source term calc. and int.  :  ',A/)
 3511   912 FORMAT (/'  Time steps : '/                                     &
 3512                ' --------------------------------------------------'/ &
 3513                '       Maximum global time step      (s) :',F8.2/     &
 3514                '       Maximum CFL time step X-Y     (s) :',F8.2/     &
 3515                '       Maximum CFL time step k-theta (s) :',F8.2/     &
 3516                '       Minimum source term time step (s) :',F8.2/)
 3517   913 FORMAT (/ '  WARNING, TIME STEP LESS THAN 1 s, NITER:',I8 /)
 3518   915 FORMAT ( '  Preprocessing namelists ...')
 3519   916 FORMAT ( '  Preprocessing namelists finished.'/)
 3520   917 FORMAT (/'  Equivalent namelists ...'/)
 
 
                                                         ww3_grid.F90  page  65
 
 
 3521   918 FORMAT (/'  Equivalent namelists finished.'/)
 3522 !
 3523   820 FORMAT (/'  Linear input (C&M-R 82) ',A/                   &
 3524         ' --------------------------------------------------')
 3525   821 FORMAT ( '       CLIN                        :',f8.2/      &
 3526                '       Factor for fPM in filter    :',F8.2/      &
 3527                '       Factor for fh in filter     :',F8.2/)
 3528  2820 FORMAT ( '  &SLN1 CLIN =',F6.1,', RFPM =',F6.2,            &
 3529                ', RFHF =',F6.2,' /')
 3530 !
 3531   920 FORMAT (/'  Wind input (WAM 4+) ',A/                            &
 3532         ' --------------------------------------------------')
 3533   921 FORMAT ( '       minimum Charnock coeff.     :',F10.4/          &
 3534                '       betamax                     :',F9.3/           &
 3535                '       power of cos. in wind input :',F9.3/           &
 3536                '       z0max                       :',F9.3/           &
 3537                '       zalp                        :',F9.3/           &
 3538                '       Height of input wind (m)    :',F8.2/           &
 3539                '       wind stress sheltering      :',F9.3/           &
 3540                '       swell attenuation param.    :',I5/             &
 3541                '       swell attenuation factor    :',F9.3/           &
 3542                '       swell attenuation factor2   :',F9.3/           &
 3543                '       swell attenuation factor3   :',F9.3/           &
 3544                '       critical Reynolds number    :',F9.1/           &
 3545                '       swell attenuation factor5   :',F9.3/           &
 3546                '       swell attenuation factor6   :',F9.3/           &
 3547                '       swell attenuation factor7   :',F14.3/          &
 3548                '       ratio of z0 for orb. & mean :',F9.3/)
 3549  2920 FORMAT ( '  &SIN4 ZWND =',F5.1,', ALPHA0 =',F8.5,', Z0MAX =',F8.5,'>
 3550                   F8.5,','/                                           &
 3551               '        SINTHP =',F8.5,', ZALP =',F8.5,', TAUWSHELTER =',F>
 3552               ', SWELLFPAR =',I2,','/                                 &
 3553               '        SWELLF =',F8.5,', SWELLF2 =',F8.5,             &
 3554               ', SWELLF3 =',F8.5,', SWELLF4 =',F9.1,','/              &
 3555               '        SWELLF5 =',F8.5,', SWELLF6 =',F8.5,            &
 3556               ', SWELLF7 =',F12.2,', Z0RAT =',F8.5,', SINBR =',F8.5,'  /')
 3557 !
 3558   922 FORMAT (/'  Nonlinear interactions (DIA) ',A/                   &
 3559                ' --------------------------------------------------')
 3560   923 FORMAT ( '       Lambda                      :',F8.2/      &
 3561                '       Prop. constant              :',E10.3/     &
 3562                '       kd conversion factor        :',F8.2/      &
 3563                '       minimum kd                  :',F8.2/      &
 3564                '       shallow water constants     :',F8.2,2F6.2/)
 3565  2922 FORMAT ( '  &SNL1 LAMBDA =',F7.3,', NLPROP =',E10.3,       &
 3566                ', KDCONV =',F7.3,', KDMIN =',F7.3,','/           &
 3567                '        SNLCS1 =',F7.3,', SNLCS2 =',F7.3,        &
 3568                ', SNLCS3 = ',F7.3,' /')
 3569 !
 3570   924 FORMAT (/' Dissipation (Ardhuin / Filipot / Romero ) ',A/          &
 3571         ' --------------------------------------------------')
 3572   925 FORMAT ( '       SDSC2, SDSBCK, SDSCUM       :',3E11.3/    &
 3573                '       Power of k in mean k        :',F8.2/)
 3574                                                             
 3575                                                             
 
 
                                                         ww3_grid.F90  page  66
 
 
 3576  2924 FORMAT ( '  &SDS4 SDSBCHOICE = ',F3.1,                       &
 3577                ', SDSC2 =',E12.4,', SDSCUM =',F6.2,', '/         &
 3578                '        SDSC4 =',F6.2,', SDSC5 =',E12.4,         &
 3579                ', SDSC6 =',E12.4,','/                            &
 3580                '        WNMEANP =',F4.2,', FXPM3 =', F4.2,       &
 3581                ', FXFM3 =',F4.1,', FXFMAGE =',F6.3, ', '/       &
 3582                '        SDSBINT =',E12.4,', SDSBCK =',E12.4,     &
 3583                ', SDSABK =',F6.3,', SDSPBK =',F6.3,', '/         &
 3584                '        SDSHCK =',F5.2,', SDSBR = ',E12.4,       &
 3585                ', SDSSTRAIN =',F5.1,', SDSSTRAINA =',F4.1,       &
 3586                ', SDSSTRAIN2 =',F5.1,', '/                       &
 3587                '        SDSBT =',F5.2,', SDSP =',F5.2,          &
 3588                ', SDSISO =',I2, &
 3589                ', SDSCOS =',F3.1,', SDSDTH =',F5.1,', '/         &
 3590                '        SDSBRF1 = ',F5.2,', SDSBRFDF =',I2,', '/ &
 3591                '        SDSBM0 = ',F5.2, ', SDSBM1 =',F5.2,      &
 3592                ', SDSBM2 =',F5.2,', SDSBM3 =',F5.2,', SDSBM4 =', &
 3593                F5.2,', '/,                                       &
 3594                '        SPMSS = ',F5.2, ', SDKOF =',F5.2,        &
 3595                ', SDSMWD =',F5.2,', SDSFACMTF =',F5.1,', '/      &
 3596                '        SDSMWPOW =',F3.1,', SDSNMTF =', F5.2,    &
 3597                ', SDSCUMP =', F3.1,', SDSNUW =', E8.3,', '/,     &
 3598                '        WHITECAPWIDTH =',F5.2, ' WHITECAPDUR =',F5.2,' /')
 3599 !
 3600   926 FORMAT (/'  Bottom friction not defined.'/)
 3601 !
 3602   928 FORMAT (/'  Surf breaking not defined.'/)
 3603 !
 3604   930 FORMAT (/'  Triad interactions not defined.'/)
 3605 !
 3606   932 FORMAT (/'  Bottom scattering not defined.'/)
 3607 !
 3608   934 FORMAT (/'  Alternative source term slot not used.'/)
 3609 !
 3610  944 FORMAT  (/'  Ice scattering not defined.'/)
 3611 !
 3612   950 FORMAT (/'  Propagation scheme : '/                             &
 3613                ' --------------------------------------------------')
 3614   951 FORMAT ( '       Type of scheme (structured) :',1X,A)
 3615  2951 FORMAT ( '       Type of scheme(unstructured):',1X,A)
 3616  2952 FORMAT ( '             wave setup computation:',1X,A)
 3617   952 FORMAT ( '                                    ',1X,A)
 3618 !
 3619   953 FORMAT ( '       CFLmax depth refraction     :',F9.3/      &
 3620                '       Averaging area factor Cg    :',F8.2)
 3621   954 FORMAT ( '       Averaging area factor theta :',F8.2)
 3622   955 FORMAT ( '            **** Internal maximum .GE.',F6.2,' ****')
 3623  2953 FORMAT ( '  &PRO3 CFLTM =',F5.2,                           &
 3624                       ', WDTHCG = ',F4.2,', WDTHTH = ',F4.2,' /')
 3625 !
 3626  2956 FORMAT ( '  &UNST UGBCCFL =',L3,', UGOBCAUTO =',L3,             &
 3627                ', UGOBCDEPTH =', F8.3,', UGOBCFILE=',A,','/           &
 3628                ',  EXPFSN =',L3,',EXPFSPSI =',L3,                     &
 3629                ',  EXPFSFCT =', L3,',IMPFSN =',L3,',EXPTOTAL=',L3,    &
 3630                ',  IMPTOTAL=',L3,',IMPREFRACTION=', L3,               &
 
 
                                                         ww3_grid.F90  page  67
 
 
 3631                ',  IMPFREQSHIFT=', L3,', IMPSOURCE=', L3,             &
 3632                ',  SETUP_APPLY_WLV=', L3,                             &
 3633                ',  JGS_TERMINATE_MAXITER=', L3,                       &
 3634                ',  JGS_TERMINATE_DIFFERENCE=', L3,                    &
 3635                ',  JGS_TERMINATE_NORM=', L3,                          &
 3636                ',  JGS_LIMITER=', L3,                                 &
 3637                ',  JGS_USE_JACOBI=', L3,                              &
 3638                ',  JGS_BLOCK_GAUSS_SEIDEL=', L3,                      &
 3639                ',  JGS_MAXITER=', I5,                                 &
 3640                ',  JGS_PMIN=', F8.3,                                  &
 3641                ',  JGS_DIFF_THR=', F8.3,                              &
 3642                ',  JGS_NORM_THR=', F8.3,                              &
 3643                ',  JGS_NLEVEL=', I3,                                  &
 3644                ',  JGS_SOURCE_NONLINEAR=', L3 / )
 3645 !
 3646   960 FORMAT (/'  Miscellaneous ',A/                                   &
 3647                ' --------------------------------------------------')
 3648  2961 FORMAT ( ' *** WAVEWATCH-III WARNING IN W3GRID :'/               &
 3649                '     CICE0.NE.CICEN requires FLAGTR>2'/                &
 3650                '     Parameters corrected: CICE0 = CICEN'/)
 3651  2962 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID : User requests', &
 3652          'CICE0=CICEN corresponding to discontinuous treatment of ',   &
 3653          'ice, so we will change FLAGTR')
 3654  2963 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/               &
 3655                '     Ice physics used, so we will change FLAGTR.')
 3656   961 FORMAT ( '       Ice concentration cut-offs  :',F8.2,F6.2)
 3657  1972 FORMAT ( '       Compression of track output  : ',L3)
 3658   965 FORMAT (/'    Dynamic source term integration scheme :'/        &
 3659                '       Xp                      (-) :',F9.3/           &
 3660                '       Xr                      (-) :',F9.3/           &
 3661                '       Xfilt                   (-) :',F9.3)
 3662   966 FORMAT (/'    Wave field partitioning :'/                       &
 3663                '       Levels                  (-) :',I5/             &
 3664                '       Minimum wave height     (m) :',F9.3/           &
 3665                '       Wind area multiplier    (-) :',F9.3/           &
 3666                '       Cut-off wind sea fract. (-) :',F9.3/           &
 3667                '       Combine wind seas           :  ',A/            &
 3668                '       Number of swells in fld out :',I5)
 3669   967 FORMAT (/'    Miche-style limiting wave height :'/              &
 3670                '       Hs,max/d factor         (-) :',F9.3/           &
 3671                '       Hrms,max/d factor       (-) :',F9.3/           &
 3672                '       Limiter activated           :  ',A)
 3673   968 FORMAT ( '          *** FACTOR DANGEROUSLY LOW ***')
 3674  1973 FORMAT (/'    Calendar type                  :  ',A)
 3675 !
 3676   969 FORMAT (/'  Shoreline reflection     ',A/                  &
 3677                ' --------------------------------------------------')
 3678 !
 3679   970 FORMAT (/'  Second order and infragravity waves  ',A/      &
 3680                ' --------------------------------------------------')
 3681 !
 3682  5971 FORMAT ('       Partitioning method         :  ',A)
 3683  5972 FORMAT ('       Namelist options overridden :  ',A)
 3684 !
 3685  8972 FORMAT ( '       Wind input reduction factor in presence of ', &
 
 
                                                         ww3_grid.F90  page  68
 
 
 3686                /'         ice :',F6.2, &
 3687                /'         (0.0==> no reduction and 1.0==> no wind', &
 3688                /'         input with 100% ice cover)')
 3689 !
 3690  4970 FORMAT (/'  Spectral output on full grid ',A/                   &
 3691                ' --------------------------------------------------')
 3692  4971 FORMAT ( '       Second order pressure at K=0:',3I4)
 3693  4972 FORMAT ( '       Spectrum of Uss             :',3I4)
 3694  4973 FORMAT ( '       Frequency spectrum          :',3I4)
 3695  4974 FORMAT ( '       Partions of Uss             :',2I4)
 3696  4975 FORMAT ( '       Partition wavenumber #',I02,'   : ',1F6.3)
 3697                                                                  
 3698 !
 3699  4980 FORMAT (/'  Coastal / iceberg reflection  ',A/                   &
 3700                ' --------------------------------------------------')
 3701  4981 FORMAT ( '       Coefficient for shorelines  :',F6.4)
 3702  4989 FORMAT ( '          *** CURVLINEAR GRID: REFLECTION NOT IMPLEMENTED>
 3703  2977 FORMAT ( '  &SIG1  IGMETHOD =',I2,', IGADDOUTP =',I2,', IGSOURCE ='>
 3704                ', IGSTERMS = ',I2,', IGBCOVERWRITE =', L3,','/        &
 3705                '        IGSWELLMAX =', L3,', IGMAXFREQ =',F6.4,       &
 3706                ', IGSOURCEATBP = ',I2,', IGKDMIN = ',F6.4,','/        &
 3707                '        IGFIXEDDEPTH = ',F6.2,', IGEMPIRICAL = ',F8.6,' />
 3708 !
 3709  2978 FORMAT ( '  &SIC2  IC2DISPER =',L3,', IC2TURB =',F6.2,          &
 3710                ', IC2ROUGH  =',F10.6,','/                             &
 3711                '        IC2REYNOLDS = ',F10.1,', IC2SMOOTH = ',F10.1, &
 3712                ', IC2VISC =',F6.3,','/                                &
 3713                ',       IC2TURBS =',F8.2,', IC2DMAX =',F5.3,' /')
 3714 !
 3715  2979 FORMAT ( '  &SIC3 IC3MAXTHK =',F6.2, ', IC3MAXCNC =',F6.2,','/  &
 3716                '        IC2TURB =',F8.2,                              &
 3717                ', IC2ROUGH  =',F7.3,','/                              &
 3718                '        IC2REYNOLDS = ',F10.1,', IC2SMOOTH = ',F10.1, &
 3719                ', IC2VISC =',F10.3,','/                               &
 3720                '        IC2TURBS =',F8.2,', IC3CHENG =',L3,           &
 3721                ', USECGICE =',L3,', IC3HILIM = ',F6.2,','/            &
 3722                '        IC3KILIM = ',E9.2,', IC3HICE = ',E9.2,        &
 3723                ', IC3VISC = ',E9.2,','/                               &
 3724                '        IC3DENS = ',E9.2,', IC3ELAS = ',E9.2,' /')
 3725 !
 3726  2981 FORMAT ( '  &SIC5 IC5MINIG = ', E9.2, ', IC5MINWT = ', F5.2,    &
 3727                ', IC5MAXKRATIO = ', E9.2, ','/                        &
 3728                '        IC5MAXKI = ', E9.2, ', IC5MINHW = ', F4.0,    &
 3729                ', IC5MAXITER = ', F4.0, ','/                          &
 3730                '        IC5RKICK = ', F2.0, ', IC5KFILTER = ', F7.4,' /')
 3731 !
 3732  2966 FORMAT ( '  &MISC CICE0 =',F6.3,', CICEN =',F6.3,               &
 3733                      ', LICE = ',F8.1,', PMOVE =',F6.3,','/           &
 3734                '        XSEED =',F6.3,', FLAGTR = ', I1,              &
 3735                      ', XP =',F6.3,', XR =',F6.3,', XFILT =', F6.3 /  &
 3736                '        IHM =',I5,', HSPM =',F6.3,', WSM =',F6.3,     &
 3737                      ', WSC =',F6.3,', FLC = ',A/                     &
 3738                '        NOSW =',I3,', FMICHE =',F6.3,', RWNDC =' ,    &
 3739                         F6.3,', WCOR1 =',F6.2,', WCOR2 =',F6.2,','/   &
 3740                '        FACBERG =',F4.1,', GSHIFT = ',E11.3,          &
 
 
                                                         ww3_grid.F90  page  69
 
 
 3741                      ', STDX = ' ,F7.2,', STDY =',F7.2,','/           &
 3742                '        STDT =', F8.2,                                &
 3743                      ', ICEHMIN =',F5.2,', ICEHFAC =',F5.2,','/       &
 3744                '        ICEHINIT =',F5.2,', ICEDISP =',L3,            &
 3745                      ', ICEHDISP =',F5.2,','/                         &
 3746                '        ICESLN = ',F6.2,', ICEWIND = ',F6.2,          &
 3747                      ', ICESNL = ',F6.2,', ICESDS = ',F5.2,','/       &
 3748                '        ICEDDISP = ',F5.2,', ICEFDISP = ',F5.2,       &
 3749                      ', CALTYPE = ',A8,' , TRCKCMPR = ', L3,','/      &
 3750                '        BTBET  = ', F6.2, ' /')
 3751 !
 3752  2976 FORMAT ( '  &OUTS P2SF  =',I2,', I1P2SF =',I2,', I2P2SF =',I3,','/&
 3753                '        US3D  =',I2,', I1US3D =',I3,', I2US3D =',I3,','/&
 3754                '        USSP  =',I2,', IUSSP  =',I3,','/&
 3755                '        E3D   =',I2,', I1E3D  =',I3,', I2E3D  =',I3,','/&
 3756                '        TH1MF =',I2,', I1TH1M =',I3,', I2TH1M =',I3,','/&
 3757                '        STH1MF=',I2,', I1STH1M=',I3,', I2STH1M=',I3,','/&
 3758                '        TH2MF =',I2,', I1TH2M =',I3,', I2TH2M =',I3,','/&
 3759                '        STH2MF=',I2,', I1STH2M=',I3,', I2STH2M=',I3,' /')
 3760 !
 3761  2986 FORMAT ( '  &REF1 REFCOAST =',F5.2,', REFFREQ =',F5.2,', REFSLOPE =>
 3762                ', REFMAP =',F4.1, ', REFMAPD =',F4.1, ', REFSUBGRID =',F5>
 3763                '        REFRMAX=',F5.2,', REFFREQPOW =',F5.2,
 3764                ', REFICEBERG =',F5.2,', REFCOSP_STRAIGHT =',F4.1,' /')
 3765 !
 3766  2987 FORMAT ( '  &FLD TAIL_ID =',I1,' TAIL_LEV =',F5.4,' TAILT1 =',F5.3,&
 3767                ' TAILT2 =',F5.3,' /')
 3768                                      
 3769  3000 FORMAT (/'  The spatial grid: '/                                &
 3770                ' --------------------------------------------------'/ &
 3771               /'       Grid type                   : ',A)
 3772  3001 FORMAT ( '       Coordinate system           : ',A)
 3773  3002 FORMAT ( '       Index closure type          : ',A)
 3774  3003 FORMAT ( '       Dimensions                  : ',I6,I8)
 3775  3004 FORMAT (/'       Increments           (deg.) :',2F10.4/         &
 3776                '       Longitude range      (deg.) :',2F10.4/         &
 3777                '       Latitude range       (deg.) :',2F10.4)
 3778  3005 FORMAT ( '       Increments             (km) :',2F8.2/          &
 3779                '       X range                (km) :',2F8.2/          &
 3780                '       Y range                (km) :',2F8.2)
 3781  3006 FORMAT (/'       X-coordinate unit           :',I6/             &
 3782                '       Scale factor                :',F10.4/           &
 3783                '       Add offset                  :',E12.4/          &
 3784                '       Layout indicator            :',I6/             &
 3785                '       Format indicator            :',I6)
 3786  3007 FORMAT (/'       Y-coordinate unit           :',I6/             &
 3787                '       Scale factor                :',F10.4/           &
 3788                '       Add offset                  :',E12.4/          &
 3789                '       Layout indicator            :',I6/             &
 3790                '       Format indicator            :',I6)
 3791  3008 FORMAT ( '       Format                      : ',A)
 3792  3009 FORMAT ( '       File name                   : ',A)
 3793   972 FORMAT (/'       Bottom level unit           :',I6/             &
 3794                '       Limiting depth          (m) :',F8.2/           &
 3795                '       Minimum depth           (m) :',F8.2/           &
 
 
                                                         ww3_grid.F90  page  70
 
 
 3796                '       Scale factor                :',F8.2/           &
 3797                '       Layout indicator            :',I6/             &
 3798                '       Format indicator            :',I6)
 3799   973 FORMAT ( '       Format                      : ',A)
 3800   974 FORMAT ( '       File name                   : ',A)
 3801   976 FORMAT (/'       Sub-grid information        : ',A)
 3802   977 FORMAT ( '       Obstructions unit           :',I6/             &
 3803                '       Scale factor                :',F10.4/          &
 3804                '       Layout indicator            :',I6/             &
 3805                '       Format indicator            :',I6)
 3806   978 FORMAT (/'       Mask information            : From file.'/     &
 3807                '       Mask unit                   :',I6/             &
 3808                '       Layout indicator            :',I6/             &
 3809                '       Format indicator            :',I6)
 3810  1977 FORMAT ( '       Shoreline slope             :',I6/             &
 3811                '       Scale factor                :',F10.4/          &
 3812                '       Layout indicator            :',I6/             &
 3813                '       Format indicator            :',I6)
 3814  1978 FORMAT ( '       Grain sizes                 :',I6/             &
 3815                '       Scale factor                :',F10.4/          &
 3816                '       Layout indicator            :',I6/             &
 3817                '       Format indicator            :',I6)
 3818 !
 3819   979 FORMAT ( '  Processing ',A)
 3820   980 FORMAT (/'  Input boundary points : '/                          &
 3821                ' --------------------------------------------------')
 3822  1980 FORMAT (/'  Excluded points : '/                                &
 3823                ' --------------------------------------------------')
 3824   981 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED), IX, IY =')
 3825  1981 FORMAT ( '   *** POINT ALREADY EXCLUDED (SKIPPED), IX, IY =')
 3826   982 FORMAT ( '   *** CANNOT CONNECT POINTS, IX, IY =')
 3827   985 FORMAT ( '       No boundary points.'/)
 3828   986 FORMAT ( '       Number of boundary points   :',I6/)
 3829  1985 FORMAT ( '       No excluded points.'/)
 3830  1986 FORMAT ( '       Number of excluded points   :',I6/)
 3831   987 FORMAT ( '         Nr.|   IX  |   IY  |  Long.  |   Lat.  '/       >
 3832                '       -----|-------|-------|---------|---------')
 3833  1987 FORMAT ( '         Nr.|   IX  |   IY  |     X     |     Y     '/   >
 3834                '       -----|-------|-------|-----------|-----------')
 3835   988 FORMAT ( '       ',I4,2(' |',I6),2(' |',F8.2))
 3836  1988 FORMAT ( '       ',I4,2(' |',I6),2(' |',F8.1,'E3'))
 3837   989 FORMAT ( ' ')
 3838 !
 3839   990 FORMAT (/'  Output boundary points : '/                         &
 3840                ' --------------------------------------------------')
 3841   991 FORMAT ( '       File nest',I1,'.ww3  Number of points  :',I6/  &
 3842                '                       Number of spectra :',I6)
 3843  1991 FORMAT ( '                       Dest. grid Polat:',F6.2,', Polon:'>
 3844   992 FORMAT (/'         Nr.|  Long.  |   Lat.  '/               &
 3845                '       -----|---------|---------')
 3846  1992 FORMAT (/'         Nr.|  Long.  |   Lat.  ',               &
 3847                '         Nr.|  Long.  |   Lat.  '/               &
 3848                '       -----|---------|---------',               &
 3849                '       -----|---------|---------')
 3850   993 FORMAT ( '       ',I4,2(' |',F8.2))
 
 
                                                         ww3_grid.F90  page  71
 
 
 3851  1993 FORMAT ( '       ',I4,2(' |',F8.2),                        &
 3852               '        ',I4,2(' |',F8.2))
 3853   994 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED) : X,Y =',2F10.5)
 3854   995 FORMAT ( '   *** POINT ON LAND      (SKIPPED) : X,Y =',2F10.5)
 3855  2992 FORMAT (/'         Nr.|     X     |     Y     '/           &
 3856                '       -----|-----------|-----------')
 3857  3992 FORMAT (/'         Nr.|     X     |     Y     ',           &
 3858                  '       Nr.|     X     |     Y     '/           &
 3859                '       -----|-----------|-----------',           &
 3860                  '     -----|-----------|-----------')
 3861  2993 FORMAT ( '       ',I4,2(' |',F8.1,'E3'))
 3862  3993 FORMAT ( '       ',I4,2(' |',F8.1,'E3'),                   &
 3863                 '      ',I4,2(' |',F8.1,'E3'))
 3864  2994 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED) : X,Y =',2(F8.1,'E3'))
 3865  2995 FORMAT ( '   *** POINT ON LAND      (SKIPPED) : X,Y =',2(F8.1,'E3'))
 3866   996 FORMAT ( '       No boundary points.'/)
 3867   997 FORMAT ( '       Number of boundary points   :',I6/             &
 3868                '       Number of spectra           :',I6/)
 3869 !
 3870   998 FORMAT (50I2)
 3871  1998 FORMAT (50I2)
 3872 !
 3873   999 FORMAT (/'  Writing model definition file ...'/)
 3874 !
 3875  1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3876                '     ERROR IN OPENING INPUT FILE'/                    &
 3877                '     IOSTAT =',I5/)
 3878 !
 3879  1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3880                '     PREMATURE END OF INPUT FILE'/)
 3881 !
 3882  1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3883                '     ERROR IN READING FROM INPUT FILE'/               &
 3884                '     IOSTAT =',I5/)
 3885 !
 3886  1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3887                '     INVALID CALENDAR TYPE: SELECT ONE OF:',          &
 3888                '     standard, 360_day, or 365_day '/)
 3889 !
 3890  1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3891                '     CANNOT READ UNFORMATTED (IDFM = 3) FROM UNIT',   &
 3892                I4,' (ww3_grid.inp)'/)
 3893 !
 3894  1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
 3895                '     BOTTOM AND OBSTRUCTION DATA FROM SAME FILE '/    &
 3896                '     BUT WITH INCOMPATIBLE FORMATS (',I1,',',I1,')'/)
 3897 !
 3898  1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3899                '     TOO MANY NESTING OUTPUT FILES '/)
 3900 !
 3901  1007 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3902                '     ILLEGAL GRID TYPE:',A4)
 3903 !
 3904  1008 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3905                '     A CARTESIAN WITH CLOSURE IS NOT ALLOWED')
 
 
                                                         ww3_grid.F90  page  72
 
 
 3906 !
 3907  1009 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3908                '     A RECTILINEAR TRIPOLE GRID IS NOT ALLOWED')
 3909 !
 3910  1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'//               &
 3911                '     NO PROPAGATION + NO SOURCE TERMS = NO WAVE MODEL'// &
 3912                '     ( USE DRY RUN FLAG TO TEMPORARILY SWITCH OFF ',  &
 3913                'CALCULATIONS )'/)
 3914 !
 3915  1011 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/              &
 3916                '     LEFT-HANDED GRID -- POSSIBLE CAUSE IS WRONG '/   &
 3917                '     IDLA:',I4,' . THIS MAY PRODUCE ERRORS '/         &
 3918                '     (COMMENT THIS EXTCDE AT YOUR OWN RISK).')
 3919 !
 3920  1012 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3921                '     ILLEGAL GRID CLOSURE TYPE:',A4)
 3922 !
 3923  1013 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/              &
 3924                '     THE GLOBAL (LOGICAL) INPUT FLAG IS DEPRECATED'/  &
 3925                '     AND REPLACED WITH A STRING INDICATING THE TYPE'/ &
 3926                '     OF GRID INDEX CLOSURE (NONE, SMPL or TRPL).'/    &
 3927                ' *** PLEASE UPDATE YOUR GRID INPUT FILE ACCORDINGLY ***'/)
 3928 !
 3929  1014 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3930                '   SMC CELL LONGITUDE RANGE OUTSIDE BASE GRID RANGE:'/&
 3931                '   ISEA =', I6, '; IX =', I4, ':', I4,'; NX =', I4/)
 3932 !
 3933  1015 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3934                '   SMC CELL LATITUDE RANGE OUTSIDE BASE GRID RANGE: '/&
 3935                '   ISEA =', I6, '; IY =', I4, ':', I4,'; NY =', I4/)
 3936 !
 3937  1020 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
 3938                '     SOURCE TERMS REQUESTED BUT NOT SELECTED'/)
 3939  1021 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
 3940                '     SOURCE TERMS SELECTED BUT NOT REQUESTED'/)
 3941  1022 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3942                '     ILLEGAL NUMBER OF !/LNn OR SEED SWITCHES :',I3)
 3943  1023 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3944                '     ILLEGAL NUMBER OF !/STn SWITCHES :',I3)
 3945  1024 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3946                '     ILLEGAL NUMBER OF !/NLn SWITCHES :',I3)
 3947  1025 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3948                '     ILLEGAL NUMBER OF !/BTn SWITCHES :',I3)
 3949  1026 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3950                '     ILLEGAL NUMBER OF !/DBn SWITCHES :',I3)
 3951  1027 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3952                '     ILLEGAL NUMBER OF !/TRn SWITCHES :',I3)
 3953  1028 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3954                '     ILLEGAL NUMBER OF !/BSn SWITCHES :',I3)
 3955  1029 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3956                '     ILLEGAL NUMBER OF !/XXn SWITCHES :',I3)
 3957 !
 3958  1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3959                '     PROPAGATION REQUESTED BUT NO SCHEME SELECTED '/)
 3960  1031 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
 
 
                                                         ww3_grid.F90  page  73
 
 
 3961                '     NO PROPAGATION REQUESTED BUT SCHEME SELECTED '/)
 3962  1032 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3963                '     NO PROPAGATION SCHEME SELECTED ( use !/PR0 ) '/)
 3964  1033 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3965                '     MULTIPLE PROPAGATION SCHEMES SELECTED :',I3/     &
 3966                '     CHECK !/PRn SWITCHES'/)
 3967  1034 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3968                '     ILLEGAL NUMBER OF !/ICn SWITCHES :',I3)
 3969  1035 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
 3970                '     ONLY FIRST PROPAGATION SCHEME WILL BE USED: ')
 3971  1036 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
 3972                '     ILLEGAL NUMBER OF !/ISn SWITCHES :',I3)
 3973 !
 3974  1040 FORMAT ( '       Space-time extremes DX      :',F10.2)
 3975  1041 FORMAT ( '       Space-time extremes DX      :',F10.2)
 3976  1042 FORMAT ( '       Space-time extremes DX-Y set to default 1000 m')
 3977  1043 FORMAT ( '       Space-time extremes Dt      :',F8.2)
 3978  1044 FORMAT ( '       Space-time extremes Dt set to default 1200 s')
 3979 !
 3980  1100 FORMAT (/'  Status map, printed in',I6,' part(s) '/             &
 3981                ' -----------------------------------'/)
 3982  1101 FORMAT (2X,180I2)
 3983  1102 FORMAT ( '  Legend : '/                                         &
 3984                ' -----------------------------'/                      &
 3985                '    0 : Land point            '/                      &
 3986                '    1 : Sea point             '/                      &
 3987                '    2 : Active boundary point '/                      &
 3988                '    3 : Excluded point        '/)
 3989  1103 FORMAT (/'  Obstruction map ',A1,', printed in',I6,' part(s) '/ &
 3990                ' ---------------------------------------------'/)
 3991  1104 FORMAT ( '  Legend : '/                                         &
 3992                ' --------------------------------'/                   &
 3993                '    fraction of obstruction * 10 '/)
 3994                                                     
 3995  1105 FORMAT (/'  Shoreline slope, printed in',I6,' part(s) '/ &
 3996                ' ---------------------------------------------'/)
 3997  1106 FORMAT ( '  Legend : '/                                         &
 3998                ' --------------------------------'/                   &
 3999                '   Slope * 100'/)
 4000                                  
 4001                                  
 4002  1150 FORMAT (/'  Reading unstructured grid definition files ...'/)
 4003 !
 4004  9997 FORMAT (/'  Summary grid statistics : '/                        &
 4005                ' --------------------------------------------------'/ &
 4006                '       Number of longitudes      :',I10/              &
 4007                '       Number of latitudes       :',I10/              &
 4008                '       Number of grid points     :',I10/              &
 4009                '       Number of sea points      :',I10,' (',F4.1,'%)'/&
 4010                '       Number of input b. points :',I10/              &
 4011                '       Number of land points     :',I10/              &
 4012                '       Number of excluded points :',I10/)
 4013  9998 FORMAT (/'  Summary grid statistics : '/                        &
 4014                ' --------------------------------------------------'/ &
 4015                '       Number of longitudes      :',I10/              &
 
 
                                                         ww3_grid.F90  page  74
 
 
 4016                '       Number of latitudes       :',I10/              &
 4017                '       Number of grid points     :',I10/              &
 4018                '       Number of sea points      :',I10,' (100%)'/    &
 4019                '       Number of input b. points :',I10/              &
 4020                '       Number of land points     :',I10/              &
 4021                '       Number of excluded points :',I10/)
 4022  9999 FORMAT (/'  End of program '/                                   &
 4023                ' ========================================'/           &
 4024                '         WAVEWATCH III Grid preprocessor '/)
 4025 !
 4026 !/
 4027 !/ Internal function READNL ------------------------------------------ /
 4028 !/
 4029       CONTAINS
 4030 !/ ------------------------------------------------------------------- /
 4031       SUBROUTINE READNL ( NDS, NAME, STATUS )
 4032 !/
 4033 !/                  +-----------------------------------+
 4034 !/                  | WAVEWATCH III           NOAA/NCEP |
 4035 !/                  |           H. L. Tolman            |
 4036 !/                  |                        FORTRAN 90 |
 4037 !/                  | Last update :         01-Jun-2013 |
 4038 !/                  +-----------------------------------+
 4039 !/
 4040 !  1. Purpose :
 4041 !
 4042 !     Read namelist info from file if namelist is found in file.
 4043 !
 4044 !  2. Method :
 4045 !
 4046 !     Look for namelist with name NAME in unit NDS and read if found.
 4047 !
 4048 !  3. Parameters :
 4049 !
 4050 !     Parameter list
 4051 !     ----------------------------------------------------------------
 4052 !       NDS     Int.   I   Data set number used for search.
 4053 !       NAME    C*4    I   Name of namelist.
 4054 !       STATUS  C*20   O   Status at end of routine,
 4055 !                            '(default values)  ' if no namelist found.
 4056 !                            '(user def. values)' if namelist read.
 4057 !     ----------------------------------------------------------------
 4058 !
 4059 !  4. Subroutines used :
 4060 !
 4061 !      Name      Type  Module   Description
 4062 !     ----------------------------------------------------------------
 4063 !      EXTCDE    Subr. W3SERVMD Abort program as graceful as possible.
 4064 !     ----------------------------------------------------------------
 4065 !
 4066 !  5. Called by :
 4067 !
 4068 !     Program in which it is contained.
 4069 !
 4070 !  6. Error messages :
 
 
                                                         ww3_grid.F90  page  75
 
 
 4071 !
 4072 !  7. Remarks :
 4073 !
 4074 !  8. Structure :
 4075 !
 4076 !  9. Switches :
 4077 !
 4078 ! 10. Source code :
 4079 !
 4080 !/ ------------------------------------------------------------------- /
 4081 !/ Parameter list
 4082 !/
 4083       INTEGER, INTENT(IN)     :: NDS
 4084       CHARACTER, INTENT(IN)   :: NAME*4
 4085       CHARACTER, INTENT(OUT)  :: STATUS*20
 4086 !/
 4087 !/ ------------------------------------------------------------------- /
 4088 !/ Local parameters
 4089 !/
 4090       INTEGER                 :: IERR, I, J
 4091       CHARACTER               :: LINE*80
 4092 !/
 4093 !/ ------------------------------------------------------------------- /
 4094 !/
 4095 !
 4096       REWIND (NDS)
 4097       STATUS  = '(default values) :  '
 4098 !
 4099       DO
 4100         READ (NDS,'(A)',END=800,ERR=800,IOSTAT=IERR) LINE
 4101         DO I=1, 70
 4102           IF ( LINE(I:I) .NE. ' ' ) THEN
 4103               IF ( LINE(I:I) .EQ. '&' ) THEN
 4104                   IF ( LINE(I+1:I+4) .EQ. NAME ) THEN
 4105                       BACKSPACE (NDS)
 4106                       SELECT CASE(NAME)
 4107                         CASE('SLN1')
 4108                           READ (NDS,NML=SLN1,END=801,ERR=802,IOSTAT=J)
 4109                         CASE('SIN4')
 4110                           READ (NDS,NML=SIN4,END=801,ERR=802,IOSTAT=J)
 4111                         CASE('SNL1')
 4112                           READ (NDS,NML=SNL1,END=801,ERR=802,IOSTAT=J)
 4113                         CASE('SDS4')
 4114                           READ (NDS,NML=SDS4,END=801,ERR=802,IOSTAT=J)
 4115                         CASE('PRO3')
 4116                           READ (NDS,NML=PRO3,END=801,ERR=802,IOSTAT=J)
 4117                        CASE('REF1')
 4118                          READ (NDS,NML=REF1,END=801,ERR=802,IOSTAT=J)
 4119                         CASE('SIG1')
 4120                          READ (NDS,NML=SIG1,END=801,ERR=802,IOSTAT=J)
 4121                         CASE('UNST')
 4122                           READ (NDS,NML=UNST,END=801,ERR=802,IOSTAT=J)
 4123                         CASE('OUTS')
 4124                           READ (NDS,NML=OUTS,END=801,ERR=802,IOSTAT=J)
 4125                         CASE('MISC')
 
 
                                                         ww3_grid.F90  page  76
 
 
 4126                           READ (NDS,NML=MISC,END=801,ERR=802,IOSTAT=J)
 4127                         CASE DEFAULT
 4128                           GOTO 803
 4129                         END SELECT
 4130                       STATUS  = '(user def. values) :'
 4131                       RETURN
 4132                     END IF
 4133                 ELSE
 4134                   EXIT
 4135                 END IF
 4136             ENDIF
 4137           END DO
 4138         END DO
 4139 !
 4140   800 CONTINUE
 4141       RETURN
 4142 !
 4143   801 CONTINUE
 4144       WRITE (NDSE,1001) NAME
 4145       CALL EXTCDE(1)
 4146       RETURN
 4147 !
 4148   802 CONTINUE
 4149       WRITE (NDSE,1002) NAME, J
 4150       CALL EXTCDE(2)
 4151       RETURN
 4152 !
 4153   803 CONTINUE
 4154       WRITE (NDSE,1003) NAME
 4155       CALL EXTCDE(3)
 4156       RETURN
 4157 !
 4158 ! Formats
 4159 !
 4160  1001 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
 4161                '     PREMATURE END OF FILE IN READING ',A/)
 4162  1002 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
 4163                '     ERROR IN READING ',A,'  IOSTAT =',I8/)
 4164  1003 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
 4165                '     NAMELIST NAME ',A,' NOT RECOGNIZED'/)
 4166 !/
 4167 !/ End of READNL ----------------------------------------------------- /
 4168 !/
 4169       END SUBROUTINE
 4170 !/
 4171 !/ End of W3GRID ----------------------------------------------------- /
 4172 !/
 4173       END PROGRAM W3GRID
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
