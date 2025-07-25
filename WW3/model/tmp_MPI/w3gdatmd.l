                                                         w3gdatmd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/
    3 !/ ------------------------------------------------------------------- /
    4 !/ Macros for enabling test output
    5 !/
    6 #define TEST_W3GDATMD___disabled
    7 #define TEST_W3GDATMD_W3NMOD___disabled
    8 #define TEST_W3GDATMD_W3DIMX___disabled
    9 #define TEST_W3GDATMD_W3DIMS___disabled
   10 #define TEST_W3GDATMD_W3SETG___disabled
   11 #define TEST_W3GDATMD_W3GNTX___disabled
   12 #define TEST_W3GDATMD_W3DIMUG___disabled
   13 #define TEST_W3GDATMD_W3SETREF___disabled
   14 !/
   15 !/ ------------------------------------------------------------------- /
   16       MODULE W3GDATMD
   17 !/
   18 !/                  +-----------------------------------+
   19 !/                  | WAVEWATCH III           NOAA/NCEP |
   20 !/                  |           H. L. Tolman            |
   21 !/                  !           J. H. Alves             !
   22 !/                  |            F. Ardhuin             |
   23 !/                  |                        FORTRAN 90 |
   24 !/                  | Last update :         15-Apr-2020 |
   25 !/                  +-----------------------------------+
   26 !/
   27 !/    24-Jun-2005 : Origination.                        ( version 3.07 )
   28 !/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
   29 !/    23-Jun-2006 : Add data for W3SLN1.                ( version 3.09 )
   30 !/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
   31 !/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
   32 !/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
   33 !/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
   34 !/                  ( J. H. Alves )
   35 !/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
   36 !/                  ( J. H. Alves )
   37 !/    06-Aug-2007 : Fixing SLNP !/SEED bug.             ( version 3.13 )
   38 !/    18-Sep-2007 : Adding WAM4 source terms.           ( version 3.13 )
   39 !/                  ( F. Ardhuin )
   40 !/    15-Apr-2008 : Clean up for distribution.          ( version 3.14 )
   41 !/    27-Jun-2008 : Expand WAM4 variants namelist       ( version 3.14 )
   42 !/                  ( F. Ardhuin )
   43 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   44 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
   45 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   46 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
   47 !/                  (W. E. Rogers & T. J. Campbell, NRL)
   48 !/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.1 )
   49 !/                  (A. Roland and F. Ardhuin)
   50 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
   51 !/                  specify index closure for a grid.   ( version 3.14 )
   52 !/                  (T. J. Campbell, NRL)
   53 !/    23-Dec-2010 : Fix HPFAC and HQFAC by including the COS(YGRD)
   54 !/                  factor with DXDP and DXDQ terms.    ( version 3.14 )
   55 !/                  (T. J. Campbell, NRL)
 
 
                                                         w3gdatmd.F90  page   2
 
 
   56 !/    05-Apr-2011 : Implement interations for DTMAX < 1s( version 3.14.1 )
   57 !/                  (F. Ardhuin)
   58 !/    01-Jul-2011 : Movable bed bottom friction BT4     ( version 4.01 )
   59 !/    03-Nov-2011 : Bug fix: GUGINIT initialization     ( version 4.04 )
   60 !/    29-Nov-2011 : Adding ST6 source term option.      ( version 4.04 )
   61 !/                  (S. Zieger)
   62 !/    14-Mar-2012 : Add PSIC for BT4                    ( version 4.04 )
   63 !/    12-Jun-2012 : Add /RTD option or rotated grid variables.
   64 !/                  (Jian-Guo Li)                       ( version 4.06 )
   65 !/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
   66 !/                  filter (SNLS) from 3.15 (HLT).      ( version 4.08 )
   67 !/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
   68 !/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.09 )
   69 !/    16-Sep-2013 : Add Arctic part SMC grid.           ( version 4.11 )
   70 !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
   71 !/                  trunk                               ( version 4.13 )
   72 !/    16-Nov-2013 : Allows reflection on curvi grids    ( version 4.14 )
   73 !/    26-Jul-2013 : Adding IG waves                     ( version 4.16 )
   74 !/    18-Dec-2013 : Moving FLAGLL into GRID TYPE        ( version 4.16 )
   75 !/    11-Jun-2014 : Changed reflection for subgrid      ( version 5.01 )
   76 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
   77 !/    21-Aug-2015 : Add SMC FUNO3, FVERG options. JGLi  ( version 5.09 )
   78 !/    04-May-2016 : Add IICEDISP                  GB&FA ( version 5.10 )
   79 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
   80 !/    20-Jan-2017 : Change to preprocessor macros to enable test output.
   81 !/                  (T.J. Campbell, NRL)                ( version 6.02 )
   82 !/    20-Jan-2017 : Change calculation of curvilinear grid metric and
   83 !/                  derivatives calculations to use W3GSRUMD:W3CGDM.
   84 !/                  (T.J. Campbell, NRL)                ( version 6.02 )
   85 !/    07-Jan-2018 : Generalizes ICE100WIND to ICESCALES ( version 6.04 )
   86 !/    26-Mar-2018 : Add FSWND optional variable.  JGLi  ( version 6.02 )
   87 !/    05-Jun-2018 : Add PDLIB/DEBUGINIT and implcit scheme parameters
   88 !/                  for unstructured grids              ( version 6.04 )
   89 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
   90 !/    20-Aug-2018:  Extra namelist variables for ST6    ( version 6.06)
   91 !/                  (Q. Liu, UoM)
   92 !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
   93 !/    27-Aug-2018 : Add BTBETA parameter                ( version 6.06 )
   94 !/    22-Feb-2020 : Add AIRGB and AIRCMIN               ( version 7.06 )
   95 !/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
   96 !/
   97 !/
   98 !/    Copyright 2009-2013 National Weather Service (NWS),
   99 !/       National Oceanic and Atmospheric Administration.  All rights
  100 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  101 !/       No unauthorized use without permission.
  102 !/
  103 !  1. Purpose :
  104 !
  105 !     Define data structures to set up wave model grids and aliases
  106 !     to use individual grids transparently. Also includes subroutines
  107 !     to manage data structure and pointing to individual models.
  108 !     Definition of grids and model set up.
  109 !
  110 !  2. Variables and types :
 
 
                                                         w3gdatmd.F90  page   3
 
 
  111 !
  112 !      Name      Type  Scope    Description
  113 !     ----------------------------------------------------------------
  114 !      NGRIDS    Int.  Public   Number of grids, initialized at -1
  115 !                               to check proper model initialization.
  116 !      NAUXGR    Int.  Public   Auxiliary grids.
  117 !      IGRID     Int.  Public   Selected spatial grid, init. at -1.
  118 !      ISGRD     Int.  Public   Selected spectral grid, init. at -1.
  119 !      IPARS     Int.  Public   Selected num. and ph. pars, init. at -1.
  120 !      RLGTYPE   I.P.  Public   Named constant for rectilinear grid type
  121 !      CLGTYPE   I.P.  Public   Named constant for curvilinear grid type
  122 !      FLAGLL    Log.  Public   Flag to indicate coordinate system for al>
  123 !                               .TRUE.: Spherical (lon/lat in degrees)
  124 !                               .FALSE.: Cartesian (meters)
  125 !      GRID      TYPE  Public   Data structure defining grid.
  126 !      GRIDS     GRID  Public   Array of grids.
  127 !      SGRD      TYPE  Public   Data structure defining spectral grid.
  128 !      SGRDS     GRID  Public   Array of spectral grids.
  129 !      MPAR      TYPE  Public   Data structure with all other model
  130 !                               parameters.
  131 !      MPARS     GRID  Public   Array of MPAR.
  132 !     ----------------------------------------------------------------
  133 !
  134 !     All elements of GRID are aliased to pointers with the same
  135 !     name. These pointers are defined as :
  136 !
  137 !      Name      Type  Scope    Description
  138 !     ----------------------------------------------------------------
  139 !      GTYPE     Int.  Public   Flag for type of grid
  140 !                               RLGTYPE: Rectilinear grid
  141 !                               CLGTYPE: Curvilinear grid
  142 !                               UNGTYPE: Unstructured triangular grid
  143 !      RSTYPE    Int.  Public   Integer identifyng restart type
  144 !      ICLOSE    Int.  Public   Parameter indicating type of index closur>
  145 !                               ICLOSE_NONE: No grid closure
  146 !                               ICLOSE_SMPL: Simple grid closure
  147 !                                 Grid is periodic in the i-index and wra>
  148 !                                 I=NX+1. In other words, (NX+1,J) => (1,>
  149 !                               ICLOSE_TRPL: Tripole grid closure
  150 !                                 Grid is periodic in the i-index and and>
  151 !                                 I=NX+1 and has closure at J=NY+1. In ot>
  152 !                                 (NX+1,J<=NY) => (1,J) and
  153 !                                 (I,NY+1) => (MOD(NX-I+1,NX)+1,NY). The >
  154 !                                 closure requires that NX be even.
  155 !      NX, NY    Int.  Public   Discrete dimensions of spatial grid.
  156 !      NSEA(L)   Int.  Public   Number of sea points (local for MPP).
  157 !      NU/VFc    Int.  Public   Number of U/V faces for SMC grid.
  158 !      NRLv      Int.  Public   Number of refined levels for SMC grid.
  159 !      NGLO      Int.  Public   Number of cells in global part for SMC gr>
  160 !      NARC      Int.  Public   Number of cells in Arctic part for SMC gr>
  161 !      NBAC      Int.  Public   Number of boundary cells in Arctic part.
  162 !      NBGL      Int.  Public   Number of boundary cells in global part.
  163 !      TRFLAG    Int.  Public   Flag for use of transparencies
  164 !                                0: No sub-grid obstacles.
  165 !                                1: Obstructions at cell boundaries.
 
 
                                                         w3gdatmd.F90  page   4
 
 
  166 !                                2: Obstructions at cell centers.
  167 !                                3: Like 1 with continuous ice.
  168 !                                4: Like 2 with continuous ice.
  169 !      MAPSTA    I.A.  Public   Grid status map.
  170 !      MAPST2    I.A.  Public   Second grid status map.
  171 !      MAPxx     I.A.  Public   Storage grid maps.
  172 !      IJKCel    I.A.  Public   Cell info array for SMC grid.
  173 !      IJKU/VFc  I.A.  Public   U/V-Face arrays for SMC grid.
  174 !      NLv*      I.A.  Public   Cell, U/V-Face numbers of refine levels.
  175 !      ICLBAC    I.A.  Public   Mapping index for Arctic boundary cells.
  176 !      SX,SY     Real  Public   Spatial (rectilinear) grid increments.
  177 !      X0,Y0     Real  Public   Lower left corner of spatial (rectilinear>
  178 !      DTCFL     Real  Public   Maximum CFL time step X-Y propagation.
  179 !      DTCFLI    Real  Public   Id. intra-spectral.
  180 !      DTMAX     Real  Public   Maximum overall time step.
  181 !      DTMIN     Real  Public   Minimum dynamic time step for source
  182 !      NITERSEC1 Real  Public   Number of interations when DTMAX < 1s
  183 !      DMIN      Real  Public   Minimum water depth.
  184 !      CTMAX     Real  Public   Maximum CFL number for depth refr.
  185 !      FICE0/N   Real  Public   Cut-off ice conc. for ice coverage.
  186 !      FICEL     Real  Public   Length scale for sea ice damping
  187 !      IICEHMIN  Real  Public   Minimum thickness of sea ice
  188 !      IICEHDISP Real  Public   Minimum thickness of sea ice in the dispe>
  189 !      IICEHFAC  Real  Public   Scale factor for sea ice thickness
  190 !      IICEHINIT Real  Public   Initial value of ice thickness
  191 !      ICESCALES R.A.  Publ.    Scaling coefficient for source terms in t>
  192 !                               Default is 1.0, meaning that 100% ice
  193 !                               concentration result in zero source term
  194 !                               If set to 0.0, then ice has no direct imp>
  195 !      IC3PARS   R.A.  Public   various parameters for use in IC4, handle>
  196 !                               an array for simplicity
  197 !      IC4_KI    R.A.  Public   KI (dissipation rate) values for use in I>
  198 !      IC4_FC    R.A.  Public   FC (frequency bin separators) for use in >
  199 !      PFMOVE    Real  Public   Tunable parameter in GSE correction
  200 !                               for moving grids.
  201 !      GRIDSHIFT Real  Public   Grid offset for multi-grid w/SCRIP
  202 !      CMPRTRCK  Log.  Public   True for traditional compression of track>
  203 !      PoLat/Lon R.A.  Public   Rotated N-Pole standard latitude/longitud>
  204 !      AnglD     R.A.  Public   Rotation angle in degree to turn rotated >
  205 !                               back to standard grid.  JGLi12Jun2012
  206 !      FLAGUNR   Log.  Public   True if rotating directions back to true >
  207 !      STEXU     Real  Public   Length-scale (X) for space-time extreme a>
  208 !      STEYU     Real  Public   Length-scale (Y) for space-time extreme a>
  209 !      STEDU     Real  Public   Time-scale for space-time extreme averagi>
  210 !      ZB        R.A.  Public   Bottom levels on storage grid.
  211 !      CLATS(I)  R.A.  Public   (Inverse) cosine of latitude at sea point>
  212 !      CTHG0S    R.A.  Public   Constant in great-circle refr. term at se>
  213 !      TRNX/Y    R.A.  Public   Transparencies in X/Y for sub-grid
  214 !      CTRNX/Y   R.A.  Public   Sub-grid transparencies for SMC grid.
  215 !      ANGARC    R.A.  Public   Rotation angle in degree for Arctic cells.
  216 !      SPCBAC    R.A.  Public   Full 2-D spectra for Arctic boundary cell>
  217 !      X/YGRD    R.A.  Public   Spatial grid coordinate arrays.
  218 !      SX/SYGRD  R.A.  Public   Spatial grid increment arrays.
  219 !      GINIT     Log.  Public   Flag identifying grid initialization.
  220 !      FLDRY     Log.  Public   Flag for 'dry' run (IO and data
 
 
                                                         w3gdatmd.F90  page   5
 
 
  221 !                               processing only).
  222 !      FLCx      Log.  Public   Flags for prop. is different spaces.
  223 !      FLSOU     Log.  Public   Flag for source term calculation.
  224 !      FUNO3     Log.  Public   Flag for 3rd order UNO3 scheme on SMC gri>
  225 !      FVERG     Log.  Public   Flag for 1-2-1 averaging smoothing on SMC>
  226 !      FSWND     Log.  Public   Flag for sea-point only wind input on SMC>
  227 !      FLAGST    L.A.  Public   Flag for source term computations
  228 !                               for individual grid points.
  229 !      IICEDISP   Log.  Public   Flag for use of the ice covered disperti>
  230 !      IICESMOOTH Log.  Public   Flag to smooth the ice covered dispertio>
  231 !
  232 !      GNAME     C*30  Public   Grid name.
  233 !      FILEXT    C*13  Public   Extension of WAVEWATCH III file names
  234 !                               default in 'ww3'.
  235 !      BTBETA    Real  Public   The constant used for separating wind sea
  236 !                               and swell when we estimate WBT
  237 !     ----------------------------------------------------------------
  238 !
  239 !     All elements of SGRD are aliased to pointers with the same
  240 !     name. These pointers are defined as :
  241 !
  242 !      Name      Type  Scope    Description
  243 !     ----------------------------------------------------------------
  244 !      NK        Int.  Public   Number of discrete wavenumbers.
  245 !      NK2       Int.  Public   Extended wavenumber range.
  246 !      NTH       Int.  Public   Number of discrete directions.
  247 !      NSPEC     Int.  Public   Number of discrete spectral bins.
  248 !      MAPxx     I.A.  Public   Spectral maps.
  249 !      DTH       Real  Public   Directional increments (radians).
  250 !      XFR       Real  Public   Frequency multiplication factor.
  251 !      FR1       Real  Public   Lowest frequency                 (Hz)
  252 !      FTE       Real  Public   Factor in tail integration energy.
  253 !      FTF       Real  Public   Id. frequency.
  254 !      FTWN      Real  Public   Id. wavenumber.
  255 !      FTTR      Real  Public   Id. wave period.
  256 !      FTWL      Real  Public   Id. wave length.
  257 !      FACTIn    Real  Public   Factors for obtaining integer cut-off
  258 !                               frequency.
  259 !      FACHFx    Real  Public   Factor for tail.
  260 !      TH        R.A   Public   Directions (radians).
  261 !      ESIN      R.A   Public   Sine of discrete directions.
  262 !      ECOS      R.A   Public   Cosine of discrete directions.
  263 !      ES2, ESC, EC2
  264 !                R.A   Public   Sine and cosine products
  265 !      SIG       R.A   Public   Relative frequencies (invariant
  266 !                                                     in grid). (rad)
  267 !      SIG2      R.A   Public   Id. for full 2-D spectrum.
  268 !      DSIP      R.A   Public   Frequency bandwidths (prop.)    (rad)
  269 !      DSII      R.A   Public   Frequency bandwidths (int.)     (rad)
  270 !      DDEN      R.A   Public   DSII * DTH * SIG (for integration
  271 !                               based on energy)
  272 !      DDEN2     R.A   Public   Idem, full spectrum.
  273 !      SINIT     Log.  Public   Flag identifying grid initialization.
  274 !     ----------------------------------------------------------------
  275 !
 
 
                                                         w3gdatmd.F90  page   6
 
 
  276 !     The structure MPAR contains all other model parameters for
  277 !     numerical methods and physical parameterizations. It contains
  278 !     itself several structures as outlined below.
  279 !
  280 !      Name      Type  Scope    Description
  281 !     ----------------------------------------------------------------
  282 !      PINIT     Log.  Public   Flag identifying initialization.
  283 !      NPARS     NPAR  Public   Numerical parameters,
  284 !      PROPS     PROP  Public   Parameters propagatrion schemes.
  285 !      SFLPS     SFLP  Public   Parameters for flux computation.
  286 !      SLNPS     SLNP  Public   Parameters Sln.
  287 !      SRCPS     SRCP  Public   Parameters Sin and Sds.
  288 !      SNLPS     SNLP  Public   Parameters Snl.
  289 !      SBTPS     SBTP  Public   Parameters Sbt.
  290 !      SDBPS     SDBP  Public   Parameters Sdb.
  291 !      STRPS     STRP  Public   Parameters Str.
  292 !      SBSPS     SBSP  Public   Parameters Sbs.
  293 !      SXXPS     SXXP  Public   Parameters Sxx.
  294 !     ----------------------------------------------------------------
  295 !
  296 !     The structure NPAR contains numerical parameters and is aliased
  297 !     as above:
  298 !
  299 !      Name      Type  Scope    Description
  300 !     ----------------------------------------------------------------
  301 !      FACP      Real  Public   Constant in maximum par. change in
  302 !                               dynamic integration scheme (depends
  303 !                               upon Xp).
  304 !      XREL      Real  Public   Id. relative change.
  305 !      XFLT      Real  Public   Id. filter level.
  306 !      FXFM      Real  Public   Constant for mean frequency in
  307 !                               cut-off.                       (!/ST1)
  308 !      FXPM      Real  Public   Id. PM.
  309 !      XFT       Real  Public   Constant for cut-off freq.     (!/ST2)
  310 !      XFC       Real  Public   Id.
  311 !      FACSD     Real  Public   Constant in seeding algorithm.
  312 !      FHMAX     Real  Public   Hs/depth ratio in limiter     (!/MLIM)
  313 !      RWINDC    Real  Public   Coefficient for current in relative
  314 !                               wind                          (!/RWND)
  315 !      WWCOR     R.A.  Public   Wind correction factors       (!/WCOR)
  316 !     ----------------------------------------------------------------
  317 !
  318 !     The structure PROP contains parameters for the propagation
  319 !     schemes and is aliased as above:
  320 !
  321 !      Name      Type  Scope    Description
  322 !     ----------------------------------------------------------------
  323 !      DTME      Real  Public   Swell age in disp. corr.      (!/PR2)
  324 !      CLATMN    Real  Public   Id. minimum cosine of lat.    (!/PR2)
  325 !
  326 !      WDCG      Real  Public   Factors in width of av. Cg.   (!/PR3)
  327 !      WDTH      Real  Public   Factors in width of av. Th.   (!/PR3)
  328 !     ----------------------------------------------------------------
  329 !
  330 !     The structure SFLP contains parameters for the fluxes
 
 
                                                         w3gdatmd.F90  page   7
 
 
  331 !     and is aliased as above:
  332 !     ----------------------------------------------------------------
  333 !                                                            (!/FLX2)
  334 !      NITTIN    Int.  Public   Number of itterations for drag calc.
  335 !      CINXSI    Real  Public   Constant in parametric description
  336 !                                                            (!/FLX3)
  337 !      NITTIN    Int.  Public   Number of itterations for drag calc.
  338 !      CAP_ID    Int   Public   Type of cap used.
  339 !      CINXSI    Real  Public   Constant in parametric description
  340 !      CD_MAX    Real  Public   Cap on Cd.
  341 !                                                            (!/FLX4)
  342 !      FLX4A0    Real  Public   Scaling value in parametric description
  343 !     ----------------------------------------------------------------
  344 !
  345 !     The structure SLNP contains parameters for the linear input
  346 !     source terms and is aliased as above:
  347 !
  348 !     ----------------------------------------------------------------
  349 !                                                             (!/LN1)
  350 !      SLNC1     Real  Public   Proportionality and other constants in
  351 !                               input source term.
  352 !      FSPM      Real  Public   Factor for fPM in filter.
  353 !      FSHF      Real  Public   Factor for fh in filter.
  354 !     ----------------------------------------------------------------
  355 !
  356 !     The structure SRCP contains parameters for the input and dis,
  357 !     source terms and is aliased as above:
  358 !
  359 !      Name      Type  Scope    Description
  360 !     ----------------------------------------------------------------
  361 !      WWNMEANPTAIL R  Public   Power of tail for WNMEAN calculation
  362 !      SSTXFTFTAIL  R  Public   Tail factor for  WNMEAN calculation
  363 !                                                             (!/ST1)
  364 !      SINC1     Real  Public   Proportionality and other constants in
  365 !                               input source term.
  366 !      SDSC1     Real  Public   Combined constant in dissipation
  367 !                               source term.
  368 !                                                             (!/ST2)
  369 !      ZWIND     Real  Public   Height at which the wind is defined
  370 !                               of drag.
  371 !      FSWELL    Real  Public   Reduction factor of negative input
  372 !                               for swell.
  373 !      SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS
  374 !                Real  Public   Factors in effective wind speed.
  375 !      CDSAn     Real  Public   Constants in high-freq. dis.
  376 !      SDSALN    Real  Public   Factor for nondimensional 1-D spectrum.
  377 !      CDSBn     Real  Public   Constants in parameterization of PHI.
  378 !      XFH       Real  Public   Constant for turbulent length scale.
  379 !      XFn       Real  Public   Constants in combining low and high
  380 !                               frequency dissipation.
  381 !                                                             (!/ST3)
  382 !      ZZWND     Real  Public   Height at which the wind is defined
  383 !      AALPHA    Real  Public   Minimum value of charnock parameter
  384 !      BBETA     Real  Public   Wind-wave coupling coefficient
  385 !      ZZALP     Real  Public   Wave age tuning coefficient in Sin
 
 
                                                         w3gdatmd.F90  page   8
 
 
  386 !      TTAUWSHELTER Real  Public Sheltering coefficient for short waves
  387 !      ZZ0MAX    Real  Public   Maximum value of air-side roughness
  388 !      ZZ0RAT    Real  Public   ratio of roughness for mean and
  389 !                               oscillatory flows
  390 !      SSINTHP   Real  Public   Power in cosine of wind input
  391 !      SSWELLF   R.A.  Public   Swell damping coefficients
  392 !      SSDSCn    Real  Public   Dissipation parameters
  393 !      SSDSBR    Real  Public   Threshold in saturation spectrum for Sds
  394 !      SSDSP     Real  Public   Power of B(k) in Sds
  395 !      WWNMEANP  Real  Public   Power that defines the mean wavenumber
  396 !                               in Sds
  397 !      SSTXFTF, SSTXFTWN Real  Public   Tail constants
  398 !      SSDSC4,   Real  Public   Threshold shift in saturation diss.
  399 !      SSDSC5,   Real  Public   Wave-turbulence dissipation factor
  400 !      SSDSC6,   Real  Public   dissipation parameter
  401 !      DDELTA1   Real  Public   Low-frequency dissipation coefficient
  402 !                               in WAM4
  403 !      DDELTA2   Real  Public   High-frequency dissipation coefficient
  404 !                               in WAM4
  405 !      SSDSDTH   Real  Public   Maximum angular sector for saturation
  406 !                               spectrum
  407 !      SSDSCOS   Real  Public   Power of cosine in saturation integral
  408 !      SSDSISO   Int.  Public   Choice of definition of the isotropic
  409 !                               saturation
  410 !     ----------------------------------------------------------------
  411 !
  412 !     The structure SNLP contains parameters for the nonl. inter.
  413 !     source term and is aliased as above:
  414 !
  415 !      Name      Type  Scope    Description
  416 !     ----------------------------------------------------------------
  417 !                                                             (!/NL1)
  418 !      SNLC1     Real  Public   Scaled proportionality constant.
  419 !      LAM       Real  Public   Factor defining quadruplet.
  420 !      KDCON     Real  Public   Conversion factor for relative depth.
  421 !      KDMN      Real  Public   Minimum relative depth.
  422 !      SNLSn     Real  Public   Constants in shallow water factor.
  423 !                                                             (!/NL2)
  424 !      IQTPE     Int.  Public   Type of depth treatment
  425 !                                1 : Deep water
  426 !                                2 : Deep water / WAM scaling
  427 !                                3 : Finite water depth
  428 !      NDPTHS    Int.  Public   Number of depth for which integration
  429 !                               space needs to be computed.
  430 !      NLTAIL    Real  Public   Tail factor for parametric tail.
  431 !      DPTHNL    R.A.  Public   Depths corresponding to NDPTHS.
  432 !                               *** NOTE: This array is not allocated
  433 !                                         in the W3DIMP routine ***
  434 !                                                             (!/NL3)
  435 !      NFR       Int.  Public   Number of frequencies or wavenumbers
  436 !                               in discrete spectral space (NFR=>NK).
  437 !      NFRMIN    Int.  Public   Minimum discrete frequency in the
  438 !                               expanded frequency space.
  439 !      NFRMAX    Int.  Public   Idem maximum for first part.
  440 !      NFRCUT    Int.  Public   Idem maximum for second part.
 
 
                                                         w3gdatmd.F90  page   9
 
 
  441 !      NTHMAX    Int.  Public   Extension of directional space.
  442 !      NTHEXP    Int   Public   Number of bins in extended dir. space.
  443 !      NSPMIN, NSPMAX, NSPMX2
  444 !                Int.  Public   1D spectral space range.
  445 !      FRQ       R.A.  Public   Expanded frequency range (Hz).
  446 !      XSI       R.A.  Public   Expanded frequency range (rad/s).
  447 !      NQA       Int.  Public   Number of actual quadruplets.
  448 !      QST1      I.A.  Public   Spectral offsets for compuation of
  449 !                               quadruplet spectral desnities.
  450 !      QST2      R.A.  Public   Idem weights.
  451 !      QST3      R.A.  Public   Proportionality constants and k factors
  452 !                               in diagonal strength.
  453 !      QST4      I.A.  Public   Spectral offsets for combining of
  454 !                               interactions and diagonal.
  455 !      QST5      R.A.  Public   Idem weights for interactions.
  456 !      QST6      R.A.  Public   Idem weights for diagonal.
  457 !      SNLNQ     Int.  Public   Number of quadruplet definitions.
  458 !      SNLMSC    Real  Public   Tuning power 'deep' scaling.
  459 !      SNLNSC    Real  Public   Tuning power 'shallow' scaling.
  460 !      SNLSFD    Real  Public   'Deep' nondimensional filer freq.
  461 !      SNLSFS    Real  Public   'Shallow' nondimensional filer freq.
  462 !      SNLL      R.A.  Public   Array with lambda for quadruplet.
  463 !      SNLM      R.A.  Public   Array with mu for quadruplet.
  464 !      SNLT      R.A.  Public   Array with Dtheta for quadruplet.
  465 !      SNLCD     R.A.  Public   Array with Cd for quadruplet.
  466 !      SNLCS     R.A.  Public   Array with Cs for quadruplet.
  467 !                                                             (!/NL4)
  468 !      ITSA      Int.  Public   Integer indicating TSA (1) or FBI (0)
  469 !      IALT      Int.  Public   Integer determining alternating looping
  470 !                                                             (!/NLS)
  471 !      NTHX      Int.  Public   Expanded discrete direction range.
  472 !      NFRX      Int.  Public   Expanded discrete frequency range.
  473 !      NSPL-H    Int.  Public   Range of 1D spectrum.
  474 !      SNSST     R.A.  Public   Array with interpolation weights.
  475 !      CNLSA     Real  Public   a34 in quadruplet definition.
  476 !      CNLSC     Real  Public   C in Snl definition.
  477 !      CNLSFM    Real  Public   Maximum relative spectral change.
  478 !      CNLSC1/3  Real  Public   Constant in frequency filter.
  479 !     ----------------------------------------------------------------
  480 !
  481 !     The structure SBTP contains parameters for the bottom friction
  482 !     source term and is aliased as above:
  483 !
  484 !      Name      Type  Scope    Description
  485 !     ----------------------------------------------------------------
  486 !      SBTC1     Real  Public   Proportionality constant.    (!/BT1)
  487 !      SBTCX     R.A.  Public   Parameters for bottom fric.  (!/BT4)
  488 !     ----------------------------------------------------------------
  489 !
  490 !     The structure SDBP contains parameters for the depth incduced
  491 !     breaking source term and is aliased as above:
  492 !
  493 !      Name      Type  Scope    Description
  494 !     ----------------------------------------------------------------
  495 !      SDBC1     Real  Public   Proportionality constant.    (!/DB1)
 
 
                                                         w3gdatmd.F90  page  10
 
 
  496 !      SDBC2     Real  Public   Hmax/d ratio.                (!/DB1)
  497 !      FDONLY    Log.  Public   Flag for checking depth only (!/DB1)
  498 !                               otherwise Miche criterion.
  499 !     ----------------------------------------------------------------
  500 !
  501 !     The structure STRP contains parameters for the triad interaction
  502 !     source term and is aliased as above:
  503 !
  504 !      Name      Type  Scope    Description
  505 !     ----------------------------------------------------------------
  506 !     ----------------------------------------------------------------
  507 !
  508 !     The structure SBSP contains parameters for the bottom scattering
  509 !     source term and is aliased as above:
  510 !
  511 !      Name      Type  Scope    Description
  512 !     ----------------------------------------------------------------
  513 !     ----------------------------------------------------------------
  514 !
  515 !     The structure SICP contains parameters for arbitrary source
  516 !     term and is aliased as above:
  517 !
  518 !      Name      Type  Scope    Description
  519 !     ----------------------------------------------------------------
  520 !     IS1C1      Real  Public   Scale factor for icecon.     (!/ISx)
  521 !     IS1C2      Real  Public   Offset for ice concentration (!/ISx)
  522 !     ----------------------------------------------------------------
  523 !
  524 !     The structure SXXP contains parameters for arbitrary source
  525 !     term and is aliased as above:
  526 !
  527 !      Name      Type  Scope    Description
  528 !     ----------------------------------------------------------------
  529 !     ----------------------------------------------------------------
  530 !
  531 !  3. Subroutines and functions :
  532 !
  533 !      Name      Type  Scope    Description
  534 !     ----------------------------------------------------------------
  535 !      W3NMOD    Subr. Public   Set number of grids.
  536 !      W3DIMX    Subr. Public   Set dimensions of spatial grid.
  537 !      W3DIMS    Subr. Public   Set dimensions of spectral grid.
  538 !      W3SETG    Subr. Public   Point to selected grid / model.
  539 !      W3GNTX    Subr. Public   Construct grid arrays
  540 !     ----------------------------------------------------------------
  541 !
  542 !  4. Subroutines and functions used :
  543 !
  544 !      Name      Type  Module   Description
  545 !     ----------------------------------------------------------------
  546 !      STRACE    Subr. W3SERVMD Subroutine tracing.
  547 !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
  548 !     ----------------------------------------------------------------
  549 !
  550 !  5. Remarks :
 
 
                                                         w3gdatmd.F90  page  11
 
 
  551 !
  552 !     - In model versions before 3.06 the parameters in the grid
  553 !       structure were stored in the module W3IOGR.
  554 !     - No subroutine DIMP is provided, instead, arrays are set
  555 !       one-by-one in W3IOGR.
  556 !
  557 !  6. Switches :
  558 !
  559 !     See subroutine documentation.
  560 !
  561 !     !/PRn  Select propagation scheme
  562 !     !/SMC  UNO2 propagation on SMC grid.
  563 !
  564 !     !/LNn  Select source terms
  565 !     !/STn
  566 !     !/NLn
  567 !     !/BTn
  568 !     !/DBn
  569 !     !/TRn
  570 !     !/BSn
  571 !     !/XXn
  572 !
  573 !     !/S    Enable subroutine tracing.
  574 !
  575 !  7. Source code :
  576 !
  577 !/ ------------------------------------------------------------------- /
  578 !/
  579 !/ Required modules
  580 !/
  581       USE W3GSRUMD
  582 !/
  583 !/ Specify default accessibility
  584 !/
  585       PUBLIC
  586 !/
  587 !/ Module private variable for checking error returns
  588 !/
  589       INTEGER, PRIVATE        :: ISTAT
  590 !/
  591 !/ Conventional declarations
  592 !/
  593       INTEGER                 :: NGRIDS = -1, IGRID = -1, ISGRD = -1, &
  594                                  IPARS = -1, NAUXGR
  595 !
  596       INTEGER, PARAMETER      :: RLGTYPE = 1
  597       INTEGER, PARAMETER      :: CLGTYPE = 2
  598       INTEGER, PARAMETER      :: UNGTYPE = 3
  599                                             
  600       INTEGER, PARAMETER      :: ICLOSE_NONE = ICLO_NONE
  601       INTEGER, PARAMETER      :: ICLOSE_SMPL = ICLO_SMPL
  602       INTEGER, PARAMETER      :: ICLOSE_TRPL = ICLO_TRPL
  603 !
  604 ! Dimensions of tables for pre-computing of dissipation
  605 !
 
 
                                                         w3gdatmd.F90  page  12
 
 
  606       INTEGER,    PARAMETER   :: NKHS=2000, NKD=1300
  607       INTEGER,    PARAMETER   :: NDTAB=2000
  608 !/
  609 !/ Data structures
  610 !/
  611 !/ Grid type
  612       TYPE GRID          ! this is the geographical grid with all associa>
  613         INTEGER          :: GTYPE
  614         INTEGER          :: RSTYPE = -1
  615         INTEGER          :: ICLOSE
  616         INTEGER          :: NX, NY, NSEA, NSEAL, TRFLAG
  617         INTEGER, POINTER :: MAPSTA(:,:), MAPST2(:,:),            &
  618                             MAPFS(:,:), MAPSF(:,:)
  619 !
  620         REAL             :: SX, SY, X0, Y0, DTCFL, DTCFLI, DTMAX,      &
  621                             DTMIN, DMIN, CTMAX, FICE0, FICEN, FICEL,   &
  622                             PFMOVE, STEXU, STEYU, STEDU, IICEHMIN,     &
  623                             IICEHINIT, ICESCALES(4), IICEHFAC, IICEHDISP,>
  624                             IICEDDISP, IICEFDISP, BTBETA, AAIRCMIN, AAIRGB
  625                                                                           
  626         REAL(8)          :: GRIDSHIFT ! see notes in WMGHGH
  627                                                            
  628                                                            
  629         REAL   , POINTER :: ZB(:)     ! BOTTOM GRID, DEFINED ON ISEA
  630         REAL   , POINTER :: CLATS(:)  ! COS(LAT), DEFINED ON SEA POINTS
  631         REAL   , POINTER :: CLATIS(:) ! INVERSE OF COS(LAT) DEFINED ON IS>
  632         REAL   , POINTER :: CTHG0S(:) ! TAN(Y)/R, DEFINED ON ISEA
  633                                                                  
  634         REAL   , POINTER :: TRNX(:,:), TRNY(:,:) ! TRANSPARENCY INFORMATI>
  635         REAL, POINTER         :: SPCBAC(:,:), ANGARC(:)
  636         REAL   , POINTER :: XGRD(:,:), YGRD(:,:) ! X AND Y DEFINED ON IX,>
  637         REAL   , POINTER :: DXDP(:,:), DXDQ(:,:) ! DX/DP & DX/DQ DEFINED >
  638         REAL   , POINTER :: DYDP(:,:), DYDQ(:,:) ! DY/DP & DY/DQ DEFINED >
  639         REAL   , POINTER :: DPDX(:,:), DPDY(:,:) ! DP/DX & DP/DY DEFINED >
  640         REAL   , POINTER :: DQDX(:,:), DQDY(:,:) ! DQ/DX & DQ/DY DEFINED >
  641         REAL   , POINTER :: GSQRT(:,:) ! SQRT(G) DEFINED ON IX,IY
  642         REAL   , POINTER :: HPFAC(:,:) ! H_P = SQRT(G_PP) DEFINED ON IX,IY
  643         REAL   , POINTER :: HQFAC(:,:) ! H_Q = SQRT(G_QQ) DEFINED ON IX,IY
  644                                                                           
  645         LOGICAL          :: GINIT, FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU,>
  646                             IICESMOOTH
  647         LOGICAL          :: FLAGLL
  648         LOGICAL          :: CMPRTRCK
  649         LOGICAL, POINTER :: FLAGST(:)
  650         CHARACTER(LEN=30):: GNAME
  651         CHARACTER(LEN=13):: FILEXT
  652         LOGICAL          :: GUGINIT
  653         REAL, POINTER    :: REFLC(:,:)  ! reflection coefficient
  654         INTEGER, POINTER :: REFLD(:,:)  ! reflection direction
  655         INTEGER          :: E3DF(3,5), P2MSF(3), US3DF(3), USSPF(2) ! fre>
  656         REAL             :: USSP_WN(25) !Max set to 25 decay scales.
  657 !
  658         TYPE(T_GSU) :: GSU ! Grid search utility object
  659 !
  660         REAL                  :: FFACBERG    ! mutiplicative factor for i>
 
 
                                                         w3gdatmd.F90  page  13
 
 
  661         LOGICAL, POINTER      :: RREF(:)
  662         REAL,    POINTER      :: REFPARS(:)
  663         REAL,    POINTER      :: IGPARS(:)
  664 !
  665 ! unstructured data
  666 !
  667         INTEGER               :: NTRI
  668         DOUBLE PRECISION, POINTER         :: XYB(:,:)
  669         INTEGER, POINTER      :: TRIGP(:,:)
  670         REAL(8), POINTER      :: LEN(:,:),SI(:), IEN(:,:)
  671                                                          
  672         REAL                  :: MAXX, MAXY, DXYMAX
  673         REAL, POINTER         :: ANGLE(:,:),ANGLE0(:,:)
  674         INTEGER               :: COUNTRI,COUNTOT,NNZ, NBEDGE
  675         INTEGER, POINTER      :: CCON(:), COUNTCON(:), IE_CELL(:), &
  676                                  POS_CELL(:), IOBP(:), IOBPD(:,:), IOBDP(>
  677                                  IAA(:), JAA(:), POSI(:,:), INDEX_CELL(:),
  678                                  I_DIAG(:), JA_IE(:,:,:)
  679         INTEGER, POINTER      :: EDGES(:,:), NEIGH(:,:)
  680         REAL(8), POINTER      :: TRIA(:)
  681         REAL, POINTER         :: CROSSDIFF(:,:)
  682                                                
  683                                                
  684       END TYPE GRID
  685 !
  686       TYPE SGRD   ! this is the spectral grid with all parameters that va>
  687         INTEGER               :: NK=0, NK2=0, NTH=0, NSPEC=0
  688         INTEGER, POINTER      :: MAPWN(:), MAPTH(:)
  689         REAL                  :: DTH=0., XFR=0., FR1=0., FTE=0., FTF=0., >
  690                                  FTWL=0., FACTI1=0., FACTI2=0., FACHFA=0.>
  691         REAL, POINTER         :: TH(:), ESIN(:), ECOS(:), ES2(:),     &
  692                                  ESC(:), EC2(:), SIG(:), SIG2(:),     &
  693                                  DSIP(:), DSII(:), DDEN(:), DDEN2(:)
  694         LOGICAL               :: SINIT=.FALSE.
  695       END TYPE SGRD
  696 !
  697       TYPE NPAR
  698         REAL                  :: FACP, XREL, XFLT, FXFM, FXPM,        &
  699                                  XFT, XFC, FACSD, FHMAX
  700         REAL            :: RWINDC
  701       END TYPE NPAR
  702 !
  703       TYPE PROP
  704         REAL                  :: WDCG, WDTH
  705       END TYPE PROP
  706 !
  707       TYPE FLDP
  708          REAL :: DUMMY
  709       END TYPE FLDP
  710       TYPE SFLP
  711         REAL                  :: DUMMY
  712       END TYPE SFLP
  713 !
  714       TYPE SLNP
  715         REAL                  :: SLNC1, FSPM, FSHF
 
 
                                                         w3gdatmd.F90  page  14
 
 
  716       END TYPE SLNP
  717 !
  718       TYPE SRCP
  719         REAL                       :: WWNMEANPTAIL, SSTXFTFTAIL
  720 !
  721         INTEGER               :: SSWELLFPAR, SSDSISO, SSDSBRFDF
  722         INTEGER,  POINTER     :: IKTAB(:,:), SATINDICES(:,:)
  723         REAL,     POINTER     :: DCKI(:,:), SATWEIGHTS(:,:),CUMULW(:,:),Q>
  724         REAL                  :: AALPHA, BBETA, ZZ0MAX, ZZ0RAT, ZZALP,&
  725                                  SSINTHP, TTAUWSHELTER, SSWELLF(1:7), &
  726                                  SSDSC(1:21), SSDSBR,                 &
  727                                  SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
  728                                  FFXPM, FFXFM, FFXFA,   &
  729                                  SSDSBRF1, SSDSBRF2, SSDSBINT,SSDSBCK,&
  730                                  SSDSHCK, SSDSABK, SSDSPBK, SSINBR
  731         REAL                  :: ZZWND
  732         REAL                  :: SSDSCOS, SSDSDTH, SSDSBT, SSDSBM(0:4)
  733 !
  734       END TYPE SRCP
  735 !
  736       TYPE SNLP
  737         REAL                  :: SNLC1, LAM, KDCON, KDMN,             &
  738                                  SNLS1, SNLS2, SNLS3
  739                                                     
  740       END TYPE SNLP
  741 !
  742       TYPE SBTP
  743         REAL                  :: DUMMY
  744       END TYPE SBTP
  745 !
  746       TYPE SDBP
  747         REAL                  :: DUMMY
  748       END TYPE SDBP
  749                    
  750                    
  751 !
  752       TYPE STRP
  753         REAL                  :: DUMMY
  754       END TYPE STRP
  755 !
  756       TYPE SBSP
  757         REAL                  :: DUMMY
  758       END TYPE SBSP
  759 !
  760       TYPE SICP
  761         REAL                  :: DUMMY
  762       END TYPE SICP
  763 !
  764       TYPE SXXP
  765         REAL                  :: DUMMY
  766       END TYPE SXXP
  767                    
  768 ! specific type for unstructured scheme
  769       TYPE SCHM
  770          LOGICAL :: FSN          = .FALSE.
 
 
                                                         w3gdatmd.F90  page  15
 
 
  771          LOGICAL :: FSPSI        = .FALSE.
  772          LOGICAL :: FSFCT        = .FALSE.
  773          LOGICAL :: FSNIMP       = .FALSE.
  774          LOGICAL :: FSTOTALIMP   = .FALSE.
  775          LOGICAL :: FSTOTALEXP   = .FALSE.
  776          LOGICAL :: FSREFRACTION = .FALSE.
  777          LOGICAL :: FSFREQSHIFT  = .FALSE.
  778          LOGICAL :: FSSOURCE     = .FALSE.
  779          LOGICAL :: FSBCCFL      = .FALSE.
  780          LOGICAL :: DO_CHANGE_WLV
  781          REAL(8) :: SOLVERTHR_STP
  782          REAL(8) :: CRIT_DEP_STP
  783          LOGICAL :: B_JGS_TERMINATE_MAXITER
  784          LOGICAL :: B_JGS_TERMINATE_DIFFERENCE
  785          LOGICAL :: B_JGS_TERMINATE_NORM
  786          LOGICAL :: B_JGS_LIMITER
  787          LOGICAL :: B_JGS_USE_JACOBI
  788          LOGICAL :: B_JGS_BLOCK_GAUSS_SEIDEL
  789          INTEGER :: B_JGS_MAXITER
  790          REAL*8  :: B_JGS_PMIN
  791          REAL*8  :: B_JGS_DIFF_THR
  792          REAL*8  :: B_JGS_NORM_THR
  793          INTEGER :: B_JGS_NLEVEL
  794          LOGICAL :: B_JGS_SOURCE_NONLINEAR
  795       END TYPE SCHM
  796 !
  797       TYPE MPAR
  798         LOGICAL               :: PINIT
  799         TYPE(NPAR)            :: NPARS
  800         TYPE(PROP)            :: PROPS
  801         TYPE(FLDP)            :: FLDPS
  802         TYPE(SFLP)            :: SFLPS
  803         TYPE(SLNP)            :: SLNPS
  804         TYPE(SRCP)            :: SRCPS
  805         TYPE(SNLP)            :: SNLPS
  806         TYPE(SBTP)            :: SBTPS
  807         TYPE(SDBP)            :: SDBPS
  808         TYPE(STRP)            :: STRPS
  809         TYPE(SBSP)            :: SBSPS
  810         TYPE(SICP)            :: SICPS
  811         TYPE(SXXP)            :: SXXPS
  812         TYPE(SCHM)            :: SCHMS
  813       END TYPE MPAR
  814 !/
  815 !/ Data storage
  816 !/
  817       TYPE(GRID), TARGET, ALLOCATABLE :: GRIDS(:)
  818       TYPE(SGRD), TARGET, ALLOCATABLE :: SGRDS(:)
  819       TYPE(MPAR), TARGET, ALLOCATABLE :: MPARS(:)
  820 !/
  821 !/ Data aliases for structure GRID(S)
  822 !/
  823       INTEGER, POINTER :: GTYPE
  824       INTEGER, POINTER :: RSTYPE
  825       INTEGER, POINTER :: ICLOSE
 
 
                                                         w3gdatmd.F90  page  16
 
 
  826       INTEGER, POINTER        :: NX, NY, NSEA, NSEAL, TRFLAG
  827       INTEGER, POINTER        :: E3DF(:,:), P2MSF(:), US3DF(:), USSPF(:)
  828       REAL,    POINTER        :: USSP_WN(:)
  829       REAL,    POINTER        :: REFLC(:,:)
  830       INTEGER, POINTER        :: REFLD(:,:)
  831       INTEGER, POINTER        :: NBEDGE
  832       INTEGER, POINTER        :: EDGES(:,:), NEIGH(:,:)
  833 !
  834 ! Variables for unstructured grids
  835 !
  836       INTEGER, POINTER        :: NTRI,COUNTRI,COUNTOT,NNZ
  837       INTEGER                 :: optionCall = 3 ! take care all other opt>
  838 !  XYB may not be necessary now that we have XGRD and YGRD
  839 !  but these XGRD and YGRD should probably be double precision
  840       DOUBLE PRECISION, POINTER  ::     XYB(:,:)
  841       INTEGER, POINTER        :: TRIGP(:,:)
  842       REAL(8), POINTER        :: IEN(:,:), LEN(:,:), SI(:)
  843       REAL, POINTER           :: ANGLE(:,:),ANGLE0(:,:)
  844       INTEGER,  POINTER       :: CCON(:),  COUNTCON(:), IE_CELL(:),      >
  845                                  POS_CELL(:), IOBP(:), IOBPD(:,:), IOBDP(>
  846                                  IAA(:), JAA(:), POSI(:,:),              >
  847                                  I_DIAG(:), JA_IE(:,:,:),                >
  848                                  INDEX_CELL(:)
  849       REAL(8), POINTER        :: TRIA(:)
  850       REAL, POINTER           :: CROSSDIFF(:,:)
  851       REAL,POINTER            :: MAXX, MAXY, DXYMAX
  852       LOGICAL, POINTER        :: GUGINIT
  853 !
  854       REAL,    POINTER        :: FFACBERG
  855       LOGICAL, POINTER        ::  RREF(:)
  856       REAL,    POINTER        :: REFPARS(:)
  857       REAL,    POINTER        :: IGPARS(:)
  858       INTEGER, POINTER        :: MAPSTA(:,:), MAPST2(:,:),            &
  859                                  MAPFS(:,:), MAPSF(:,:)
  860 !
  861       REAL, POINTER           :: SX, SY, X0, Y0, DTCFL, DTCFLI, DTMAX, &
  862                                  DTMIN, DMIN, CTMAX, FICE0, FICEN,     &
  863                                  FICEL, PFMOVE, STEXU, STEYU, STEDU,   &
  864                                  IICEHMIN, IICEHINIT, ICESCALES(:),    &
  865                                  IICEHFAC, IICEHDISP, IICEDDISP, IICEFDIS>
  866                                  BTBETA, AAIRCMIN, AAIRGB
  867       REAL(8),POINTER         :: GRIDSHIFT ! see notes in WMGHGH
  868       REAL, POINTER           :: ZB(:), CLATS(:)
  869       REAL   , POINTER :: CLATIS(:) ! INVERSE OF COS(LAT) DEFINED ON ISEA
  870       REAL   , POINTER :: CTHG0S(:) ! TAN(Y)/R, DEFINED ON ISEA
  871                                                                
  872       REAL   , POINTER :: TRNX(:,:), TRNY(:,:) ! TRANSPARENCY INFORMATION>
  873       REAL, POINTER         :: SPCBAC(:,:), ANGARC(:)
  874       REAL   , POINTER :: XGRD(:,:), YGRD(:,:) ! X AND Y DEFINED ON IX,IY
  875       REAL   , POINTER :: DXDP(:,:), DXDQ(:,:) ! DX/DP & DX/DQ DEFINED ON>
  876       REAL   , POINTER :: DYDP(:,:), DYDQ(:,:) ! DY/DP & DY/DQ DEFINED ON>
  877       REAL   , POINTER :: DPDX(:,:), DPDY(:,:) ! DP/DX & DP/DY DEFINED ON>
  878       REAL   , POINTER :: DQDX(:,:), DQDY(:,:) ! DQ/DX & DQ/DY DEFINED ON>
  879       REAL   , POINTER :: GSQRT(:,:) ! SQRT(G) DEFINED ON IX,IY
  880       REAL   , POINTER :: HPFAC(:,:) ! H_P = SQRT(G_PP) DEFINED ON IX,IY
 
 
                                                         w3gdatmd.F90  page  17
 
 
  881       REAL   , POINTER :: HQFAC(:,:) ! H_Q = SQRT(G_QQ) DEFINED ON IX,IY
  882                                                                         
  883       LOGICAL, POINTER :: GINIT, FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU, I>
  884                           IICESMOOTH
  885       LOGICAL, POINTER :: FLAGLL
  886       LOGICAL, POINTER :: CMPRTRCK
  887       LOGICAL, POINTER :: FLAGST(:)
  888                                    
  889       CHARACTER(LEN=30), POINTER :: GNAME
  890       CHARACTER(LEN=13), POINTER :: FILEXT
  891                                           
  892       TYPE(T_GSU), POINTER :: GSU ! Grid search utility object
  893 !/
  894 !/ Data aliasses for structure SGRD(S)
  895 !/
  896       INTEGER, POINTER        :: NK, NK2, NTH, NSPEC
  897       INTEGER, POINTER        :: MAPWN(:), MAPTH(:)
  898       REAL, POINTER           :: DTH, XFR, FR1, FTE, FTF, FTWN, FTTR, &
  899                                  FTWL, FACTI1, FACTI2, FACHFA, FACHFE
  900       REAL, POINTER           :: TH(:), ESIN(:), ECOS(:), ES2(:),     &
  901                                  ESC(:), EC2(:), SIG(:), SIG2(:),     &
  902                                  DSIP(:), DSII(:), DDEN(:), DDEN2(:)
  903       LOGICAL, POINTER        :: SINIT
  904 !/
  905 !/ Data aliasses for structure MPAR(S)
  906 !/
  907       LOGICAL, POINTER        :: PINIT
  908 !/
  909 !/ Data aliasses for structure NPAR(S)
  910 !/
  911       REAL, POINTER           :: FACP, XREL, XFLT, FXFM, FXPM,        &
  912                                  XFT, XFC, FACSD, FHMAX
  913  REAL, POINTER           :: RWINDC
  914 !/
  915 !/ Data aliasses for structure PROP(S)
  916 !/
  917       REAL, POINTER           :: WDCG, WDTH
  918 !/
  919 !/ Data aliasses for structure FLDP(S)
  920 !/
  921 !/
  922 !/ Data aliasses for structure SFLP(S)
  923 !/
  924 !/
  925 !/ Data aliasses for structure SLNP(S)
  926 !/
  927       REAL, POINTER           :: SLNC1, FSPM, FSHF
  928 !/
  929 !/ Data aliasses for structure SRCP(S)
  930 !/
  931       INTEGER, POINTER        :: SSWELLFPAR, SSDSISO,SSDSBRFDF,       &
  932                                  IKTAB(:,:), SATINDICES(:,:),SSDSDIK
  933       REAL, POINTER           :: DCKI(:,:), SATWEIGHTS(:,:),CUMULW(:,:),Q>
  934       REAL, POINTER           :: ZZWND, AALPHA, BBETA, ZZ0MAX, ZZ0RAT,&
  935                                  ZZALP, FFXFA,          &
 
 
                                                         w3gdatmd.F90  page  18
 
 
  936                                  FFXFM, FFXPM, SSDSBRF1, SSDSBRF2,    &
  937                                  SSDSBINT, SSDSBCK, SSDSHCK, SSDSABK, &
  938                                  SSDSPBK, SSINBR,SSINTHP,TTAUWSHELTER,&
  939                                  SSWELLF(:), SSDSC(:), SSDSBR,        &
  940                                  SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
  941                                  SSDSBT, SSDSCOS, SSDSDTH, SSDSBM(:)
  942       REAL, POINTER           :: WWNMEANPTAIL, SSTXFTFTAIL
  943 !/
  944 !/ Data aliasses for structure SNLP(S)
  945 !/
  946       REAL, POINTER           :: SNLC1, LAM, KDCON, KDMN,             &
  947                                  SNLS1, SNLS2, SNLS3
  948 !/
  949 !/ Data aliasses for structure SBTP(S)
  950 !/
  951 !/
  952 !/ Data aliasses for structure SDBP(S)
  953 !/
  954 !/
  955 !/
  956 !/ Data aliasing for structure SCHM(S)
  957       LOGICAL, POINTER  :: FSN,FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP
  958       LOGICAL, POINTER  :: FSREFRACTION, FSFREQSHIFT, FSSOURCE, FSBCCFL
  959       LOGICAL, POINTER  :: DO_CHANGE_WLV
  960       REAL(8), POINTER  :: SOLVERTHR_STP
  961       REAL(8), POINTER  :: CRIT_DEP_STP
  962       LOGICAL, POINTER :: B_JGS_TERMINATE_MAXITER
  963       LOGICAL, POINTER :: B_JGS_TERMINATE_DIFFERENCE
  964       LOGICAL, POINTER :: B_JGS_TERMINATE_NORM
  965       LOGICAL, POINTER :: B_JGS_LIMITER
  966       LOGICAL, POINTER :: B_JGS_USE_JACOBI
  967       LOGICAL, POINTER :: B_JGS_BLOCK_GAUSS_SEIDEL
  968       INTEGER, POINTER :: B_JGS_MAXITER
  969       REAL(8), POINTER :: B_JGS_PMIN
  970       REAL(8), POINTER :: B_JGS_DIFF_THR
  971       REAL(8), POINTER :: B_JGS_NORM_THR
  972       INTEGER, POINTER :: B_JGS_NLEVEL
  973       LOGICAL, POINTER :: B_JGS_SOURCE_NONLINEAR
  974 !/
  975 !/ Data aliasing for structure SICP(S)
  976 !/
  977       CONTAINS
  978 !/ ------------------------------------------------------------------- /
  979       SUBROUTINE W3NMOD ( NUMBER, NDSE, NDST, NAUX )
  980 !/
  981 !/                  +-----------------------------------+
  982 !/                  | WAVEWATCH III           NOAA/NCEP |
  983 !/                  |           H. L. Tolman            |
  984 !/                  |                        FORTRAN 90 |
  985 !/                  | Last update :         10-Dec-2014 !
  986 !/                  +-----------------------------------+
  987 !/
  988 !/    24-Feb-2004 : Origination.                        ( version 3.06 )
  989 !/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
  990 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
 
 
                                                         w3gdatmd.F90  page  19
 
 
  991 !/
  992 !  1. Purpose :
  993 !
  994 !     Set up the number of grids to be used.
  995 !
  996 !  2. Method :
  997 !
  998 !     Store in NGRIDS and allocate GRIDS.
  999 !
 1000 !  3. Parameters :
 1001 !
 1002 !     Parameter list
 1003 !     ----------------------------------------------------------------
 1004 !       NUMBER  Int.   I   Number of grids to be used.
 1005 !       NDSE    Int.   I   Error output unit number.
 1006 !       NDST    Int.   I   Test output unit number.
 1007 !       NAUX    Int.   I   Number of auxiliary grids to be used.
 1008 !                          Grids -NAUX:NUBMER are defined, optional
 1009 !                          parameters.
 1010 !     ----------------------------------------------------------------
 1011 !
 1012 !  4. Subroutines used :
 1013 !
 1014 !     See module documentation.
 1015 !
 1016 !  5. Called by :
 1017 !
 1018 !     Any program that uses this grid structure.
 1019 !
 1020 !  6. Error messages :
 1021 !
 1022 !     - Error checks on previous setting of variable.
 1023 !
 1024 !  7. Remarks :
 1025 !
 1026 !  8. Structure :
 1027 !
 1028 !  9. Switches :
 1029 !
 1030 !     !/S    Enable subroutine tracing.
 1031 !
 1032 ! 10. Source code :
 1033 !
 1034 !/ ------------------------------------------------------------------- /
 1035       USE W3SERVMD, ONLY: EXTCDE
 1036 !
 1037       IMPLICIT NONE
 1038 !/
 1039 !/ ------------------------------------------------------------------- /
 1040 !/ Parameter list
 1041 !/
 1042       INTEGER, INTENT(IN)           :: NUMBER, NDSE, NDST
 1043       INTEGER, INTENT(IN), OPTIONAL :: NAUX
 1044 !/
 1045 !/ ------------------------------------------------------------------- /
 
 
                                                         w3gdatmd.F90  page  20
 
 
 1046 !/ Local parameters
 1047 !/
 1048       INTEGER                 :: I, NLOW
 1049 !/
 1050 !
 1051 ! -------------------------------------------------------------------- /
 1052 ! 1.  Test input and module status
 1053 !
 1054       IF ( NGRIDS .NE. -1 ) THEN
 1055           WRITE (NDSE,1001) NGRIDS
 1056           CALL EXTCDE (1)
 1057         END IF
 1058 !
 1059       IF ( NUMBER .LT. 1 ) THEN
 1060           WRITE (NDSE,1002) NUMBER
 1061           CALL EXTCDE (2)
 1062         END IF
 1063 !
 1064       IF ( PRESENT(NAUX) ) THEN
 1065           NLOW   = -NAUX
 1066         ELSE
 1067           NLOW   = 1
 1068         END IF
 1069 !
 1070       IF ( NLOW .GT. 1 ) THEN
 1071           WRITE (NDSE,1003) -NLOW
 1072           CALL EXTCDE (3)
 1073         END IF
 1074 !
 1075 ! -------------------------------------------------------------------- /
 1076 ! 1.  Set variable and allocate arrays
 1077 !
 1078       NGRIDS = NUMBER
 1079       NAUXGR = - NLOW
 1080       ALLOCATE ( GRIDS(NLOW:NUMBER), &
 1081                  SGRDS(NLOW:NUMBER), &
 1082                  MPARS(NLOW:NUMBER), &
 1083                  STAT=ISTAT )
 1084       CHECK_ALLOC_STATUS ( ISTAT )
 1085 !
 1086 ! -------------------------------------------------------------------- /
 1087 ! 2.  Initialize GINIT and SINIT
 1088 !
 1089       DO I=NLOW, NUMBER
 1090         GRIDS(I)%GINIT  = .FALSE.
 1091         GRIDS(I)%GUGINIT  = .FALSE.
 1092         SGRDS(I)%SINIT  = .FALSE.
 1093         MPARS(I)%PINIT  = .FALSE.
 1094         END DO
 1095 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3NMOD)
 1096       WRITE (NDST,9000) NLOW, NGRIDS
 1097 #endif
 1098 !
 1099       RETURN
 1100 !
 
 
                                                         w3gdatmd.F90  page  21
 
 
 1101 ! Formats
 1102 !
 1103  1001 FORMAT (/' *** ERROR W3NMOD : GRIDS ALREADY INITIALIZED *** '/  &
 1104                '                    NGRIDS = ',I10/)
 1105  1002 FORMAT (/' *** ERROR W3NMOD : ILLEGAL NUMBER OF GRIDS *** '/    &
 1106                '                    NUMBER = ',I10/)
 1107  1003 FORMAT (/' *** ERROR W3NMOD : ILLEGAL NUMBER OF AUX GRIDS *** '/&
 1108                '                    NUMBER = ',I10/)
 1109 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3NMOD)
 1110  9000 FORMAT (' TEST W3NMOD : SETTING UP FOR GRIDS ',I3,           &
 1111               ' THROUGH ',I3)
 1112 #endif
 1113 !/
 1114 !/ End of W3NMOD ----------------------------------------------------- /
 1115 !/
 1116       END SUBROUTINE W3NMOD
 1117 !/ ------------------------------------------------------------------- /
 1118       SUBROUTINE W3DIMX  ( IMOD, MX, MY, MSEA, NDSE, NDST   &
 1119                          )
 1120 !/
 1121 !/                  +-----------------------------------+
 1122 !/                  | WAVEWATCH III           NOAA/NCEP |
 1123 !/                  |           H. L. Tolman            |
 1124 !/                  |                        FORTRAN 90 |
 1125 !/                  | Last update :         10-Dec-2014 |
 1126 !/                  +-----------------------------------+
 1127 !/
 1128 !/    24-Jun-2005 : Origination.                        ( version 3.07 )
 1129 !/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
 1130 !/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
 1131 !/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
 1132 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
 1133 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 1134 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
 1135 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 1136 !/    30-Oct-2009 : Implement unstructured grids        ( version 3.14.1)
 1137 !/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
 1138 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
 1139 !/
 1140 !  1. Purpose :
 1141 !
 1142 !     Initialize an individual spatial grid at the proper dimensions.
 1143 !
 1144 !  2. Method :
 1145 !
 1146 !     Allocate directly into the structure array GRIDS. Note that
 1147 !     this cannot be done through the pointer alias!
 1148 !
 1149 !  3. Parameters :
 1150 !
 1151 !     Parameter list
 1152 !     ----------------------------------------------------------------
 1153 !       IMOD    Int.   I   Model number to point to.
 1154 !       NDSE    Int.   I   Error output unit number.
 1155 !       NDST    Int.   I   Test output unit number.
 
 
                                                         w3gdatmd.F90  page  22
 
 
 1156 !       MX, MY, MSEA       Like NX, NY, NSEA in data structure.
 1157 !     ----------------------------------------------------------------
 1158 !
 1159 !  4. Subroutines used :
 1160 !
 1161 !       See module documentation.
 1162 !
 1163 !  5. Called by :
 1164 !
 1165 !      Name      Type  Module   Description
 1166 !     ----------------------------------------------------------------
 1167 !      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
 1168 !      WW3_GRID  Prog.   N/A    Model set up program.
 1169 !     ----------------------------------------------------------------
 1170 !
 1171 !  6. Error messages :
 1172 !
 1173 !     - Check on input parameters.
 1174 !     - Check on previous allocation.
 1175 !
 1176 !  7. Remarks :
 1177 !
 1178 !     - Grid dimensions apre passed through parameter list and then
 1179 !       locally stored to assure consistency between allocation and
 1180 !       data in structure.
 1181 !     - W3SETG needs to be called after allocation to point to
 1182 !       proper allocated arrays.
 1183 !
 1184 !  8. Structure :
 1185 !
 1186 !     See source code.
 1187 !
 1188 !  9. Switches :
 1189 !
 1190 !     !/S    Enable subroutine tracing.
 1191 !
 1192 ! 10. Source code :
 1193 !
 1194 !/ ------------------------------------------------------------------- /
 1195       USE W3SERVMD, ONLY: EXTCDE
 1196 !
 1197       IMPLICIT NONE
 1198 !
 1199 !/
 1200 !/ ------------------------------------------------------------------- /
 1201 !/ Parameter list
 1202 !/
 1203       INTEGER, INTENT(IN)     :: IMOD, MX, MY, MSEA, NDSE, NDST
 1204 !/
 1205 !/ ------------------------------------------------------------------- /
 1206 !/ Local parameters
 1207 !/
 1208 !/
 1209 !
 1210 ! -------------------------------------------------------------------- /
 
 
                                                         w3gdatmd.F90  page  23
 
 
 1211 ! 1.  Test input and module status
 1212 !
 1213       IF ( NGRIDS .EQ. -1 ) THEN
 1214           WRITE (NDSE,1001)
 1215           CALL EXTCDE (1)
 1216         END IF
 1217 !
 1218       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
 1219           WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
 1220           CALL EXTCDE (2)
 1221         END IF
 1222 !
 1223       IF ( MX.LT.3 .OR. (MY.LT.3.AND.GTYPE.NE.UNGTYPE) .OR. MSEA.LT.1 ) T>
 1224         WRITE (NDSE,1003) MX, MY, MSEA, GTYPE
 1225         CALL EXTCDE (3)
 1226         END IF
 1227 !
 1228       IF ( GRIDS(IMOD)%GINIT ) THEN
 1229           WRITE (NDSE,1004)
 1230           CALL EXTCDE (4)
 1231         END IF
 1232 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 1233       WRITE (NDST,9000) IMOD, MX, MY, MSEA
 1234 #endif
 1235 !
 1236 ! -------------------------------------------------------------------- /
 1237 ! 2.  Allocate arrays
 1238 !
 1239 ! NB: Some array start at 0 because MAPFS(IY,IX)=0 for missing points
 1240 !
 1241       ALLOCATE ( GRIDS(IMOD)%MAPSTA(MY,MX),  &
 1242                  GRIDS(IMOD)%MAPST2(MY,MX),  &
 1243                  GRIDS(IMOD)%MAPFS(MY,MX),   &
 1244                  GRIDS(IMOD)%MAPSF(MSEA,3),  &
 1245                  GRIDS(IMOD)%FLAGST(MSEA),   &
 1246                  GRIDS(IMOD)%ZB(MSEA),       &
 1247                  GRIDS(IMOD)%CLATS(0:MSEA),  &
 1248                  GRIDS(IMOD)%CLATIS(0:MSEA), &
 1249                  GRIDS(IMOD)%CTHG0S(0:MSEA), &
 1250                  GRIDS(IMOD)%TRNX(MY,MX),    &
 1251                  GRIDS(IMOD)%TRNY(MY,MX),    &
 1252                  GRIDS(IMOD)%XGRD(MY,MX),    &
 1253                  GRIDS(IMOD)%YGRD(MY,MX),    &
 1254                  GRIDS(IMOD)%DXDP(MY,MX),    &
 1255                  GRIDS(IMOD)%DXDQ(MY,MX),    &
 1256                  GRIDS(IMOD)%DYDP(MY,MX),    &
 1257                  GRIDS(IMOD)%DYDQ(MY,MX),    &
 1258                  GRIDS(IMOD)%DPDX(MY,MX),    &
 1259                  GRIDS(IMOD)%DPDY(MY,MX),    &
 1260                  GRIDS(IMOD)%DQDX(MY,MX),    &
 1261                  GRIDS(IMOD)%DQDY(MY,MX),    &
 1262                  GRIDS(IMOD)%GSQRT(MY,MX),   &
 1263                  GRIDS(IMOD)%HPFAC(MY,MX),   &
 1264                  GRIDS(IMOD)%HQFAC(MY,MX),   &
 1265                  STAT=ISTAT                  )
 
 
                                                         w3gdatmd.F90  page  24
 
 
 1266       CHECK_ALLOC_STATUS ( ISTAT )
 1267 !!/DEBUGINIT         WRITE(740+IAPROC,*) 'After alocation of MAPST2, MY='>
 1268 !!/DEBUGINIT         FLUSH(740+IAPROC)
 1269 !
 1270       GRIDS(IMOD)%FLAGST = .TRUE.
 1271       GRIDS(IMOD)%GINIT  = .TRUE.
 1272       GRIDS(IMOD)%MAPSF(:,3)=0.
 1273       GRIDS(IMOD)%CLATS(0)=1.
 1274       GRIDS(IMOD)%CLATIS(0)=1.
 1275       GRIDS(IMOD)%CTHG0S(0)=1.
 1276 !
 1277       ALLOCATE ( GRIDS(IMOD)%RREF(4),     &
 1278                  GRIDS(IMOD)%REFPARS(10), &
 1279                  STAT=ISTAT               )
 1280       CHECK_ALLOC_STATUS ( ISTAT )
 1281 !
 1282       GRIDS(IMOD)%RREF(:)=.FALSE.
 1283       GRIDS(IMOD)%REFPARS(:)=0.
 1284 !
 1285       ALLOCATE ( GRIDS(IMOD)%REFLC(4,0:NSEA), &
 1286                  GRIDS(IMOD)%REFLD(6,0:NSEA), &
 1287                  STAT=ISTAT                    )
 1288       CHECK_ALLOC_STATUS ( ISTAT )
 1289        ALLOCATE ( GRIDS(IMOD)%IGPARS(12), STAT=ISTAT )
 1290        CHECK_ALLOC_STATUS ( ISTAT )
 1291                                    
 1292 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 1293       WRITE (NDST,9001)
 1294 #endif
 1295 !
 1296       GRIDS(IMOD)%REFLC(1:4,0:NSEA)=0.
 1297       GRIDS(IMOD)%REFLD(:,:)=0
 1298       GRIDS(IMOD)%IGPARS(:)=0.
 1299 !
 1300 ! -------------------------------------------------------------------- /
 1301 ! 2.  Update counters in grid
 1302 !
 1303       GRIDS(IMOD)%NX     = MX
 1304       GRIDS(IMOD)%NY     = MY
 1305       GRIDS(IMOD)%NSEA   = MSEA
 1306 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 1307       WRITE (NDST,9002)
 1308 #endif
 1309 !
 1310 ! -------------------------------------------------------------------- /
 1311 ! 3.  Point to allocated arrays
 1312 !
 1313       CALL W3SETG ( IMOD, NDSE, NDST )
 1314 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 1315       WRITE (NDST,9003)
 1316 #endif
 1317 !
 1318       RETURN
 1319 !
 1320 ! Formats
 
 
                                                         w3gdatmd.F90  page  25
 
 
 1321 !
 1322  1001 FORMAT (/' *** ERROR W3DIMX : GRIDS NOT INITIALIZED *** '/      &
 1323                '                    RUN W3NMOD FIRST '/)
 1324  1002 FORMAT (/' *** ERROR W3DIMX : ILLEGAL MODEL NUMBER *** '/       &
 1325                '                    IMOD   = ',I10/                   &
 1326                '                    NAUXGR = ',I10/                   &
 1327                '                    NGRIDS = ',I10/)
 1328  1003 FORMAT (/' *** ERROR W3DIMX : ILLEGAL GRID DIMENSION(S) *** '/  &
 1329                '                    INPUT = ',4I10 /)
 1330  1004 FORMAT (/' *** ERROR W3DIMX : ARRAY(S) ALREADY ALLOCATED *** ')
 1331 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 1332  9000 FORMAT (' TEST W3DIMX : MODEL ',I4,' DIM. AT ',2I5,I7)
 1333  9001 FORMAT (' TEST W3DIMX : ARRAYS ALLOCATED')
 1334  9002 FORMAT (' TEST W3DIMX : DIMENSIONS STORED')
 1335  9003 FORMAT (' TEST W3DIMX : POINTERS RESET')
 1336 #endif
 1337 !/
 1338 !/ End of W3DIMX ----------------------------------------------------- /
 1339 !/
 1340       END SUBROUTINE W3DIMX
 1341 !/ ------------------------------------------------------------------- /
 1342       SUBROUTINE W3DIMS  ( IMOD, MK, MTH, NDSE, NDST )
 1343 !/
 1344 !/                  +-----------------------------------+
 1345 !/                  | WAVEWATCH III           NOAA/NCEP |
 1346 !/                  |           H. L. Tolman            |
 1347 !/                  |                        FORTRAN 90 |
 1348 !/                  | Last update :         10-Dec-2014 !
 1349 !/                  +-----------------------------------+
 1350 !/
 1351 !/    19-Feb-2004 : Origination.                        ( version 3.06 )
 1352 !/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
 1353 !/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
 1354 !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
 1355 !/
 1356 !  1. Purpose :
 1357 !
 1358 !     Initialize an individual spatial grid at the proper dimensions.
 1359 !
 1360 !  2. Method :
 1361 !
 1362 !     Allocate directly into the structure array GRIDS. Note that
 1363 !     this cannot be done through the pointer alias!
 1364 !
 1365 !  3. Parameters :
 1366 !
 1367 !     Parameter list
 1368 !     ----------------------------------------------------------------
 1369 !       IMOD    Int.   I   Model number to point to.
 1370 !       NDSE    Int.   I   Error output unit number.
 1371 !       MK,MTH  Int.   I   Spectral dimensions.
 1372 !       NDST    Int.   I   Test output unit number.
 1373 !     ----------------------------------------------------------------
 1374 !
 1375 !  4. Subroutines used :
 
 
                                                         w3gdatmd.F90  page  26
 
 
 1376 !
 1377 !     See module documentation.
 1378 !
 1379 !  5. Called by :
 1380 !
 1381 !      Name      Type  Module   Description
 1382 !     ----------------------------------------------------------------
 1383 !      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
 1384 !      WW3_GRID  Prog.   N/A    Model set up program.
 1385 !     ----------------------------------------------------------------
 1386 !
 1387 !  6. Error messages :
 1388 !
 1389 !     - Check on input parameters.
 1390 !     - Check on previous allocation.
 1391 !
 1392 !  7. Remarks :
 1393 !
 1394 !     - Grid dimensions apre passed through parameter list and then
 1395 !       locally stored to assure consistency between allocation and
 1396 !       data in structure.
 1397 !     - W3SETG needs to be called after allocation to point to
 1398 !       proper allocated arrays.
 1399 !
 1400 !  8. Structure :
 1401 !
 1402 !     See source code.
 1403 !
 1404 !  9. Switches :
 1405 !
 1406 !     !/S    Enable subroutine tracing.
 1407 !
 1408 ! 10. Source code :
 1409 !
 1410 !/ ------------------------------------------------------------------- /
 1411       USE W3SERVMD, ONLY: EXTCDE
 1412   USE CONSTANTS, ONLY: RADE
 1413 !
 1414       IMPLICIT NONE
 1415 !
 1416 !/
 1417 !/ ------------------------------------------------------------------- /
 1418 !/ Parameter list
 1419 !/
 1420       INTEGER, INTENT(IN)     :: IMOD, MK, MTH, NDSE, NDST
 1421 !/
 1422 !/ ------------------------------------------------------------------- /
 1423 !/ Local parameters
 1424 !/
 1425       INTEGER, SAVE           :: MK2, MSPEC
 1426   INTEGER                  :: SDSNTH
 1427 !/
 1428 !
 1429 ! -------------------------------------------------------------------- /
 1430 ! 1.  Test input and module status
 
 
                                                         w3gdatmd.F90  page  27
 
 
 1431 !
 1432       IF ( NGRIDS .EQ. -1 ) THEN
 1433           WRITE (NDSE,1001)
 1434           CALL EXTCDE (1)
 1435         END IF
 1436 !
 1437       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
 1438           WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
 1439           CALL EXTCDE (2)
 1440         END IF
 1441 !
 1442       IF ( MK.LT.3 .OR. MTH.LT.4 ) THEN
 1443           WRITE (NDSE,1003) MK, MTH
 1444           CALL EXTCDE (3)
 1445         END IF
 1446 !
 1447       IF ( SGRDS(IMOD)%SINIT ) THEN
 1448           WRITE (NDSE,1004)
 1449           CALL EXTCDE (4)
 1450         END IF
 1451 !
 1452       MK2    = MK + 2
 1453       MSPEC  = MK * MTH
 1454 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 1455       WRITE (NDST,9000) IMOD, MTH, MK, MK2, MSPEC
 1456 #endif
 1457 !
 1458 ! -------------------------------------------------------------------- /
 1459 ! 2.  Allocate arrays
 1460 !
 1461       ALLOCATE ( SGRDS(IMOD)%MAPWN(MSPEC+MTH),                        &
 1462                  SGRDS(IMOD)%MAPTH(MSPEC+MTH),                        &
 1463                  SGRDS(IMOD)%TH(MTH),                                 &
 1464                  SGRDS(IMOD)%ESIN(MSPEC+MTH),                         &
 1465                  SGRDS(IMOD)%ECOS(MSPEC+MTH),                         &
 1466                  SGRDS(IMOD)%ES2(MSPEC+MTH),                          &
 1467                  SGRDS(IMOD)%ESC(MSPEC+MTH),                          &
 1468                  SGRDS(IMOD)%EC2(MSPEC+MTH),                          &
 1469                  SGRDS(IMOD)%SIG(0:MK+1),                             &
 1470                  SGRDS(IMOD)%SIG2(MSPEC),                             &
 1471                  SGRDS(IMOD)%DSIP(0:MK+1),                            &
 1472                  SGRDS(IMOD)%DSII(MK),                                &
 1473                  SGRDS(IMOD)%DDEN(MK),                                &
 1474                  SGRDS(IMOD)%DDEN2(MSPEC),                            &
 1475                  STAT=ISTAT                                           )
 1476       CHECK_ALLOC_STATUS ( ISTAT )
 1477       SGRDS(IMOD)%MAPWN(:)=0.
 1478       SGRDS(IMOD)%MAPTH(:)=0.
 1479       SGRDS(IMOD)%TH(:)=0.
 1480       SGRDS(IMOD)%ESIN(:)=0.
 1481       SGRDS(IMOD)%ECOS(:)=0.
 1482       SGRDS(IMOD)%ES2(:)=0.
 1483       SGRDS(IMOD)%ESC(:)=0.
 1484       SGRDS(IMOD)%EC2(:)=0.
 1485       SGRDS(IMOD)%SIG(:)=0.
 
 
                                                         w3gdatmd.F90  page  28
 
 
 1486       SGRDS(IMOD)%SIG2(:)=0.
 1487       SGRDS(IMOD)%DSIP(:)=0.
 1488       SGRDS(IMOD)%DSII(:)=0.
 1489       SGRDS(IMOD)%DDEN(:)=0.
 1490       SGRDS(IMOD)%DDEN2(:)=0.
 1491       ALLOCATE ( MPARS(IMOD)%SRCPS%IKTAB(MK,NDTAB), &
 1492                  MPARS(IMOD)%SRCPS%DCKI(NKHS,NKD),  &
 1493                  MPARS(IMOD)%SRCPS%QBI(NKHS,NKD),   &
 1494                  STAT=ISTAT                         )
 1495       CHECK_ALLOC_STATUS ( ISTAT )
 1496       SDSNTH  = MTH/2-1 !MIN(NINT(SSDSDTH/(DTH*RADE)),MTH/2-1)
 1497       ALLOCATE( MPARS(IMOD)%SRCPS%SATINDICES(2*SDSNTH+1,MTH), &
 1498                 MPARS(IMOD)%SRCPS%SATWEIGHTS(2*SDSNTH+1,MTH), &
 1499                 MPARS(IMOD)%SRCPS%CUMULW(MSPEC,MSPEC),        &
 1500                  STAT=ISTAT                                   )
 1501       CHECK_ALLOC_STATUS ( ISTAT )
 1502 !
 1503       SGRDS(IMOD)%SINIT  = .TRUE.
 1504 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 1505       WRITE (NDST,9001)
 1506 #endif
 1507 !
 1508 ! -------------------------------------------------------------------- /
 1509 ! 3.  Point to allocated arrays
 1510 !
 1511       CALL W3SETG ( IMOD, NDSE, NDST )
 1512 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 1513       WRITE (NDST,9002)
 1514 #endif
 1515 !
 1516 ! -------------------------------------------------------------------- /
 1517 ! 4.  Update counters in grid
 1518 !
 1519       NK     = MK
 1520       NK2    = MK + 2
 1521       NTH    = MTH
 1522       NSPEC  = MK * MTH
 1523 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 1524       WRITE (NDST,9003)
 1525 #endif
 1526 !
 1527       RETURN
 1528 !
 1529 ! Formats
 1530 !
 1531  1001 FORMAT (/' *** ERROR W3DIMS : GRIDS NOT INITIALIZED *** '/      &
 1532                '                    RUN W3NMOD FIRST '/)
 1533  1002 FORMAT (/' *** ERROR W3DIMS : ILLEGAL MODEL NUMBER *** '/       &
 1534                '                    IMOD   = ',I10/                   &
 1535                '                    NAUXGR = ',I10/                   &
 1536                '                    NGRIDS = ',I10/)
 1537  1003 FORMAT (/' *** ERROR W3DIMS : ILLEGAL GRID DIMENSION(S) *** '/  &
 1538                '                    INPUT = ',4I10/)
 1539  1004 FORMAT (/' *** ERROR W3DIMS : ARRAY(S) ALREADY ALLOCATED *** ')
 1540 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 
 
                                                         w3gdatmd.F90  page  29
 
 
 1541  9000 FORMAT (' TEST W3DIMS : MODEL ',I4,' DIM. AT ',3I5,I7)
 1542  9001 FORMAT (' TEST W3DIMS : ARRAYS ALLOCATED')
 1543  9002 FORMAT (' TEST W3DIMS : POINTERS RESET')
 1544  9003 FORMAT (' TEST W3DIMS : DIMENSIONS STORED')
 1545 #endif
 1546 !/
 1547 !/ End of W3DIMS ----------------------------------------------------- /
 1548 !/
 1549       END SUBROUTINE W3DIMS
 1550 !/ ------------------------------------------------------------------- /
 1551       SUBROUTINE W3SETG ( IMOD, NDSE, NDST )
 1552 !/
 1553 !/                  +-----------------------------------+
 1554 !/                  | WAVEWATCH III           NOAA/NCEP |
 1555 !/                  |           H. L. Tolman            |
 1556 !/                  !           J. H. Alves             !
 1557 !/                  |                        FORTRAN 90 |
 1558 !/                  | Last update :         03-Sep-2012 |
 1559 !/                  +-----------------------------------+
 1560 !/
 1561 !/    24-Jun-2005 : Origination.                        ( version 3.07 )
 1562 !/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
 1563 !/    23-Jun-2006 : Add data for W3SLN1.                ( version 3.09 )
 1564 !/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
 1565 !/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
 1566 !/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
 1567 !/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
 1568 !/                  ( J. H. Alves )
 1569 !/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
 1570 !/                  ( J. H. Alves )
 1571 !/    18-Sep-2007 : Adding WAM4 source terms.           ( version 3.13 )
 1572 !/                  ( F. Ardhuin )
 1573 !/    27-Jun-2008 : Expand WAM4 variants namelist       ( version 3.14 )
 1574 !/                  ( F. Ardhuin )
 1575 !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
 1576 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 1577 !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
 1578 !/                  (W. E. Rogers & T. J. Campbell, NRL)
 1579 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
 1580 !/                  specify index closure for a grid.   ( version 3.14 )
 1581 !/                  (T. J. Campbell, NRL)
 1582 !/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
 1583 !/                  filter (SNLS) from 3.15 (HLT).      ( version 4.08 )
 1584 !/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
 1585 !/
 1586 !  1. Purpose :
 1587 !
 1588 !     Select one of the WAVEWATCH III grids / models.
 1589 !
 1590 !  2. Method :
 1591 !
 1592 !     Point pointers to the proper variables in the proper element of
 1593 !     the GRIDS array.
 1594 !
 1595 !  3. Parameters :
 
 
                                                         w3gdatmd.F90  page  30
 
 
 1596 !
 1597 !     Parameter list
 1598 !     ----------------------------------------------------------------
 1599 !       IMOD    Int.   I   Model number to point to.
 1600 !       NDSE    Int.   I   Error output unit number.
 1601 !       NDST    Int.   I   Test output unit number.
 1602 !     ----------------------------------------------------------------
 1603 !
 1604 !  4. Subroutines used :
 1605 !
 1606 !     See module documentation.
 1607 !
 1608 !  5. Called by :
 1609 !
 1610 !     Many subroutines in eth WAVEWATCH system.
 1611 !
 1612 !  6. Error messages :
 1613 !
 1614 !     Checks on parameter list IMOD.
 1615 !
 1616 !  7. Remarks :
 1617 !
 1618 !  8. Structure :
 1619 !
 1620 !  9. Switches :
 1621 !
 1622 !     !/PRn  Select propagation scheme
 1623 !
 1624 !     !/STn  Select source terms
 1625 !     !/NLn
 1626 !     !/BTn
 1627 !
 1628 !     !/S    Enable subroutine tracing.
 1629 !
 1630 ! 10. Source code :
 1631 !
 1632 !/ ------------------------------------------------------------------- /
 1633       USE W3SERVMD, ONLY: EXTCDE
 1634 !
 1635       IMPLICIT NONE
 1636 !
 1637 !/
 1638 !/ ------------------------------------------------------------------- /
 1639 !/ Parameter list
 1640 !/
 1641       INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
 1642 !/
 1643 !/ ------------------------------------------------------------------- /
 1644 !/ Local parameters
 1645 !/
 1646 !/
 1647 !
 1648 ! -------------------------------------------------------------------- /
 1649 ! 1.  Test input and module status
 1650 !
 
 
                                                         w3gdatmd.F90  page  31
 
 
 1651       IF ( NGRIDS .EQ. -1 ) THEN
 1652           WRITE (NDSE,1001)
 1653           CALL EXTCDE (1)
 1654         END IF
 1655 !
 1656       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
 1657           WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
 1658           CALL EXTCDE (2)
 1659         END IF
 1660 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETG)
 1661       WRITE (NDST,9000) IMOD
 1662 #endif
 1663 !
 1664 ! -------------------------------------------------------------------- /
 1665 ! 2.  Set model numbers
 1666 !
 1667       IGRID  = IMOD
 1668       ISGRD  = IMOD
 1669       IPARS  = IMOD
 1670 !
 1671 ! -------------------------------------------------------------------- /
 1672 ! 3.  Set pointers in structure GRID
 1673 !
 1674       GTYPE  => GRIDS(IMOD)%GTYPE
 1675       RSTYPE => GRIDS(IMOD)%RSTYPE
 1676       ICLOSE => GRIDS(IMOD)%ICLOSE
 1677 !
 1678       NX     => GRIDS(IMOD)%NX
 1679       NY     => GRIDS(IMOD)%NY
 1680       NSEA   => GRIDS(IMOD)%NSEA
 1681       NSEAL  => GRIDS(IMOD)%NSEAL
 1682       TRFLAG => GRIDS(IMOD)%TRFLAG
 1683       FLAGLL => GRIDS(IMOD)%FLAGLL
 1684 !
 1685       E3DF   => GRIDS(IMOD)%E3DF
 1686       P2MSF  => GRIDS(IMOD)%P2MSF
 1687       US3DF  => GRIDS(IMOD)%US3DF
 1688       USSPF  => GRIDS(IMOD)%USSPF
 1689       USSP_WN => GRIDS(IMOD)%USSP_WN
 1690       REFLC  => GRIDS(IMOD)%REFLC
 1691       REFLD  => GRIDS(IMOD)%REFLD
 1692       FFACBERG => GRIDS(IMOD)%FFACBERG
 1693       RREF   => GRIDS(IMOD)%RREF
 1694       REFPARS=> GRIDS(IMOD)%REFPARS
 1695       IGPARS => GRIDS(IMOD)%IGPARS
 1696       SX     => GRIDS(IMOD)%SX
 1697       SY     => GRIDS(IMOD)%SY
 1698       X0     => GRIDS(IMOD)%X0
 1699       Y0     => GRIDS(IMOD)%Y0
 1700 !
 1701       DTCFL  => GRIDS(IMOD)%DTCFL
 1702       DTCFLI => GRIDS(IMOD)%DTCFLI
 1703       DTMAX  => GRIDS(IMOD)%DTMAX
 1704       DTMIN  => GRIDS(IMOD)%DTMIN
 1705       DMIN   => GRIDS(IMOD)%DMIN
 
 
                                                         w3gdatmd.F90  page  32
 
 
 1706       CTMAX  => GRIDS(IMOD)%CTMAX
 1707       FICE0  => GRIDS(IMOD)%FICE0
 1708       GRIDSHIFT  => GRIDS(IMOD)%GRIDSHIFT
 1709       CMPRTRCK => GRIDS(IMOD)%CMPRTRCK
 1710       FICEN  => GRIDS(IMOD)%FICEN
 1711       FICEL  => GRIDS(IMOD)%FICEL
 1712       IICEHMIN  => GRIDS(IMOD)%IICEHMIN
 1713       IICEHDISP  => GRIDS(IMOD)%IICEHDISP
 1714       IICEFDISP  => GRIDS(IMOD)%IICEFDISP
 1715       IICEDDISP  => GRIDS(IMOD)%IICEDDISP
 1716       IICEHFAC  => GRIDS(IMOD)%IICEHFAC
 1717       IICEHINIT  => GRIDS(IMOD)%IICEHINIT
 1718       ICESCALES  => GRIDS(IMOD)%ICESCALES
 1719       PFMOVE => GRIDS(IMOD)%PFMOVE
 1720       STEXU  => GRIDS(IMOD)%STEXU
 1721       STEYU  => GRIDS(IMOD)%STEYU
 1722       STEDU  => GRIDS(IMOD)%STEDU
 1723       BTBETA => GRIDS(IMOD)%BTBETA
 1724       AAIRGB => GRIDS(IMOD)%AAIRGB
 1725       AAIRCMIN => GRIDS(IMOD)%AAIRCMIN
 1726 !
 1727       GINIT  => GRIDS(IMOD)%GINIT
 1728       GUGINIT  => GRIDS(IMOD)%GUGINIT
 1729       FLDRY  => GRIDS(IMOD)%FLDRY
 1730       FLCX   => GRIDS(IMOD)%FLCX
 1731       FLCY   => GRIDS(IMOD)%FLCY
 1732       FLCTH  => GRIDS(IMOD)%FLCTH
 1733       FLCK   => GRIDS(IMOD)%FLCK
 1734       FLSOU  => GRIDS(IMOD)%FLSOU
 1735       IICEDISP => GRIDS(IMOD)%IICEDISP
 1736       IICESMOOTH => GRIDS(IMOD)%IICESMOOTH
 1737 !
 1738       GNAME  => GRIDS(IMOD)%GNAME
 1739       FILEXT => GRIDS(IMOD)%FILEXT
 1740       XYB    => GRIDS(IMOD)%XYB
 1741       TRIGP  => GRIDS(IMOD)%TRIGP
 1742       NTRI     => GRIDS(IMOD)%NTRI
 1743       COUNTRI     => GRIDS(IMOD)%COUNTRI
 1744       SI     => GRIDS(IMOD)%SI
 1745       COUNTOT    => GRIDS(IMOD)%COUNTOT
 1746       IEN     => GRIDS(IMOD)%IEN
 1747       LEN     => GRIDS(IMOD)%LEN
 1748       ANGLE     => GRIDS(IMOD)%ANGLE
 1749       ANGLE0     => GRIDS(IMOD)%ANGLE0
 1750       CCON     => GRIDS(IMOD)%CCON
 1751       COUNTCON     => GRIDS(IMOD)%COUNTCON
 1752       INDEX_CELL  => GRIDS(IMOD)%INDEX_CELL
 1753       IE_CELL     => GRIDS(IMOD)%IE_CELL
 1754       POS_CELL     => GRIDS(IMOD)%POS_CELL
 1755       IOBP     => GRIDS(IMOD)%IOBP
 1756       IAA      => GRIDS(IMOD)%IAA
 1757       JAA      => GRIDS(IMOD)%JAA
 1758       POSI     => GRIDS(IMOD)%POSI
 1759       I_DIAG     => GRIDS(IMOD)%I_DIAG
 1760       JA_IE     => GRIDS(IMOD)%JA_IE
 
 
                                                         w3gdatmd.F90  page  33
 
 
 1761       NBEDGE    => GRIDS(IMOD)%NBEDGE
 1762       EDGES     => GRIDS(IMOD)%EDGES
 1763       NEIGH     => GRIDS(IMOD)%NEIGH
 1764       NNZ      => GRIDS(IMOD)%NNZ
 1765       IOBPD     => GRIDS(IMOD)%IOBPD
 1766       IOBDP     => GRIDS(IMOD)%IOBDP
 1767       IOBPA     => GRIDS(IMOD)%IOBPA
 1768       TRIA     => GRIDS(IMOD)%TRIA
 1769       CROSSDIFF => GRIDS(IMOD)%CROSSDIFF
 1770       MAXX     => GRIDS(IMOD)%MAXX
 1771       MAXY     => GRIDS(IMOD)%MAXY
 1772       DXYMAX   => GRIDS(IMOD)%DXYMAX
 1773                                     
 1774 !
 1775       IF ( GINIT ) THEN
 1776 !
 1777           MAPSTA => GRIDS(IMOD)%MAPSTA
 1778           MAPST2 => GRIDS(IMOD)%MAPST2
 1779           MAPFS  => GRIDS(IMOD)%MAPFS
 1780           MAPSF  => GRIDS(IMOD)%MAPSF
 1781           FLAGST => GRIDS(IMOD)%FLAGST
 1782 !
 1783           ZB     => GRIDS(IMOD)%ZB
 1784           CLATS  => GRIDS(IMOD)%CLATS
 1785           CLATIS => GRIDS(IMOD)%CLATIS
 1786           CTHG0S => GRIDS(IMOD)%CTHG0S
 1787           TRNX   => GRIDS(IMOD)%TRNX
 1788           TRNY   => GRIDS(IMOD)%TRNY
 1789 !
 1790           XGRD   => GRIDS(IMOD)%XGRD
 1791           YGRD   => GRIDS(IMOD)%YGRD
 1792           DXDP   => GRIDS(IMOD)%DXDP
 1793           DXDQ   => GRIDS(IMOD)%DXDQ
 1794           DYDP   => GRIDS(IMOD)%DYDP
 1795           DYDQ   => GRIDS(IMOD)%DYDQ
 1796           DPDX   => GRIDS(IMOD)%DPDX
 1797           DPDY   => GRIDS(IMOD)%DPDY
 1798           DQDX   => GRIDS(IMOD)%DQDX
 1799           DQDY   => GRIDS(IMOD)%DQDY
 1800           GSQRT  => GRIDS(IMOD)%GSQRT
 1801           HPFAC  => GRIDS(IMOD)%HPFAC
 1802           HQFAC  => GRIDS(IMOD)%HQFAC
 1803 !
 1804           GSU  => GRIDS(IMOD)%GSU
 1805 !
 1806         END IF
 1807 !
 1808 ! -------------------------------------------------------------------- /
 1809 ! 4.  Set pointers in structure SGRD
 1810 !
 1811       NK     => SGRDS(IMOD)%NK
 1812       NK2    => SGRDS(IMOD)%NK2
 1813       NTH    => SGRDS(IMOD)%NTH
 1814       NSPEC  => SGRDS(IMOD)%NSPEC
 1815 !
 
 
                                                         w3gdatmd.F90  page  34
 
 
 1816       DTH    => SGRDS(IMOD)%DTH
 1817       XFR    => SGRDS(IMOD)%XFR
 1818       FR1    => SGRDS(IMOD)%FR1
 1819       FTE    => SGRDS(IMOD)%FTE
 1820       FTF    => SGRDS(IMOD)%FTF
 1821       FTWN   => SGRDS(IMOD)%FTWN
 1822       FTTR   => SGRDS(IMOD)%FTTR
 1823       FTWL   => SGRDS(IMOD)%FTWL
 1824       FACTI1 => SGRDS(IMOD)%FACTI1
 1825       FACTI2 => SGRDS(IMOD)%FACTI2
 1826       FACHFA => SGRDS(IMOD)%FACHFA
 1827       FACHFE => SGRDS(IMOD)%FACHFE
 1828 !
 1829       SINIT  => SGRDS(IMOD)%SINIT
 1830 !
 1831       IF ( SINIT ) THEN
 1832 !
 1833           MAPWN  => SGRDS(IMOD)%MAPWN
 1834           MAPTH  => SGRDS(IMOD)%MAPTH
 1835 !
 1836           TH     => SGRDS(IMOD)%TH
 1837           ESIN   => SGRDS(IMOD)%ESIN
 1838           ECOS   => SGRDS(IMOD)%ECOS
 1839           ES2    => SGRDS(IMOD)%ES2
 1840           ESC    => SGRDS(IMOD)%ESC
 1841           EC2    => SGRDS(IMOD)%EC2
 1842           SIG    => SGRDS(IMOD)%SIG
 1843           SIG2   => SGRDS(IMOD)%SIG2
 1844           DSIP   => SGRDS(IMOD)%DSIP
 1845           DSII   => SGRDS(IMOD)%DSII
 1846           DDEN   => SGRDS(IMOD)%DDEN
 1847           DDEN2  => SGRDS(IMOD)%DDEN2
 1848 !
 1849         END IF
 1850 !
 1851 ! -------------------------------------------------------------------- /
 1852 ! 5.  Set pointers in structure MPAR
 1853 !
 1854       PINIT  => MPARS(IMOD)%PINIT
 1855 !
 1856 !     Structure NPARS
 1857 !
 1858       FACP   => MPARS(IMOD)%NPARS%FACP
 1859       XREL   => MPARS(IMOD)%NPARS%XREL
 1860       XFLT   => MPARS(IMOD)%NPARS%XFLT
 1861       FXFM   => MPARS(IMOD)%NPARS%FXFM
 1862       FXPM   => MPARS(IMOD)%NPARS%FXPM
 1863       XFT    => MPARS(IMOD)%NPARS%XFT
 1864       XFC    => MPARS(IMOD)%NPARS%XFC
 1865       FACSD  => MPARS(IMOD)%NPARS%FACSD
 1866       FHMAX  => MPARS(IMOD)%NPARS%FHMAX
 1867     RWINDC => MPARS(IMOD)%NPARS%RWINDC
 1868 !
 1869 !     Structure PROPS
 1870 !
 
 
                                                         w3gdatmd.F90  page  35
 
 
 1871       WDCG   => MPARS(IMOD)%PROPS%WDCG
 1872       WDTH   => MPARS(IMOD)%PROPS%WDTH
 1873 !
 1874 !     Structure FLDP
 1875 !
 1876 !     Structure SFLPS
 1877 !
 1878 !     Structure SLNPS
 1879 !
 1880       SLNC1  => MPARS(IMOD)%SLNPS%SLNC1
 1881       FSPM   => MPARS(IMOD)%SLNPS%FSPM
 1882       FSHF   => MPARS(IMOD)%SLNPS%FSHF
 1883 !
 1884 !     Structure SRCPS
 1885 !
 1886       WWNMEANPTAIL=> MPARS(IMOD)%SRCPS%WWNMEANPTAIL
 1887       SSTXFTFTAIL => MPARS(IMOD)%SRCPS%SSTXFTFTAIL
 1888 !
 1889       ZZWND    => MPARS(IMOD)%SRCPS%ZZWND
 1890       AALPHA   => MPARS(IMOD)%SRCPS%AALPHA
 1891       BBETA    => MPARS(IMOD)%SRCPS%BBETA
 1892       SSINTHP  => MPARS(IMOD)%SRCPS%SSINTHP
 1893       ZZ0MAX   => MPARS(IMOD)%SRCPS%ZZ0MAX
 1894       ZZ0RAT   => MPARS(IMOD)%SRCPS%ZZ0RAT
 1895       ZZALP    => MPARS(IMOD)%SRCPS%ZZALP
 1896       TTAUWSHELTER  => MPARS(IMOD)%SRCPS%TTAUWSHELTER
 1897       SSWELLFPAR  => MPARS(IMOD)%SRCPS%SSWELLFPAR
 1898       SSWELLF  => MPARS(IMOD)%SRCPS%SSWELLF
 1899       SSDSC    => MPARS(IMOD)%SRCPS%SSDSC
 1900       SSDSBR   => MPARS(IMOD)%SRCPS%SSDSBR
 1901       SSDSBT   => MPARS(IMOD)%SRCPS%SSDSBT
 1902       SSDSBRF1 => MPARS(IMOD)%SRCPS%SSDSBRF1
 1903       SSDSBRF2 => MPARS(IMOD)%SRCPS%SSDSBRF2
 1904       SSDSBRFDF  => MPARS(IMOD)%SRCPS%SSDSBRFDF
 1905       SSDSBM   => MPARS(IMOD)%SRCPS%SSDSBM
 1906       SSDSBCK  => MPARS(IMOD)%SRCPS%SSDSBCK
 1907       SSDSABK  => MPARS(IMOD)%SRCPS%SSDSABK
 1908       SSDSPBK  => MPARS(IMOD)%SRCPS%SSDSPBK
 1909       SSDSHCK  => MPARS(IMOD)%SRCPS%SSDSHCK
 1910       SSDSBINT => MPARS(IMOD)%SRCPS%SSDSBINT
 1911       SSDSP    => MPARS(IMOD)%SRCPS%SSDSP
 1912       WWNMEANP => MPARS(IMOD)%SRCPS%WWNMEANP
 1913       FFXFM    => MPARS(IMOD)%SRCPS%FFXFM
 1914       FFXFA    => MPARS(IMOD)%SRCPS%FFXFA
 1915       FFXPM    => MPARS(IMOD)%SRCPS%FFXPM
 1916       SSDSDTH  => MPARS(IMOD)%SRCPS%SSDSDTH
 1917       SSTXFTF  => MPARS(IMOD)%SRCPS%SSTXFTF
 1918       SSTXFTWN => MPARS(IMOD)%SRCPS%SSTXFTWN
 1919       SSDSCOS  => MPARS(IMOD)%SRCPS%SSDSCOS
 1920       SSDSISO  => MPARS(IMOD)%SRCPS%SSDSISO
 1921       IKTAB    => MPARS(IMOD)%SRCPS%IKTAB
 1922       DCKI     => MPARS(IMOD)%SRCPS%DCKI
 1923       QBI      => MPARS(IMOD)%SRCPS%QBI
 1924       CUMULW   => MPARS(IMOD)%SRCPS%CUMULW
 1925       SATINDICES    => MPARS(IMOD)%SRCPS%SATINDICES
 
 
                                                         w3gdatmd.F90  page  36
 
 
 1926       SATWEIGHTS   => MPARS(IMOD)%SRCPS%SATWEIGHTS
 1927       SSINBR   => MPARS(IMOD)%SRCPS%SSINBR
 1928 !
 1929 !     Structure SRNLS
 1930 !
 1931       SNLC1  => MPARS(IMOD)%SNLPS%SNLC1
 1932       LAM    => MPARS(IMOD)%SNLPS%LAM
 1933       KDCON  => MPARS(IMOD)%SNLPS%KDCON
 1934       KDMN   => MPARS(IMOD)%SNLPS%KDMN
 1935       SNLS1  => MPARS(IMOD)%SNLPS%SNLS1
 1936       SNLS2  => MPARS(IMOD)%SNLPS%SNLS2
 1937       SNLS3  => MPARS(IMOD)%SNLPS%SNLS3
 1938 !
 1939 !     Structure SBTPS
 1940 !
 1941 !     Structure SDBPS
 1942 !
 1943 !     Structure SICPS
 1944 !
 1945 !    Structure SCHM
 1946        FSBCCFL => MPARS(IMOD)%SCHMS%FSBCCFL
 1947        FSN => MPARS(IMOD)%SCHMS%FSN
 1948        FSPSI => MPARS(IMOD)%SCHMS%FSPSI
 1949        FSFCT => MPARS(IMOD)%SCHMS%FSFCT
 1950        FSNIMP => MPARS(IMOD)%SCHMS%FSNIMP
 1951        FSTOTALIMP => MPARS(IMOD)%SCHMS%FSTOTALIMP
 1952        FSTOTALEXP => MPARS(IMOD)%SCHMS%FSTOTALEXP
 1953        FSREFRACTION => MPARS(IMOD)%SCHMS%FSREFRACTION
 1954        FSFREQSHIFT => MPARS(IMOD)%SCHMS%FSFREQSHIFT
 1955        FSSOURCE => MPARS(IMOD)%SCHMS%FSSOURCE
 1956        DO_CHANGE_WLV => MPARS(IMOD)%SCHMS%DO_CHANGE_WLV
 1957        SOLVERTHR_STP => MPARS(IMOD)%SCHMS%SOLVERTHR_STP
 1958        CRIT_DEP_STP => MPARS(IMOD)%SCHMS%CRIT_DEP_STP
 1959        B_JGS_TERMINATE_MAXITER => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_MAXIT>
 1960        B_JGS_TERMINATE_DIFFERENCE => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_DI>
 1961        B_JGS_TERMINATE_NORM => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_NORM
 1962        B_JGS_LIMITER => MPARS(IMOD)%SCHMS%B_JGS_LIMITER
 1963        B_JGS_USE_JACOBI => MPARS(IMOD)%SCHMS%B_JGS_USE_JACOBI
 1964        B_JGS_BLOCK_GAUSS_SEIDEL => MPARS(IMOD)%SCHMS%B_JGS_BLOCK_GAUSS_SE>
 1965        B_JGS_MAXITER => MPARS(IMOD)%SCHMS%B_JGS_MAXITER
 1966        B_JGS_PMIN => MPARS(IMOD)%SCHMS%B_JGS_PMIN
 1967        B_JGS_DIFF_THR => MPARS(IMOD)%SCHMS%B_JGS_DIFF_THR
 1968        B_JGS_NORM_THR => MPARS(IMOD)%SCHMS%B_JGS_NORM_THR
 1969        B_JGS_NLEVEL => MPARS(IMOD)%SCHMS%B_JGS_NLEVEL
 1970        B_JGS_SOURCE_NONLINEAR => MPARS(IMOD)%SCHMS%B_JGS_SOURCE_NONLINEAR
 1971       RETURN
 1972 !
 1973 ! Formats
 1974 !
 1975  1001 FORMAT (/' *** ERROR W3SETG : GRIDS NOT INITIALIZED *** '/      &
 1976                '                    RUN W3NMOD FIRST '/)
 1977  1002 FORMAT (/' *** ERROR W3SETG : ILLEGAL MODEL NUMBER *** '/       &
 1978                '                    IMOD   = ',I10/                   &
 1979                '                    NAUXGR = ',I10/                   &
 1980                '                    NGRIDS = ',I10/)
 
 
                                                         w3gdatmd.F90  page  37
 
 
 1981 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETG)
 1982  9000 FORMAT (' TEST W3SETG : GRID/MODEL ',I4,' SELECTED')
 1983 #endif
 1984 !/
 1985 !/ End of W3SETG ----------------------------------------------------- /
 1986 !/
 1987       END SUBROUTINE W3SETG
 1988 !/ ------------------------------------------------------------------- /
 1989       SUBROUTINE W3GNTX ( IMOD, NDSE, NDST )
 1990 !/
 1991 !/                  +-----------------------------------+
 1992 !/                  | WAVEWATCH-III           NOAA/NCEP |
 1993 !/                  |           T. J. Campbell          |
 1994 !/                  |                        FORTRAN 90 |
 1995 !/                  | Last update :         20-Jul-2011 |
 1996 !/                  +-----------------------------------+
 1997 !/
 1998 !/    30-Oct-2009 : Origination.                        ( version 3.13 )
 1999 !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
 2000 !/                  specify index closure for a grid.   ( version 3.14 )
 2001 !/                  (T. J. Campbell, NRL)
 2002 !/    23-Dec-2010 : Fix HPFAC and HQFAC by including the COS(YGRD)
 2003 !/                  factor with DXDP and DXDQ terms.    ( version 3.14 )
 2004 !/                  (T. J. Campbell, NRL)
 2005 !/    20-Jul-2011 : HPFAC and HQFAC are now calculated using W3DIST.
 2006 !/                  Result should be very similar except near pole.
 2007 !/                  Due to precision issues, HPFAC and HQFAC revert
 2008 !/                  to SX and SY in case of regular grids.
 2009 !/                  (W. E. Rogers, NRL)                 ( version 3.14 )
 2010 !/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
 2011 !/    20-Jan-2017 : Change calculation of curvilinear grid metric and
 2012 !/                  derivatives calculations to use W3GSRUMD:W3CGDM.
 2013 !/                  (T.J. Campbell, NRL)                ( version 6.02 )
 2014 !/
 2015 !  1. Purpose :
 2016 !
 2017 !     Construct required spatial grid quantities for curvilinear grids.
 2018 !
 2019 !  2. Method :
 2020 !
 2021 !  3. Parameters :
 2022 !
 2023 !     Parameter list
 2024 !     ----------------------------------------------------------------
 2025 !       IMOD    Int.   I   Model number to point to.
 2026 !       NDSE    Int.   I   Error output unit number.
 2027 !     ----------------------------------------------------------------
 2028 !
 2029 !  4. Subroutines used :
 2030 !
 2031 !     See module documentation.
 2032 !
 2033 !  5. Called by :
 2034 !
 2035 !     Any program that uses this grid structure.
 
 
                                                         w3gdatmd.F90  page  38
 
 
 2036 !
 2037 !  6. Error messages :
 2038 !
 2039 !     - Check on previous initialization of grids.
 2040 !
 2041 !  7. Remarks :
 2042 !
 2043 !  8. Structure :
 2044 !
 2045 !  9. Switches :
 2046 !
 2047 !     !/S    Enable subroutine tracing.
 2048 !
 2049 ! 10. Source code :
 2050 !
 2051 !/ ------------------------------------------------------------------- /
 2052       USE W3SERVMD, ONLY: EXTCDE
 2053 !
 2054       IMPLICIT NONE
 2055 !/
 2056 !/ ------------------------------------------------------------------- /
 2057 !/ Parameter list
 2058 !/
 2059       INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
 2060 !/
 2061 !/ ------------------------------------------------------------------- /
 2062 !/ Local parameters
 2063 !/
 2064       INTEGER, PARAMETER :: NFD    = 4
 2065       LOGICAL, PARAMETER :: PTILED = .FALSE.
 2066       LOGICAL, PARAMETER :: QTILED = .FALSE.
 2067       LOGICAL, PARAMETER :: IJG    = .FALSE.
 2068       LOGICAL, PARAMETER :: SPHERE = .FALSE.
 2069       INTEGER :: PRANGE(2), QRANGE(2)
 2070       INTEGER :: LBI(2), UBI(2), LBO(2), UBO(2), ISTAT
 2071       REAL   , ALLOCATABLE :: COSA(:,:)
 2072 !/
 2073 !
 2074 ! -------------------------------------------------------------------- /
 2075 ! 1.  Test input and module status
 2076 !
 2077       IF ( NGRIDS .EQ. -1 ) THEN
 2078           WRITE (NDSE,1001)
 2079           CALL EXTCDE (1)
 2080         END IF
 2081 !
 2082       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
 2083           WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
 2084           CALL EXTCDE (2)
 2085         END IF
 2086 !
 2087       SELECT CASE ( GRIDS(IMOD)%GTYPE )
 2088         CASE ( RLGTYPE )
 2089         CASE ( CLGTYPE )
 2090         CASE DEFAULT
 
 
                                                         w3gdatmd.F90  page  39
 
 
 2091           WRITE (NDSE,1003) GRIDS(IMOD)%GTYPE
 2092           CALL EXTCDE (3)
 2093         END SELECT
 2094 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2095       WRITE (NDST,9000) IMOD
 2096 #endif
 2097 !
 2098 ! -------------------------------------------------------------------- /
 2099 ! 2.  Create grid search utility object
 2100 !
 2101       GRIDS(IMOD)%GSU = W3GSUC( IJG, FLAGLL, GRIDS(IMOD)%ICLOSE, &
 2102                                 GRIDS(IMOD)%XGRD, GRIDS(IMOD)%YGRD )
 2103 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2104       CALL W3GSUP(GRIDS(IMOD)%GSU, NDST)
 2105       WRITE (NDST,9001)
 2106 #endif
 2107 !
 2108 ! -------------------------------------------------------------------- /
 2109 ! 3.  Reset grid pointers
 2110 !
 2111       CALL W3SETG ( IMOD, NDSE, NDST )
 2112 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2113       WRITE (NDST,9002)
 2114 #endif
 2115 !
 2116 ! -------------------------------------------------------------------- /
 2117 ! 4.  Construct curvilinear grid derivatives and metric
 2118 !     Note that in the case of lon/lat grids, these quantities do not
 2119 !     include the spherical coordinate metric (SPHERE=.FALSE.).
 2120 !
 2121 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2122       ALLOCATE ( COSA(NY,NX), STAT=ISTAT )
 2123       CHECK_ALLOC_STATUS ( ISTAT )
 2124 #endif
 2125       PRANGE = (/ 1,NX/)
 2126       QRANGE = (/ 1,NY/)
 2127       LBI = (/ 1, 1/)
 2128       UBI = (/NY,NX/)
 2129       LBO = (/ 1, 1/)
 2130       UBO = (/NY,NX/)
 2131       SELECT CASE ( GTYPE )
 2132         CASE ( RLGTYPE )
 2133           CALL W3CGDM( IJG, FLAGLL, ICLOSE, PTILED, QTILED,            &
 2134                        PRANGE, QRANGE, LBI, UBI, LBO, UBO, XGRD, YGRD, &
 2135                        NFD=NFD, SPHERE=SPHERE, DX=SX, DY=SY,           &
 2136                        DXDP=DXDP, DYDP=DYDP, DXDQ=DXDQ, DYDQ=DYDQ,     &
 2137                        DPDX=DPDX, DPDY=DPDY, DQDX=DQDX, DQDY=DQDY,     &
 2138                        HPFC=HPFAC, HQFC=HQFAC, GSQR=GSQRT,             &
 2139 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2140                        COSA=COSA,                                      &
 2141 #endif
 2142                        RC=ISTAT )
 2143           IF ( ISTAT.NE.0 ) THEN
 2144               WRITE (NDSE,1004) GTYPE
 2145               CALL EXTCDE (4)
 
 
                                                         w3gdatmd.F90  page  40
 
 
 2146             END IF
 2147         CASE ( CLGTYPE )
 2148           CALL W3CGDM( IJG, FLAGLL, ICLOSE, PTILED, QTILED,            &
 2149                        PRANGE, QRANGE, LBI, UBI, LBO, UBO, XGRD, YGRD, &
 2150                        NFD=NFD, SPHERE=SPHERE,                         &
 2151                        DXDP=DXDP, DYDP=DYDP, DXDQ=DXDQ, DYDQ=DYDQ,     &
 2152                        DPDX=DPDX, DPDY=DPDY, DQDX=DQDX, DQDY=DQDY,     &
 2153                        HPFC=HPFAC, HQFC=HQFAC, GSQR=GSQRT,             &
 2154 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2155                        COSA=COSA,                                      &
 2156 #endif
 2157                        RC=ISTAT )
 2158           IF ( ISTAT.NE.0 ) THEN
 2159               WRITE (NDSE,1004) GTYPE
 2160               CALL EXTCDE (4)
 2161             END IF
 2162         END SELECT
 2163 !
 2164 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2165       WRITE(NDST,'(A,2E14.6)')'HPFAC MIN/MAX:',MINVAL(HPFAC),MAXVAL(HPFAC)
 2166       WRITE(NDST,'(A,2E14.6)')'HQFAC MIN/MAX:',MINVAL(HQFAC),MAXVAL(HQFAC)
 2167       WRITE(NDST,'(A,2E14.6)')'GSQRT MIN/MAX:',MINVAL(GSQRT),MAXVAL(GSQRT)
 2168       WRITE(NDST,'(A,2E14.6)')'DXDP  MIN/MAX:',MINVAL(DXDP),MAXVAL(DXDP)
 2169       WRITE(NDST,'(A,2E14.6)')'DYDP  MIN/MAX:',MINVAL(DYDP),MAXVAL(DYDP)
 2170       WRITE(NDST,'(A,2E14.6)')'DXDQ  MIN/MAX:',MINVAL(DXDQ),MAXVAL(DXDQ)
 2171       WRITE(NDST,'(A,2E14.6)')'DYDQ  MIN/MAX:',MINVAL(DYDQ),MAXVAL(DYDQ)
 2172       WRITE(NDST,'(A,2E14.6)')'DPDX  MIN/MAX:',MINVAL(DPDX),MAXVAL(DPDX)
 2173       WRITE(NDST,'(A,2E14.6)')'DPDY  MIN/MAX:',MINVAL(DPDY),MAXVAL(DPDY)
 2174       WRITE(NDST,'(A,2E14.6)')'DQDX  MIN/MAX:',MINVAL(DQDX),MAXVAL(DQDX)
 2175       WRITE(NDST,'(A,2E14.6)')'DQDY  MIN/MAX:',MINVAL(DQDY),MAXVAL(DQDY)
 2176       WRITE(NDST,'(A,2E14.6)')'COSA  MIN/MAX:',MINVAL(COSA),MAXVAL(COSA)
 2177       WRITE (NDST,9003)
 2178       DEALLOCATE ( COSA, STAT=ISTAT )
 2179       CHECK_DEALLOC_STATUS ( ISTAT )
 2180 #endif
 2181 !
 2182 ! Formats
 2183 !
 2184  1001 FORMAT (/' *** ERROR W3GNTX : GRIDS NOT INITIALIZED *** '/      &
 2185                '                    RUN W3NMOD FIRST '/)
 2186  1002 FORMAT (/' *** ERROR W3GNTX : ILLEGAL MODEL NUMBER *** '/       &
 2187                '                    IMOD   = ',I10/                   &
 2188                '                    NAUXGR = ',I10/                   &
 2189                '                    NGRIDS = ',I10/)
 2190  1003 FORMAT (/' *** ERROR W3GNTX : UNSUPPORTED TYPE OF GRID *** '/   &
 2191                '                    GTYPE  = ',I10/)
 2192  1004 FORMAT (/' *** ERROR W3GNTX : ERROR OCCURED IN W3CGDM *** '/    &
 2193                '                    GTYPE  = ',I10/)
 2194 !
 2195 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 2196  9000 FORMAT (' TEST W3GNTX : MODEL ',I4)
 2197  9001 FORMAT (' TEST W3GNTX : SEARCH OBJECT CREATED')
 2198  9002 FORMAT (' TEST W3GNTX : POINTERS RESET')
 2199  9003 FORMAT (' TEST W3GNTX : GRID ARRAYS CONSTRUCTED')
 2200 #endif
 
 
                                                         w3gdatmd.F90  page  41
 
 
 2201 !/
 2202 !/ End of W3GNTX ----------------------------------------------------- /
 2203 !/
 2204       END SUBROUTINE W3GNTX
 2205 !/ ------------------------------------------------------------------- /
 2206       SUBROUTINE W3DIMUG  ( IMOD, MTRI, MX, COUNTOTA, NNZ, NDSE, NDST )
 2207 !/
 2208 !/                  +-----------------------------------+
 2209 !/                  | WAVEWATCH-III           NOAA/NCEP |
 2210 !/                  |             F.ardhuin             |
 2211 !/                  |                        FORTRAN 90 |
 2212 !/                  | Last update :         15-Mar-2007 !
 2213 !/                  +-----------------------------------+
 2214 !/
 2215 !/    15-Mar-2007 : Origination.                        ( version 3.14 )
 2216 !/    11-May-2015 : Updates to 2-ways nestings for UG   ( version 5.08 )
 2217 !/
 2218 !  1. Purpose :
 2219 !
 2220 !     Initialize an individual spatial grid at the proper dimensions.
 2221 !
 2222 !  2. Method :
 2223 !
 2224 !     Allocate directly into the structure array GRIDS. Note that
 2225 !     this cannot be done through the pointer alias!
 2226 !
 2227 !  3. Parameters :
 2228 !
 2229 !     Parameter list
 2230 !     ----------------------------------------------------------------
 2231 !       IMOD    Int.   I   Model number to point to.
 2232 !       NDSE    Int.   I   Error output unit number.
 2233 !       NDST    Int.   I   Test output unit number.
 2234 !       MX, MTRI, MSEA       Like NX, NTRI, NSEA in data structure.
 2235 !     ----------------------------------------------------------------
 2236 !
 2237 !  4. Subroutines used :
 2238 !
 2239 !       See module documentation.
 2240 !
 2241 !  5. Called by :
 2242 !
 2243 !      Name      Type  Module   Description
 2244 !     ----------------------------------------------------------------
 2245 !      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
 2246 !      WW3_GRID  Prog.   N/A    Model set up program.
 2247 !     ----------------------------------------------------------------
 2248 !
 2249 !  6. Error messages :
 2250 !
 2251 !     - Check on input parameters.
 2252 !     - Check on previous allocation.
 2253 !
 2254 !  7. Remarks :
 2255 !
 
 
                                                         w3gdatmd.F90  page  42
 
 
 2256 !     - Grid dimensions apre passed through parameter list and then
 2257 !       locally stored to assure consistency between allocation and
 2258 !       data in structure.
 2259 !     - W3SETG needs to be called after allocation to point to
 2260 !       proper allocated arrays.
 2261 !
 2262 !  8. Structure :
 2263 !
 2264 !     See source code.
 2265 !
 2266 !  9. Switches :
 2267 !
 2268 !     !/S    Enable subroutine tracing.
 2269 !
 2270 ! 10. Source code :
 2271 !
 2272 !/ ------------------------------------------------------------------- /
 2273       USE W3SERVMD, ONLY: EXTCDE
 2274 !
 2275       IMPLICIT NONE
 2276 !
 2277 !/
 2278 !/ ------------------------------------------------------------------- /
 2279 !/ Parameter list
 2280 !/
 2281       INTEGER, INTENT(IN)     :: IMOD, MTRI, MX, COUNTOTA, NNZ, NDSE, NDST
 2282       INTEGER                 :: IAPROC = 1
 2283 !/
 2284 !/ ------------------------------------------------------------------- /
 2285 !/ Local parameters
 2286 !/
 2287 !/
 2288 !
 2289 ! -------------------------------------------------------------------- /
 2290 ! 1.  Test input and module status
 2291 !
 2292       IF ( NGRIDS .EQ. -1 ) THEN
 2293           WRITE (NDSE,1001)
 2294           CALL EXTCDE (1)
 2295         END IF
 2296 !
 2297       IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
 2298           WRITE (NDSE,1002) IMOD, NGRIDS
 2299           CALL EXTCDE (2)
 2300         END IF
 2301       IF ( GRIDS(IMOD)%GUGINIT ) THEN
 2302         WRITE (NDSE,1004)
 2303         CALL EXTCDE (4)
 2304         END IF
 2305 !
 2306 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 2307       WRITE (NDST,9000) IMOD, MX, MTRI
 2308 #endif
 2309       
 2310 !
 
 
                                                         w3gdatmd.F90  page  43
 
 
 2311 ! -------------------------------------------------------------------- /
 2312 ! 2.  Allocate arrays
 2313 !
 2314       ALLOCATE ( GRIDS(IMOD)%TRIGP(MTRI,3),                         &
 2315                  GRIDS(IMOD)%XYB(MX,3),                             &
 2316                  GRIDS(IMOD)%SI(MX),                                &
 2317                  GRIDS(IMOD)%TRIA(MTRI),                            &
 2318                  GRIDS(IMOD)%CROSSDIFF(6,MTRI),                     &
 2319                  GRIDS(IMOD)%IEN(MTRI,6),                           &
 2320                  GRIDS(IMOD)%LEN(MTRI,3),                           &
 2321                  GRIDS(IMOD)%ANGLE(MTRI,3),                         &
 2322                  GRIDS(IMOD)%ANGLE0(MTRI,3),                        &
 2323                  GRIDS(IMOD)%CCON(MX),                              &
 2324                  GRIDS(IMOD)%COUNTCON(MX),                          &
 2325                  GRIDS(IMOD)%INDEX_CELL(MX+1),                      &
 2326                  GRIDS(IMOD)%IE_CELL(COUNTOTA),                     &
 2327                  GRIDS(IMOD)%POS_CELL(COUNTOTA),                    &
 2328                  GRIDS(IMOD)%IAA(NX+1),                             &
 2329                  GRIDS(IMOD)%JAA(NNZ),                              &
 2330                  GRIDS(IMOD)%POSI(3,COUNTOTA),                      &
 2331                  GRIDS(IMOD)%I_DIAG(NX),                            &
 2332                  GRIDS(IMOD)%JA_IE(3,3,MTRI),                       &
 2333                  GRIDS(IMOD)%IOBP(MX),                              &
 2334                  GRIDS(IMOD)%IOBPD(NTH,MX),                         &
 2335                  GRIDS(IMOD)%IOBDP(MX),                             &
 2336                  GRIDS(IMOD)%IOBPA(MX),                             &
 2337                  STAT=ISTAT                                         )
 2338       CHECK_ALLOC_STATUS ( ISTAT )
 2339 !
 2340                  GRIDS(IMOD)%IOBP(:)=1
 2341 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 2342       WRITE (NDST,9001)
 2343 #endif
 2344 !
 2345 !some segmentation troubles can appear, they are related with the allocat>
 2346 !normal(1st dimension) and the nesting of the triangulated grid.
 2347 ! -------------------------------------------------------------------- /
 2348 ! 3.  Point to allocated arrays
 2349 !
 2350       CALL W3SETG ( IMOD, NDSE, NDST )
 2351 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 2352       WRITE (NDST,9002)
 2353 #endif
 2354 !
 2355 ! -------------------------------------------------------------------- /
 2356 ! 4.  Update counters in grid
 2357 !     Note that in the case of lon/lat grids, these quantities do not
 2358 !     include the spherical coordinate metric (SPHERE=.FALSE.).
 2359 !
 2360       NTRI   = MTRI
 2361       COUNTOT=COUNTOTA
 2362       GRIDS(IMOD)%GUGINIT  = .TRUE.
 2363 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 2364       WRITE (NDST,9003)
 2365 #endif
 
 
                                                         w3gdatmd.F90  page  44
 
 
 2366       
 2367       RETURN
 2368 !
 2369 ! Formats
 2370 !
 2371  1001 FORMAT (/' *** ERROR W3DIMUG : GRIDS NOT INITIALIZED *** '/      &
 2372                '                    RUN W3NMOD FIRST '/)
 2373  1002 FORMAT (/' *** ERROR W3DIMUG : ILLEGAL MODEL NUMBER *** '/       &
 2374                '                    IMOD   = ',I10/                   &
 2375                '                    NGRIDS = ',I10/)
 2376  1004 FORMAT (/' *** ERROR W3DIMUG : ARRAY(S) ALREADY ALLOCATED *** ')
 2377 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 2378  9000 FORMAT (' TEST W3DIMUG: MODEL ',I4,' DIM. AT ',2I5,I7)
 2379  9001 FORMAT (' TEST W3DIMUG : ARRAYS ALLOCATED')
 2380  9002 FORMAT (' TEST W3DIMUG : POINTERS RESET')
 2381  9003 FORMAT (' TEST W3DIMUG : DIMENSIONS STORED')
 2382 #endif
 2383 !/
 2384 !/ End of W3DIMUG ----------------------------------------------------- /
 2385 !/
 2386       END SUBROUTINE W3DIMUG
 2387 !/ ------------------------------------------------------------------- /
 2388       SUBROUTINE W3SETREF
 2389 !/
 2390 !/                  +-----------------------------------+
 2391 !/                  | WAVEWATCH III           NOAA/NCEP |
 2392 !/                  |           F. Ardhuin              |
 2393 !/                  |                        FORTRAN 90 |
 2394 !/                  | Last update :         13-Nov-2013 |
 2395 !/                  +-----------------------------------+
 2396 !/
 2397 !/    13-Nov-2013 : Origination.                        ( version 4.13 )
 2398 !/
 2399 !  1. Purpose :
 2400 !
 2401 !     Update reflection directions at shoreline.
 2402 !
 2403 !  2. Method :
 2404 !
 2405 !  3. Parameters :
 2406 !
 2407 !     Parameter list
 2408 !     ----------------------------------------------------------------
 2409 !       None
 2410 !     ----------------------------------------------------------------
 2411 !
 2412 !  4. Subroutines used :
 2413 !
 2414 !     See module documentation.
 2415 !
 2416 !  5. Called by :
 2417 !
 2418 !      Name      Type  Module   Description
 2419 !     ----------------------------------------------------------------
 2420 !      WW3_GRID  Prog. WW3_GRID Grid preprocessor
 
 
                                                         w3gdatmd.F90  page  45
 
 
 2421 !      W3ULEV    Subr. W3UPDTMD Water level update
 2422 !     ----------------------------------------------------------------
 2423 !
 2424 !  6. Error messages :
 2425 !
 2426 !     None.
 2427 !
 2428 !  7. Remarks :
 2429 !
 2430 !  8. Structure :
 2431 !
 2432 !     See source code.
 2433 !
 2434 !  9. Switches :
 2435 !
 2436 !       !/S      Enable subroutine tracing.
 2437 !
 2438 ! 10. Source code :
 2439 !
 2440 !/ ------------------------------------------------------------------- /
 2441       USE CONSTANTS
 2442 !
 2443       IMPLICIT NONE
 2444 !/
 2445 !/ ------------------------------------------------------------------- /
 2446 !/
 2447       INTEGER                 :: ISEA, IX, IY, IXY, IXN, IXP, IYN, IYP
 2448       INTEGER                 :: J, K, NEIGH1(0:7)
 2449       INTEGER                 :: ILEV, NLEV
 2450                                            
 2451       REAL                    :: TRIX(NY*NX), TRIY(NY*NX), DX, DY,    &
 2452                                  COSAVG, SINAVG, THAVG, ANGLES(0:7), CLAT
 2453 !/
 2454 !/ ------------------------------------------------------------------- /
 2455 !/
 2456 !
 2457 ! 1.  Preparations --------------------------------------------------- *
 2458 !
 2459       IF (REFPARS(2).GT.0) RREF(2)=.TRUE.
 2460       IF (REFPARS(3).GT.0) RREF(3)=.TRUE.
 2461       IF (REFPARS(4).GT.0) RREF(4)=.TRUE.
 2462 !
 2463       DO IY=2, NY-1
 2464         DO IX=2, NX-1
 2465           IF (REFPARS(1).GT.0) RREF(1)=.TRUE.
 2466           IF (FLAGLL.AND.(YGRD(IY,IX).GT.85)) RREF(1)=.FALSE.
 2467           IF (MAPSTA(IY,IX).GT.0) THEN
 2468             IF (RREF(2)) &
 2469               REFLC(2,MAPFS(IY,IX))= MAX((1. - TRNX(IY,IX)),(1.-TRNY(IY,I>
 2470             IF (RREF(4)) &
 2471               REFLC(4,MAPFS(IY,IX))= 1.
 2472             IF (RREF(1)) THEN
 2473               REFLC(1,  MAPFS(IY,IX)) = 0.
 2474               REFLD(1:6,MAPFS(IY,IX)) = 0
 2475               NEIGH1(0)=8*MAPST2(IY,IX+1)+MAPSTA(IY,IX+1)
 
 
                                                         w3gdatmd.F90  page  46
 
 
 2476               NEIGH1(1:3)=8*MAPST2(IY+1,IX+1:IX-1:-1)+MAPSTA(IY+1,IX+1:IX>
 2477               NEIGH1(4)=8*MAPST2(IY,IX-1)+MAPSTA(IY,IX-1)
 2478               NEIGH1(5:7)=8*MAPST2(IY-1,IX-1:IX+1)+MAPSTA(IY-1,IX-1:IX+1)
 2479               IF (MINVAL(ABS(NEIGH1)).EQ.0) THEN
 2480                 IF ( FLAGLL ) THEN
 2481                   CLAT   = COS(YGRD(IY,IX)*DERA)
 2482                 ELSE
 2483                   CLAT = 1.
 2484                   END IF
 2485                 ANGLES(0)= ATAN2(DYDP(IY,IX),DXDP(IY,IX)*CLAT)
 2486                 ANGLES(1)= ATAN2(DYDP(IY,IX)+DYDQ(IY,IX),(DXDP(IY,IX)+DXD>
 2487                 ANGLES(2)= ATAN2(DYDQ(IY,IX),DXDQ(IY,IX)*CLAT)
 2488                 ANGLES(3)= ATAN2(DYDQ(IY,IX)-DYDP(IY,IX),(DXDQ(IY,IX)-DXD>
 2489                 ANGLES(4:7)= ANGLES(0:3)+PI
 2490                 IF ((NEIGH1(0).GE.1).AND.(NEIGH1(4).GE.1)) THEN
 2491                   REFLD(3,MAPFS(IY,IX))=0
 2492                 ELSE
 2493                   IF ((NEIGH1(0).GE.1).OR.(NEIGH1(4).GE.1)) REFLD(3,MAPFS>
 2494                   END IF
 2495                 IF ((NEIGH1(2).EQ.1).AND.(NEIGH1(6).GE.1)) THEN
 2496                   REFLD(4,MAPFS(IY,IX))=0
 2497                 ELSE
 2498                   IF ((NEIGH1(2).GE.1).OR.(NEIGH1(6).GE.1)) REFLD(4,MAPFS>
 2499                   END IF
 2500                 J=0
 2501                 REFLD(1,MAPFS(IY,IX))=0
 2502                 COSAVG=0
 2503                 SINAVG=0
 2504                 REFLD(5,MAPFS(IY,IX))= MOD(NTH+NINT(ANGLES(0)/TPI*NTH),NT>
 2505                 REFLD(6,MAPFS(IY,IX))= MOD(NTH+NINT((ANGLES(2)/TPI-0.25)*>
 2506                 DO K=0,7
 2507                   IF (NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+7,8)).EQ.0 &
 2508                     .AND.NEIGH1(MOD(K+1,8)).EQ.0 &
 2509                     .AND.NEIGH1(MOD(K+4,8)).NE.0) THEN
 2510                     REFLC(1,MAPFS(IY,IX))= REFPARS(1)
 2511                     REFLD(2,MAPFS(IY,IX))= 2
 2512                     COSAVG=COSAVG+COS(ANGLES(K))  !ECOS(1+(K*NTH)/8)
 2513                     SINAVG=SINAVG+SIN(ANGLES(K))  !ESIN(1+(K*NTH)/8)
 2514                     J=J+1
 2515                     ENDIF
 2516                   END DO
 2517                 IF (J.GT.0) THEN
 2518                   IF (J.GT.1) REFLD(2,MAPFS(IY,IX))= 1
 2519                   THAVG=ATAN2(SINAVG,COSAVG)
 2520 #if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETREF)
 2521                   !WRITE (6,*) 'COASTAL REFLECTION:',IX,IY,   &
 2522                   !SINAVG,COSAVG,THAVG/TPI,NINT(THAVG/TPI*NTH),MOD(NTH+NI>
 2523 #endif
 2524                   REFLD(1,MAPFS(IY,IX))=1+MOD(NTH+NINT(THAVG/TPI*NTH),NTH)
 2525                 ELSE
 2526                     
 2527                   DO K=0,7
 2528                     IF (NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+1,8)).EQ.0 &
 2529                       .AND.NEIGH1(MOD(K+4,8)).NE.0) THEN
 2530                       REFLC(1,MAPFS(IY,IX))= REFPARS(1)
 
 
                                                         w3gdatmd.F90  page  47
 
 
 2531                       REFLD(1,MAPFS(IY,IX))= 1+MOD((K*NTH+(K+1)*NTH)/16,N>
 2532                       REFLD(2,MAPFS(IY,IX))= 1
 2533                       ENDIF
 2534                     END DO
 2535                   IF (REFLC(1,MAPFS(IY,IX)).LE.0) THEN
 2536                     DO K=0,7,2
 2537                       IF ( NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+4,8)).NE.0) TH>
 2538                         REFLC(1,MAPFS(IY,IX))= REFPARS(1)
 2539                         REFLD(1,MAPFS(IY,IX))= 1+(K*NTH)/8
 2540                         REFLD(2,MAPFS(IY,IX))= 0
 2541                         !WRITE(6,*) 'NEIGH3:',IX,IY,K,NEIGH1,K*(NTH/8)
 2542                         END IF
 2543                       END DO
 2544                     END IF
 2545                   END IF
 2546                 END IF
 2547               END IF
 2548             END IF
 2549           END DO
 2550         END DO
 2551 !
 2552       RETURN
 2553 !
 2554 ! Formats
 2555 !
 2556 !/
 2557 !/ End of W3SETREF ----------------------------------------------------- /
 2558 !/
 2559       END SUBROUTINE W3SETREF
 2560                              
 2561 !/
 2562 !/ End of module W3GDATMD -------------------------------------------- /
 2563 !/
 2564       END MODULE W3GDATMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
