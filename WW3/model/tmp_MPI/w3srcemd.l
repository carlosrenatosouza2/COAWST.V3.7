                                                         w3srcemd.F90  page   1
 
 
    1 #include "w3macros.h"
    2 !/ ------------------------------------------------------------------- /
    3       MODULE W3SRCEMD
    4 !/
    5 !/                  +-----------------------------------+
    6 !/                  | WAVEWATCH III           NOAA/NCEP |
    7 !/                  |           H. L. Tolman            |
    8 !/                  |            F. Ardhuin             |
    9 !/                  |                        FORTRAN 90 |
   10 !/                  | Last update :         07-Jan-2018 |
   11 !/                  +-----------------------------------+
   12 !/
   13 !/    For updates see subroutine.
   14 !/
   15 !  1. Purpose :
   16 !
   17 !     Source term integration routine.
   18 !
   19 !  2. Variables and types :
   20 !
   21 !      Name      Type  Scope    Description
   22 !     ----------------------------------------------------------------
   23 !      OFFSET    R.P.  Private  Offset in time integration scheme.
   24 !                               0.5 in original WAM, now 1.0
   25 !     ----------------------------------------------------------------
   26 !
   27 !  3. Subroutines and functions :
   28 !
   29 !      Name      Type  Scope    Description
   30 !     ----------------------------------------------------------------
   31 !      W3SRCE    Subr. Public   Calculate and integrate source terms.
   32 !     ----------------------------------------------------------------
   33 !
   34 !  4. Subroutines and functions used :
   35 !
   36 !     See corresponding documentation of W3SRCE.
   37 !
   38 !  5. Remarks :
   39 !
   40 !  6. Switches :
   41 !
   42 !       See section 9 of W3SRCE.
   43 !
   44 !  7. Source code :
   45 !
   46 !/ ------------------------------------------------------------------- /
   47 !/
   48       REAL, PARAMETER, PRIVATE:: OFFSET = 1.
   49 !/
   50       CONTAINS
   51 !/ ------------------------------------------------------------------- /
   52       SUBROUTINE W3SRCE ( srce_call, IT, JSEA, IX, IY, IMOD,          &
   53                           SPECOLD, SPEC, VSIO, VDIO, SHAVEIO,         &
   54                           ALPHA, WN1, CG1,                            &
   55                           D_INP, U10ABS, U10DIR, AS, USTAR, USTDIR,   &
 
 
                                                         w3srcemd.F90  page   2
 
 
   56                           CX, CY,  ICE, ICEH, ICEF, ICEDMAX,          &
   57                           REFLEC, REFLED, DELX, DELY, DELA, TRNX,     &
   58                           TRNY, BERG, FPI, DTDYN, FCUT, DTG, TAUWX,   &
   59                           TAUWY, TAUOX, TAUOY, TAUWIX, TAUWIY, TAUWNX,&
   60                           TAUWNY, PHIAW, CHARN, TWS, PHIOC, WHITECAP, &
   61                           D50, PSIC, BEDFORM , PHIBBL, TAUBBL, TAUICE,&
   62                           PHICE, COEF)
   63 !/
   64 !/                  +-----------------------------------+
   65 !/                  | WAVEWATCH III           NOAA/NCEP |
   66 !/                  |           H. L. Tolman            |
   67 !/                  |            F. Ardhuin             |
   68 !/                  |            A. Roland              |
   69 !/                  |            M. Dutour Sikiric      |
   70 !/                  |                        FORTRAN 90 |
   71 !/                  | Last update :         07-Jan-2018 |
   72 !/                  +-----------------------------------+
   73 !/
   74 !/    06-Dec-1996 : Final FORTRAN 77                    ( version 1.18 )
   75 !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
   76 !/    14-Feb-2000 : Exact-NL added                      ( version 2.01 )
   77 !/    04-May-2000 : Non-central integration             ( version 2.03 )
   78 !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
   79 !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
   80 !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
   81 !/    27-Nov-2002 : First version of VDIA and MDIA.     ( version 3.01 )
   82 !/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
   83 !/    24-Dec-2004 : Multiple model version.             ( version 3.06 )
   84 !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
   85 !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
   86 !/    04-Jul-2006 : Separation of stress computation.   ( version 3.09 )
   87 !/    16-Apr-2007 : Miche style limiter added.          ( version 3.11 )
   88 !/                  (J. H. Alves)
   89 !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
   90 !/                  (J. H. Alves)
   91 !/    09-Oct-2007 : Adding WAM 4+ and SB1 options.      ( version 3.13 )
   92 !/                  (F. Ardhuin)
   93 !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
   94 !/    19-Aug-2010 : Making treatment of 0 water depth   ( version 3.14.6 )
   95 !/                  consistent with the rest of the model.
   96 !/    31-Mar-2010 : Adding ice conc. and reflections    ( version 3.14.4 )
   97 !/    15-May-2010 : Adding transparencies               ( version 3.14.4 )
   98 !/    01-Jun-2011 : Movable bed bottom friction in BT4  ( version 4.01 )
   99 !/    01-Jul-2011 : Energy and momentum flux, friction  ( version 4.01 )
  100 !/    24-Aug-2011 : Uses true depth for depth-induced   ( version 4.04 )
  101 !/    16-Sep-2011 : Initialization of TAUWAX, TAUWAY    ( version 4.04 )
  102 !/     1-Dec-2011 : Adding BYDRZ source term package    ( version 4.04 )
  103 !/                  ST6 and optional Hwang (2011)
  104 !/                  stresses FLX4.
  105 !/    14-Mar-2012 : Update of BT4, passing PSIC         ( version 4.04 )
  106 !/    13-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
  107 !/                  from 3.15 (HLT).                    ( version 4.08 )
  108 !/    28-Aug-2013 : Corrected MLIM application          ( version 4.11 )
  109 !/    10-Sep-2013 : Special treatment for IG band       ( version 4.15 )
  110 !/    14-Nov-2013 : Make orphaned pars in par lst local ( version 4.13 )
 
 
                                                         w3srcemd.F90  page   3
 
 
  111 !/    17-Nov-2013 : Coupling fraction of ice-free       ( version 4.13 )
  112 !/                  surface to SIN and SDS. (S. Zieger)
  113 !/    01-Avr-2014 : Adding ice thickness and floe size  ( version 4.18 )
  114 !/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
  115 !/    27-Aug-2015 : Adding inputs to function W3SIS2    ( version 5.10 )
  116 !/    13-Dec-2015 : Implicit integration of Sice (F.A.) ( version 5.10 )
  117 !/    30-Jul-2017 : Adds TWS in interface               ( version 6.04 )
  118 !/    07-Jan-2018 : Allows variable ice scaling (F.A.)  ( version 6.04 )
  119 !/    01-Jan-2018 : Add implicit source term integration ( version 6.04)
  120 !/    01-Jan-2018 : within PDLIB (A. Roland, M. Dutour
  121 !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version  6.06)
  122 !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
  123 !/
  124 !/    Copyright 2009-2013 National Weather Service (NWS),
  125 !/       National Oceanic and Atmospheric Administration.  All rights
  126 !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  127 !/       No unauthorized use without permission.
  128 !/
  129 !  1. Purpose :
  130 !
  131 !     Calculate and integrate source terms for a single grid point.
  132 !
  133 !  2. Method :
  134 !
  135 !     Physics  : see manual and corresponding subroutines.
  136 !
  137 !     Numerics :
  138 !
  139 !     Dynamic-implicit integration of the source terms based on
  140 !     WW-II (Tolman 1992). The dynamic time step is calculated
  141 !     given a maximum allowed change of spectral densities for
  142 !     frequencies / wavenumbers below the usual cut-off.
  143 !     The maximum change is given by the minimum of a parametric
  144 !     and a relative change. The parametric change relates to a
  145 !     PM type equilibrium range
  146 !
  147 !                                -1  (2pi)**4       1
  148 !       dN(k)     =  Xp alpha  pi   ---------- ------------
  149 !            max                       g**2     k**3 sigma
  150 !
  151 !                              1                                     .
  152 !                 =  FACP ------------                              (1)
  153 !                          k**3 sigma                                .
  154 !
  155 !     where
  156 !           alpha = 0.62e-4                       (set in W3GRID)
  157 !           Xp      fraction of PM shape          (read in W3GRID)
  158 !           FACP    combined factor               (set in W3GRID)
  159 !
  160 !     The maximum relative change is given as
  161 !
  162 !                           /            +-                  -+ \    .
  163 !       dN(k)     =  Xr max | N(k) , max | Nx , Xfilt N(k)    | |   (2)
  164 !            max            \            +-               max-+ /    .
  165 !
 
 
                                                         w3srcemd.F90  page   4
 
 
  166 !     where
  167 !           Xr      fraction of relative change   (read in W3GRID)
  168 !           Xfilt   filter level                  (read in W3GRID)
  169 !           Nx      Maximum parametric change (1)
  170 !                   for largest wavenumber.
  171 !
  172 !  3. Parameters :
  173 !
  174 !     Parameter list
  175 !     ----------------------------------------------------------------
  176 !       IX,IY   Int.   I   Discrete grid point counters.
  177 !       IMOD    Int.   I   Model number.
  178 !       SPEC    R.A.  I/O  Spectrum (action) in 1-D form.
  179 !       ALPHA   R.A.  I/O  Nondimenional 1-D spectrum corresponding
  180 !                          to above full spectra (Phillip's const.).
  181 !                          Calculated separately for numerical
  182 !                          economy on vector machine (W3SPR2).
  183 !       WN1     R.A.   I   Discrete wavenumbers.
  184 !       CG1     R.A.   I   Id. group velocities.
  185 !       D_INP   Real.  I   Depth. Compared to DMIN to get DEPTH.
  186 !       U10ABS  Real.  I   Wind speed at reference height.
  187 !       U10DIR  Real.  I   Id. wind direction.
  188 !       AS      Real.  I   Air-sea temp. difference.      ( !/ST3 )
  189 !       USTAR   Real. !/O  Friction velocity.
  190 !       USTDIR  Real  !/O  Idem, direction.
  191 !       CX-Y    Real.  I   Current velocity components.   ( !/BS1 )
  192 !       ICE     Real   I   Sea ice concentration
  193 !       ICEH    Real   I   Sea ice thickness
  194 !       ICEF    Real  I/O  Sea ice maximum floe diameter  (updated)
  195 !       ICEDMAX Real  I/O  Sea ice maximum floe diameter
  196 !       BERG    Real   I   Iceberg damping coefficient    ( !/BS1 )
  197 !       REFLEC  R.A.   I   reflection coefficients        ( !/BS1 )
  198 !       REFLED  I.A.   I   reflection direction           ( !/BS1 )
  199 !       TRNX-Y  Real   I   Grid transparency in X and Y   ( !/BS1 )
  200 !       DELX    Real.  I   grid cell size in X direction  ( !/BS1 )
  201 !       DELY    Real.  I   grid cell size in Y direction  ( !/BS1 )
  202 !       DELA    Real.  I   grid cell area                 ( !/BS1 )
  203 !       FPI     Real  I/O  Peak-input frequency.          ( !/ST2 )
  204 !      WHITECAP R.A.   O   Whitecap statisics             ( !/ST4 )
  205 !       DTDYN   Real   O   Average dynamic time step.
  206 !       FCUT    Real   O   Cut-off frequency for tail.
  207 !       DTG     Real   I   Global time step.
  208 !       D50     Real   I   Sand grain size                ( !/BT4 )
  209 !       BEDFORM R.A.  I/O  Bedform parameters             ( !/BT4 )
  210 !       PSIC    Real   I   Critical Shields               ( !/BT4 )
  211 !       PHIBBL  Real   O   Energy flux to BBL             ( !/BTx )
  212 !       TAUBBL  R.A.   O   Momentum flux to BBL           ( !/BTx )
  213 !       TAUICE  R.A.   O   Momentum flux to sea ice       ( !/ICx )
  214 !       PHICE   Real   O   Energy flux to sea ice         ( !/ICx )
  215 !     ----------------------------------------------------------------
  216 !       Note: several pars are set to I/O to avoid compiler warnings.
  217 !
  218 !  4. Subroutines used :
  219 !
  220 !      Name      Type  Module   Description
 
 
                                                         w3srcemd.F90  page   5
 
 
  221 !     ----------------------------------------------------------------
  222 !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
  223 !                               source terms.
  224 !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
  225 !      W3SLNn    Subr. W3SLNnMD Linear input.
  226 !      W3SINn    Subr. W3SRCnMD Input source term.
  227 !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
  228 !      W3SNLS    Subr. W3SNLSMD Nonlinear smoother.
  229 !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
  230 !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
  231 !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
  232 !      W3STRn    Subr. W3STRnMD Triad interaction source term.
  233 !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
  234 !      W3REFn    Subr. W3REFnMD Reflexions (shore, icebergs ...).
  235 !      W3SXXn    Subr. W3SXXnMD Unclassified source term.
  236 !      STRACE    Subr. W3SERVMD Subroutine tracing (!/S)
  237 !     ----------------------------------------------------------------
  238 !
  239 !  5. Called by :
  240 !
  241 !      Name      Type  Module   Description
  242 !     ----------------------------------------------------------------
  243 !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
  244 !     ----------------------------------------------------------------
  245 !
  246 !  6. Error messages :
  247 !
  248 !       None.
  249 !
  250 !  7. Remarks :
  251 !
  252 !     - No testing is performed on the status of the grid point.
  253 !
  254 !  8. Structure :
  255 !
  256 !     -----------------------------------------------------------------
  257 !       1.   Preparations
  258 !         a  Set maximum change and wavenumber arrays.
  259 !         b  Prepare dynamic time stepping.
  260 !         c  Compute mean parameters.                       ( W3SPRn )
  261 !         d  Compute stresses (if posible).
  262 !         e  Prepare cut-off
  263 !         f  Test output for !/NNT option.
  264 !     --start-dynamic-integration-loop---------------------------------
  265 !       2.  Calculate source terms
  266 !         a Input.                                  ( W3SLNx, W3SINn )
  267 !         b Nonlinear interactions.                         ( W3SNLn )
  268 !         c Dissipation                                     ( W3SDSn )
  269 !           1 as included in source terms                   ( W3SDSn )
  270 !           2 optional dissipation due to different physics ( W3SWLn )
  271 !         d Bottom friction.                                ( W3SBTn )
  272 !       3.  Calculate cut-off frequencie(s)
  273 !       4.  Summation of source terms and diagonal term and time step.
  274 !       5.  Increment spectrum.
  275 !       6.  Add tail
 
 
                                                         w3srcemd.F90  page   6
 
 
  276 !         a Mean wave parameters and cut-off                ( W3SPRn )
  277 !         b 'Seeding' of spectrum.                          ( !/SEED )
  278 !         c Add tail
  279 !       7.  Check if integration complete.
  280 !     --end-dynamic-integration-loop-----------------------------------
  281 !       8.  Save integration data.
  282 !     -----------------------------------------------------------------
  283 !
  284 !  9. Switches :
  285 !
  286 !     !/FLX1  Wu (1980) stress computation.              ( Choose one )
  287 !     !/FLX2  T&C (1996) stress computation.
  288 !     !/FLX3  T&C (1996) stress computation with cap.
  289 !     !/FLX4  Hwang (2011) stress computation (2nd order).
  290 !
  291 !     !/LN0   No linear input.                           ( Choose one )
  292 !     !/LNX   User-defined bottom friction.
  293 !
  294 !     !/ST0   No input and dissipation.                  ( Choose one )
  295 !     !/ST1   WAM-3 input and dissipation.
  296 !     !/ST2   Tolman and Chalikov (1996)  input and dissipation.
  297 !     !/ST3   WAM 4+ input and dissipation.
  298 !     !/ST4   Ardhuin et al. (2009, 2010)
  299 !     !/ST6   BYDB source terms after Babanin, Young, Donelan and Banner.
  300 !     !/STX   User-defined input and dissipation.
  301 !
  302 !     !/NL0   No nonlinear interactions.                 ( Choose one )
  303 !     !/NL1   Discrete interaction approximation.
  304 !     !/NL2   Exact nonlinear interactions.
  305 !     !/NL3   Generalized Multiple DIA.
  306 !     !/NL4   Two Scale Approximation
  307 !     !/NLX   User-defined nonlinear interactions.
  308 !     !/NLS   Nonlinear HF smoother.
  309 !
  310 !     !/BT0   No bottom friction.                        ( Choose one )
  311 !     !/BT1   JONSWAP bottom friction.
  312 !     !/BT4   Bottom friction using movable bed roughness
  313 !                  (Tolman 1994, Ardhuin & al. 2003)
  314 !     !/BT8   Muddy bed (Dalrymple & Liu).
  315 !     !/BT9   Muddy bed (Ng).
  316 !     !/BTX   User-defined bottom friction.
  317 !
  318 !     !/IC1   Dissipation via interaction with ice according to simple
  319 !             methods: 1) uniform in frequency or
  320 !     !/IC2            2) Liu et al. model
  321 !     !/IC3   Dissipation via interaction with ice according to a
  322 !             viscoelastic sea ice model (Wang and Shen 2010).
  323 !     !/IC4   Dissipation via interaction with ice as a function of freq.
  324 !             (empirical/parametric methods)
  325 !     !/IC5   Dissipation via interaction with ice according to a
  326 !             viscoelastic sea ice model (Mosig et al. 2015).
  327 !     !/DB0   No depth-limited breaking.                 ( Choose one )
  328 !     !/DB1   Battjes-Janssen depth-limited breaking.
  329 !     !/DBX   User-defined bottom friction.
  330 !
 
 
                                                         w3srcemd.F90  page   7
 
 
  331 !     !/TR0   No triad interactions.                     ( Choose one )
  332 !     !/TR1   Lumped Triad Approximation (LTA).
  333 !     !/TRX   User-defined triad interactions.
  334 !
  335 !     !/BS0   No bottom scattering.                      ( Choose one )
  336 !     !/BS1   Scattering term by Ardhuin and Magne (2007).
  337 !     !/BSX   User-defined bottom scattering
  338 !
  339 !     !/XX0   No arbitrary additional source term.       ( Choose one )
  340 !     !/XXX   User-defined bottom friction.
  341 !
  342 !     !/MLIM  Miche style limiter for shallow water and steepness.
  343 !
  344 !     !/SEED  'Seeding' of lowest frequency for suffuciently strong
  345 !             winds.
  346 !
  347 !     !/NNT   Write output to file test_data_NNN.ww3 for NN training.
  348 !
  349 !     !/S     Enable subroutine tracing.
  350 !     !/T     Enable general test output.
  351 !
  352 ! 10. Source code :
  353 !
  354 !/ ------------------------------------------------------------------- /
  355       USE CONSTANTS
  356       USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH, DMIN, DTMAX,       &
  357                           DTMIN, FACTI1, FACTI2, FACSD, FACHFA, FACP, &
  358                           XFC, XFLT, XREL, XFT, FXFM, FXPM, DDEN,     &
  359                           FTE, FTF, FHMAX, ECOS, ESIN, IICEDISP,      &
  360                           ICESCALES, IICESMOOTH
  361       USE W3GDATMD, ONLY: FSSOURCE, optionCall
  362       USE W3GDATMD, ONLY: B_JGS_NLEVEL, B_JGS_SOURCE_NONLINEAR
  363  USE W3GDATMD, ONLY: IOBP, IOBPD, IOBDP, GTYPE, UNGTYPE, REFPARS
  364       USE W3WDATMD, ONLY: TIME
  365       USE W3ODATMD, ONLY: NDSE, NDST, IAPROC
  366       USE W3IDATMD, ONLY: INFLAGS1, INFLAGS2, ICEP2
  367       USE W3DISPMD
  368       USE W3SLN1MD
  369       USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
  370       USE W3GDATMD, ONLY : ZZWND, FFXFM, FFXPM, FFXFA
  371       USE W3SNL1MD
  372      USE W3REF1MD
  373       USE W3GDATMD, ONLY : IGPARS
  374 !/
  375       IMPLICIT NONE
  376 !/
  377 !/ ------------------------------------------------------------------- /
  378 !/ Parameter list
  379 !/
  380       INTEGER, INTENT(IN)     :: srce_call, IT, JSEA, IX, IY, IMOD
  381       REAL, intent(in)        :: SPECOLD(NSPEC)
  382       REAL, INTENT(OUT)       :: VSIO(NSPEC), VDIO(NSPEC)
  383       LOGICAL, INTENT(OUT)    :: SHAVEIO
  384       REAL, INTENT(IN)        :: D_INP, U10ABS,     &
  385                                  U10DIR, AS, CX, CY, DTG, D50,PSIC,   &
 
 
                                                         w3srcemd.F90  page   8
 
 
  386                                  ICE, ICEH
  387       INTEGER, INTENT(IN)     :: REFLED(6)
  388       REAL, INTENT(IN)        :: REFLEC(4), DELX, DELY, DELA,         &
  389                                  TRNX, TRNY, BERG, ICEDMAX
  390       REAL, INTENT(INOUT)     :: WN1(NK), CG1(NK), &
  391                                  SPEC(NSPEC), ALPHA(NK), USTAR,       &
  392                                  USTDIR, FPI, TAUOX, TAUOY,           &
  393                                  TAUWX, TAUWY, PHIAW, PHIOC, PHICE,   &
  394                                  CHARN, TWS, BEDFORM(3), PHIBBL,      &
  395                                  TAUBBL(2), TAUICE(2), WHITECAP(4),   &
  396                                  TAUWIX, TAUWIY, TAUWNX, TAUWNY,      &
  397                                  ICEF
  398       REAL, INTENT(OUT)       :: DTDYN, FCUT
  399       REAL, INTENT(IN)        :: COEF
  400 !/
  401 !/ ------------------------------------------------------------------- /
  402 !/ Local parameters
  403 !/
  404       INTEGER                 :: IK, ITH, IS, IS0, NSTEPS,  NKH, NKH1,&
  405                                  IKS1, IS1, NSPECH, IDT, IERR, NKI, NKD
  406       REAL                    :: DTTOT, FHIGH, DT, AFILT, DAMAX, AFAC,&
  407                                  HDT, ZWND, FP, DEPTH, TAUSCX, TAUSCY, FH>
  408 ! Scaling factor for SIN, SDS, SNL
  409       REAL                    :: ICESCALELN, ICESCALEIN, ICESCALENL, ICES>
  410       REAL                    :: EMEAN, FMEAN, WNMEAN, AMAX, CD, Z0, SCAT>
  411                                  SMOOTH_ICEDISP
  412       REAL                    :: WN_R(NK), CG_ICE(NK),ALPHA_LIU(NK), ICEC>
  413                                  R(NK)
  414       DOUBLE PRECISION        :: ATT, ISO
  415       REAL                    :: FMEANS, FH1, FH2, FAGE, DLWMEAN
  416       REAL                    :: QCERR  = 0.     !/XNL2 and !/NNT
  417       REAL                    :: HM, EM
  418       REAL                    :: EBAND, DIFF, EFINISH, HSTOT, PHINL,     >
  419                                  FMEAN1, FMEANWS, MWXINIT, MWYINIT,      >
  420                                  FACTOR, FACTOR2, DRAT, TAUWAX, TAUWAY,  >
  421                                  MWXFINISH, MWYFINISH, A1BAND, B1BAND,   >
  422                                  COSI(2)
  423       REAL                    :: SPECINIT(NSPEC), SPEC2(NSPEC)
  424       REAL                    :: DAM (NSPEC), WN2 (NSPEC),            &
  425                                  VSLN(NSPEC),                         &
  426                                  VSIN(NSPEC), VDIN(NSPEC),            &
  427                                  VSNL(NSPEC), VDNL(NSPEC),            &
  428                                  VSDS(NSPEC), VDDS(NSPEC),            &
  429                                  VSBT(NSPEC), VDBT(NSPEC),            &
  430                                 VREF(NSPEC),                         &
  431                                  VS  (NSPEC), VD  (NSPEC), EB(NK)
  432       LOGICAL                 :: LLWS(NSPEC)
  433       REAL                    :: BRLAMBDA(NSPEC)
  434       REAL                    :: FOUT(NK,NTH), SOUT(NK,NTH), DOUT(NK,NTH)
  435       REAL, SAVE              :: TAUNUX, TAUNUY
  436       LOGICAL, SAVE           :: FLTEST = .FALSE., FLAGNN = .TRUE.
  437       LOGICAL                 :: SHAVE
  438       LOGICAL                 :: LBREAK
  439       LOGICAL, SAVE           :: FIRST = .TRUE.
  440       LOGICAL                 :: PrintDeltaSmDA
 
 
                                                         w3srcemd.F90  page   9
 
 
  441       REAL                    :: eInc1, eInc2
  442       REAL                    :: DeltaSRC(NSPEC), MAXDAC(NSPEC)
  443                                                                
  444 !/
  445 !/ ------------------------------------------------------------------- /
  446 !/
  447 !
  448       DEPTH  = MAX ( DMIN , D_INP )
  449       IKS1 = 1
  450       ICESCALELN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(1)))
  451       ICESCALEIN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(2)))
  452       ICESCALENL = MAX(0.,MIN(1.,1.-ICE*ICESCALES(3)))
  453       ICESCALEDS = MAX(0.,MIN(1.,1.-ICE*ICESCALES(4)))
  454       IF (NINT(IGPARS(12)).EQ.0) IKS1 = NINT(IGPARS(5))
  455       IS1=(IKS1-1)*NTH+1
  456 !
  457       VSIN = 0.
  458       VDIN = 0.
  459                
  460       VSBT = 0.
  461       VDBT = 0.
  462 !
  463       ZWND   = ZZWND
  464 !
  465        DRAT  = DAIR / DWAT
  466 !
  467 ! 1.  Preparations --------------------------------------------------- *
  468 !
  469 ! 1.a Set maximum change and wavenumber arrays.
  470 !
  471 !XP     = 0.15
  472 !FACP   = XP / PI * 0.62E-3 * TPI**4 / GRAV**2
  473 !
  474       DO IK=1, NK
  475         DAM(1+(IK-1)*NTH) = FACP / ( SIG(IK) * WN1(IK)**3 )
  476         WN2(1+(IK-1)*NTH) = WN1(IK)
  477         END DO
  478 !
  479       DO IK=1, NK
  480         IS0    = (IK-1)*NTH
  481         DO ITH=2, NTH
  482           DAM(ITH+IS0) = DAM(1+IS0)
  483           WN2(ITH+IS0) = WN2(1+IS0)
  484           END DO
  485         END DO
  486 !
  487 ! 1.b Prepare dynamic time stepping
  488 !
  489       DTDYN  = 0.
  490       DTTOT  = 0.
  491       NSTEPS = 0
  492       PHIAW  = 0.
  493       CHARN  = 0.
  494       TWS    = 0.
  495       PHINL  = 0.
 
 
                                                         w3srcemd.F90  page  10
 
 
  496       PHIBBL = 0.
  497       TAUWIX = 0.
  498       TAUWIY = 0.
  499       TAUWNX = 0.
  500       TAUWNY = 0.
  501       TAUWAX = 0.
  502       TAUWAY = 0.
  503       TAUSCX = 0.
  504       TAUSCY = 0.
  505       TAUBBL = 0.
  506       TAUICE = 0.
  507       PHICE  = 0.
  508                  
  509                  
  510       DLWMEAN= 0.
  511       BRLAMBDA(:)=0.
  512       WHITECAP(:)=0.
  513 !
  514 ! 1.c Set mean parameters
  515 !
  516       TAUWX=0.
  517       TAUWY=0.
  518       IF ( IT .eq. 0 ) THEN
  519           LLWS(:) = .TRUE.
  520           USTAR=0.
  521           USTDIR=0.
  522       ELSE
  523         CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN, &
  524                    AMAX, U10ABS, U10DIR, USTAR, USTDIR,            &
  525                    TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
  526                                                                        
  527                                                                        
  528         CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,       &
  529                  U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,       &
  530                  VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
  531         END IF
  532               
  533       CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN, &
  534                    AMAX, U10ABS, U10DIR, USTAR, USTDIR,          &
  535                    TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
  536       TWS = 1./FMEANWS
  537 !
  538 ! 1.c2 Stores the initial data
  539 !
  540       SPECINIT = SPEC
  541 !
  542 ! 1.d Stresses
  543 !
  544 ! 1.e Prepare cut-off beyond which the tail is imposed with a power law
  545 !
  546 ! !/ST4      FAGE   = FFXFA*TANH(0.3*U10ABS*FMEANWS*TPI/GRAV)
  547       FAGE   = 0.
  548       FHIGH  = MAX( (FFXFM + FAGE ) * MAX(FMEAN1,FMEANWS), FFXPM / USTAR)
  549       FHIGI  = FFXFA * FMEAN1
  550 !
 
 
                                                         w3srcemd.F90  page  11
 
 
  551 ! 1.f Prepare output file for !/NNT option
  552 !
  553 ! ... Branch point dynamic integration - - - - - - - - - - - - - - - -
  554 !
  555       DO
  556 !
  557         NSTEPS = NSTEPS + 1
  558 !
  559 ! 2.  Calculate source terms ----------------------------------------- *
  560 !
  561 ! 2.a Input.
  562 !
  563         CALL W3SLN1 (       WN1, FHIGH, USTAR, U10DIR , VSLN       )
  564 !
  565         CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,       &
  566                  U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,       &
  567                  VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
  568                                                      
  569                                                      
  570 !
  571 ! 2.b Nonlinear interactions.
  572 !
  573         CALL W3SNL1 ( SPEC, CG1, WNMEAN*DEPTH,        VSNL, VDNL )
  574 !
  575 ! 2.c Dissipation... except for ST4
  576 ! 2.c1   as in source term package
  577 !
  578         CALL W3SDS4 ( SPEC, WN1, CG1, USTAR, USTDIR, DEPTH, VSDS,    &
  579                       VDDS, IX, IY, BRLAMBDA, WHITECAP, DLWMEAN )
  580                                                                  
  581                                                                  
  582 !
  583 ! 2.c2   optional dissipation parameterisations
  584 !
  585 ! 2.d Bottom interactions.
  586 !
  587 ! 2.e Unresolved Obstacles Source Term
  588 !
  589 ! 2.f Additional sources.
  590 !
  591 ! 2.g Dump training data if necessary
  592 !
  593 ! 3.  Set frequency cut-off ------------------------------------------ *
  594 !
  595         NKH    = MIN ( NK , INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FHIGH))) )
  596         NKH1   = MIN ( NK , NKH+1 )
  597         NSPECH = NKH1*NTH
  598 !
  599 ! 4.  Summation of source terms and diagonal term and time step ------ *
  600 !
  601         DT     = MIN ( DTG-DTTOT , DTMAX )
  602         AFILT  = MAX ( DAM(NSPEC) , XFLT*AMAX )
  603 !
  604 !     For input and dissipation calculate the fraction of the ice-free
  605 !     surface. In the presence of ice, the effective water surface
 
 
                                                         w3srcemd.F90  page  12
 
 
  606 !     is reduce to a fraction of the cell size free from ice, and so is
  607 !     input :
  608 !             SIN = (1-ICE)**ISCALEIN*SIN and SDS=(1-ICE)**ISCALEDS*SDS ->
  609 !     INFLAGS2(4) is true if ice concentration was ever read during
  610 !             this simulation
  611         IF ( INFLAGS2(4) ) THEN
  612           VSNL(1:NSPECH) = ICESCALENL * VSNL(1:NSPECH)
  613           VDNL(1:NSPECH) = ICESCALENL * VDNL(1:NSPECH)
  614           VSLN(1:NSPECH) = ICESCALELN * VSLN(1:NSPECH)
  615           VSIN(1:NSPECH) = ICESCALEIN * VSIN(1:NSPECH)
  616           VDIN(1:NSPECH) = ICESCALEIN * VDIN(1:NSPECH)
  617           VSDS(1:NSPECH) = ICESCALEDS * VSDS(1:NSPECH)
  618           VDDS(1:NSPECH) = ICESCALEDS * VDDS(1:NSPECH)
  619           END IF
  620 !
  621         VS = 0
  622         VD = 0
  623         DO IS=IS1, NSPECH
  624           VS(IS) = VSLN(IS) + VSIN(IS) + VSNL(IS)  &
  625                  + VSDS(IS) + VSBT(IS)
  626           VD(IS) =  VDIN(IS) + VDNL(IS)  &
  627                  + VDDS(IS) + VDBT(IS)
  628           DAMAX  = MIN ( DAM(IS) , MAX ( XREL*SPECINIT(IS) , AFILT ) )
  629           AFAC   = 1. / MAX( 1.E-10 , ABS(VS(IS)/DAMAX) )
  630           DT     = MIN ( DT , AFAC / ( MAX ( 1.E-10,                  &
  631                          1. + OFFSET*AFAC*MIN(0.,VD(IS)) ) ) )
  632 !          IF (IX == DEBUG_NODE) THEN
  633 !            WRITE(*,'(A20,I10,10F30.10)') 'TIME STEP COMP', IS, DAMAX, D>
  634 !          ENDIF
  635         END DO  ! end of loop on IS
  636 !
  637 !        WRITE(*,*) 'NODE_NUMBER', IX
  638 !        IF (IX == DEBUG_NODE) WRITE(*,*) 'TIMINGS 1', DT
  639         DT     = MAX ( 0.5, DT )                   ! Here we have a hardl>
  640 !
  641         DTDYN  = DTDYN + DT
  642         IDT    = 1 + INT ( 0.99*(DTG-DTTOT)/DT ) ! number of iterations
  643         DT     = (DTG-DTTOT)/REAL(IDT)           ! actualy time step
  644         SHAVE  = DT.LT.DTMIN .AND. DT.LT.DTG-DTTOT   ! limiter check ...
  645         SHAVEIO = SHAVE
  646         DT     = MAX ( DT , MIN (DTMIN,DTG-DTTOT) ) ! override dt with in>
  647         IF (srce_call .eq. srce_imp_post) DT = DTG  ! for implicit part
  648         HDT    = OFFSET * DT
  649         DTTOT  = DTTOT + DT
  650                            
  651                            
  652                            
  653         IF (srce_call .eq. srce_imp_pre) THEN
  654           PrintDeltaSmDA=.FALSE.
  655           IF (PrintDeltaSmDA .eqv. .TRUE.) THEN
  656             DO IS=1,NSPEC
  657               DeltaSRC(IS) = VSIN(IS) - SPEC(IS)*VDIN(IS)
  658             END DO
  659             WRITE(740+IAPROC,*) 'min/max/sum(VSIN)=', minval(VSIN), maxva>
  660             WRITE(740+IAPROC,*) 'min/max/sum(DeltaIN)=', minval(DeltaSRC)>
 
 
                                                         w3srcemd.F90  page  13
 
 
  661             !
  662             DO IS=1,NSPEC
  663               DeltaSRC(IS) = VSNL(IS) - SPEC(IS)*VDNL(IS)
  664             END DO
  665             WRITE(740+IAPROC,*) 'min/max/sum(VSNL)=', minval(VSNL), maxva>
  666             WRITE(740+IAPROC,*) 'min/max/sum(DeltaNL)=', minval(DeltaSRC)>
  667             !
  668             DO IS=1,NSPEC
  669               DeltaSRC(IS) = VSDS(IS) - SPEC(IS)*VDDS(IS)
  670             END DO
  671             WRITE(740+IAPROC,*) 'min/max/sum(VSDS)=', minval(VSDS), maxva>
  672             WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC)>
  673             !
  674 !            DO IS=1,NSPEC
  675 !              DeltaSRC(IS) = VSIC(IS) - SPEC(IS)*VDIC(IS)
  676 !            END DO
  677             WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC)>
  678           END IF
  679                 
  680             IF (optionCall .eq. 1) THEN
  681               CALL SIGN_VSD_PATANKAR_WW3(SPEC,VS,VD)
  682             ELSE IF (optionCall .eq. 2) THEN
  683               CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC,VS,VD)
  684             ELSE IF (optionCall .eq. 3) THEN
  685               CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC,VS,VD)
  686             ENDIF
  687             VSIO=VS
  688             VDIO=VD
  689 !!/DEBUGSRC          IF (IX == DEBUG_NODE) WRITE(44,'(10EN15.4)') SUM(VS)>
  690           RETURN ! return everything is done for the implicit ...
  691         END IF ! srce_imp_pre
  692 !
  693 ! 5.  Increment spectrum --------------------------------------------- *
  694 !
  695         IF (srce_call .eq. srce_direct) THEN
  696 !          SHAVE = .FALSE.
  697 !         IF (IX == DEBUG_NODE) THEN
  698 !            WRITE(*,'(A20,I20,F20.10,L20,4F20.10)') 'BEFORE', IX, DEPTH,>
  699 !          ENDIF
  700           IF ( SHAVE ) THEN
  701             DO IS=IS1, NSPECH
  702               eInc1 = VS(IS) * DT / MAX ( 1. , (1.-HDT*VD(IS)))
  703               eInc2 = SIGN ( MIN (DAM(IS),ABS(eInc1)) , eInc1 )
  704               SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc2 )
  705             END DO
  706           ELSE
  707 !
  708             DO IS=IS1, NSPECH
  709               eInc1 = VS(IS) * DT / MAX ( 1. , (1.-HDT*VD(IS)))
  710               SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc1 )
  711             END DO
  712           END IF
  713 !          IF (IX == DEBUG_NODE) THEN
  714 !            WRITE(*,'(A20,I20,F20.10,L20,4F20.10)') 'AFTER', IX, DEPTH, >
  715 !          ENDIF
 
 
                                                         w3srcemd.F90  page  14
 
 
  716 !!/DEBUGSRC          IF (IX == DEBUG_NODE) WRITE(44,'(10EN15.4)') SUM(VS)>
  717         END IF
  718               
  719               
  720 !
  721 ! 5.b  Computes
  722 !              atmos->wave flux PHIAW-------------------------------- *
  723 !              wave ->BBL  flux PHIBBL------------------------------- *
  724 !              wave ->ice  flux PHICE ------------------------------- *
  725 !
  726        WHITECAP(3)=0.
  727        HSTOT=0.
  728        DO IK=IKS1, NK
  729          FACTOR = DDEN(IK)/CG1(IK)                    !Jacobian to get en>
  730          FACTOR2= FACTOR*GRAV*WN1(IK)/SIG(IK)         ! coefficient to ge>
  731                                                                           
  732          ! Wave direction is "direction to"
  733          ! therefore there is a PLUS sign for the stress
  734          DO ITH=1, NTH
  735            IS   = (IK-1)*NTH + ITH
  736            COSI(1)=ECOS(IS)
  737            COSI(2)=ESIN(IS)
  738            PHIAW = PHIAW + (VSIN(IS))* DT * FACTOR                    &
  739              / MAX ( 1. , (1.-HDT*VDIN(IS))) ! semi-implict integration s>
  740                                                                           
  741            PHIBBL= PHIBBL- (VSBT(IS))* DT * FACTOR                    &
  742              / MAX ( 1. , (1.-HDT*VDBT(IS))) ! semi-implict integration s>
  743            PHINL = PHINL + VSNL(IS)* DT * FACTOR                      &
  744              / MAX ( 1. , (1.-HDT*VDNL(IS))) ! semi-implict integration s>
  745            IF (VSIN(IS).GT.0.) WHITECAP(3) = WHITECAP(3) + SPEC(IS)  * FA>
  746            HSTOT = HSTOT + SPEC(IS) * FACTOR
  747            END DO
  748          END DO
  749        WHITECAP(3)=4.*SQRT(WHITECAP(3))
  750        HSTOT=4.*SQRT(HSTOT)
  751        TAUWIX= TAUWIX+ TAUWX * DRAT *DT
  752        TAUWIY= TAUWIY+ TAUWY * DRAT *DT
  753        TAUWNX= TAUWNX+ TAUWAX * DRAT *DT
  754        TAUWNY= TAUWNY+ TAUWAY * DRAT *DT
  755        ! MISSING: TAIL TO BE ADDED ?
  756 !
  757 ! 6.  Add tail ------------------------------------------------------- *
  758 !   a Mean parameters
  759 !
  760         CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN,&
  761                    AMAX, U10ABS, U10DIR, USTAR, USTDIR,           &
  762                    TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
  763 !
  764         FAGE   = FFXFA*TANH(0.3*U10ABS*FMEANWS*TPI/GRAV)
  765         FH1    = (FFXFM+FAGE) * FMEAN1
  766                                       
  767         FH2    = FFXPM / USTAR
  768         FHIGH  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
  769         NKH    = MAX ( 2 , MIN ( NKH1 ,                           &
  770                  INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH)) ) ) )
 
 
                                                         w3srcemd.F90  page  15
 
 
  771 !
  772 ! 6.b Limiter for shallow water or Miche style criterion
  773 !     Last time step ONLY !
  774 !     uses true depth (D_INP) instead of limited depth
  775 !
  776         IF ( DTTOT .GE. 0.9999*DTG ) THEN
  777             HM     = FHMAX *TANH(WNMEAN*MAX(0.,D_INP)) / MAX(1.E-4,WNMEAN>
  778             EM     = HM * HM / 16.
  779             IF ( EMEAN.GT.EM .AND. EMEAN.GT.1.E-30 ) THEN
  780                 SPEC   = SPEC / EMEAN * EM
  781                 EMEAN  = EM
  782               END IF
  783           END IF
  784 !
  785 ! 6.c Seeding of spectrum
  786 !     alpha = 0.005 , 0.5 in eq., 0.25 for directional distribution
  787 !
  788 ! 6.d Add tail
  789 !
  790         DO IK=NKH+1, NK
  791           DO ITH=1, NTH
  792             SPEC(ITH+(IK-1)*NTH) = SPEC(ITH+(IK-2)*NTH) * FACHFA         &
  793                        + 0.
  794             END DO
  795           END DO
  796 !
  797 ! 6.e  Update wave-supported stress----------------------------------- *
  798 !
  799         CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,      &
  800                       U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY, &
  801                       VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
  802                                                           
  803 !
  804 ! 7.  Check if integration complete ---------------------------------- *
  805 !
  806           IF (srce_call .eq. srce_imp_post) THEN
  807             EXIT
  808             ENDIF
  809           IF ( DTTOT .GE. 0.9999*DTG ) THEN
  810 !            IF (IX == DEBUG_NODE) WRITE(*,*) 'DTTOT, DTG', DTTOT, DTG
  811             EXIT
  812             ENDIF
  813         END DO ! INTEGRATIN LOOP
  814 !
  815 ! ... End point dynamic integration - - - - - - - - - - - - - - - - - -
  816 !
  817 ! 8.  Save integration data ------------------------------------------ *
  818 !
  819       DTDYN  = DTDYN / REAL(MAX(1,NSTEPS))
  820       FCUT   = FHIGH * TPIINV
  821 !
  822       GOTO 888
  823 !
  824 ! Error escape locations
  825 !
 
 
                                                         w3srcemd.F90  page  16
 
 
  826   888 CONTINUE
  827 !
  828 ! 9.a  Computes PHIOC------------------------------------------ *
  829 !     The wave to ocean flux is the difference between initial energy
  830 !     and final energy, plus wind input plus the SNL flux to high freq.,
  831 !     minus the energy lost to the bottom boundary layer (BBL)
  832 !
  833       EFINISH  = 0.
  834       MWXFINISH  = 0.
  835       MWYFINISH  = 0.
  836       DO IK=1, NK
  837         EBAND = 0.
  838         A1BAND = 0.
  839         B1BAND = 0.
  840         DO ITH=1, NTH
  841           DIFF = SPECINIT(ITH+(IK-1)*NTH)-SPEC(ITH+(IK-1)*NTH)
  842           EBAND = EBAND + DIFF
  843           A1BAND = A1BAND + DIFF*ECOS(ITH)
  844           B1BAND = B1BAND + DIFF*ESIN(ITH)
  845           END DO
  846         EFINISH  = EFINISH  + EBAND * DDEN(IK) / CG1(IK)
  847         MWXFINISH  = MWXFINISH  + A1BAND * DDEN(IK) / CG1(IK)        &
  848                   * WN1(IK)/SIG(IK)
  849         MWYFINISH  = MWYFINISH  + B1BAND * DDEN(IK) / CG1(IK)        &
  850                   * WN1(IK)/SIG(IK)
  851         END DO
  852 !
  853 ! Transformation in momentum flux in m^2 / s^2
  854 !
  855       TAUOX=(GRAV*MWXFINISH+TAUWIX-TAUBBL(1))/DTG
  856       TAUOY=(GRAV*MWYFINISH+TAUWIY-TAUBBL(2))/DTG
  857       TAUWIX=TAUWIX/DTG
  858       TAUWIY=TAUWIY/DTG
  859       TAUWNX=TAUWNX/DTG
  860       TAUWNY=TAUWNY/DTG
  861       TAUBBL(:)=TAUBBL(:)/DTG
  862 !
  863 ! Transformation in wave energy flux in W/m^2=kg / s^3
  864 !
  865       PHIOC =DWAT*GRAV*(EFINISH+PHIAW-PHIBBL)/DTG
  866       PHIAW =DWAT*GRAV*PHIAW /DTG
  867       PHINL =DWAT*GRAV*PHINL /DTG
  868       PHIBBL=DWAT*GRAV*PHIBBL/DTG
  869 !
  870 ! 10.1  Adds ice scattering and dissipation: implicit integration-------->
  871 !     INFLAGS2(4) is true if ice concentration was ever read during
  872 !             this simulation
  873 !
  874  
  875       IF ( INFLAGS2(4).AND.ICE.GT.0 ) THEN
  876                                           
  877          IF (IICEDISP) THEN
  878            ICECOEF2 = 1E-6
  879            CALL LIU_FORWARD_DISPERSION (ICEH,ICECOEF2,DEPTH, &
  880                                         SIG,WN_R,CG_ICE,ALPHA_LIU)
 
 
                                                         w3srcemd.F90  page  17
 
 
  881 !
  882       IF (IICESMOOTH) THEN
  883         END IF
  884      ELSE
  885       WN_R=WN1
  886       CG_ICE=CG1
  887      END IF
  888 !
  889      R(:)=1 ! In case IC2 is defined but not IS2
  890 !
  891  
  892 !
  893    SPEC2 = SPEC
  894 !
  895    TAUICE(:) = 0.
  896    PHICE = 0.
  897    DO IK=1,NK
  898      IS = 1+(IK-1)*NTH
  899 !
  900 ! First part of ice term integration: dissipation part
  901 !
  902      ATT=1.
  903              SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH) = ATT*SPEC2(1+(IK-1)*NTH:N>
  904 !
  905 ! Second part of ice term integration: scattering including re-distributi>
  906 !
  907 ! 10.2  Fluxes of energy and momentum due to ice effects
  908 !
  909              FACTOR = DDEN(IK)/CG1(IK)                    !Jacobian to ge>
  910              FACTOR2= FACTOR*GRAV*WN1(IK)/SIG(IK)         ! coefficient t>
  911              DO ITH = 1,NTH
  912                IS = ITH+(IK-1)*NTH
  913                PHICE = PHICE + (SPEC(IS)-SPEC2(IS)) * FACTOR
  914                COSI(1)=ECOS(IS)
  915                COSI(2)=ESIN(IS)
  916                TAUICE(:) = TAUICE(:) - (SPEC(IS)-SPEC2(IS))*FACTOR2*COSI(>
  917                END DO
  918              END DO
  919            PHICE =-1.*DWAT*GRAV*PHICE /DTG
  920            TAUICE(:)=TAUICE(:)/DTG
  921            ELSE
  922            END IF
  923 !
  924 ! - - - - - - - - - - - - - - - - - - - - - -
  925 ! 11. Sea state dependent stress routine calls
  926 ! - - - - - - - - - - - - - - - - - - - - - -
  927 !Note the Sea-state dependent stress calculations are primarily for high->
  928 !conditions (>10 m/s).  It is not recommended to use these at lower wind
  929 !in their current state.
  930 !
  931  
  932 ! FLD1/2 requires the calculation of FPI:
  933 !
  934 ! 12. includes shoreline reflection ------------------------------------->
  935 !
 
 
                                                         w3srcemd.F90  page  18
 
 
  936         IF (REFLEC(1).GT.0.OR.REFLEC(2).GT.0.OR.(REFLEC(4).GT.0.AND.BERG.>
  937           CALL W3SREF ( SPEC, CG1, WN1, EMEAN, FMEAN, DEPTH, CX, CY,   &
  938                         REFLEC, REFLED, TRNX, TRNY,  &
  939                         BERG, DTG, IX, IY,  VREF )
  940           IF (GTYPE.EQ.UNGTYPE.AND.REFPARS(3).LT.0.5) THEN
  941 !AR: this can be further simplified let's do some simple tests 1st ...
  942             IF (IOBP(IX).EQ.0) THEN
  943               DO IK=1, NK
  944                 DO ITH=1, NTH
  945                   IF (IOBPD(ITH,IX).EQ.0) SPEC(ITH+(IK-1)*NTH) = DTG*VREF>
  946                 END DO
  947               END DO
  948             ELSE
  949               IF (IOBDP(IX) .EQ. -1) THEN
  950                 SPEC(:) = SPEC(:) + DTG * VREF(:)
  951               ENDIF
  952             ENDIF
  953           ELSE
  954             SPEC(:) = SPEC(:) + DTG * VREF(:)
  955             END IF
  956           END IF
  957 !
  958  
  959       FIRST  = .FALSE.
  960                       
  961       IF (IT.EQ.0) SPEC = SPECINIT
  962                                   
  963       SPEC = MAX(0., SPEC)
  964 !
  965       RETURN
  966 !
  967 ! Formats
  968 !
  969  9006 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
  970               ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
  971               ' ------------- ')
  972 !
  973  9062 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
  974               '               NKH        : ',I3)
  975 !/
  976 !/ End of W3SRCE ----------------------------------------------------- /
  977 !/
  978       END SUBROUTINE W3SRCE
  979 !/ ------------------------------------------------------------------- /
  980       SUBROUTINE CALC_FPI( A, CG, FPI, S )
  981 !/
  982 !/                  +-----------------------------------+
  983 !/                  | WAVEWATCH III           NOAA/NCEP |
  984 !/                  |          Jessica Meixner          |
  985 !/                  |                                   |
  986 !/                  |                        FORTRAN 90 |
  987 !/                  | Last update :         06-Jun-2018 |
  988 !/                  +-----------------------------------+
  989 !/
  990 !/    06-Jul-2016 : Origination                         ( version 5.12 )
 
 
                                                         w3srcemd.F90  page  19
 
 
  991 !/    06-Jul-2016 : Add SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3
  992 !/                  Add optional DEBUGSRC/PDLIB           ( version 6.04 )
  993 !/
  994 !  1. Purpose :
  995 !
  996 !     Calculate equivalent peak frequency
  997 !
  998 !  2. Method :
  999 !
 1000 !     Tolman and Chalikov (1996), equivalent peak frequency from source
 1001                                                                        
 1002 !  3. Parameters :
 1003 !
 1004 !     Parameter list
 1005 !     ----------------------------------------------------------------
 1006 !       A       R.A.  I   Action density spectrum (1-D).
 1007 !       CG      R.A.  I   Group velocities for k-axis of spectrum.
 1008 !       FPI     R.A.  O   Input 'peak' frequency.
 1009 !       S       R.A.  I   Source term (1-D version).
 1010 !     ----------------------------------------------------------------
 1011 !
 1012 !  4. Subroutines used :
 1013 !
 1014 !      Name      Type  Module   Description
 1015 !     ----------------------------------------------------------------
 1016 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1017 !     ----------------------------------------------------------------
 1018 !
 1019 !  5. Called by :
 1020 !
 1021 !      Name      Type  Module   Description
 1022 !     ----------------------------------------------------------------
 1023 !      W3SRCE Subr.
 1024 !     ----------------------------------------------------------------
 1025 !
 1026 !  6. Error messages :
 1027 !
 1028 !  7. Remarks :
 1029 !
 1030 !  8. Structure :
 1031 !
 1032 !     See source code.
 1033 !
 1034 !  9. Switches :
 1035 !
 1036 !       !/S      Enable subroutine tracing.
 1037 !
 1038 ! 10. Source code :
 1039 !
 1040 !/ ------------------------------------------------------------------- /
 1041       USE CONSTANTS
 1042       USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, DDEN, SIG,FTE, FTTR
 1043 !
 1044       IMPLICIT NONE
 1045 !/
 
 
                                                         w3srcemd.F90  page  20
 
 
 1046 !/ ------------------------------------------------------------------- /
 1047 !/ Parameter list
 1048 !/
 1049       REAL, INTENT(IN)        :: A(NSPEC), CG(NK), S(NSPEC)
 1050       REAL, INTENT(OUT)       :: FPI
 1051 !/
 1052 !/ ------------------------------------------------------------------- /
 1053 !/ Local parameters
 1054 !/
 1055       INTEGER                 :: IS, IK
 1056       REAL                    ::  M0, M1, SIN1A(NK)
 1057 !/
 1058 !/ ------------------------------------------------------------------- /
 1059 !/
 1060 !
 1061 !     Calculate FPI: equivalent peak frequncy from wind source term
 1062 !     input
 1063 !
 1064       DO IK=1, NK
 1065         SIN1A(IK) = 0.
 1066         DO IS=(IK-1)*NTH+1, IK*NTH
 1067           SIN1A(IK) = SIN1A(IK) + MAX ( 0. , S(IS) )
 1068         END DO
 1069       END DO
 1070 !
 1071       M0     = 0.
 1072       M1     = 0.
 1073       DO IK=1, NK
 1074         SIN1A(IK) = SIN1A(IK) * DDEN(IK) / ( CG(IK) * SIG(IK)**3 )
 1075         M0        = M0 + SIN1A(IK)
 1076         M1        = M1 + SIN1A(IK)/SIG(IK)
 1077       END DO
 1078 !
 1079       SIN1A(NK) = SIN1A(NK) / DDEN(NK)
 1080       M0        = M0 + SIN1A(NK) * FTE
 1081       M1        = M1 + SIN1A(NK) * FTTR
 1082       IF ( M1 .LT. 1E-20 ) THEN
 1083           FPI    = XFR * SIG(NK)
 1084       ELSE
 1085           FPI    = M0 / M1
 1086       END IF
 1087             
 1088       END SUBROUTINE CALC_FPI
 1089 !/ ------------------------------------------------------------------- /!
 1090       SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC, VS, VD)
 1091 !/
 1092 !/                  +-----------------------------------+
 1093 !/                  | WAVEWATCH III           NOAA/NCEP |
 1094 !/                  |                                   |
 1095 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1096 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1097 !/                  |                                   |
 1098 !/                  |                        FORTRAN 90 |
 1099 !/                  | Last update :        01-June-2018 |
 1100 !/                  +-----------------------------------+
 
 
                                                         w3srcemd.F90  page  21
 
 
 1101 !/
 1102 !/    01-June-2018 : Origination.                        ( version 6.04 )
 1103 !/
 1104 !  1. Purpose : Put source term in matrix same as done always
 1105 !  2. Method :
 1106 !  3. Parameters :
 1107 !
 1108 !     Parameter list
 1109 !     ----------------------------------------------------------------
 1110 !     ----------------------------------------------------------------
 1111 !
 1112 !  4. Subroutines used :
 1113 !
 1114 !      Name      Type  Module   Description
 1115 !     ----------------------------------------------------------------
 1116 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1117 !     ----------------------------------------------------------------
 1118 !
 1119 !  5. Called by :
 1120 !
 1121 !      Name      Type  Module   Description
 1122 !     ----------------------------------------------------------------
 1123 !     ----------------------------------------------------------------
 1124 !
 1125 !  6. Error messages :
 1126 !  7. Remarks
 1127 !  8. Structure :
 1128 !  9. Switches :
 1129 !
 1130 !     !/S  Enable subroutine tracing.
 1131 !
 1132 ! 10. Source code :
 1133 !
 1134 !/ ------------------------------------------------------------------- /
 1135 !
 1136         USE W3GDATMD, only : NTH, NK, NSPEC
 1137         IMPLICIT NONE
 1138 !/
 1139 !/ ------------------------------------------------------------------- /
 1140 !/ Parameter list
 1141 !/
 1142 !/ ------------------------------------------------------------------- /
 1143 !/ Local PARAMETERs
 1144 !/
 1145 !/
 1146 !/ ------------------------------------------------------------------- /
 1147 !/
 1148   
 1149         INTEGER             :: ISP, ITH, IK, IS
 1150         REAL, INTENT(IN)    :: SPEC(NSPEC)
 1151         REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
 1152         DO IS=1,NSPEC
 1153           VD(IS) = MIN(0., VD(IS))
 1154         END DO
 1155       END SUBROUTINE
 
 
                                                         w3srcemd.F90  page  22
 
 
 1156 !/ ------------------------------------------------------------------- /
 1157       SUBROUTINE SIGN_VSD_PATANKAR_WW3(SPEC, VS, VD)
 1158 !/
 1159 !/                  +-----------------------------------+
 1160 !/                  | WAVEWATCH III           NOAA/NCEP |
 1161 !/                  |                                   |
 1162 !/                  | Aron Roland (BGS IT&E GmbH)       |
 1163 !/                  | Mathieu Dutour-Sikiric (IRB)      |
 1164 !/                  |                                   |
 1165 !/                  |                        FORTRAN 90 |
 1166 !/                  | Last update :        01-June-2018 |
 1167 !/                  +-----------------------------------+
 1168 !/
 1169 !/    01-June-2018 : Origination.                        ( version 6.04 )
 1170 !/
 1171 !  1. Purpose : Put source term in matrix Patankar style (experimental)
 1172 !  2. Method :
 1173 !  3. Parameters :
 1174 !
 1175 !     Parameter list
 1176 !     ----------------------------------------------------------------
 1177 !     ----------------------------------------------------------------
 1178 !
 1179 !  4. Subroutines used :
 1180 !
 1181 !      Name      Type  Module   Description
 1182 !     ----------------------------------------------------------------
 1183 !      STRACE    Subr. W3SERVMD Subroutine tracing.
 1184 !     ----------------------------------------------------------------
 1185 !
 1186 !  5. Called by :
 1187 !
 1188 !      Name      Type  Module   Description
 1189 !     ----------------------------------------------------------------
 1190 !     ----------------------------------------------------------------
 1191 !
 1192 !  6. Error messages :
 1193 !  7. Remarks
 1194 !  8. Structure :
 1195 !  9. Switches :
 1196 !
 1197 !     !/S  Enable subroutine tracing.
 1198 !
 1199 ! 10. Source code :
 1200 !
 1201 !/ ------------------------------------------------------------------- /
 1202 !
 1203  
 1204         USE W3GDATMD, only : NTH, NK, NSPEC
 1205         IMPLICIT NONE
 1206 !/
 1207 !/ ------------------------------------------------------------------- /
 1208 !/ Parameter list
 1209 !/
 1210 !/ ------------------------------------------------------------------- /
 
 
                                                         w3srcemd.F90  page  23
 
 
 1211 !/ Local PARAMETERs
 1212 !/
 1213 !/
 1214 !/ ------------------------------------------------------------------- /
 1215 !/
 1216         INTEGER             :: ISP, ITH, IK, IS
 1217         REAL, INTENT(IN)    :: SPEC(NSPEC)
 1218         REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
 1219         DO IS=1,NSPEC
 1220           VD(IS) = MIN(0., VD(IS))
 1221           VS(IS) = MAX(0., VS(IS))
 1222         END DO
 1223       END SUBROUTINE
 1224 !/
 1225 !/ End of module W3SRCEMD -------------------------------------------- /
 1226 !/
 1227       END MODULE W3SRCEMD
------------
ftn -c -module /home/luciano.pezzi/COAWST.V3.7/WW3/model/mod -g -i4 -r4 -Kieee -byteswapio -O3  -O3 -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/mod_MPI -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7//WRF/main -I/home/luciano.pezzi/COAWST.V3.7//WRF/external/esmf_time_f90 -I/home/luciano.pezzi/COAWST.V3.7//WRF/frame -I/home/luciano.pezzi/COAWST.V3.7//WRF/share -I/home/luciano.pezzi/COAWST.V3.7/Lib/MCT/pgi/include -I/home/luciano.pezzi/COAWST.V3.7/Build -I/home/luciano.pezzi/COAWST.V3.7/WW3/model/ftn  
------------
------------
------------
