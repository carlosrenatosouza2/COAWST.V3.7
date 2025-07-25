/*
** svn $Id: sandy.h 25 2007-04-09 23:43:58Z jcwarner $
*******************************************************************************
** Copyright (c) 2002-2007 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Sandy Test.
**
** Application flag:   SANDY
*/

#define ROMS_MODEL
#undef NESTING
#define WRF_MODEL
#undef SWAN_MODEL
#undef  WW3_MODEL
#define MCT_LIB
#define MCT_INTERP_OC2AT
#undef MCT_INTERP_WV2AT
#undef MCT_INTERP_OC2WV
#define NO_LBC_ATT

#if defined WRF_MODEL && (defined SWAN_MODEL || defined WW3_MODEL)
# define DRAGLIM_DAVIS
# define COARE_TAYLOR_YELLAND
#endif

#ifdef ROMS_MODEL
/* Physics + numerics */
# if defined WW3_MODEL || defined SWAN_MODEL
#  define WEC_VF
#  define WDISS_WAVEMOD
#  define UV_KIRBY
# endif
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define MIX_S_UV
# undef  TS_FIXED

# undef SSW_BBL
# ifdef SSW_BBL
#  define SSW_CALC_ZNOT
#  define ANA_SEDIMENT
# else
#  define UV_LOGDRAG
# endif
# if !defined SWAN_MODEL && defined SSW_BBL
#  define ANA_WWAVE
# endif

# define DJ_GRADPS
# define TS_DIF2
# define MIX_GEO_TS
# define CURVGRID

# define SALINITY
# define SOLVE3D
# define SPLINES_VDIFF
# define SPLINES_VVISC
# define AVERAGES
# define NONLIN_EOS

/* Grid and Initial */
# define MASKING

/* Forcing */
# ifdef WRF_MODEL
#  undef  BULK_FLUXES
#  define ATM2OCN_FLUXES
#  undef  ANA_SSFLUX
#  undef  LONGWAVE_OUT
# else
#  define BULK_FLUXES
# endif
# define ANA_NUDGCOEF
# define ATM_PRESS
# define ANA_BTFLUX
# define ANA_BSFLUX
# define ANA_BPFLUX
# define ANA_SPFLUX
# define ANA_SRFLUX
# define EMINUSP
# define SOLAR_SOURCE

/* Turbulence closure */
# define GLS_MIXING
# undef  MY25_MIXING
# undef LIMIT_VDIFF
# undef LIMIT_VVISC
# define AKLIMIT

# if defined GLS_MIXING || defined MY25_MIXING
#  define KANTHA_CLAYSON
#  define N2S2_HORAVG
#  define RI_SPLINES
#  define CRAIG_BANNER
#  define CHARNOK
# endif
/* Turbulence closure Kate Suggestion */

# undef LMD_MIXING
# ifdef LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
#  define LMD_SKPP
#  define LI_FOX_KEMPER
#  undef LMD_BKPP
#  define LMD_NONLOCAL
#  define LMD_SHAPIRO
#  define LMD_DDMIX
#  define LIMIT_VDIFF
# endif




# undef SSH_TIDES
# undef UV_TIDES
# undef RAMP_TIDES
# undef ANA_FSOBC
# undef ANA_M2OBC

/* Output */
# define DIAGNOSTICS_UV
# define DIAGNOSTICS_TS
#endif
