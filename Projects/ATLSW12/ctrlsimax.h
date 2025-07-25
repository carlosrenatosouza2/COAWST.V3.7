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
#define MCT_LIB
#define MCT_INTERP_OC2AT
#undef MCT_INTERP_WV2AT
#undef MCT_INTERP_OC2WV
#define NO_LBC_ATT

#if defined WRF_MODEL && defined SWAN_MODEL
# define DRAGLIM_DAVIS
# define COARE_TAYLOR_YELLAND
#endif

#ifdef ROMS_MODEL
/* Physics + numerics */
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define MIX_S_UV
# undef  TS_FIXED
# define TS_U3HADVECTION
# define TS_C4VADVECTION
# undef  TS_MPDATA

# undef SSW_BBL
# ifdef SSW_BBL
#  define SSW_CALC_ZNOT
# /*define ANA_SEDIMENT*/
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
# define AVERAGES2
# define NONLIN_EOS

/* Seaice */
#ifdef SOLVE3D
# define ICE_MODEL
# ifdef ICE_MODEL
#  define ANA_ICE
#  define INI_GLORYS_ICE
#  define OUTFLOW_MASK
#  undef  SNOWFALL
#  define ICE_LANDFAST
#  define ICE_THERMO
#  define ICE_MK
#  define ICE_MOMENTUM
#  define ICE_MOM_BULK
#  define ICE_EVP
#  define ICE_STRENGTH_QUAD
#  define ICE_ADVECT
#  define ICE_SMOLAR
#  define ICE_UPWIND
#  define ICE_BULK_FLUXES
#  define ICE_CONVSNOW
#  define ICE_I_O
#  undef  ICE_DIAGS
#  undef  CCSM_ICE_SHORTWAVE
# endif
#endif

/* Grid and Initial */
# define MASKING

/* Forcing */
# ifdef WRF_MODEL
#  undef  BULK_FLUXES
#  define ATM2OCN_FLUXES
#  define ANA_SSFLUX
#  undef  LONGWAVE_OUT
#  define ATM_PRESS
# else
#  define ANA_SMFLUX
#  define ANA_STFLUX
#  define ANA_SSFLUX
# endif
# define ANA_BTFLUX
# define ANA_BSFLUX
# define EMINUSP
# define SOLAR_SOURCE

/* Turbulence closure */
# define GLS_MIXING
# undef  MY25_MIXING
# define AKLIMIT

# if defined GLS_MIXING || defined MY25_MIXING
#  define KANTHA_CLAYSON
#  define N2S2_HORAVG
# define RI_SPLINES
# endif

/* Output */
# define DIAGNOSTICS_UV
# define DIAGNOSTICS_TS
#endif
