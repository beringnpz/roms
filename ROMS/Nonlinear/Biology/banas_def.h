/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Defines BANAS input parameters in                                 **
**  output NetCDF files. It is included in routine "def_info.F".      **
**                                                                    **
************************************************************************
*/

!
!  Define Banas ecosystem model parameters.
!
      Vinfo( 1)='BioIter'
      Vinfo( 2)='number of iterations to achieve convergence'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='mu0'
      Vinfo( 2)='maximum phytoplankton growth rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='att_sw'
      Vinfo( 2)='light attenuation by seawater'
      Vinfo( 3)='m^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='att_p'
      Vinfo( 2)='light attneuation by phytoplankton'
      Vinfo( 3)='m^-1 uM N^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='alpha_win'
      Vinfo( 2)='initial growth-light slope, winter'
      Vinfo( 3)='(W M^-2)^-1 d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='alpha_sum'
      Vinfo( 2)='initial growth-light slope, summer'
      Vinfo( 3)='(W M^-2)^-1 d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='Ecrit'
      Vinfo( 2)='light level of alpha_win/alpha_sum transition'
      Vinfo( 3)='W m^-2'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='deltaE'
      Vinfo( 2)='width of alpha_win/alpha_sum transition'
      Vinfo( 3)='W m^-2'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='kmin'
      Vinfo( 2)='minmimum half-saturation for NO3'
      Vinfo( 3)='uM N'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='phi_NH4'
      Vinfo( 2)='preference for NH4'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='CNratio'
      Vinfo( 2)='phytoplankton C:N ratio'
      Vinfo( 3)='molC/molN'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='chlNratio'
      Vinfo( 2)='chlorohpyll:N ratio'
      Vinfo( 3)='mg chl/uM N'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='m_P'
      Vinfo( 2)='phytoplankton mortality'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='m_agg'
      Vinfo( 2)='phytoplankton loss via aggregation'
      Vinfo( 3)='(uM N)^-1 d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='I0'
      Vinfo( 2)='max microzooplankton ingestion rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='K'
      Vinfo( 2)='grazing half-saturation'
      Vinfo( 3)='uM N'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='epsil'
      Vinfo( 2)='microzooplankton growth efficiency'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='fex'
      Vinfo( 2)='fraction of grazing excreted to NH4'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='m_Z'
      Vinfo( 2)='microzooplankton mortality'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='w_S'
      Vinfo( 2)='small detritus sinking rate'
      Vinfo( 3)='m d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='w_L'
      Vinfo( 2)='large detritus sinking rate'
      Vinfo( 3)='m d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='r_remin'
      Vinfo( 2)='detrital remineralization rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='r_nitr'
      Vinfo( 2)='nitrification rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='Q_P'
      Vinfo( 2)='Q10 for phytoplankton'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='Q_Z'
      Vinfo( 2)='Q10 for zooplankton'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='Q_R'
      Vinfo( 2)='Q10 for bacterial respiration'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#ifdef COASTAL_ATTEN
      Vinfo( 1)='k_sed1'
      Vinfo( 2)='Depth-based attenuation coefficient, factor'
      Vinfo( 3)='m^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     
      Vinfo( 1)='k_sed2'
      Vinfo( 2)='Depth-based attenuation coefficient, exponent'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#endif

