/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Defines BESTNPZ input parameters in                               **                                                                                      
**  output NetCDF files. It is included in routine "def_info.F".      **                                                                                      
**                                                                    **
************************************************************************
*/

!
!  Define BESTNPZ ecosystem model parameters.                                                                                                    
!
      Vinfo( 1)='BioIter'
      Vinfo( 2)='number of iterations to achieve convergence'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     
      Vinfo( 1)='PARfrac'
      Vinfo( 2)='Fraction of irradiance that is photosynthetically available (PAR)'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k_ext'
      Vinfo( 2)='Clear-water attenuation coefficient'
      Vinfo( 3)='m^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k_chlA'
      Vinfo( 2)='Chlorophyll attenuation coefficient, factor'
      Vinfo( 3)='m^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k_chlB'
      Vinfo( 2)='Chlorophyll attenuation coefficient, exponent'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k_chlC'
      Vinfo( 2)='Other material (CDOM,sediment,etc.) attenuation coefficient'
      Vinfo( 3)='m^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
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
   
      Vinfo( 1)='xi'
      Vinfo( 2)='Nitrogen:Carbon ratio'
      Vinfo( 3)='mmol N / mg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='ccr'
      Vinfo( 2)='Carbon:Chlorophyll ratio, small phyto'
      Vinfo( 3)='mg C / mg Chl-a'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='ccrPhL'
      Vinfo( 2)='Carbon:Chlorophyll ratio, large phyto'
      Vinfo( 3)='mg C / mg Chl-a'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='FeC'
      Vinfo( 2)='Fe:Carbon ratio    (2 umol Fe : mol C)'
      Vinfo( 3)='umol Fe / mg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='DiS'
      Vinfo( 2)='Doubling rate parameter'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='DiL'
      Vinfo( 2)='Doubling rate parameter'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='DpS'
      Vinfo( 2)='Doubling rate exponent'
      Vinfo( 3)='degC^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='DpL'
      Vinfo( 2)='Doubling rate exponent'
      Vinfo( 3)='degC^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='alphaPhS'
      Vinfo( 2)='photosynthetic efficiency'
      Vinfo( 3)='mg C m^2 (mg Chl-a)^-1 (E^-1)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='alphaPhL'
      Vinfo( 2)='photosynthetic efficiency'
      Vinfo( 3)='mg C m^2 (mg Chl-a)^-1 (E^-1)'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k1PhS'
      Vinfo( 2)='Half-saturation constant for NO3 limitation'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k1PhL'
      Vinfo( 2)='Half-saturation constant for NO3 limitation'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k2PhS'
      Vinfo( 2)='Half-saturation constant for NH4 limitation'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='k2PhL'
      Vinfo( 2)='Half-saturation constant for NH4 limitation'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='FeCritPS'
      Vinfo( 2)='Threshold below which PhS is limited'
      Vinfo( 3)='umol Fe m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='FeCritPL'
      Vinfo( 2)='Threshold below which PhS is limited'
      Vinfo( 3)='umol Fe m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='kfePhS'
      Vinfo( 2)='Half-saturation constant for Fe'
      Vinfo( 3)='umol m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='kfePhL'
      Vinfo( 2)='Half-saturation constant for Fe'
      Vinfo( 3)='umol m^-3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhSMZL'
      Vinfo( 2)='PhS->MZL  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhLMZL'
      Vinfo( 2)='PhL->MZL  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhSCop'
      Vinfo( 2)='PhS->Cop  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhLCop'
      Vinfo( 2)='PhL->Cop  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpMZLCop'
      Vinfo( 2)='MZL->Cop  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhSNCa'
      Vinfo( 2)='PhS->NCa  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhLNCa'
      Vinfo( 2)='PhL->NCa  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpMZLNCa'
      Vinfo( 2)='MZL->NCa  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhSEup'
      Vinfo( 2)='PhS->Eup  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpPhLEup'
      Vinfo( 2)='PhL->Eup  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpMZLEup'
      Vinfo( 2)='MZL->Eup  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpCopEup'
      Vinfo( 2)='Cop->Eup  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpDetEup'
      Vinfo( 2)='Det->Eup  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpDetEupO'
      Vinfo( 2)='Det->EupO Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpCopJel'
      Vinfo( 2)='Cop->Jel  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpNCaJel'
      Vinfo( 2)='NCa->Jel  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fpEupJel'
      Vinfo( 2)='Eup->Jel  Feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='eMZL'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='mg C/mg C/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='eCop'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='mg C/mg C/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='eNCa'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='mg C/mg C/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='eEup'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='mg C/mg C/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='eJel'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='mg C/mg C/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10MZL'
      Vinfo( 2)='Q10 for growth rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10Cop'
      Vinfo( 2)='Q10 for growth rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10NCa'
      Vinfo( 2)='Q10 for growth rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10Eup'
      Vinfo( 2)='Q10 for growth rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10Jele'
      Vinfo( 2)='Q10 for growth rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10MZLT'
      Vinfo( 2)='Temperature coefficient for Q10'
      Vinfo( 3)='deg. C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10CopT'
      Vinfo( 2)='Temperature coefficient for Q10'
      Vinfo( 3)='deg. C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10NCaT'
      Vinfo( 2)='Temperature coefficient for Q10'
      Vinfo( 3)='deg. C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10EupT'
      Vinfo( 2)='Temperature coefficient for Q10'
      Vinfo( 3)='deg. C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='Q10JelTe'
      Vinfo( 2)='Temperature coefficient for Q10'
      Vinfo( 3)='deg. C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fMZL'
      Vinfo( 2)='Half-saturation constant for grazing'
      Vinfo( 3)='mg C/m3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fCop'
      Vinfo( 2)='Half-saturation constant for grazing'
      Vinfo( 3)='mg C/m3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fNCa'
      Vinfo( 2)='Half-saturation constant for grazing'
      Vinfo( 3)='mg C/m3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fEup'
      Vinfo( 2)='Half-saturation constant for grazing'
      Vinfo( 3)='mg C/m3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='fJel'
      Vinfo( 2)='Half-saturation constant for grazing'
      Vinfo( 3)='mg C/m3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='gammaMZL'
      Vinfo( 2)='Growth efficiency'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='gammaCop'
      Vinfo( 2)='Growth efficiency'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='gammaNCa'
      Vinfo( 2)='Growth efficiency'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='gammaEup'
      Vinfo( 2)='Growth efficiency'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='gammaJel'
      Vinfo( 2)='Growth efficiency'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='mPhS'
      Vinfo( 2)='daily linear mortality rate (senescence)'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='mPhL'
      Vinfo( 2)='daily linear mortality rate (senescence)'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
   
      Vinfo( 1)='mMZL'
      Vinfo( 2)='daily linear mortality rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mpredMZL'
      Vinfo( 2)='Daily mortality for Large Microzoo.'
      Vinfo( 3)='1/d/mgC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mpredCop'
      Vinfo( 2)='Daily mortality for Copepods'
      Vinfo( 3)='1/d/mgC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mpredNCa'
      Vinfo( 2)='Daily mortality for Neocalanus'
      Vinfo( 3)='1/d/mgC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mpredEup'
      Vinfo( 2)='Daily mortality for Euphausiids'
      Vinfo( 3)='1/d/mgC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mpredJel'
      Vinfo( 2)='Daily mortality for Large Microzoo.'
      Vinfo( 3)='1/d/mgC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wPhS'
      Vinfo( 2)='Sinking rate for Small Phytoplankton'
      Vinfo( 3)='m/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wPhL'
      Vinfo( 2)='Sinking rate for Large Phytoplankton'
      Vinfo( 3)='m/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wDet'
      Vinfo( 2)='Sinking rate for Detritus'
      Vinfo( 3)='m/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wDetF'
      Vinfo( 2)='Sinking rate for Detritus'
      Vinfo( 3)='m/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respPhS'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respPhL'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respMZL'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respCop'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respNCa'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respEup'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='respJel'
      Vinfo( 2)='Specific respiration rate'
      Vinfo( 3)='d^-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Q10Jelr'
      Vinfo( 2)='Q10 for respiration rate, jellyfish'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Q10JelTr'
      Vinfo( 2)='reference temperature for Q10 respiration, jellyfish'
      Vinfo( 3)='1/degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KtBm_PhS'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KtBm_PhL'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KtBm_MZL'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ktbmC'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ktbmN'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ktbmE'
      Vinfo( 2)='temperature coefficient for respiration'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TmaxPhS'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TmaxPhL'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TmaxMZL'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TrefC'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TrefN'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='TrefE'
      Vinfo( 2)='reference temperature for respiration'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feinlo'
      Vinfo( 2)='inshore/surface'
      Vinfo( 3)='micromol Fe m-3 or nM'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feinhi'
      Vinfo( 2)='inshore/deep'
      Vinfo( 3)='micromol Fe m-3 or nM'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feinh'
      Vinfo( 2)='inshore isobath of transition'
      Vinfo( 3)='m'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feofflo'
      Vinfo( 2)='offshore/surface'
      Vinfo( 3)='micromol Fe m-3 or nM'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feoffhi'
      Vinfo( 2)='offshore/deep'
      Vinfo( 3)='micromol Fe m-3 or nM'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Feoffh'
      Vinfo( 2)='offshore isobath of transition'
      Vinfo( 3)='m'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wNCrise'
      Vinfo( 2)='upward velocity , tuned not data'
      Vinfo( 3)='m/day'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='wNCsink'
      Vinfo( 2)='downward velocity , tuned not data'
      Vinfo( 3)='m/day'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='RiseStart'
      Vinfo( 2)='Date NCaO begin to move upward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='RiseEnd'
      Vinfo( 2)='Date NCaO stop moving upward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='SinkStart'
      Vinfo( 2)='Date NCaO begin to move downward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='SinkEnd'
      Vinfo( 2)='Date NCaO stop moving downward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='RiseStartCM'
      Vinfo( 2)='Date NCaS begin to move upward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='RiseEndCM'
      Vinfo( 2)='Date NCaS stop moving upward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='SinkStartCM'
      Vinfo( 2)='Date NCaS begin to move downward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='SinkEndCM'
      Vinfo( 2)='Date NCaS stop moving downward'
      Vinfo( 3)='Day of Year'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Pv0'
      Vinfo( 2)='PON decompositon at 0 deg C'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='PvT'
      Vinfo( 2)='Temperature coefficient for remineralization'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Nitr0'
      Vinfo( 2)='Nitrification rate at 0C'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ktntr'
      Vinfo( 2)='Temperature coefficient for nitrification'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KNH4Nit'
      Vinfo( 2)='Half saturation constant for nitrification'
      Vinfo( 3)='mmolN/m^3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ToptNtr'
      Vinfo( 2)='Optimal temperature for nitrification'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  ifdef BENTHIC 
      Vinfo( 1)='q10r'
      Vinfo( 2)='Q10 for growth/feeding and mortality rate'
      Vinfo( 3)='unitless'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Rup'
      Vinfo( 2)='maximum specific ingestion rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KupD'
      Vinfo( 2)='Half-saturation constant for feeding on benthic prey'
      Vinfo( 3)='mg C/m^2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='KupP'
      Vinfo( 2)='Half-saturation constant for feeding on pelagic prey'
      Vinfo( 3)='mg C/m^2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='LupD'
      Vinfo( 2)='Lower threshold for feeding on benthic prey'
      Vinfo( 3)='mg C/m^2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='LupP'
      Vinfo( 2)='Lower threshold for feeding on pelagic prey'
      Vinfo( 3)='mg C/m^2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Qres'
      Vinfo( 2)='Active metabolic rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='Rres'
      Vinfo( 2)='Basal metabolism rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='rmort'
      Vinfo( 2)='linear mortality rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='eex'
      Vinfo( 2)='fraction of living food excreted (1 - growth efficiency)'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='eexD'
      Vinfo( 2)='fraction of detrital food excreted'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='prefD'
      Vinfo( 2)='DetBen->Ben feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='prefPL'
      Vinfo( 2)='PhL->Ben feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='prefPS'
      Vinfo( 2)='PhS->Ben feeding preference'
      Vinfo( 3)=' '
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='T0benr'
      Vinfo( 2)='Reference temperature for growth/feeding rate'
      Vinfo( 3)='degC'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='BenPred'
      Vinfo( 2)='Quadratic mortality rate due to undefined predation'
      Vinfo( 3)='1/mgC/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  endif
#  ifdef ICE_BIO
 
      Vinfo( 1)='alphaIb'
      Vinfo( 2)='IcePhL Chl-a specific attenuation coefficient'
      Vinfo( 3)='W^-1 m^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='betaI'
      Vinfo( 2)='IcePhL photosynthetic efficiency'
      Vinfo( 3)='W^-1 m^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='inhib'
      Vinfo( 2)='IcePhL NH4 inhibition on NO3 uptake'
      Vinfo( 3)='m^3/mmol N'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ksnut1'
      Vinfo( 2)='IcePhL half-saturation constant for NO3'
      Vinfo( 3)='mmolN/m^3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='ksnut2'
      Vinfo( 2)='IcePhL half-saturation constant for NH4'
      Vinfo( 3)='mmolN/m^3'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='mu0'
      Vinfo( 2)='IcePhL maximum growth rate at 0 deg C'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='R0i'
      Vinfo( 2)='IcePhL respiration rate'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='rg0'
      Vinfo( 2)='IcePhL mortality rate at 0 deg C'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='rg'
      Vinfo( 2)='IcePhL temperature coefficient for mortality'
      Vinfo( 3)='1/deg C'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='annit'
      Vinfo( 2)='IcePhL nitrification factor'
      Vinfo( 3)='1/d'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
 
      Vinfo( 1)='aidz'
      Vinfo( 2)='Ice thickness'
      Vinfo( 3)='m'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     
      Vinfo( 1)='tI0'
      Vinfo( 2)='Nitrification light threshold '
      Vinfo( 3)='W m^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
    
      Vinfo( 1)='KI'
      Vinfo( 2)='Nitrification light half-saturation constant'
      Vinfo( 3)='W m^-2'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Vinfo( 1)='fracUnburied'
      Vinfo( 2)='fraction of sinking flux across bottom boundary that remains in system (unburied)'
      Vinfo( 3)='None'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  endif
