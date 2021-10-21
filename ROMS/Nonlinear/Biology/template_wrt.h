/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Writes TODO:name ecosystem model input parameters into **                                                                                      
**  output NetCDF files. It is included in routine "wrt_info.F".      **                                                                                      
**                                                                    **
************************************************************************
*/

!
!  Write out TODO:name ecosystem model parameters.                                                                                                 
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     
!*****************************************************************************************
!     TODO: Add write statements for all parameters
!
!     CALL netcdf_put_fvar (ng, model, ncname, 'AttSW',                 &
!    &                      AttSW(ng), (/0/), (/0/),                    &
!    &                      ncid = ncid)
!     IF (FoundError(exit_flag, NoError, __LINE__,                      &
!    &               __FILE__)) RETURN 
!
!*****************************************************************************************