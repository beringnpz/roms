/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Writes BESTNPZ ecosystem model input parameters into              **                                                                                      
**  output NetCDF files. It is included in routine "wrt_info.F".      **                                                                                      
**                                                                    **
************************************************************************
*/

!
!  Write out BESTNPZ ecosystem model parameters.                                                                                                 
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     
      CALL netcdf_put_fvar (ng, model, ncname, 'PARfrac'                &
     &                      PARfrac(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_ext'                  &
     &                      k_ext(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_chlA'                 &
     &                      k_chlA(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_chlB'                 &
     &                      k_chlB(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_chlC'                 &
     &                      k_chlC(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_sed1'                 &
     &                      k_sed1(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k_sed2'                 &
     &                      k_sed2(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'xi'                     &
     &                      xi(ng), (/0/), (/0/)                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ccr'                    &
     &                      ccr(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ccrPhL'                 &
     &                      ccrPhL(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'FeC'                    &
     &                      FeC(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'DiS'                    &
     &                      DiS(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'DiL'                    &
     &                      DiL(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'DpS'                    &
     &                      DpS(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'DpL'                    &
     &                      DpL(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'alphaPhS'               &
     &                      alphaPhS(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'alphaPhL'               &
     &                      alphaPhL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k1PhS'                  &
     &                      k1PhS(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k1PhL'                  &
     &                      k1PhL(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k2PhS'                  &
     &                      k2PhS(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'k2PhL'                  &
     &                      k2PhL(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'FeCritPS'               &
     &                      FeCritPS(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'FeCritPL'               &
     &                      FeCritPL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kfePhS'                 &
     &                      kfePhS(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'kfePhL'                 &
     &                      kfePhL(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhSMZL'               &
     &                      fpPhSMZL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhLMZL'               &
     &                      fpPhLMZL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhSCop'               &
     &                      fpPhSCop(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhLCop'               &
     &                      fpPhLCop(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpMZLCop'               &
     &                      fpMZLCop(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhSNCa'               &
     &                      fpPhSNCa(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhLNCa'               &
     &                      fpPhLNCa(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpMZLNCa'               &
     &                      fpMZLNCa(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhSEup'               &
     &                      fpPhSEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpPhLEup'               &
     &                      fpPhLEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpMZLEup'               &
     &                      fpMZLEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpCopEup'               &
     &                      fpCopEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpDetEup'               &
     &                      fpDetEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpDetEupO'              &
     &                      fpDetEupO(ng), (/0/), (/0/)                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpCopJel'               &
     &                      fpCopJel(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpNCaJel'               &
     &                      fpNCaJel(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fpEupJel'               &
     &                      fpEupJel(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eMZL'                   &
     &                      eMZL(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eCop'                   &
     &                      eCop(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eNCa'                   &
     &                      eNCa(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eEup'                   &
     &                      eEup(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eJel'                   &
     &                      eJel(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10MZL'                 &
     &                      Q10MZL(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10Cop'                 &
     &                      Q10Cop(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10NCa'                 &
     &                      Q10NCa(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10Eup'                 &
     &                      Q10Eup(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10Jele'                &
     &                      Q10Jele(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10MZLT'                &
     &                      Q10MZLT(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10CopT'                &
     &                      Q10CopT(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10NCaT'                &
     &                      Q10NCaT(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10EupT'                &
     &                      Q10EupT(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10JelTe'               &
     &                      Q10JelTe(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fMZL'                   &
     &                      fMZL(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fCop'                   &
     &                      fCop(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fNCa'                   &
     &                      fNCa(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fEup'                   &
     &                      fEup(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'fJel'                   &
     &                      fJel(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gammaMZL'               &
     &                      gammaMZL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gammaCop'               &
     &                      gammaCop(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gammaNCa'               &
     &                      gammaNCa(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gammaEup'               &
     &                      gammaEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'gammaJel'               &
     &                      gammaJel(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mPhS'                   &
     &                      mPhS(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mPhL'                   &
     &                      mPhL(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mMZL'                   &
     &                      mMZL(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mpredMZL'               &
     &                      mpredMZL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mpredCop'               &
     &                      mpredCop(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mpredNCa'               &
     &                      mpredNCa(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mpredEup'               &
     &                      mpredEup(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mpredJel'               &
     &                      mpredJel(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wPhS'                   &
     &                      wPhS(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wPhL'                   &
     &                      wPhL(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wDet'                   &
     &                      wDet(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wDetF'                  &
     &                      wDetF(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respPhS'                &
     &                      respPhS(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respPhL'                &
     &                      respPhL(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respMZL'                &
     &                      respMZL(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respCop'                &
     &                      respCop(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respNCa'                &
     &                      respNCa(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respEup'                &
     &                      respEup(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'respJel'                &
     &                      respJel(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10Jelr'                &
     &                      Q10Jelr(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Q10JelTr'               &
     &                      Q10JelTr(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KtBm_PhS'               &
     &                      KtBm_PhS(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KtBm_PhL'               &
     &                      KtBm_PhL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KtBm_MZL'               &
     &                      KtBm_MZL(ng), (/0/), (/0/)                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ktbmC'                  &
     &                      ktbmC(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ktbmN'                  &
     &                      ktbmN(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ktbmE'                  &
     &                      ktbmE(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TmaxPhS'                &
     &                      TmaxPhS(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TmaxPhL'                &
     &                      TmaxPhL(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TmaxMZL'                &
     &                      TmaxMZL(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TrefC'                  &
     &                      TrefC(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TrefN'                  &
     &                      TrefN(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'TrefE'                  &
     &                      TrefE(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feinlo'                 &
     &                      Feinlo(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feinhi'                 &
     &                      Feinhi(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feinh'                  &
     &                      Feinh(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feofflo'                &
     &                      Feofflo(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feoffhi'                &
     &                      Feoffhi(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Feoffh'                 &
     &                      Feoffh(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wNCrise'                &
     &                      wNCrise(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'wNCsink'                &
     &                      wNCsink(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'RiseStart'              &
     &                      RiseStart(ng), (/0/), (/0/)                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'RiseEnd'                &
     &                      RiseEnd(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SinkStart'              &
     &                      SinkStart(ng), (/0/), (/0/)                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SinkEnd'                &
     &                      SinkEnd(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'RiseStartCM'            &
     &                      RiseStartCM(ng), (/0/), (/0/)               &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'RiseEndCM'              &
     &                      RiseEndCM(ng), (/0/), (/0/)                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SinkStartCM'            &
     &                      SinkStartCM(ng), (/0/), (/0/)               &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SinkEndCM'              &
     &                      SinkEndCM(ng), (/0/), (/0/)                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Pv0'                    &
     &                      Pv0(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'PvT'                    &
     &                      PvT(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Nitr0'                  &
     &                      Nitr0(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ktntr'                  &
     &                      ktntr(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KNH4Nit'                &
     &                      KNH4Nit(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ToptNtr'                &
     &                      ToptNtr(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  ifdef BENTHIC
      CALL netcdf_put_fvar (ng, model, ncname, 'q10r'                   &
     &                      q10r(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Rup'                    &
     &                      Rup(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KupD'                   &
     &                      KupD(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'KupP'                   &
     &                      KupP(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'LupD'                   &
     &                      LupD(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'LupP'                   &
     &                      LupP(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Qres'                   &
     &                      Qres(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'Rres'                   &
     &                      Rres(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'rmort'                  &
     &                      rmort(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eex'                    &
     &                      eex(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'eexD'                   &
     &                      eexD(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'prefD'                  &
     &                      prefD(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'prefPL'                 &
     &                      prefPL(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'prefPS'                 &
     &                      prefPS(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'T0benr'                 &
     &                      T0benr(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'BenPred'                &
     &                      BenPred(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  endif
#  ifdef ICE_BIO
      CALL netcdf_put_fvar (ng, model, ncname, 'alphaIb'                &
     &                      alphaIb(ng), (/0/), (/0/)                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'betaI'                  &
     &                      betaI(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'inhib'                  &
     &                      inhib(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksnut1'                 &
     &                      ksnut1(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'ksnut2'                 &
     &                      ksnut2(ng), (/0/), (/0/)                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'mu0'                    &
     &                      mu0(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'R0i'                    &
     &                      R0i(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'rg0'                    &
     &                      rg0(ng), (/0/), (/0/)                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'rg'                     &
     &                      rg(ng), (/0/), (/0/)                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'annit'                  &
     &                      annit(ng), (/0/), (/0/)                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'aidz'                   &
     &                      aidz(ng), (/0/), (/0/)                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
#  endif