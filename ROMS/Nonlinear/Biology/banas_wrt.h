/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Writes Banas ecosystem model input parameters into                **
**  output NetCDF files. It is included in routine "wrt_info.F".      **
**                                                                    **
************************************************************************
*/

!
!  Write out Banas ecosystem model parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'mu0',                   &
     &                      mu0(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'att_sw',                &
     &                      att_sw(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'att_p',                 &
     &                      att_p(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'alpha_win',             &
     &                      alpha_win(ng), (/0/), (/0/),                &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'alpha_sum',             &
     &                      alpha_sum(ng), (/0/), (/0/),                &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Ecrit',                 &
     &                      Ecrit(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
     CALL netcdf_put_fvar (ng, model, ncname, 'deltaE',                &
     &                      deltaE(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'kmin',                  &
     &                      kmin(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'phi_NH4',               &
     &                      phi_NH4(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'CNratio',               &
     &                      CNratio(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'chlNratio',             &
     &                      chlNratio(ng), (/0/), (/0/),                &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'm_P',                   &
     &                      m_P(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'm_agg',                 &
     &                      m_agg(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'I0',                    &
     &                      I0(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'K',                     &
     &                      K(ng), (/0/), (/0/),                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'epsil',                 &
     &                      epsil(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'fex',                   &
     &                      fex(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'm_Z',                   &
     &                      m_Z(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'w_S',                   &
     &                      w_S(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'w_L',                   &
     &                      w_L(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'r_remin',               &
     &                      r_remin(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'r_nitr',                &
     &                      r_nitr(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Q_P',                   &
     &                      Q_P(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Q_Z',                   &
     &                      Q_Z(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Q_R',                   &
     &                      Q_R(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN