/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Assigns metadata indices for the TODO:name ecosystem              **                                                                                      
**  model variables that are used in input and output NetCDF files.   **                                                                                      
**  The metadata information is read from "varinfo.dat".              **                                                                                      
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/

/*
**  Model state biological tracers.
*/
!*****************************************************************************************
!             TODO: List all state variables, diagnostics, boundary conditions from 
!                   varinfo.dat
!
!             CASE ('idTvar(iNO3_)')
!               idTvar(iNO3_)=varid
!             CASE ('idTvar(iNH4_)')
!               idTvar(iNH4_)=varid
!
!*****************************************************************************************