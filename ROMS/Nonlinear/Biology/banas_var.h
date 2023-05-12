/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Assigns metadata indices for the Banas ecosystem                  **
**  model variables that are used in input and output NetCDF files.   **
**  The metadata information is read from "varinfo.dat".              **
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/
              CASE ('idTvar(iphyto)')
                idTvar(iphyto)=varid
              CASE ('idTvar(izoo)')
                idTvar(izoo)=varid
              CASE ('idTvar(idets)')
                idTvar(idets)=varid
              CASE ('idTvar(idetl)')
                idTvar(idetl)=varid
              CASE ('idTvar(inh4)')
                idTvar(inh4)=varid
              CASE ('idTvar(ino3)')
                idTvar(ino3)=varid
              CASE ('idTbry(ieast,iphyto)')
                idTbry(ieast,iphyto)=varid
              CASE ('idTbry(inorth,iphyto)')
                idTbry(inorth,iphyto)=varid
              CASE ('idTbry(iwest,iphyto)')
                idTbry(iwest,iphyto)=varid
              CASE ('idTbry(isouth,iphyto)')
                idTbry(isouth,iphyto)=varid
              CASE ('idTbry(ieast,izoo)')
                idTbry(ieast,izoo)=varid
              CASE ('idTbry(inorth,izoo)')
                idTbry(inorth,izoo)=varid
              CASE ('idTbry(iwest,izoo)')
                idTbry(iwest,izoo)=varid
              CASE ('idTbry(isouth,izoo)')
                idTbry(isouth,izoo)=varid
              CASE ('idTbry(ieast,idets)')
                idTbry(ieast,idets)=varid
              CASE ('idTbry(inorth,idets)')
                idTbry(inorth,idets)=varid
              CASE ('idTbry(iwest,idets)')
                idTbry(iwest,idets)=varid
              CASE ('idTbry(isouth,idets)')
                idTbry(isouth,idets)=varid
              CASE ('idTbry(ieast,idetl)')
                idTbry(ieast,idetl)=varid
              CASE ('idTbry(inorth,idetl)')
                idTbry(inorth,idetl)=varid
              CASE ('idTbry(iwest,idetl)')
                idTbry(iwest,idetl)=varid
              CASE ('idTbry(isouth,idetl)')
                idTbry(isouth,idetl)=varid
              CASE ('idTbry(ieast,inh4)')
                idTbry(ieast,inh4)=varid
              CASE ('idTbry(inorth,inh4)')
                idTbry(inorth,inh4)=varid
              CASE ('idTbry(iwest,inh4)')
                idTbry(iwest,inh4)=varid
              CASE ('idTbry(isouth,inh4)')
                idTbry(isouth,inh4)=varid
              CASE ('idTbry(ieast,ino3)')
                idTbry(ieast,ino3)=varid
              CASE ('idTbry(inorth,ino3)')
                idTbry(inorth,ino3)=varid
              CASE ('idTbry(iwest,ino3)')
                idTbry(iwest,ino3)=varid
              CASE ('idTbry(isouth,ino3)')
                idTbry(isouth,ino3)=varid
#ifdef DIAGNOSTICS_BIO
              CASE ('iDbio3(iflxnpp)')
                iDbio3(iflxnpp)=varid
              CASE ('iDbio3(iflxgra)')
                iDbio3(iflxgra)=varid
              CASE ('iDbio3(iflxagg)')
                iDbio3(iflxagg)=varid
              CASE ('iDbio3(iflxpmor)')
                iDbio3(iflxpmor)=varid
              CASE ('iDbio3(iflxzmor)')
                iDbio3(iflxzmor)=varid
              CASE ('iDbio3(iflxsrem)')
                iDbio3(iflxsrem)=varid
              CASE ('iDbio3(iflxlrem)')
                iDbio3(iflxlrem)=varid
              CASE ('iDbio3(iflxnit)')
                iDbio3(iflxnit)=varid
              CASE ('iDbio3(ifratio)')
                iDbio3(ifratio)=varid
#endif