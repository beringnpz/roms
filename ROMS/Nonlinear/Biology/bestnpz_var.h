/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2020 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Assigns metadata indices for the BESTNPZ ecosystem                **                                                                                      
**  model variables that are used in input and output NetCDF files.   **                                                                                      
**  The metadata information is read from "varinfo.dat".              **                                                                                      
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/

              print *, "idTvar"
              CASE ('idTvar(iNO3)')
                idTvar(iNO3)=varid
              CASE ('idTvar(iNH4)')
                idTvar(iNH4)=varid
              CASE ('idTvar(iPhS)')
                idTvar(iPhS)=varid
              CASE ('idTvar(iPhL)')
                idTvar(iPhL)=varid
              CASE ('idTvar(iMZS)')
                idTvar(iMZS)=varid
              CASE ('idTvar(iMZL)')
                idTvar(iMZL)=varid
              CASE ('idTvar(iCop)')
                idTvar(iCop)=varid
              CASE ('idTvar(iNCaS)')
                idTvar(iNCaS)=varid
              CASE ('idTvar(iEupS)')
                idTvar(iEupS)=varid
              CASE ('idTvar(iNCaO)')
                idTvar(iNCaO)=varid
              CASE ('idTvar(iEupO)')
                idTvar(iEupO)=varid
              CASE ('idTvar(iDet)')
                idTvar(iDet)=varid
              CASE ('idTvar(iDetF)')
                idTvar(iDetF)=varid
              CASE ('idTvar(iJel)')
                idTvar(iJel)=varid
              CASE ('idTvar(iFe)')
                idTvar(iFe)=varid
              print *, "idTbry"
              CASE ('idTbry(ieast,iMZS)')
                idTbry(ieast,iMZS)=varid
              CASE ('idTbry(inorth,iMZS)')
                idTbry(inorth,iMZS)=varid
              CASE ('idTbry(isouth,iMZS)')
                idTbry(isouth,iMZS)=varid
              CASE ('idTbry(iwest,iMZS)')
                idTbry(iwest,iMZS)=varid
              CASE ('idTbry(ieast,iCop)')
                idTbry(ieast,iCop)=varid
              CASE ('idTbry(ieast,iDet)')
                idTbry(ieast,iDet)=varid
              CASE ('idTbry(ieast,iDetF)')
                idTbry(ieast,iDetF)=varid
              CASE ('idTbry(ieast,iEupO)')
                idTbry(ieast,iEupO)=varid
              CASE ('idTbry(ieast,iEupS)')
                idTbry(ieast,iEupS)=varid
              CASE ('idTbry(ieast,iJel)')
                idTbry(ieast,iJel)=varid
              CASE ('idTbry(ieast,iMZL)')
                idTbry(ieast,iMZL)=varid
              CASE ('idTbry(ieast,iNCaO)')
                idTbry(ieast,iNCaO)=varid
              CASE ('idTbry(ieast,iNCaS)')
                idTbry(ieast,iNCaS)=varid
              CASE ('idTbry(ieast,iPhL)')
                idTbry(ieast,iPhL)=varid
              CASE ('idTbry(ieast,iPhS)')
                idTbry(ieast,iPhS)=varid
              CASE ('idTbry(inorth,iCop)')
                idTbry(inorth,iCop)=varid
              CASE ('idTbry(inorth,iDet)')
                idTbry(inorth,iDet)=varid
              CASE ('idTbry(inorth,iDetF)')
                idTbry(inorth,iDetF)=varid
              CASE ('idTbry(inorth,iEupO)')
                idTbry(inorth,iEupO)=varid
              CASE ('idTbry(inorth,iEupS)')
                idTbry(inorth,iEupS)=varid
              CASE ('idTbry(inorth,iJel)')
                idTbry(inorth,iJel)=varid
              CASE ('idTbry(inorth,iMZL)')
                idTbry(inorth,iMZL)=varid
              CASE ('idTbry(inorth,iNCaO)')
                idTbry(inorth,iNCaO)=varid
              CASE ('idTbry(inorth,iNCaS)')
                idTbry(inorth,iNCaS)=varid
              CASE ('idTbry(inorth,iPhL)')
                idTbry(inorth,iPhL)=varid
              CASE ('idTbry(inorth,iPhS)')
                idTbry(inorth,iPhS)=varid
              CASE ('idTbry(isouth,iCop)')
                idTbry(isouth,iCop)=varid
              CASE ('idTbry(isouth,iDet)')
                idTbry(isouth,iDet)=varid
              CASE ('idTbry(isouth,iDetF)')
                idTbry(isouth,iDetF)=varid
              CASE ('idTbry(isouth,iEupO)')
                idTbry(isouth,iEupO)=varid
              CASE ('idTbry(isouth,iEupS)')
                idTbry(isouth,iEupS)=varid
              CASE ('idTbry(isouth,iJel)')
                idTbry(isouth,iJel)=varid
              CASE ('idTbry(isouth,iMZL)')
                idTbry(isouth,iMZL)=varid
              CASE ('idTbry(isouth,iNCaO)')
                idTbry(isouth,iNCaO)=varid
              CASE ('idTbry(isouth,iNCaS)')
                idTbry(isouth,iNCaS)=varid
              CASE ('idTbry(isouth,iPhL)')
                idTbry(isouth,iPhL)=varid
              CASE ('idTbry(isouth,iPhS)')
                idTbry(isouth,iPhS)=varid
              CASE ('idTbry(iwest,iCop)')
                idTbry(iwest,iCop)=varid
              CASE ('idTbry(iwest,iDet)')
                idTbry(iwest,iDet)=varid
              CASE ('idTbry(iwest,iDetF)')
                idTbry(iwest,iDetF)=varid
              CASE ('idTbry(iwest,iEupO)')
                idTbry(iwest,iEupO)=varid
              CASE ('idTbry(iwest,iEupS)')
                idTbry(iwest,iEupS)=varid
              CASE ('idTbry(iwest,iJel)')
                idTbry(iwest,iJel)=varid
              CASE ('idTbry(iwest,iMZL)')
                idTbry(iwest,iMZL)=varid
              CASE ('idTbry(iwest,iNCaO)')
                idTbry(iwest,iNCaO)=varid
              CASE ('idTbry(iwest,iNCaS)')
                idTbry(iwest,iNCaS)=varid
              CASE ('idTbry(iwest,iPhL)')
                idTbry(iwest,iPhL)=varid
              CASE ('idTbry(iwest,iPhS)')
                idTbry(iwest,iPhS)=varid
              CASE ('idTbry(ieast,iFe)')
                idTbry(ieast,iFe)=varid
              CASE ('idTbry(ieast,iNH4)')
                idTbry(ieast,iNH4)=varid
              CASE ('idTbry(ieast,iNO3)')
                idTbry(ieast,iNO3)=varid
              CASE ('idTbry(inorth,iFe)')
                idTbry(inorth,iFe)=varid
              CASE ('idTbry(inorth,iNH4)')
                idTbry(inorth,iNH4)=varid
              CASE ('idTbry(inorth,iNO3)')
                idTbry(inorth,iNO3)=varid
              CASE ('idTbry(isouth,iFe)')
                idTbry(isouth,iFe)=varid
              CASE ('idTbry(isouth,iNH4)')
                idTbry(isouth,iNH4)=varid
              CASE ('idTbry(isouth,iNO3)')
                idTbry(isouth,iNO3)=varid
              CASE ('idTbry(iwest,iFe)')
                idTbry(iwest,iFe)=varid
              CASE ('idTbry(iwest,iNH4)')
                idTbry(iwest,iNH4)=varid
              CASE ('idTbry(iwest,iNO3)')
                idTbry(iwest,iNO3)=varid
#ifdef CARBON
              print *, "carbon"
              CASE ('idpCO2air')
                idpCO2air=varid
              CASE ('idTvar(iTAlk)')
                idTvar(iTAlk)=varid
              CASE ('idTvar(iTIC_)')
                idTvar(iTIC_)=varid
              CASE ('idTbry(ieast,iTAlk)')
                idTbry(ieast,iTAlk)=varid
              CASE ('idTbry(ieast,iTIC_)')
                idTbry(ieast,iTIC_)=varid
              CASE ('idTbry(inorth,iTAlk)')
                idTbry(inorth,iTAlk)=varid
              CASE ('idTbry(inorth,iTIC_)')
                idTbry(inorth,iTIC_)=varid
              CASE ('idTbry(isouth,iTAlk)')
                idTbry(isouth,iTAlk)=varid
              CASE ('idTbry(isouth,iTIC_)')
                idTbry(isouth,iTIC_)=varid
              CASE ('idTbry(iwest,iTAlk)')
                idTbry(iwest,iTAlk)=varid
              CASE ('idTbry(iwest,iTIC_)')
                idTbry(iwest,iTIC_)=varid
#endif
#ifdef CARBON_FLUX
              print *, "carbon flux"
              CASE ('idTAFlux')
                idTAFlux=varid
              CASE ('idDICFlux')
                idDICFlux=varid
#endif
#ifdef OXYGEN
              print *, "oxygen"
              CASE ('idTvar(iOxyg)')
                idTvar(iOxyg)=varid
              CASE ('idTbry(ieast,iOxyg)')
                idTbry(ieast,iOxyg)=varid
              CASE ('idTbry(inorth,iOxyg)')
                idTbry(inorth,iOxyg)=varid
              CASE ('idTbry(isouth,iOxyg)')
                idTbry(isouth,iOxyg)=varid
              CASE ('idTbry(iwest,iOxyg)')
                idTbry(iwest,iOxyg)=varid
#endif
#ifdef ICE_BIO
              print *, "ice bio"
              CASE ('idIceTvar(iIcPhL)')
!                 idIcePhL=varid
                idIceTvar(iIcPhL)=varid
              CASE ('idIceTvar(iIcNO3)')
!                 idIceNO3=varid
                idIceTvar(iIcNO3)=varid
              CASE ('idIceTvar(icINH4)')
!                 idIceNH4=varid
                idIceTvar(iIcNH4)=varid
!               CASE ('iIceLog')
!                 idIceLog=varid
#endif
#ifdef BENTHIC
              print *, "benthic"
              CASE ('idBeTvar(iBen)')
                idBeTvar(iBen)=varid
              CASE ('idBeTvar(iDetBen)')
                idBeTvar(iDetBen)=varid
#endif
#ifdef DIAGNOSTICS_BIO
              print *, "diagnostics"
              CASE ('iDbio3(iilims)')
                iDbio3(iilims)=varid
              CASE ('iDbio3(iiliml)')
                iDbio3(iiliml)=varid
              CASE ('iDbio3(inolims)')
                iDbio3(inolims)=varid
              CASE ('iDbio3(inoliml)')
                iDbio3(inoliml)=varid
              CASE ('iDbio3(inhlims)')
                iDbio3(inhlims)=varid
              CASE ('iDbio3(inhliml)')
                iDbio3(inhliml)=varid
              CASE ('iDbio3(ifelims)')
                iDbio3(ifelims)=varid
              CASE ('iDbio3(ifeliml)')
                iDbio3(ifeliml)=varid
              CASE ('iDbio3(iflx_Gpp_NO3_PhS)')
                iDbio3(iflx_Gpp_NO3_PhS)=varid
              CASE ('iDbio3(iflx_Gpp_NO3_PhL)')
                iDbio3(iflx_Gpp_NO3_PhL)=varid
              CASE ('iDbio3(iflx_Gpp_NH4_PhS)')
                iDbio3(iflx_Gpp_NH4_PhS)=varid
              CASE ('iDbio3(iflx_Gpp_NH4_PhL)')
                iDbio3(iflx_Gpp_NH4_PhL)=varid
              CASE ('iDbio3(iflx_Gra_PhS_MZL)')
                iDbio3(iflx_Gra_PhS_MZL)=varid
              CASE ('iDbio3(iflx_Gra_PhL_MZL)')
                iDbio3(iflx_Gra_PhL_MZL)=varid
              CASE ('iDbio3(iflx_Ege_MZL_Det)')
                iDbio3(iflx_Ege_MZL_Det)=varid
              CASE ('iDbio3(iflx_Gra_PhS_Cop)')
                iDbio3(iflx_Gra_PhS_Cop)=varid
              CASE ('iDbio3(iflx_Gra_PhL_Cop)')
                iDbio3(iflx_Gra_PhL_Cop)=varid
              CASE ('iDbio3(iflx_Gra_MZL_Cop)')
                iDbio3(iflx_Gra_MZL_Cop)=varid
              CASE ('iDbio3(iflx_Gra_IPhL_Cop)')
                iDbio3(iflx_Gra_IPhL_Cop)=varid
              CASE ('iDbio3(iflx_Ege_Cop_DetF)')
                iDbio3(iflx_Ege_Cop_DetF)=varid
              CASE ('iDbio3(iflx_Gra_PhS_NCaS)')
                iDbio3(iflx_Gra_PhS_NCaS)=varid
              CASE ('iDbio3(iflx_Gra_PhL_NCaS)')
                iDbio3(iflx_Gra_PhL_NCaS)=varid
              CASE ('iDbio3(iflx_Gra_MZL_NCaS)')
                iDbio3(iflx_Gra_MZL_NCaS)=varid
              CASE ('iDbio3(iflx_Gra_IPhL_NCaS)')
                iDbio3(iflx_Gra_IPhL_NCaS)=varid
              CASE ('iDbio3(iflx_Ege_NCaS_DetF)')
                iDbio3(iflx_Ege_NCaS_DetF)=varid
              CASE ('iDbio3(iflx_Gra_PhS_NCaO)')
                iDbio3(iflx_Gra_PhS_NCaO)=varid
              CASE ('iDbio3(iflx_Gra_PhL_NCaO)')
                iDbio3(iflx_Gra_PhL_NCaO)=varid
              CASE ('iDbio3(iflx_Gra_MZL_NCaO)')
                iDbio3(iflx_Gra_MZL_NCaO)=varid
              CASE ('iDbio3(iflx_Gra_IPhL_NCaO)')
                iDbio3(iflx_Gra_IPhL_NCaO)=varid
              CASE ('iDbio3(iflx_Ege_NCaO_DetF)')
                iDbio3(iflx_Ege_NCaO_DetF)=varid
              CASE ('iDbio3(iflx_Gra_PhS_EupS)')
                iDbio3(iflx_Gra_PhS_EupS)=varid
              CASE ('iDbio3(iflx_Gra_PhL_EupS)')
                iDbio3(iflx_Gra_PhL_EupS)=varid
              CASE ('iDbio3(iflx_Gra_MZL_EupS)')
                iDbio3(iflx_Gra_MZL_EupS)=varid
              CASE ('iDbio3(iflx_Gra_Cop_EupS)')
                iDbio3(iflx_Gra_Cop_EupS)=varid
              CASE ('iDbio3(iflx_Gra_IPhL_EupS)')
                iDbio3(iflx_Gra_IPhL_EupS)=varid
              CASE ('iDbio3(iflx_Gra_Det_EupS)')
                iDbio3(iflx_Gra_Det_EupS)=varid
              CASE ('iDbio3(iflx_Gra_DetF_EupS)')
                iDbio3(iflx_Gra_DetF_EupS)=varid
              CASE ('iDbio3(iflx_Ege_EupS_DetF)')
                iDbio3(iflx_Ege_EupS_DetF)=varid
              CASE ('iDbio3(iflx_Gra_PhS_EupO)')
                iDbio3(iflx_Gra_PhS_EupO)=varid
              CASE ('iDbio3(iflx_Gra_PhL_EupO)')
                iDbio3(iflx_Gra_PhL_EupO)=varid
              CASE ('iDbio3(iflx_Gra_MZL_EupO)')
                iDbio3(iflx_Gra_MZL_EupO)=varid
              CASE ('iDbio3(iflx_Gra_Cop_EupO)')
                iDbio3(iflx_Gra_Cop_EupO)=varid
              CASE ('iDbio3(iflx_Gra_IPhL_EupO)')
                iDbio3(iflx_Gra_IPhL_EupO)=varid
              CASE ('iDbio3(iflx_Gra_Det_EupO)')
                iDbio3(iflx_Gra_Det_EupO)=varid
              CASE ('iDbio3(iflx_Gra_DetF_EupO)')
                iDbio3(iflx_Gra_DetF_EupO)=varid
              CASE ('iDbio3(iflx_Ege_EupO_DetF)')
                iDbio3(iflx_Ege_EupO_DetF)=varid
              CASE ('iDbio3(iflx_Gra_Cop_Jel)')
                iDbio3(iflx_Gra_Cop_Jel)=varid
              CASE ('iDbio3(iflx_Gra_EupS_Jel)')
                iDbio3(iflx_Gra_EupS_Jel)=varid
              CASE ('iDbio3(iflx_Gra_EupO_Jel)')
                iDbio3(iflx_Gra_EupO_Jel)=varid
              CASE ('iDbio3(iflx_Gra_NCaS_Jel)')
                iDbio3(iflx_Gra_NCaS_Jel)=varid
              CASE ('iDbio3(iflx_Gra_NCaO_Jel)')
                iDbio3(iflx_Gra_NCaO_Jel)=varid
              CASE ('iDbio3(iflx_Ege_Jel_DetF)')
                iDbio3(iflx_Ege_Jel_DetF)=varid
              CASE ('iDbio3(iflx_Mor_PhS_Det)')
                iDbio3(iflx_Mor_PhS_Det)=varid
              CASE ('iDbio3(iflx_Mor_PhL_Det)')
                iDbio3(iflx_Mor_PhL_Det)=varid
              CASE ('iDbio3(iflx_Mor_MZL_Det)')
                iDbio3(iflx_Mor_MZL_Det)=varid
              CASE ('iDbio3(iflx_Mor_Cop_DetF)')
                iDbio3(iflx_Mor_Cop_DetF)=varid
              CASE ('iDbio3(iflx_Mor_NCaS_DetF)')
                iDbio3(iflx_Mor_NCaS_DetF)=varid
              CASE ('iDbio3(iflx_Mor_EupS_DetF)')
                iDbio3(iflx_Mor_EupS_DetF)=varid
              CASE ('iDbio3(iflx_Mor_NCaO_DetF)')
                iDbio3(iflx_Mor_NCaO_DetF)=varid
              CASE ('iDbio3(iflx_Mor_EupO_DetF)')
                iDbio3(iflx_Mor_EupO_DetF)=varid
              CASE ('iDbio3(iflx_Mor_Jel_DetF)')
                iDbio3(iflx_Mor_Jel_DetF)=varid
              CASE ('iDbio3(iflx_Res_PhS_NH4)')
                iDbio3(iflx_Res_PhS_NH4)=varid
              CASE ('iDbio3(iflx_Res_PhL_NH4)')
                iDbio3(iflx_Res_PhL_NH4)=varid
              CASE ('iDbio3(iflx_Res_MZL_NH4)')
                iDbio3(iflx_Res_MZL_NH4)=varid
              CASE ('iDbio3(iflx_Res_Cop_NH4)')
                iDbio3(iflx_Res_Cop_NH4)=varid
              CASE ('iDbio3(iflx_Res_NCaS_NH4)')
                iDbio3(iflx_Res_NCaS_NH4)=varid
              CASE ('iDbio3(iflx_Res_NCaO_NH4)')
                iDbio3(iflx_Res_NCaO_NH4)=varid
              CASE ('iDbio3(iflx_Res_EupS_NH4)')
                iDbio3(iflx_Res_EupS_NH4)=varid
              CASE ('iDbio3(iflx_Res_EupO_NH4)')
                iDbio3(iflx_Res_EupO_NH4)=varid
              CASE ('iDbio3(iflx_Res_Jel_NH4)')
                iDbio3(iflx_Res_Jel_NH4)=varid
              CASE ('iDbio3(iflx_Rem_Det_NH4)')
                iDbio3(iflx_Rem_Det_NH4)=varid
              CASE ('iDbio3(iflx_Rem_DetF_NH4)')
                iDbio3(iflx_Rem_DetF_NH4)=varid
              CASE ('iDbio3(iflx_Nit_NH4_NO3)')
                iDbio3(iflx_Nit_NH4_NO3)=varid
              CASE ('iDbio3(iflx_Gra_Det_Ben)')
                iDbio3(iflx_Gra_Det_Ben)=varid
              CASE ('iDbio3(iflx_Gra_DetF_Ben)')
                iDbio3(iflx_Gra_DetF_Ben)=varid
              CASE ('iDbio3(iflx_Gra_PhS_Ben)')
                iDbio3(iflx_Gra_PhS_Ben)=varid
              CASE ('iDbio3(iflx_Gra_PhL_Ben)')
                iDbio3(iflx_Gra_PhL_Ben)=varid
              CASE ('iDbio3(iflx_Gra_DetB_Ben)')
                iDbio3(iflx_Gra_DetB_Ben)=varid
              CASE ('iDbio3(iflx_Exc_Ben_NH4)')
                iDbio3(iflx_Exc_Ben_NH4)=varid
              CASE ('iDbio3(iflx_Exc_Ben_DetB)')
                iDbio3(iflx_Exc_Ben_DetB)=varid
              CASE ('iDbio3(iflx_Res_Ben_NH4)')
                iDbio3(iflx_Res_Ben_NH4)=varid
              CASE ('iDbio3(iflx_Mor_Ben_DetB)')
                iDbio3(iflx_Mor_Ben_DetB)=varid
              CASE ('iDbio3(iflx_Rem_DetB_NH4)')
                iDbio3(iflx_Rem_DetB_NH4)=varid
              CASE ('iDbio3(iflx_Gpp_INO3_IPhL)')
                iDbio3(iflx_Gpp_INO3_IPhL)=varid
              CASE ('iDbio3(iflx_Gpp_INH4_IPhL)')
                iDbio3(iflx_Gpp_INH4_IPhL)=varid
              CASE ('iDbio3(iflx_Res_IPhL_INH4)')
                iDbio3(iflx_Res_IPhL_INH4)=varid
              CASE ('iDbio3(iflx_Mor_IPhL_INH4)')
                iDbio3(iflx_Mor_IPhL_INH4)=varid
              CASE ('iDbio3(iflx_Nit_INH4_INO3)')
                iDbio3(iflx_Nit_INH4_INO3)=varid
              CASE ('iDbio3(iflx_Twi_IPhL_PhL)')
                iDbio3(iflx_Twi_IPhL_PhL)=varid
              CASE ('iDbio3(iflx_Twi_INO3_NO3)')
                iDbio3(iflx_Twi_INO3_NO3)=varid
              CASE ('iDbio3(iflx_Twi_INH4_NH4)')
                iDbio3(iflx_Twi_INH4_NH4)=varid
              CASE ('iDbio3(iflx_Ver_PhS_DetB)')
                iDbio3(iflx_Ver_PhS_DetB)=varid
              CASE ('iDbio3(iflx_Ver_PhS_Out)')
                iDbio3(iflx_Ver_PhS_Out)=varid
              CASE ('iDbio3(iflx_Ver_PhL_DetB)')
                iDbio3(iflx_Ver_PhL_DetB)=varid
              CASE ('iDbio3(iflx_Ver_PhL_Out)')
                iDbio3(iflx_Ver_PhL_Out)=varid
              CASE ('iDbio3(iflx_Ver_Det_DetB)')
                iDbio3(iflx_Ver_Det_DetB)=varid
              CASE ('iDbio3(iflx_Ver_Det_Out)')
                iDbio3(iflx_Ver_Det_Out)=varid
              CASE ('iDbio3(iflx_Ver_DetF_DetB)')
                iDbio3(iflx_Ver_DetF_DetB)=varid
              CASE ('iDbio3(iflx_Ver_DetF_Out)')
                iDbio3(iflx_Ver_DetF_Out)=varid
              CASE ('iDbio3(iflx_Ver_NCaO_DetB)')
                iDbio3(iflx_Ver_NCaO_DetB)=varid
              CASE ('iDbio3(iflx_Ver_NCaS_DetF)')
                iDbio3(iflx_Ver_NCaS_DetF)=varid
              CASE ('iDbio3(iflx_Ver_NCaS_DetB)')
                iDbio3(iflx_Ver_NCaS_DetB)=varid
              CASE ('iDbio3(iflx_Frz_PhL_IPhL)')
                iDbio3(iflx_Frz_PhL_IPhL)=varid
              CASE ('iDbio3(iflx_Frz_NO3_INO3)')
                iDbio3(iflx_Frz_NO3_INO3)=varid
              CASE ('iDbio3(iflx_Frz_NH4_INH4)')
                iDbio3(iflx_Frz_NH4_INH4)=varid
              CASE ('iDbio3(iprod_PhS)')
                iDbio3(iprod_PhS)=varid
              CASE ('iDbio3(iprod_PhL)')
                iDbio3(iprod_PhL)=varid
              CASE ('iDbio3(iprod_MZL)')
                iDbio3(iprod_MZL)=varid
              CASE ('iDbio3(iprod_Cop)')
                iDbio3(iprod_Cop)=varid
              CASE ('iDbio3(iprod_NCaS)')
                iDbio3(iprod_NCaS)=varid
              CASE ('iDbio3(iprod_EupS)')
                iDbio3(iprod_EupS)=varid
              CASE ('iDbio3(iprod_NCaO)')
                iDbio3(iprod_NCaO)=varid
              CASE ('iDbio3(iprod_EupO)')
                iDbio3(iprod_EupO)=varid
              CASE ('iDbio3(iprod_Jel)')
                iDbio3(iprod_Jel)=varid
              CASE ('iDbio3(iprod_Ben)')
                iDbio3(iprod_Ben)=varid
              CASE ('iDbio3(iprod_IcePhL)')
                iDbio3(iprod_IcePhL)=varid
              CASE ('iDbio3(ibiomem_NO3)')
                iDbio3(ibiomem_NO3)=varid
              CASE ('iDbio3(ibiomem_NH4)')
                iDbio3(ibiomem_NH4)=varid
              CASE ('iDbio3(ibiomem_PhS)')
                iDbio3(ibiomem_PhS)=varid
              CASE ('iDbio3(ibiomem_PhL)')
                iDbio3(ibiomem_PhL)=varid
              CASE ('iDbio3(ibiomem_MZL)')
                iDbio3(ibiomem_MZL)=varid
              CASE ('iDbio3(ibiomem_Cop)')
                iDbio3(ibiomem_Cop)=varid
              CASE ('iDbio3(ibiomem_NCaS)')
                iDbio3(ibiomem_NCaS)=varid
              CASE ('iDbio3(ibiomem_EupS)')
                iDbio3(ibiomem_EupS)=varid
              CASE ('iDbio3(ibiomem_NCaO)')
                iDbio3(ibiomem_NCaO)=varid
              CASE ('iDbio3(ibiomem_EupO)')
                iDbio3(ibiomem_EupO)=varid
              CASE ('iDbio3(ibiomem_Det)')
                iDbio3(ibiomem_Det)=varid
              CASE ('iDbio3(ibiomem_DetF)')
                iDbio3(ibiomem_DetF)=varid
              CASE ('iDbio3(ibiomem_Jel)')
                iDbio3(ibiomem_Jel)=varid
              CASE ('iDbio3(ibiomem_Fe)')
                iDbio3(ibiomem_Fe)=varid
              CASE ('iDbio3(iflx_Adv_NO3)')
                iDbio3(iflx_Adv_NO3)=varid
              CASE ('iDbio3(iflx_Adv_NH4)')
                iDbio3(iflx_Adv_NH4)=varid
              CASE ('iDbio3(iflx_Adv_PhS)')
                iDbio3(iflx_Adv_PhS)=varid
              CASE ('iDbio3(iflx_Adv_PhL)')
                iDbio3(iflx_Adv_PhL)=varid
              CASE ('iDbio3(iflx_Adv_MZL)')
                iDbio3(iflx_Adv_MZL)=varid
              CASE ('iDbio3(iflx_Adv_Cop)')
                iDbio3(iflx_Adv_Cop)=varid
              CASE ('iDbio3(iflx_Adv_NCaS)')
                iDbio3(iflx_Adv_NCaS)=varid
              CASE ('iDbio3(iflx_Adv_EupS)')
                iDbio3(iflx_Adv_EupS)=varid
              CASE ('iDbio3(iflx_Adv_NCaO)')
                iDbio3(iflx_Adv_NCaO)=varid
              CASE ('iDbio3(iflx_Adv_EupO)')
                iDbio3(iflx_Adv_EupO)=varid
              CASE ('iDbio3(iflx_Adv_Det)')
                iDbio3(iflx_Adv_Det)=varid
              CASE ('iDbio3(iflx_Adv_DetF)')
                iDbio3(iflx_Adv_DetF)=varid
              CASE ('iDbio3(iflx_Adv_Jel)')
                iDbio3(iflx_Adv_Jel)=varid
              CASE ('iDbio3(iflx_Adv_Fe)')
                iDbio3(iflx_Adv_Fe)=varid
              CASE ('iDbio3(ipar)')
                iDbio3(ipar)=varid
              CASE ('iDbio3(itotprod)')
                iDbio3(itotprod)=varid
              CASE ('iDbio3(itotresp)')
                iDbio3(itotresp)=varid
              CASE ('iDbio3(itotrem)')
                iDbio3(itotrem)=varid
              CASE ('iDbio3(iflx_Frz_Alk)')
                iDbio3(iflx_Frz_Alk)=varid
              CASE ('iDbio2(ico2flx)')
                iDbio2(ico2flx)=varid
              CASE ('iDbio2(ipco2)')
                iDbio2(ipco2)=varid
              CASE ('iDbio2(io2flx)')
                iDbio2(io2flx)=varid
              print *, "Done"
#endif