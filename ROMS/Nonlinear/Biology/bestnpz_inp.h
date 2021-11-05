      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in BESTNPZ ecosystem model input                 !
!  parameters. They are specified in input script bio_bestnpz.in".     !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
!
      USE inp_decode_mod
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, itracer, itrc, ng, nline, status
#ifdef DIAGNOSTICS_BIO
      logical, dimension(Ngrids) :: Lbio
#endif
      logical, dimension(NBT,Ngrids) :: Ltrc

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(dp), dimension(nRval) :: Rval

      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1                          ! first LBC tracer to process
      iTrcEnd=NBT                        ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
!
!-----------------------------------------------------------------------
!  Read in BESTNPZ biological model parameters.
!-----------------------------------------------------------------------
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lbiology')
              Npts=load_l(Nval, Cval, Ngrids, Lbiology)
            CASE ('BioIter')
              Npts=load_i(Nval, Rval, Ngrids, BioIter)

            ! Light

            CASE ('PARfrac')
              Npts=load_r(Nval, Rval, Ngrids, PARfrac)
            CASE ('k_ext')
              Npts=load_r(Nval, Rval, Ngrids, k_ext)
            CASE ('k_chlA')
              Npts=load_r(Nval, Rval, Ngrids, k_chlA)
            CASE ('k_chlB')
              Npts=load_r(Nval, Rval, Ngrids, k_chlB)
            CASE ('k_chlC')
              Npts=load_r(Nval, Rval, Ngrids, k_chlC)
            CASE ('k_sed1')
              Npts=load_r(Nval, Rval, Ngrids, k_sed1)
            CASE ('k_sed2')
              Npts=load_r(Nval, Rval, Ngrids, k_sed1)

            ! Conversion factors

            CASE ('xi')
              Npts=load_r(Nval, Rval, Ngrids, xi)
            CASE ('ccr')
              Npts=load_r(Nval, Rval, Ngrids, ccr)
            CASE ('ccrPhL')
              Npts=load_r(Nval, Rval, Ngrids, ccrPhL)
            CASE ('FeC')
              Npts=load_r(Nval, Rval, Ngrids, FeC)

            ! Phytoplankton growth parameters
            CASE ('DiS')
              Npts=load_r(Nval, Rval, Ngrids, DiS)
            CASE ('DiL')
              Npts=load_r(Nval, Rval, Ngrids, DiL)
            CASE ('DpS')
              Npts=load_r(Nval, Rval, Ngrids, DpS)
            CASE ('DpL')
              Npts=load_r(Nval, Rval, Ngrids, DpL)
            CASE ('alphaPhS')
              Npts=load_r(Nval, Rval, Ngrids, alphaPhS)
            CASE ('alphaPhL')
              Npts=load_r(Nval, Rval, Ngrids, alphaPhL)
            CASE ('k1PhS')
              Npts=load_r(Nval, Rval, Ngrids, k1PhS)
            CASE ('k1PhL')
              Npts=load_r(Nval, Rval, Ngrids, k1PhL)
            CASE ('k2PhS')
              Npts=load_r(Nval, Rval, Ngrids, k2PhS)
            CASE ('k2PhL')
              Npts=load_r(Nval, Rval, Ngrids, k2PhL)
            CASE ('FeCritPS')
              Npts=load_r(Nval, Rval, Ngrids, FeCritPS)
            CASE ('FeCritPL')
              Npts=load_r(Nval, Rval, Ngrids, FeCritPL)
            CASE ('kfePhS')
              Npts=load_r(Nval, Rval, Ngrids, kfePhS)
            CASE ('kfePhL')
              Npts=load_r(Nval, Rval, Ngrids, kfePhL)

            ! Feeding preferences

            CASE ('fpPhSMZL')
              Npts=load_r(Nval, Rval, Ngrids, fpPhSMZL)
            CASE ('fpPhLMZL')
              Npts=load_r(Nval, Rval, Ngrids, fpPhLMZL)
            CASE ('fpPhSCop')
              Npts=load_r(Nval, Rval, Ngrids, fpPhSCop)
            CASE ('fpPhLCop')
              Npts=load_r(Nval, Rval, Ngrids, fpPhLCop)
            CASE ('fpMZLCop')
              Npts=load_r(Nval, Rval, Ngrids, fpMZLCop)
            CASE ('fpPhSNCa')
              Npts=load_r(Nval, Rval, Ngrids, fpPhSNCa)
            CASE ('fpPhLNCa')
              Npts=load_r(Nval, Rval, Ngrids, fpPhLNCa)
            CASE ('fpMZLNCa')
              Npts=load_r(Nval, Rval, Ngrids, fpMZLNCa)
            CASE ('fpPhSEup')
              Npts=load_r(Nval, Rval, Ngrids, fpPhSEup)
            CASE ('fpPhLEup')
              Npts=load_r(Nval, Rval, Ngrids, fpPhLEup)
            CASE ('fpMZLEup')
              Npts=load_r(Nval, Rval, Ngrids, fpMZLEup)
            CASE ('fpCopEup')
              Npts=load_r(Nval, Rval, Ngrids, fpCopEup)
            CASE ('fpDetEup')
              Npts=load_r(Nval, Rval, Ngrids, fpDetEup)
            CASE ('fpDetEupO')
              Npts=load_r(Nval, Rval, Ngrids, fpDetEupO)
            CASE ('fpCopJel')
              Npts=load_r(Nval, Rval, Ngrids, fpCopJel)
            CASE ('fpNCaJel')
              Npts=load_r(Nval, Rval, Ngrids, fpNCaJel)
            CASE ('fpEupJel')
              Npts=load_r(Nval, Rval, Ngrids, fpEupJel)

            ! Zooplankton growth and feeding

            CASE ('eMZL')
              Npts=load_r(Nval, Rval, Ngrids, eMZL)
            CASE ('eCop')
              Npts=load_r(Nval, Rval, Ngrids, eCop)
            CASE ('eNCa')
              Npts=load_r(Nval, Rval, Ngrids, eNCa)
            CASE ('eEup')
              Npts=load_r(Nval, Rval, Ngrids, eEup)
            CASE ('eJel')
              Npts=load_r(Nval, Rval, Ngrids, eJel)
            CASE ('Q10MZL')
              Npts=load_r(Nval, Rval, Ngrids, Q10MZL)
            CASE ('Q10Cop')
              Npts=load_r(Nval, Rval, Ngrids, Q10Cop)
            CASE ('Q10NCa')
              Npts=load_r(Nval, Rval, Ngrids, Q10NCa)
            CASE ('Q10Eup')
              Npts=load_r(Nval, Rval, Ngrids, Q10Eup)
            CASE ('Q10Jele')
              Npts=load_r(Nval, Rval, Ngrids, Q10Jele)
            CASE ('Q10MZLT')
              Npts=load_r(Nval, Rval, Ngrids, Q10MZLT)
            CASE ('Q10CopT')
              Npts=load_r(Nval, Rval, Ngrids, Q10CopT)
            CASE ('Q10NCaT')
              Npts=load_r(Nval, Rval, Ngrids, Q10NCaT)
            CASE ('Q10EupT')
              Npts=load_r(Nval, Rval, Ngrids, Q10EupT)
            CASE ('Q10JelTe')
              Npts=load_r(Nval, Rval, Ngrids, Q10JelTe)
            CASE ('fMZL')
              Npts=load_r(Nval, Rval, Ngrids, fMZL)
            CASE ('fCop')
              Npts=load_r(Nval, Rval, Ngrids, fCop)
            CASE ('fNCa')
              Npts=load_r(Nval, Rval, Ngrids, fNCa)
            CASE ('fEup')
              Npts=load_r(Nval, Rval, Ngrids, fEup)
            CASE ('fJel')
              Npts=load_r(Nval, Rval, Ngrids, fJel)
            CASE ('gammaMZL')
              Npts=load_r(Nval, Rval, Ngrids, gammaMZL)
            CASE ('gammaCop')
              Npts=load_r(Nval, Rval, Ngrids, gammaCop)
            CASE ('gammaNCa')
              Npts=load_r(Nval, Rval, Ngrids, gammaNCa)
            CASE ('gammaEup')
              Npts=load_r(Nval, Rval, Ngrids, gammaEup)
            CASE ('gammaJel')
              Npts=load_r(Nval, Rval, Ngrids, gammaJel)

            ! Linear mortality

            CASE ('mPhS')
              Npts=load_r(Nval, Rval, Ngrids, mPhS)
            CASE ('mPhL')
              Npts=load_r(Nval, Rval, Ngrids, mPhL)
            CASE ('mMZL')
              Npts=load_r(Nval, Rval, Ngrids, mMZL)

            ! Predation closure

            CASE ('mpredMZL')
              Npts=load_r(Nval, Rval, Ngrids, mpredMZL)
            CASE ('mpredCop')
              Npts=load_r(Nval, Rval, Ngrids, mpredCop)
            CASE ('mpredNCa')
              Npts=load_r(Nval, Rval, Ngrids, mpredNCa)
            CASE ('mpredEup')
              Npts=load_r(Nval, Rval, Ngrids, mpredEup)
            CASE ('mpredJel')
              Npts=load_r(Nval, Rval, Ngrids, mpredJel)

            ! Sinking

            CASE ('wPhS')
              Npts=load_r(Nval, Rval, Ngrids, wPhS)
            CASE ('wPhL')
              Npts=load_r(Nval, Rval, Ngrids, wPhL)
            CASE ('wDet')
              Npts=load_r(Nval, Rval, Ngrids, wDet)
            CASE ('wDetF')
              Npts=load_r(Nval, Rval, Ngrids, wDetF)

            ! Respriation

            CASE ('respPhS')
              Npts=load_r(Nval, Rval, Ngrids, respPhS)
            CASE ('respPhL')
              Npts=load_r(Nval, Rval, Ngrids, respPhL)
            CASE ('respMZL')
              Npts=load_r(Nval, Rval, Ngrids, respMZL)
            CASE ('respCop')
              Npts=load_r(Nval, Rval, Ngrids, respCop)
            CASE ('respNCa')
              Npts=load_r(Nval, Rval, Ngrids, respNCa)
            CASE ('respEup')
              Npts=load_r(Nval, Rval, Ngrids, respEup)
            CASE ('respJel')
              Npts=load_r(Nval, Rval, Ngrids, respJel)
            CASE ('Q10Jelr')
              Npts=load_r(Nval, Rval, Ngrids, Q10Jelr)
            CASE ('Q10JelTr')
              Npts=load_r(Nval, Rval, Ngrids, Q10JelTr)
            CASE ('KtBm_PhS')
              Npts=load_r(Nval, Rval, Ngrids, KtBm_PhS)
            CASE ('KtBm_PhL')
              Npts=load_r(Nval, Rval, Ngrids, KtBm_PhL)
            CASE ('KtBm_MZL')
              Npts=load_r(Nval, Rval, Ngrids, KtBm_MZL)
            CASE ('ktbmC')
              Npts=load_r(Nval, Rval, Ngrids, ktbmC)
            CASE ('ktbmN')
              Npts=load_r(Nval, Rval, Ngrids, ktbmN)
            CASE ('ktbmE')
              Npts=load_r(Nval, Rval, Ngrids, ktbmE)
            CASE ('TmaxPhS')
              Npts=load_r(Nval, Rval, Ngrids, TmaxPhS)
            CASE ('TmaxPhL')
              Npts=load_r(Nval, Rval, Ngrids, TmaxPhL)
            CASE ('TmaxMZL')
              Npts=load_r(Nval, Rval, Ngrids, TmaxMZL)
            CASE ('TrefC')
              Npts=load_r(Nval, Rval, Ngrids, TrefC)
            CASE ('TrefN')
              Npts=load_r(Nval, Rval, Ngrids, TrefN)
            CASE ('TrefE')
              Npts=load_r(Nval, Rval, Ngrids, TrefE)

            ! Iron climatology

            CASE ('Feinlo')
              Npts=load_r(Nval, Rval, Ngrids, Feinlo)
            CASE ('Feinhi')
              Npts=load_r(Nval, Rval, Ngrids, Feinhi)
            CASE ('Feinh')
              Npts=load_r(Nval, Rval, Ngrids, Feinh)
            CASE ('Feofflo')
              Npts=load_r(Nval, Rval, Ngrids, Feofflo)
            CASE ('Feoffhi')
              Npts=load_r(Nval, Rval, Ngrids, Feoffhi)
            CASE ('Feoffh')
              Npts=load_r(Nval, Rval, Ngrids, Feoffh)

            ! Diapause

            CASE ('wNCrise')
              Npts=load_r(Nval, Rval, Ngrids, wNCrise)
            CASE ('wNCsink')
              Npts=load_r(Nval, Rval, Ngrids, wNCsink)
            CASE ('RiseStart')
              Npts=load_r(Nval, Rval, Ngrids, RiseStart)
            CASE ('RiseEnd')
              Npts=load_r(Nval, Rval, Ngrids, RiseEnd)
            CASE ('SinkStart')
              Npts=load_r(Nval, Rval, Ngrids, SinkStart)
            CASE ('SinkEnd')
              Npts=load_r(Nval, Rval, Ngrids, SinkEnd)
            CASE ('RiseStartCM')
              Npts=load_r(Nval, Rval, Ngrids, RiseStartCM)
            CASE ('RiseEndCM')
              Npts=load_r(Nval, Rval, Ngrids, RiseEndCM)
            CASE ('SinkStartCM')
              Npts=load_r(Nval, Rval, Ngrids, SinkStartCM)
            CASE ('SinkEndCM')
              Npts=load_r(Nval, Rval, Ngrids, SinkEndCM)

            ! Remineralization and nitrification

            CASE ('Pv0')
              Npts=load_r(Nval, Rval, Ngrids, Pv0)
            CASE ('PvT')
              Npts=load_r(Nval, Rval, Ngrids, PvT)
            CASE ('Nitr0')
              Npts=load_r(Nval, Rval, Ngrids, Nitr0)
            CASE ('ktntr')
              Npts=load_r(Nval, Rval, Ngrids, ktntr)
            CASE ('KNH4Nit')
              Npts=load_r(Nval, Rval, Ngrids, KNH4Nit)
            CASE ('ToptNtr')
              Npts=load_r(Nval, Rval, Ngrids, ToptNtr)

#ifdef BENTHIC
            ! Benthos

            CASE ('q10r')
              Npts=load_r(Nval, Rval, Ngrids, q10r)
            CASE ('Rup')
              Npts=load_r(Nval, Rval, Ngrids, Rup)
            CASE ('KupD')
              Npts=load_r(Nval, Rval, Ngrids, KupD)
            CASE ('KupP')
              Npts=load_r(Nval, Rval, Ngrids, KupP)
            CASE ('LupD')
              Npts=load_r(Nval, Rval, Ngrids, LupD)
            CASE ('LupP')
              Npts=load_r(Nval, Rval, Ngrids, LupP)
            CASE ('Qres')
              Npts=load_r(Nval, Rval, Ngrids, Qres)
            CASE ('Rres')
              Npts=load_r(Nval, Rval, Ngrids, Rres)
            CASE ('rmort')
              Npts=load_r(Nval, Rval, Ngrids, rmort)
            CASE ('eex')
              Npts=load_r(Nval, Rval, Ngrids, eex)
            CASE ('eexD')
              Npts=load_r(Nval, Rval, Ngrids, eexD)
            CASE ('prefD')
              Npts=load_r(Nval, Rval, Ngrids, prefD)
            CASE ('prefPL')
              Npts=load_r(Nval, Rval, Ngrids, prefPL)
            CASE ('prefPS')
              Npts=load_r(Nval, Rval, Ngrids, prefPS)
            CASE ('T0benr')
              Npts=load_r(Nval, Rval, Ngrids, T0benr)
            CASE ('BenPred')
              Npts=load_r(Nval, Rval, Ngrids, BenPred)
#endif
#ifdef ICE_BIO

            ! Ice biology

            CASE ('alphaIb')
              Npts=load_r(Nval, Rval, Ngrids, alphaIb)
            CASE ('betaI')
              Npts=load_r(Nval, Rval, Ngrids, betaI)
            CASE ('inhib')
              Npts=load_r(Nval, Rval, Ngrids, inhib)
            CASE ('ksnut1')
              Npts=load_r(Nval, Rval, Ngrids, ksnut1)
            CASE ('ksnut2')
              Npts=load_r(Nval, Rval, Ngrids, ksnut2)
            CASE ('mu0')
              Npts=load_r(Nval, Rval, Ngrids, mu0)
            CASE ('R0i')
              Npts=load_r(Nval, Rval, Ngrids, R0i)
            CASE ('rg0')
              Npts=load_r(Nval, Rval, Ngrids, rg0)
            CASE ('rg')
              Npts=load_r(Nval, Rval, Ngrids, rg)
            CASE ('annit')
              Npts=load_r(Nval, Rval, Ngrids, annit)
            CASE ('aidz')
              Npts=load_r(Nval, Rval, Ngrids, aidz)
            CASE ('tI0')
              Npts=load_r(Nval, Rval, Ngrids, tI0)
            CASE ('KI')
              Npts=load_r(Nval, Rval, Ngrids, KI)
#endif
            CASE ('TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu2(i,ng)=Rbio(itrc,ng)
                  tl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                  tl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('LtracerSponge')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSponge(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('AKT_BAK')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Akt_bak(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_AKT_fac')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNUDG')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Tnudg(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Vadvection)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.(NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Vadvection)
#endif
            CASE ('LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), LBC)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), ad_LBC)
#endif
            CASE ('LtracerSrc')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LtracerCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LnudgeTCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LnudgeTCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTvar(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTsur(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#ifdef BENTHIC
            CASE ('Hout(idBeTvar)')
              Npts=load_l(Nval, Cval,NBEN, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBEN
                  i=idBeTvar(idben(itrc))
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef ICEBIO
            CASE ('Hout(idIcePhL)')
              IF (idIcePhL.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIcePhL'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idIcePhL,:))
            CASE ('Hout(idIceNO3)')
              IF (idIceNO3.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIceNO3'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idIceNO3,:))
            CASE ('Hout(idIceNH4)')
              IF (idIcePhL.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIceNH4'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Hout(idIceNH4,:))
#endif
            CASE ('Qout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idsurT)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idsurT(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idsurT(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#ifdef BENTHIC
            CASE ('Aout(idBeTvar)')
              Npts=load_l(Nval, Cval,NBEN, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBEN
                  i=idBeTvar(idben(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef ICEBIO
            CASE ('Aout(idIcePhL)')
              IF (idIcePhL.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIcePhL'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Aout(idIcePhL,:))
            CASE ('Aout(idIceNO3)')
              IF (idIceNO3.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIceNO3'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Aout(idIceNO3,:))
            CASE ('Aout(idIceNH4)')
              IF (idIcePhL.eq.0) THEN
                IF (Master) WRITE (out,40) 'idIceNH4'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Aout(idIceNH4,:))
#endif
            CASE ('Aout(idTTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_TS
            CASE ('Dout(iTrate)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iThadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTyadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTvadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined TS_DIF2 || defined TS_DIF4
            CASE ('Dout(iThdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTydif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            CASE ('Dout(iTsdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  endif
# endif
            CASE ('Dout(iTvdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_BIO
            CASE ('Dout(iilims)')
              IF (iDbio3(iilims).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iilims)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iilims)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iiliml)')
              IF (iDbio3(iiliml).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iiliml)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iiliml)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inolims)')
              IF (iDbio3(inolims).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inolims)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inolims)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inoliml)')
              IF (iDbio3(inoliml).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inoliml)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inoliml)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inhlims)')
              IF (iDbio3(inhlims).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inhlims)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inhlims)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inhliml)')
              IF (iDbio3(inhliml).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inhliml)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inhliml)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifelims)')
              IF (iDbio3(ifelims).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifelims)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifelims)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifeliml)')
              IF (iDbio3(ifeliml).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifeliml)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifeliml)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_NO3_PhS)')
              IF (iDbio3(iflx_Gpp_NO3_PhS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_NO3_PhS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_NO3_PhS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_NO3_PhL)')
              IF (iDbio3(iflx_Gpp_NO3_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_NO3_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_NO3_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_NH4_PhS)')
              IF (iDbio3(iflx_Gpp_NH4_PhS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_NH4_PhS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_NH4_PhS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_NH4_PhL)')
              IF (iDbio3(iflx_Gpp_NH4_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_NH4_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_NH4_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_MZL)')
              IF (iDbio3(iflx_Gra_PhS_MZL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_MZL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_MZL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_MZL)')
              IF (iDbio3(iflx_Gra_PhL_MZL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_MZL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_MZL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_MZL_Det)')
              IF (iDbio3(iflx_Ege_MZL_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_MZL_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_MZL_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_Cop)')
              IF (iDbio3(iflx_Gra_PhS_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_Cop)')
              IF (iDbio3(iflx_Gra_PhL_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_MZL_Cop)')
              IF (iDbio3(iflx_Gra_MZL_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_MZL_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_MZL_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_IPhL_Cop)')
              IF (iDbio3(iflx_Gra_IPhL_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_IPhL_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_IPhL_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_Cop_DetF)')
              IF (iDbio3(iflx_Ege_Cop_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_Cop_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_Cop_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_NCaS)')
              IF (iDbio3(iflx_Gra_PhS_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_NCaS)')
              IF (iDbio3(iflx_Gra_PhL_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_MZL_NCaS)')
              IF (iDbio3(iflx_Gra_MZL_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_MZL_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_MZL_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_IPhL_NCaS)')
              IF (iDbio3(iflx_Gra_IPhL_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_IPhL_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_IPhL_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_NCaS_DetF)')
              IF (iDbio3(iflx_Ege_NCaS_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_NCaS_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_NCaS_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_NCaO)')
              IF (iDbio3(iflx_Gra_PhS_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_NCaO)')
              IF (iDbio3(iflx_Gra_PhL_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_MZL_NCaO)')
              IF (iDbio3(iflx_Gra_MZL_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_MZL_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_MZL_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_IPhL_NCaO)')
              IF (iDbio3(iflx_Gra_IPhL_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_IPhL_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_IPhL_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_NCaO_DetF)')
              IF (iDbio3(iflx_Ege_NCaO_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_NCaO_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_NCaO_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_EupS)')
              IF (iDbio3(iflx_Gra_PhS_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_EupS)')
              IF (iDbio3(iflx_Gra_PhL_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_MZL_EupS)')
              IF (iDbio3(iflx_Gra_MZL_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_MZL_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_MZL_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Cop_EupS)')
              IF (iDbio3(iflx_Gra_Cop_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Cop_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Cop_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_IPhL_EupS)')
              IF (iDbio3(iflx_Gra_IPhL_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_IPhL_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_IPhL_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Det_EupS)')
              IF (iDbio3(iflx_Gra_Det_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Det_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Det_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_DetF_EupS)')
              IF (iDbio3(iflx_Gra_DetF_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_DetF_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_DetF_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_EupS_DetF)')
              IF (iDbio3(iflx_Ege_EupS_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_EupS_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_EupS_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_EupO)')
              IF (iDbio3(iflx_Gra_PhS_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_EupO)')
              IF (iDbio3(iflx_Gra_PhL_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_MZL_EupO)')
              IF (iDbio3(iflx_Gra_MZL_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_MZL_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_MZL_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Cop_EupO)')
              IF (iDbio3(iflx_Gra_Cop_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Cop_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Cop_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_IPhL_EupO)')
              IF (iDbio3(iflx_Gra_IPhL_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_IPhL_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_IPhL_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Det_EupO)')
              IF (iDbio3(iflx_Gra_Det_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Det_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Det_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_DetF_EupO)')
              IF (iDbio3(iflx_Gra_DetF_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_DetF_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_DetF_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_EupO_DetF)')
              IF (iDbio3(iflx_Ege_EupO_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_EupO_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_EupO_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Cop_Jel)')
              IF (iDbio3(iflx_Gra_Cop_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Cop_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Cop_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_EupS_Jel)')
              IF (iDbio3(iflx_Gra_EupS_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_EupS_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_EupS_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_EupO_Jel)')
              IF (iDbio3(iflx_Gra_EupO_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_EupO_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_EupO_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_NCaS_Jel)')
              IF (iDbio3(iflx_Gra_NCaS_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_NCaS_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_NCaS_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_NCaO_Jel)')
              IF (iDbio3(iflx_Gra_NCaO_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_NCaO_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_NCaO_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ege_Jel_DetF)')
              IF (iDbio3(iflx_Ege_Jel_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ege_Jel_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ege_Jel_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_PhS_Det)')
              IF (iDbio3(iflx_Mor_PhS_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_PhS_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_PhS_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_PhL_Det)')
              IF (iDbio3(iflx_Mor_PhL_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_PhL_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_PhL_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_MZL_Det)')
              IF (iDbio3(iflx_Mor_MZL_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_MZL_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_MZL_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_Cop_DetF)')
              IF (iDbio3(iflx_Mor_Cop_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_Cop_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_Cop_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_NCaS_DetF)')
              IF (iDbio3(iflx_Mor_NCaS_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_NCaS_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_NCaS_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_EupS_DetF)')
              IF (iDbio3(iflx_Mor_EupS_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_EupS_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_EupS_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_NCaO_DetF)')
              IF (iDbio3(iflx_Mor_NCaO_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_NCaO_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_NCaO_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_EupO_DetF)')
              IF (iDbio3(iflx_Mor_EupO_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_EupO_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_EupO_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_Jel_DetF)')
              IF (iDbio3(iflx_Mor_Jel_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_Jel_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_Jel_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_PhS_NH4)')
              IF (iDbio3(iflx_Res_PhS_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_PhS_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_PhS_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_PhL_NH4)')
              IF (iDbio3(iflx_Res_PhL_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_PhL_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_PhL_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_MZL_NH4)')
              IF (iDbio3(iflx_Res_MZL_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_MZL_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_MZL_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_Cop_NH4)')
              IF (iDbio3(iflx_Res_Cop_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_Cop_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_Cop_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_NCaS_NH4)')
              IF (iDbio3(iflx_Res_NCaS_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_NCaS_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_NCaS_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_NCaO_NH4)')
              IF (iDbio3(iflx_Res_NCaO_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_NCaO_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_NCaO_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_EupS_NH4)')
              IF (iDbio3(iflx_Res_EupS_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_EupS_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_EupS_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_EupO_NH4)')
              IF (iDbio3(iflx_Res_EupO_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_EupO_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_EupO_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_Jel_NH4)')
              IF (iDbio3(iflx_Res_Jel_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_Jel_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_Jel_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Rem_Det_NH4)')
              IF (iDbio3(iflx_Rem_Det_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Rem_Det_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Rem_Det_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Rem_DetF_NH4)')
              IF (iDbio3(iflx_Rem_DetF_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Rem_DetF_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Rem_DetF_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Nit_NH4_NO3)')
              IF (iDbio3(iflx_Nit_NH4_NO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Nit_NH4_NO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Nit_NH4_NO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_Det_Ben)')
              IF (iDbio3(iflx_Gra_Det_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_Det_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_Det_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_DetF_Ben)')
              IF (iDbio3(iflx_Gra_DetF_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_DetF_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_DetF_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhS_Ben)')
              IF (iDbio3(iflx_Gra_PhS_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhS_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhS_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_PhL_Ben)')
              IF (iDbio3(iflx_Gra_PhL_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_PhL_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_PhL_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gra_DetB_Ben)')
              IF (iDbio3(iflx_Gra_DetB_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gra_DetB_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gra_DetB_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Exc_Ben_NH4)')
              IF (iDbio3(iflx_Exc_Ben_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Exc_Ben_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Exc_Ben_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Exc_Ben_DetB)')
              IF (iDbio3(iflx_Exc_Ben_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Exc_Ben_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Exc_Ben_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_Ben_NH4)')
              IF (iDbio3(iflx_Res_Ben_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_Ben_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_Ben_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_Ben_DetB)')
              IF (iDbio3(iflx_Mor_Ben_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_Ben_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_Ben_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Rem_DetB_NH4)')
              IF (iDbio3(iflx_Rem_DetB_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Rem_DetB_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Rem_DetB_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_INO3_IPhL)')
              IF (iDbio3(iflx_Gpp_INO3_IPhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_INO3_IPhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_INO3_IPhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Gpp_INH4_IPhL)')
              IF (iDbio3(iflx_Gpp_INH4_IPhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Gpp_INH4_IPhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Gpp_INH4_IPhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Res_IPhL_INH4)')
              IF (iDbio3(iflx_Res_IPhL_INH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Res_IPhL_INH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Res_IPhL_INH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Mor_IPhL_INH4)')
              IF (iDbio3(iflx_Mor_IPhL_INH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Mor_IPhL_INH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Mor_IPhL_INH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Nit_INH4_INO3)')
              IF (iDbio3(iflx_Nit_INH4_INO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Nit_INH4_INO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Nit_INH4_INO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Twi_IPhL_PhL)')
              IF (iDbio3(iflx_Twi_IPhL_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Twi_IPhL_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Twi_IPhL_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Twi_INO3_NO3)')
              IF (iDbio3(iflx_Twi_INO3_NO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Twi_INO3_NO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Twi_INO3_NO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Twi_INH4_NH4)')
              IF (iDbio3(iflx_Twi_INH4_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Twi_INH4_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Twi_INH4_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_PhS_DetB)')
              IF (iDbio3(iflx_Ver_PhS_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_PhS_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_PhS_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_PhS_Out)')
              IF (iDbio3(iflx_Ver_PhS_Out).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_PhS_Out)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_PhS_Out)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_PhL_DetB)')
              IF (iDbio3(iflx_Ver_PhL_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_PhL_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_PhL_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_PhL_Out)')
              IF (iDbio3(iflx_Ver_PhL_Out).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_PhL_Out)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_PhL_Out)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_Det_DetB)')
              IF (iDbio3(iflx_Ver_Det_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_Det_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_Det_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_Det_Out)')
              IF (iDbio3(iflx_Ver_Det_Out).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_Det_Out)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_Det_Out)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_DetF_DetB)')
              IF (iDbio3(iflx_Ver_DetF_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_DetF_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_DetF_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_DetF_Out)')
              IF (iDbio3(iflx_Ver_DetF_Out).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_DetF_Out)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_DetF_Out)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_NCaO_DetB)')
              IF (iDbio3(iflx_Ver_NCaO_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_NCaO_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_NCaO_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_NCaS_DetF)')
              IF (iDbio3(iflx_Ver_NCaS_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_NCaS_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_NCaS_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Ver_NCaS_DetB)')
              IF (iDbio3(iflx_Ver_NCaS_DetB).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Ver_NCaS_DetB)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Ver_NCaS_DetB)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Frz_PhL_IPhL)')
              IF (iDbio3(iflx_Frz_PhL_IPhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Frz_PhL_IPhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Frz_PhL_IPhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Frz_NO3_INO3)')
              IF (iDbio3(iflx_Frz_NO3_INO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Frz_NO3_INO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Frz_NO3_INO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Frz_NH4_INH4)')
              IF (iDbio3(iflx_Frz_NH4_INH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Frz_NH4_INH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Frz_NH4_INH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Frz_TIC)')
              IF (iDbio3(iflx_Frz_TIC).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Frz_TIC)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Frz_TIC)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Frz_Alk)')
              IF (iDbio3(iflx_Frz_Alk).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Frz_Alk)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Frz_Alk)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_NO3)')
              IF (iDbio3(iflx_Adv_NO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_NO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_NO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_NH4)')
              IF (iDbio3(iflx_Adv_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_PhS)')
              IF (iDbio3(iflx_Adv_PhS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_PhS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_PhS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_PhL)')
              IF (iDbio3(iflx_Adv_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_MZL)')
              IF (iDbio3(iflx_Adv_MZL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_MZL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_MZL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Cop)')
              IF (iDbio3(iflx_Adv_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_NCaS)')
              IF (iDbio3(iflx_Adv_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_EupS)')
              IF (iDbio3(iflx_Adv_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_NCaO)')
              IF (iDbio3(iflx_Adv_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_EupO)')
              IF (iDbio3(iflx_Adv_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Det)')
              IF (iDbio3(iflx_Adv_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_DetF)')
              IF (iDbio3(iflx_Adv_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Jel)')
              IF (iDbio3(iflx_Adv_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Fe)')
              IF (iDbio3(iflx_Adv_Fe).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Fe)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Fe)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_TIC)')
              IF (iDbio3(iflx_Adv_TIC).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_TIC)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_TIC)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Alk)')
              IF (iDbio3(iflx_Adv_Alk).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Alk)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Alk)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_Adv_Oxyg)')
              IF (iDbio3(iflx_Adv_Oxyg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_Adv_Oxyg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_Adv_Oxyg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_PhS)')
              IF (iDbio3(iprod_PhS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_PhS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_PhS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_PhL)')
              IF (iDbio3(iprod_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_MZL)')
              IF (iDbio3(iprod_MZL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_MZL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_MZL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_Cop)')
              IF (iDbio3(iprod_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_NCaS)')
              IF (iDbio3(iprod_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_EupS)')
              IF (iDbio3(iprod_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_NCaO)')
              IF (iDbio3(iprod_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_EupO)')
              IF (iDbio3(iprod_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_Jel)')
              IF (iDbio3(iprod_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_Ben)')
              IF (iDbio3(iprod_Ben).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_Ben)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_Ben)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_IcePhL)')
              IF (iDbio3(iprod_IcePhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_IcePhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_IcePhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(itotprod)')
              IF (iDbio3(itotprod).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(itotprod)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(itotprod)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(itotresp)')
              IF (iDbio3(itotresp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(itotresp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(itotresp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(itotrem)')
              IF (iDbio3(itotrem).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(itotrem)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(itotrem)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(io2flx)')
              IF (iDbio2(io2flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(io2flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(io2flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico2flx)')
              IF (iDbio2(ico2flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ico2flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ico2flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipco2)')
              IF (iDbio2(ipco2).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipco2)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipco2)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_NO3)')
              IF (iDbio3(ibiomem_NO3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_NO3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_NO3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_NH4)')
              IF (iDbio3(ibiomem_NH4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_NH4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_NH4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_PhS)')
              IF (iDbio3(ibiomem_PhS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_PhS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_PhS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_PhL)')
              IF (iDbio3(ibiomem_PhL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_PhL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_PhL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_MZL)')
              IF (iDbio3(ibiomem_MZL).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_MZL)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_MZL)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Cop)')
              IF (iDbio3(ibiomem_Cop).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Cop)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Cop)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_NCaS)')
              IF (iDbio3(ibiomem_NCaS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_NCaS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_NCaS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_EupS)')
              IF (iDbio3(ibiomem_EupS).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_EupS)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_EupS)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_NCaO)')
              IF (iDbio3(ibiomem_NCaO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_NCaO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_NCaO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_EupO)')
              IF (iDbio3(ibiomem_EupO).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_EupO)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_EupO)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Det)')
              IF (iDbio3(ibiomem_Det).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Det)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Det)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_DetF)')
              IF (iDbio3(ibiomem_DetF).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_DetF)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_DetF)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Jel)')
              IF (iDbio3(ibiomem_Jel).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Jel)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Jel)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Fe)')
              IF (iDbio3(ibiomem_Fe).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Fe)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Fe)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_TIC)')
              IF (iDbio3(ibiomem_TIC).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_TIC)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_TIC)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Alk)')
              IF (iDbio3(ibiomem_Alk).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Alk)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Alk)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ibiomem_Oxyg)')
              IF (iDbio3(ibiomem_Oxyg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ibiomem_Oxyg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ibiomem_Oxyg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,50) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Report input parameters.
!-----------------------------------------------------------------------
!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lbiology(ng)) THEN
            WRITE (out,60) ng
            WRITE (out,70) BioIter(ng), 'BioIter',                      &
     &            'Number of iterations for nonlinear convergence.'
!*****************************************************************************************
            WRITE (out,80) PARfrac(ng), 'PARfrac',                      &
     &            'Fraction of irradiance that is photosynthetically available (PAR) (unitless)'
            WRITE (out,80) k_ext(ng), 'k_ext',                          &
     &            'Clear-water attenuation coefficient (m^-1)'
            WRITE (out,80) k_chlA(ng), 'k_chlA',                        &
     &            'Chlorophyll attenuation coefficient, factor (m^-1)'
            WRITE (out,80) k_chlB(ng), 'k_chlB',                        &
     &            'Chlorophyll attenuation coefficient, exponent (unitless)'
            WRITE (out,80) k_chlC(ng), 'k_chlC',                        &
     &            'Other material (CDOM,sediment,etc.) attenuation coefficient (m^-1)'
            WRITE (out,80) k_sed1(ng), 'k_sed1',                        &
     &            'Depth-based attenuation coefficient, factor (m^-1)'
            WRITE (out,80) k_sed1(ng), 'k_sed1',                        &
     &            'Depth-based attenuation coefficient, exponent (unitless)'
            WRITE (out,80) xi(ng), 'xi',                                &
     &            'Nitrogen:Carbon ratio (mmol N / mg C)'
            WRITE (out,80) ccr(ng), 'ccr',                              &
     &            'Carbon:Chlorophyll ratio, small phyto (mg C / mg Chl-a)'
            WRITE (out,80) ccrPhL(ng), 'ccrPhL',                        &
     &            'Carbon:Chlorophyll ratio, large phyto (mg C / mg Chl-a)'
            WRITE (out,80) FeC(ng), 'FeC',                              &
     &            'Fe:Carbon ratio    (2 umol Fe : mol C) (umol Fe / mg C)'
            WRITE (out,80) DiS(ng), 'DiS',                              &
     &            'Doubling rate parameter (d^-1)'
            WRITE (out,80) DiL(ng), 'DiL',                              &
     &            'Doubling rate parameter (d^-1)'
            WRITE (out,80) DpS(ng), 'DpS',                              &
     &            'Doubling rate exponent (degC^-1)'
            WRITE (out,80) DpL(ng), 'DpL',                              &
     &            'Doubling rate exponent (degC^-1)'
            WRITE (out,80) alphaPhS(ng), 'alphaPhS',                    &
     &            'photosynthetic efficiency (mg C m^2 (mg Chl-a)^-1 (E^-1))'
            WRITE (out,80) alphaPhL(ng), 'alphaPhL',                    &
     &            'photosynthetic efficiency (mg C m^2 (mg Chl-a)^-1 (E^-1))'
            WRITE (out,80) k1PhS(ng), 'k1PhS',                          &
     &            'Half-saturation constant for NO3 limitation ( )'
            WRITE (out,80) k1PhL(ng), 'k1PhL',                          &
     &            'Half-saturation constant for NO3 limitation ( )'
            WRITE (out,80) k2PhS(ng), 'k2PhS',                          &
     &            'Half-saturation constant for NH4 limitation ( )'
            WRITE (out,80) k2PhL(ng), 'k2PhL',                          &
     &            'Half-saturation constant for NH4 limitation ( )'
            WRITE (out,80) FeCritPS(ng), 'FeCritPS',                    &
     &            'Threshold below which PhS is limited (umol Fe m^-3)'
            WRITE (out,80) FeCritPL(ng), 'FeCritPL',                    &
     &            'Threshold below which PhS is limited (umol Fe m^-3)'
            WRITE (out,80) kfePhS(ng), 'kfePhS',                        &
     &            'Half-saturation constant for Fe (umol m^-3)'
            WRITE (out,80) kfePhL(ng), 'kfePhL',                        &
     &            'Half-saturation constant for Fe (umol m^-3)'
            WRITE (out,80) fpPhSMZL(ng), 'fpPhSMZL',                    &
     &            'PhS->MZL  Feeding preference ( )'
            WRITE (out,80) fpPhLMZL(ng), 'fpPhLMZL',                    &
     &            'PhL->MZL  Feeding preference ( )'
            WRITE (out,80) fpPhSCop(ng), 'fpPhSCop',                    &
     &            'PhS->Cop  Feeding preference ( )'
            WRITE (out,80) fpPhLCop(ng), 'fpPhLCop',                    &
     &            'PhL->Cop  Feeding preference ( )'
            WRITE (out,80) fpMZLCop(ng), 'fpMZLCop',                    &
     &            'MZL->Cop  Feeding preference ( )'
            WRITE (out,80) fpPhSNCa(ng), 'fpPhSNCa',                    &
     &            'PhS->NCa  Feeding preference ( )'
            WRITE (out,80) fpPhLNCa(ng), 'fpPhLNCa',                    &
     &            'PhL->NCa  Feeding preference ( )'
            WRITE (out,80) fpMZLNCa(ng), 'fpMZLNCa',                    &
     &            'MZL->NCa  Feeding preference ( )'
            WRITE (out,80) fpPhSEup(ng), 'fpPhSEup',                    &
     &            'PhS->Eup  Feeding preference ( )'
            WRITE (out,80) fpPhLEup(ng), 'fpPhLEup',                    &
     &            'PhL->Eup  Feeding preference ( )'
            WRITE (out,80) fpMZLEup(ng), 'fpMZLEup',                    &
     &            'MZL->Eup  Feeding preference ( )'
            WRITE (out,80) fpCopEup(ng), 'fpCopEup',                    &
     &            'Cop->Eup  Feeding preference ( )'
            WRITE (out,80) fpDetEup(ng), 'fpDetEup',                    &
     &            'Det->Eup  Feeding preference ( )'
            WRITE (out,80) fpDetEupO(ng), 'fpDetEupO',                  &
     &            'Det->EupO Feeding preference ( )'
            WRITE (out,80) fpCopJel(ng), 'fpCopJel',                    &
     &            'Cop->Jel  Feeding preference ( )'
            WRITE (out,80) fpNCaJel(ng), 'fpNCaJel',                    &
     &            'NCa->Jel  Feeding preference ( )'
            WRITE (out,80) fpEupJel(ng), 'fpEupJel',                    &
     &            'Eup->Jel  Feeding preference ( )'
            WRITE (out,80) eMZL(ng), 'eMZL',                            &
     &            'maximum specific ingestion rate (mg C/mg C/d)'
            WRITE (out,80) eCop(ng), 'eCop',                            &
     &            'maximum specific ingestion rate (mg C/mg C/d)'
            WRITE (out,80) eNCa(ng), 'eNCa',                            &
     &            'maximum specific ingestion rate (mg C/mg C/d)'
            WRITE (out,80) eEup(ng), 'eEup',                            &
     &            'maximum specific ingestion rate (mg C/mg C/d)'
            WRITE (out,80) eJel(ng), 'eJel',                            &
     &            'maximum specific ingestion rate (mg C/mg C/d)'
            WRITE (out,80) Q10MZL(ng), 'Q10MZL',                        &
     &            'Q10 for growth rate (unitless)'
            WRITE (out,80) Q10Cop(ng), 'Q10Cop',                        &
     &            'Q10 for growth rate (unitless)'
            WRITE (out,80) Q10NCa(ng), 'Q10NCa',                        &
     &            'Q10 for growth rate (unitless)'
            WRITE (out,80) Q10Eup(ng), 'Q10Eup',                        &
     &            'Q10 for growth rate (unitless)'
            WRITE (out,80) Q10Jele(ng), 'Q10Jele',                      &
     &            'Q10 for growth rate (unitless)'
            WRITE (out,80) Q10MZLT(ng), 'Q10MZLT',                      &
     &            'Temperature coefficient for Q10 (deg. C)'
            WRITE (out,80) Q10CopT(ng), 'Q10CopT',                      &
     &            'Temperature coefficient for Q10 (deg. C)'
            WRITE (out,80) Q10NCaT(ng), 'Q10NCaT',                      &
     &            'Temperature coefficient for Q10 (deg. C)'
            WRITE (out,80) Q10EupT(ng), 'Q10EupT',                      &
     &            'Temperature coefficient for Q10 (deg. C)'
            WRITE (out,80) Q10JelTe(ng), 'Q10JelTe',                    &
     &            'Temperature coefficient for Q10 (deg. C)'
            WRITE (out,80) fMZL(ng), 'fMZL',                            &
     &            'Half-saturation constant for grazing (mg C/m3)'
            WRITE (out,80) fCop(ng), 'fCop',                            &
     &            'Half-saturation constant for grazing (mg C/m3)'
            WRITE (out,80) fNCa(ng), 'fNCa',                            &
     &            'Half-saturation constant for grazing (mg C/m3)'
            WRITE (out,80) fEup(ng), 'fEup',                            &
     &            'Half-saturation constant for grazing (mg C/m3)'
            WRITE (out,80) fJel(ng), 'fJel',                            &
     &            'Half-saturation constant for grazing (mg C/m3)'
            WRITE (out,80) gammaMZL(ng), 'gammaMZL',                    &
     &            'Growth efficiency ( )'
            WRITE (out,80) gammaCop(ng), 'gammaCop',                    &
     &            'Growth efficiency ( )'
            WRITE (out,80) gammaNCa(ng), 'gammaNCa',                    &
     &            'Growth efficiency ( )'
            WRITE (out,80) gammaEup(ng), 'gammaEup',                    &
     &            'Growth efficiency ( )'
            WRITE (out,80) gammaJel(ng), 'gammaJel',                    &
     &            'Growth efficiency ( )'
            WRITE (out,80) mPhS(ng), 'mPhS',                            &
     &            'daily linear mortality rate (senescence) (1/d)'
            WRITE (out,80) mPhL(ng), 'mPhL',                            &
     &            'daily linear mortality rate (senescence) (1/d)'
            WRITE (out,80) mMZL(ng), 'mMZL',                            &
     &            'daily linear mortality rate (1/d)'
            WRITE (out,80) mpredMZL(ng), 'mpredMZL',                    &
     &            'Daily mortality for Large Microzoo. (1/d/mgC)'
            WRITE (out,80) mpredCop(ng), 'mpredCop',                    &
     &            'Daily mortality for Copepods (1/d/mgC)'
            WRITE (out,80) mpredNCa(ng), 'mpredNCa',                    &
     &            'Daily mortality for Neocalanus (1/d/mgC)'
            WRITE (out,80) mpredEup(ng), 'mpredEup',                    &
     &            'Daily mortality for Euphausiids (1/d/mgC)'
            WRITE (out,80) mpredJel(ng), 'mpredJel',                    &
     &            'Daily mortality for Large Microzoo. (1/d/mgC)'
            WRITE (out,80) wPhS(ng), 'wPhS',                            &
     &            'Sinking rate for Small Phytoplankton (m/d)'
            WRITE (out,80) wPhL(ng), 'wPhL',                            &
     &            'Sinking rate for Large Phytoplankton (m/d)'
            WRITE (out,80) wDet(ng), 'wDet',                            &
     &            'Sinking rate for Detritus (m/d)'
            WRITE (out,80) wDetF(ng), 'wDetF',                          &
     &            'Sinking rate for Detritus (m/d)'
            WRITE (out,80) respPhS(ng), 'respPhS',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respPhL(ng), 'respPhL',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respMZL(ng), 'respMZL',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respCop(ng), 'respCop',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respNCa(ng), 'respNCa',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respEup(ng), 'respEup',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) respJel(ng), 'respJel',                      &
     &            'Specific respiration rate (d^-1)'
            WRITE (out,80) Q10Jelr(ng), 'Q10Jelr',                      &
     &            'Q10 for respiration rate, jellyfish (degC)'
            WRITE (out,80) Q10JelTr(ng), 'Q10JelTr',                    &
     &            'reference temperature for Q10 respiration, jellyfish (1/degC)'
            WRITE (out,80) KtBm_PhS(ng), 'KtBm_PhS',                    &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) KtBm_PhL(ng), 'KtBm_PhL',                    &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) KtBm_MZL(ng), 'KtBm_MZL',                    &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) ktbmC(ng), 'ktbmC',                          &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) ktbmN(ng), 'ktbmN',                          &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) ktbmE(ng), 'ktbmE',                          &
     &            'temperature coefficient for respiration (1/deg C)'
            WRITE (out,80) TmaxPhS(ng), 'TmaxPhS',                      &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) TmaxPhL(ng), 'TmaxPhL',                      &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) TmaxMZL(ng), 'TmaxMZL',                      &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) TrefC(ng), 'TrefC',                          &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) TrefN(ng), 'TrefN',                          &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) TrefE(ng), 'TrefE',                          &
     &            'reference temperature for respiration (degC)'
            WRITE (out,80) Feinlo(ng), 'Feinlo',                        &
     &            'inshore/surface (micromol Fe m-3 or nM)'
            WRITE (out,80) Feinhi(ng), 'Feinhi',                        &
     &            'inshore/deep (micromol Fe m-3 or nM)'
            WRITE (out,80) Feinh(ng), 'Feinh',                          &
     &            'inshore isobath of transition (m)'
            WRITE (out,80) Feofflo(ng), 'Feofflo',                      &
     &            'offshore/surface (micromol Fe m-3 or nM)'
            WRITE (out,80) Feoffhi(ng), 'Feoffhi',                      &
     &            'offshore/deep (micromol Fe m-3 or nM)'
            WRITE (out,80) Feoffh(ng), 'Feoffh',                        &
     &            'offshore isobath of transition (m)'
            WRITE (out,80) wNCrise(ng), 'wNCrise',                      &
     &            'upward velocity , tuned not data (m/day)'
            WRITE (out,80) wNCsink(ng), 'wNCsink',                      &
     &            'downward velocity , tuned not data (m/day)'
            WRITE (out,80) RiseStart(ng), 'RiseStart',                  &
     &            'Date NCaO begin to move upward (Day of Year)'
            WRITE (out,80) RiseEnd(ng), 'RiseEnd',                      &
     &            'Date NCaO stop moving upward (Day of Year)'
            WRITE (out,80) SinkStart(ng), 'SinkStart',                  &
     &            'Date NCaO begin to move downward (Day of Year)'
            WRITE (out,80) SinkEnd(ng), 'SinkEnd',                      &
     &            'Date NCaO stop moving downward (Day of Year)'
            WRITE (out,80) RiseStartCM(ng), 'RiseStartCM',              &
     &            'Date NCaS begin to move upward (Day of Year)'
            WRITE (out,80) RiseEndCM(ng), 'RiseEndCM',                  &
     &            'Date NCaS stop moving upward (Day of Year)'
            WRITE (out,80) SinkStartCM(ng), 'SinkStartCM',              &
     &            'Date NCaS begin to move downward (Day of Year)'
            WRITE (out,80) SinkEndCM(ng), 'SinkEndCM',                  &
     &            'Date NCaS stop moving downward (Day of Year)'
            WRITE (out,80) Pv0(ng), 'Pv0',                              &
     &            'PON decompositon at 0 deg C (1/d)'
            WRITE (out,80) PvT(ng), 'PvT',                              &
     &            'Temperature coefficient for remineralization (1/deg C)'
            WRITE (out,80) Nitr0(ng), 'Nitr0',                          &
     &            'Nitrification rate at 0C (1/d)'
            WRITE (out,80) ktntr(ng), 'ktntr',                          &
     &            'Temperature coefficient for nitrification (1/deg C)'
            WRITE (out,80) KNH4Nit(ng), 'KNH4Nit',                      &
     &            'Half saturation constant for nitrification (mmolN/m^3)'
            WRITE (out,80) ToptNtr(ng), 'ToptNtr',                      &
     &            'Optimal temperature for nitrification (degC)'
#ifdef BENTHIC
            WRITE (out,80) q10r(ng), 'q10r',                            &
     &            'Q10 for growth/feeding and mortality rate (unitless)'
            WRITE (out,80) Rup(ng), 'Rup',                              &
     &            'maximum specific ingestion rate (1/d)'
            WRITE (out,80) KupD(ng), 'KupD',                            &
     &            'Half-saturation constant for feeding on benthic prey (mg C/m^2)'
            WRITE (out,80) KupP(ng), 'KupP',                            &
     &            'Half-saturation constant for feeding on pelagic prey (mg C/m^2)'
            WRITE (out,80) LupD(ng), 'LupD',                            &
     &            'Lower threshold for feeding on benthic prey (mg C/m^2)'
            WRITE (out,80) LupP(ng), 'LupP',                            &
     &            'Lower threshold for feeding on pelagic prey (mg C/m^2)'
            WRITE (out,80) Qres(ng), 'Qres',                            &
     &            'Active metabolic rate (1/d)'
            WRITE (out,80) Rres(ng), 'Rres',                            &
     &            'Basal metabolism rate (1/d)'
            WRITE (out,80) rmort(ng), 'rmort',                          &
     &            'linear mortality rate (1/d)'
            WRITE (out,80) eex(ng), 'eex',                              &
     &            'fraction of living food excreted (1 - growth efficiency) ( )'
            WRITE (out,80) eexD(ng), 'eexD',                            &
     &            'fraction of detrital food excreted ( )'
            WRITE (out,80) prefD(ng), 'prefD',                          &
     &            'DetBen->Ben feeding preference ( )'
            WRITE (out,80) prefPL(ng), 'prefPL',                        &
     &            'PhL->Ben feeding preference ( )'
            WRITE (out,80) prefPS(ng), 'prefPS',                        &
     &            'PhS->Ben feeding preference ( )'
            WRITE (out,80) T0benr(ng), 'T0benr',                        &
     &            'Reference temperature for growth/feeding rate (degC)'
            WRITE (out,80) BenPred(ng), 'BenPred',                      &
     &            'Quadratic mortality rate due to undefined predation (1/mgC/d)'
#endif
#ifdef ICE_BIO
            WRITE (out,80) alphaIb(ng), 'alphaIb',                      &
     &            'IcePhL Chl-a specific attenuation coefficient (W^-1 m^-2)'
            WRITE (out,80) betaI(ng), 'betaI',                          &
     &            'IcePhL photosynthetic efficiency (W^-1 m^-2)'
            WRITE (out,80) inhib(ng), 'inhib',                          &
     &            'IcePhL NH4 inhibition on NO3 uptake (m^3/mmol N)'
            WRITE (out,80) ksnut1(ng), 'ksnut1',                        &
     &            'IcePhL half-saturation constant for NO3 (mmolN/m^3)'
            WRITE (out,80) ksnut2(ng), 'ksnut2',                        &
     &            'IcePhL half-saturation constant for NH4 (mmolN/m^3)'
            WRITE (out,80) mu0(ng), 'mu0',                              &
     &            'IcePhL maximum growth rate at 0 deg C (1/d)'
            WRITE (out,80) R0i(ng), 'R0i',                              &
     &            'IcePhL respiration rate (1/d)'
            WRITE (out,80) rg0(ng), 'rg0',                              &
     &            'IcePhL mortality rate at 0 deg C (1/d)'
            WRITE (out,80) rg(ng), 'rg',                                &
     &            'IcePhL temperature coefficient for mortality (1/deg C)'
            WRITE (out,80) annit(ng), 'annit',                          &
     &            'IcePhL nitrification factor (1/d)'
            WRITE (out,80) aidz(ng), 'aidz',                            &
     &            'Ice thickness (m)'
           WRITE (out,80) tI0(ng), 'tI0',                               &
    &            'Nitrification light threshold (W m^-2])'
           WRITE (out,80) KI(ng), 'KI',                                 &
    &            'Nitrification light half-saturation constant (W m^-2)'
     
#endif
#ifdef TS_DIF2
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu2(i,ng), 'nl_tnu2', i,              &
     &              'NLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu2(i,ng), 'ad_tnu2', i,              &
     &              'ADM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu2(i,ng), 'tl_tnu2', i,              &
     &              'TLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
#ifdef TS_DIF4
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu4(i,ng), 'nl_tnu4', i,              &
     &              'NLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu4(i,ng), 'ad_tnu4', i,              &
     &              'ADM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu4(i,ng), 'tl_tnu4', i,              &
     &              'TLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSponge(i,ng)) THEN
                WRITE (out,110) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning ON  sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning OFF sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,100) Akt_bak(i,ng), 'Akt_bak', i,               &
     &             'Background vertical mixing coefficient (m2/s)',     &
     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef FORWARD_MIXING
            DO itrc=1,NBT
              i=idbio(itrc)
# ifdef ADJOINT
              WRITE (out,100) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,        &
     &              'ADM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,        &
     &              'TLM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) Tnudg(i,ng), 'Tnudg', i,                  &
     &              'Nudging/relaxation time scale (days)',             &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSrc(i,ng)) THEN
                WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning ON  point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning OFF point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerCLM(i,ng)) THEN
                WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning ON  processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning OFF processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LnudgeTCLM(i,ng)) THEN
                WRITE (out,110) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning ON  nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning OFF nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            IF ((nHIS(ng).gt.0).and.ANY(Hout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Hout(idTvar(i),ng), 'Hout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTsur(i),ng)) WRITE (out,120)                 &
     &              Hout(idTsur(i),ng), 'Hout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
#ifdef BENTHIC
              DO itrc=1,NBEN
                i=idben(itrc)
                IF (Hout(idBeTvar(i),ng)) WRITE (out,120)               &
   &                Hout(idBeTvar(i),ng), 'Hout(idBeTvar)',             &
   &                'Write out benthic tracer ', i, TRIM(Vname(1,idBeTvar(i)))
              END DO
#endif
#ifdef ICE_BIO
              IF (Hout(idIcePhL,ng)) WRITE (out,120)                    &
     &            Hout(idIcePhL,ng), 'Hout(idIcePhL)',                  &
     &            'Write out ice tracer', 0,                            &
     &            TRIM(Vname(1,idIcePhL))
              IF (Hout(idIceNO3,ng)) WRITE (out,120)                    &
     &            Hout(idIceNO3,ng), 'Hout(idIceNO3)',                  &
     &            'Write out ice tracer', 0,                            &
     &            TRIM(Vname(1,idIceNO3))
              IF (Hout(idIceNH4,ng)) WRITE (out,120)                    &
     &            Hout(idIceNH4,ng), 'Hout(idIceNH4)',                  &
     &            'Write out ice tracer', 0,                            &
     &            TRIM(Vname(1,idIceNH4))
#endif
            END IF
            IF ((nQCK(ng).gt.0).and.ANY(Qout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Qout(idTvar(i),ng), 'Qout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idsurT(i),ng)) WRITE (out,120)                 &
     &              Qout(idsurT(i),ng), 'Qout(idsurT)',                 &
     &              'Write out surface tracer ', i,                     &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTsur(i),ng)) WRITE (out,120)                 &
     &              Qout(idTsur(i),ng), 'Qout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            IF ((nAVG(ng).gt.0).and.ANY(Aout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Aout(idTvar(i),ng), 'Aout(idTvar)',                 &
     &              'Write out averaged tracer ', i,                    &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idTTav(i),ng), 'Aout(idTTav)',                 &
     &              'Write out averaged <t*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idUTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idUTav(i),ng), 'Aout(idUTav)',                 &
     &              'Write out averaged <u*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idVTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idVTav(i),ng), 'Aout(idVTav)',                 &
     &              'Write out averaged <v*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHUTav(i),ng)) WRITE (out,120)                 &
     &              Aout(iHUTav(i),ng), 'Aout(iHUTav)',                 &
     &              'Write out averaged <Huon*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHVTav(i),ng)) WRITE (out,120)                 &
     &              Aout(iHVTav(i),ng), 'Aout(iHVTav)',                 &
     &              'Write out averaged <Hvom*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
#ifdef BENTHIC
              DO itrc=1,NBEN
                i=idben(itrc)
                IF (Aout(idBeTvar(i),ng)) WRITE (out,120)               &
   &                Aout(idBeTvar(i),ng), 'Aout(idBeTvar)',             &
   &                'Write out averaged benthic tracer ', i, TRIM(Vname(1,idBeTvar(i)))
              END DO
#endif
#ifdef ICE_BIO
              IF (Aout(idIcePhL,ng)) WRITE (out,120)                    &
     &            Aout(idIcePhL,ng), 'Aout(idIcePhL)',                  &
     &            'Write out averaged ice tracer', 0,                   &
     &            TRIM(Vname(1,idIcePhL))
              IF (Aout(idIceNO3,ng)) WRITE (out,120)                    &
     &            Aout(idIceNO3,ng), 'Aout(idIceNO3)',                  &
     &            'Write out averaged ice tracer', 0,                   &
     &            TRIM(Vname(1,idIceNO3))
              IF (Aout(idIceNH4,ng)) WRITE (out,120)                    &
     &            Aout(idIceNH4,ng), 'Aout(idIceNH4)',                  &
     &            'Write out averaged ice tracer', 0,                   &
     &            TRIM(Vname(1,idIceNH4))
#endif
            END IF
#endif
#ifdef DIAGNOSTICS_TS
            IF ((nDIA(ng).gt.0).and.ANY(Dout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTrate),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTrate)',                 &
     &                'Write out rate of change of tracer ', itrc,        &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThadv),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iThadv)',                 &
     &                'Write out horizontal advection, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTxadv),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTxadv)',                 &
     &                'Write out horizontal X-advection, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTyadv),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTyadv)',                 &
     &                'Write out horizontal Y-advection, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvadv),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTvadv)',                 &
     &                'Write out vertical advection, tracer ', itrc,      &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined TS_DIF2 || defined TS_DIF4
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThdif),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iThdif)',                 &
     &                'Write out horizontal diffusion, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(i,iTxdif),ng))                            &
     &            WRITE (out,120) .TRUE., 'Dout(iTxdif)',                 &
     &                'Write out horizontal X-diffusion, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTydif),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTydif)',                 &
     &                'Write out horizontal Y-diffusion, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTsdif),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTsdif)',                 &
     &                'Write out horizontal S-diffusion, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
#  endif
# endif
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvdif),ng))                         &
     &            WRITE (out,120) .TRUE., 'Dout(iTvdif)',                 &
     &                'Write out vertical diffusion, tracer ', itrc,      &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
            END IF
#endif
#ifdef DIAGNOSTICS_BIO
            IF (nDIA(ng).gt.0) THEN
              IF (NDbio2d.gt.0) THEN
                DO itrc=1,NDbio2d
                  i=iDbio2(itrc)
                  IF (Dout(i,ng)) WRITE (out,130)                         &
     &                Dout(i,ng), 'Dout(iDbio2)',                         &
     &                'Write out diagnostics for', TRIM(Vname(1,i))
                END DO
              END IF
              DO itrc=1,NDbio3d
                i=iDbio3(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                           &
     &              Dout(i,ng), 'Dout(iDbio3)',                           &
     &              'Write out diagnostics for', TRIM(Vname(1,i))
              END DO
            END IF
#endif
!
!-----------------------------------------------------------------------
!  Rescale biological tracer parameters.
!-----------------------------------------------------------------------
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
      DO ng=1,Ngrids
        DO itrc=1,NBT
          i=idbio(itrc)
          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
#ifdef ADJOINT
          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
#endif
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
          IF (Tnudg(i,ng).gt.0.0_r8) THEN
            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
          ELSE
            Tnudg(i,ng)=0.0_r8
          END IF
        END DO
      END DO

  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - variable info not yet loaded, ',a)
  50  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  60  FORMAT (/,/,' BESTNPZ Model Parameters, Grid: ',i2.2,             &
     &        /,  ' =================================',/)
  70  FORMAT (1x,i10,2x,a,t32,a)
  80  FORMAT (1p,e11.4,2x,a,t32,a)
  90  FORMAT (1p,e11.4,2x,a,t32,a,/,t34,a)
 100  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t32,a,/,t34,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,'(',i2.2,')',t32,a,i2.2,':',1x,a)
 120  FORMAT (10x,l1,2x,a,t32,a,i2.2,':',1x,a)
 130  FORMAT (10x,l1,2x,a,t32,a,1x,a)

      RETURN
      END SUBROUTINE read_BioPar