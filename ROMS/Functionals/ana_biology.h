      SUBROUTINE ana_biology (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets initial conditions for biological tracer fields   !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_ocean

#ifdef BEST_NPZ
# if defined CLIM_ICE_1D
      USE mod_clima
# endif
# ifdef  ICE_BIO
      USE mod_ice
# endif
#endif

#if defined BIO_GOANPZ || defined BEST_NPZ
      USE mod_grid
      USE mod_biology
#endif
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_biology_tile (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
#ifdef BEST_NPZ
     &                       GRID(ng) % z_r,                            &
     &                       GRID(ng) % h,                              &
# ifdef BENTHIC
     &                       OCEAN(ng) % bt,                            &
# endif
# ifdef ICE_BIO
     &                       OCEAN(ng) % it,                            &
# endif
#endif
#ifdef BIO_GOANPZ
     &                       GRID(ng) % z_r,                            &
#endif
     &                       OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME( 1)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_biology
!
!***********************************************************************
      SUBROUTINE ana_biology_tile (ng, tile, model,                     &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
#ifdef BEST_NPZ
     &                             z_r, h,                              &
# ifdef BENTHIC
     &                             bt,                                  &
# endif
# ifdef ICE_BIO
                                   it,                                  &
# endif
#endif
# ifdef BIO_GOANPZ
     &                             z_r,                                 &
# endif
     &                             t)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_iounits
      USE mod_scalars
!
      USE stats_mod, ONLY : stats_3dfld
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
# if defined BEST_NPZ
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: h(LBi:,LBj:)
#  ifdef BENTHIC
      real(r8), intent(inout) :: bt(LBi:,LBj:,:,:,:)
#  endif
#  ifdef ICE_BIO
      real(r8), intent(inout) :: it(LBi:,LBj:,:,:)
#  endif
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
# if defined BEST_NPZ
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
#   ifdef BENTHIC
      real(r8), intent(inout) :: bt(LBi:UBi,LBj:UBj,NBL(ng),3,NBeT(ng))
#   endif
#  ifdef ICE_BIO
      real(r8), intent(inout) :: it(LBi:UBi,LBj:UBj,3,NIceT(ng))
# endif

# if defined BIO_GOANPZ
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      logical, save :: first = .TRUE.

      integer :: i, is, itrc, j, k

#if defined BIO_FENNEL || defined NEMURO || defined BIO_UMAINE
      real(r8) :: SiO4, cff1, cff2, temp
#elif defined ECOSIM
      real(r8) :: cff1, cff2, cff3, cff4, cff5, cff6, cff7, cff8, cff9
      real(r8) :: cff10, cff11, cff12, cff13, cff14, cff15
      real(r8) :: salt, sftm, temp
#elif defined BIO_GOANPZ || defined BEST_NPZ
      real(r8) :: var1, var2, var3, var4, var5, var6, var7
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,N(ng)) :: biod
      real(r8), dimension(NT(ng)) :: deepval
      real(r8), dimension(NT(ng)) :: loval
# ifdef IRON_LIMIT
      real(r8) :: FeSurf, FeDeep
# endif
      real(r8), parameter :: eps = 1.0E-20_r8
#endif
!
!   Maximum 80 biological tracers consider for field statistics.
!
      TYPE (T_STATS), save :: Stats(80)

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Initialize field statistics structure.
!-----------------------------------------------------------------------
!
      IF (first) THEN
        first=.FALSE.
        DO i=1,SIZE(Stats,1)
          Stats(i) % count=0.0_r8
          Stats(i) % min=Large
          Stats(i) % max=-Large
          Stats(i) % avg=0.0_r8
          Stats(i) % rms=0.0_r8
        END DO
      END IF

#if defined BIO_FENNEL
!
!-----------------------------------------------------------------------
!  Fennel et al. (2006), nitrogen-based biology model.
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iPhyt)=0.08_r8
            t(i,j,k,1,iZoop)=0.06_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iLDeN)=0.02_r8
            t(i,j,k,1,iSDeN)=0.04_r8
            t(i,j,k,1,iChlo)=0.02_r8
            t(i,j,k,1,iOxyg)=0.00_r8
#ifdef CARBON
            t(i,j,k,1,iTIC_)=2100.0_r8
            t(i,j,k,1,iTAlk)=2350.0_r8
            t(i,j,k,1,iLDeC)=0.002_r8
            t(i,j,k,1,iSDeC)=0.06_r8
#endif
#ifdef OXYGEN
            t(i,j,k,1,iOxyg)=10.0_r8/0.02241_r8
#endif
          END DO
        END DO
      END DO

#elif defined BIO_UMAINE
!
!-----------------------------------------------------------------------
!  UMaine Carbon, Silicate, Nitrogen Ecosystem (CoSiNE) Model
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iSiOH)=1.50_r8*t(i,j,k,1,iNO3_)
            t(i,j,k,1,iSphy)=0.08_r8
            t(i,j,k,1,iLphy)=0.08_r8
            t(i,j,k,1,iSzoo)=0.06_r8
            t(i,j,k,1,iLzoo)=0.06_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iSDet)=0.02_r8
            t(i,j,k,1,iopal)=0.04_r8
            t(i,j,k,1,iPO4_)=t(i,j,k,1,iNO3_)/16.0_r8
#ifdef OXYGEN
            t(i,j,k,1,iOxyg)=10.0_r8/0.02241_r8
#endif
#ifdef CARBON
            t(i,j,k,1,iTIC_)=2100.0_r8
            t(i,j,k,1,iTAlk)=2350.0_r8
#endif
          END DO
        END DO
      END DO

#elif defined NEMURO
!
!-----------------------------------------------------------------------
!  Nemuro lower trophic level ecosystem model.
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrR,JendR
          DO i=IstrR,IendR
#ifdef NEMSAN_TEST
            t(i,j,k,1,iSphy)=0.0_r8
            t(i,j,k,1,iLphy)=0.0_r8
# if defined H_BEHAVE
! Horizontal gradient
            t(i,j,k,1,iNO3_)=20.0_r8*REAL(i,r8)/REAL(Lm(ng),r8)
            t(i,j,k,1,iSzoo)=1.0_r8*                                    &
     &                 sin(3.14159_r8*REAL(i,r8)/REAL(Lm(ng),r8))*      &
     &                 sin(3.14159_r8*REAL(j,r8)/REAL(Mm(ng),r8))
            t(i,j,k,1,iLzoo)=1.0_r8*                                    &
     &                 sin(3.14159_r8*REAL(i,r8)/REAL(Lm(ng),r8))*      &
     &                 sin(3.14159_r8*REAL(j,r8)/REAL(Mm(ng),r8))
# elif defined V_BEHAVE
! Vertical gradient
            t(i,j,k,1,iNO3_)=20.0_r8*REAL(k,r8)/REAL(N(ng),r8)
            t(i,j,k,1,iSzoo)=1.0_r8*sin(3.14159_r8*REAL(k,r8)/          &
     &                                         REAL(N(ng),r8))
            t(i,j,k,1,iLzoo)=1.0_r8*sin(3.14159_r8*REAL(k,r8)/          &
     &                                         REAL(N(ng),r8))
# else
            t(i,j,k,1,iNO3_)=15.0_r8
            t(i,j,k,1,iSzoo)=0.0_r8
            t(i,j,k,1,iLzoo)=0.0_r8
# endif
            t(i,j,k,1,iPzoo)=0.0_r8
            t(i,j,k,1,iNH4_)=0.0_r8
            t(i,j,k,1,iPON_)=0.0_r8
            t(i,j,k,1,iDON_)=0.0_r8
            t(i,j,k,1,iSiOH)=0.0_r8
            t(i,j,k,1,iopal)=0.0_r8
#else
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iSphy)=0.06_r8
            t(i,j,k,1,iLphy)=0.06_r8
            t(i,j,k,1,iSzoo)=0.05_r8
            t(i,j,k,1,iLzoo)=0.05_r8
            t(i,j,k,1,iPzoo)=0.05_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iPON_)=0.001_r8
            t(i,j,k,1,iDON_)=0.001_r8
            t(i,j,k,1,iSiOH)=SiO4
            t(i,j,k,1,iopal)=0.001_r8
# ifdef IRON_LIMIT
            t(i,j,k,1,iFeD_)=2.0_r8
            t(i,j,k,1,iFeSp)=0.001_r8
            t(i,j,k,1,iFeLp)=0.001_r8
# endif
#endif
          END DO
        END DO
      END DO

#elif defined NPZD_FRANKS || defined NPZD_POWELL
!
!-----------------------------------------------------------------------
!  NPZD biology model.
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,iNO3_)=BioIni(iNO3_,ng)
            t(i,j,k,1,iPhyt)=BioIni(iPhyt,ng)
            t(i,j,k,1,iZoop)=BioIni(iZoop,ng)
            t(i,j,k,1,iSDet)=BioIni(iSDet,ng)
          END DO
        END DO
      END DO

#elif defined NPZD_IRON
!
!-----------------------------------------------------------------------
!  NPZD biology model with or without iron limitation on phytoplankton
!  growth.
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,iNO3_)=BioIni(iNO3_,ng)
            t(i,j,k,1,iPhyt)=BioIni(iPhyt,ng)
            t(i,j,k,1,iZoop)=BioIni(iZoop,ng)
            t(i,j,k,1,iSDet)=BioIni(iSDet,ng)
# ifdef IRON_LIMIT
            t(i,j,k,1,iFphy)=BioIni(iFphy,ng)
            t(i,j,k,1,iFdis)=BioIni(iFdis,ng)
# endif
          END DO
        END DO
      END DO

#elif defined ECOSIM
!
!---------------------------------------------------------------------
!  EcoSim initial fields.
!---------------------------------------------------------------------
!
! Assumed maximum temperature gradient.
!
      cff3=1.0_r8/14.0_r8
      cff4=1.0_r8/16.0_r8
      cff5=32.0_r8
      cff7=1.0_r8/0.0157_r8
      cff8=1.0_r8/6.625_r8
      cff9=1.0_r8/16.0_r8
      cff10=1.0_r8/15.0_r8
      cff11=1.0_r8/8.0_r8
      cff12=1.0_r8/128.0_r8
      cff13=1.0_r8/1000.0_r8
      cff14=1.0_r8/12.0_r8
      cff15=cff5*cff8*cff14                  ! mole N : gram Chl

      DO k=N(ng),1,-1
        DO j=JstrT,JendT
          DO i=IstrT,IendT
!
! Initialization of surface chlorophyll.
!
            sftm=t(i,j,N(ng),1,itemp)
            temp=t(i,j,k,1,itemp)
            salt=t(i,j,k,1,isalt)
            cff1=-0.0827_r8*sftm+2.6386_r8
            cff2=MAX(0.00001_r8,cff1*(1.0_r8-(sftm-temp)*cff3))
!
! Initialization of nutrients.
!
            t(i,j,k,1,iNH4_)=0.053_r8*temp+0.7990_r8
            t(i,j,k,1,iNO3_)=8.5_r8-cff2*cff15-t(i,j,k,1,iNH4_)
            t(i,j,k,1,iPO4_)=(t(i,j,k,1,iNH4_)+t(i,j,k,1,iNO3_))*cff4
            t(i,j,k,1,iFeO_)=1.0_r8
!
! Assuming diatoms are 75% of initialized chlorophyll.
!
            t(i,j,k,1,iSiO_)=5.5_r8-(cff2*0.75_r8)*cff15*1.20_r8
            t(i,j,k,1,iDIC_)=2000.0_r8
!
! Bacteria Initialization.
!
            DO is=1,Nbac
              t(i,j,k,1,iBacC(is))=0.85_r8
              t(i,j,k,1,iBacN(is))=t(i,j,k,1,iBacC(is))*N2cBAC(ng)
              t(i,j,k,1,iBacP(is))=t(i,j,k,1,iBacC(is))*P2cBAC(ng)
              t(i,j,k,1,iBacF(is))=t(i,j,k,1,iBacC(is))*Fe2cBAC(ng)
            END DO
!
! Initialize phytoplankton populations.
!
            t(i,j,k,1,iPhyC(1))=MAX(0.02_r8,                            &
     &                              0.75_r8*0.75_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(2))=MAX(0.02_r8,                            &
     &                              0.75_r8*0.25_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(3))=MAX(0.02_r8,                            &
     &                              0.125_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(4))=t(i,j,k,1,iPhyC(3))
            DO is=1,Nphy
              t(i,j,k,1,iPhyN(is))=t(i,j,k,1,iPhyC(is))*cff8
              t(i,j,k,1,iPhyP(is))=t(i,j,k,1,iPhyN(is))*cff4
              t(i,j,k,1,iPhyF(is))=t(i,j,k,1,iPhyC(is))*cff13
              IF (iPhyS(is).gt.0) THEN
                t(i,j,k,1,iPhyS(is))=t(i,j,k,1,iPhyN(is))*1.20_r8
              END IF
!
!  Initialize Pigments in ugrams/liter (not umole/liter).
!  Chlorophyll-a
!
              cff6=12.0_r8/cff5
              t(i,j,k,1,iPigs(is,1))=cff6*t(i,j,k,1,iPhyC(is))
!
!  Chlorophyll-b.
!
              cff6=cff5-b_C2Cl(is,ng)
              IF (iPigs(is,2).gt.0) THEN
                 t(i,j,k,1,iPigs(is,2))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_ChlB(is,ng)+                 &
     &                                   mxChlB(is,ng)*cff6)
              END IF
!
!  Chlorophyll-c.
!
              IF (iPigs(is,3).gt.0) THEN
                 t(i,j,k,1,iPigs(is,3))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_ChlC(is,ng)+                 &
     &                                   mxChlC(is,ng)*cff6)
              END IF
!
!  Photosynthetic Carotenoids.
!
              IF (iPigs(is,4).gt.0) THEN
                 t(i,j,k,1,iPigs(is,4))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_PSC(is,ng)+                  &
     &                                   mxPSC(is,ng)*cff6)
              END IF
!
!  Photoprotective Carotenoids.
!
              IF (iPigs(is,5).gt.0) THEN
                 t(i,j,k,1,iPigs(is,5))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_PPC(is,ng)+                  &
     &                                   mxPPC(is,ng)*cff6)
              END IF
!
!  Low Urobilin Phycoeurythin Carotenoids.
!
              IF (iPigs(is,6).gt.0) THEN
                 t(i,j,k,1,iPigs(is,6))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_LPUb(is,ng)+                 &
     &                                   mxLPUb(is,ng)*cff6)
              END IF
!
!  High Urobilin Phycoeurythin Carotenoids.
!
              IF (iPigs(is,7).gt.0) THEN
                 t(i,j,k,1,iPigs(is,7))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_HPUb(is,ng)+                 &
     &                                   mxHPUb(is,ng)*cff6)
              END IF
            END DO
!
! DOC initialization.
!
            cff6=MAX(0.001_r8,-0.9833_r8*salt+33.411_r8)
            t(i,j,k,1,iDOMC(1))=0.1_r8
            t(i,j,k,1,iDOMN(1))=t(i,j,k,1,iDOMC(1))*cff8
            t(i,j,k,1,iDOMP(1))=t(i,j,k,1,iDOMN(1))*cff9
            t(i,j,k,1,iCDMC(1))=t(i,j,k,1,iDOMC(1))*cDOCfrac_c(1,ng)
            t(i,j,k,1,iDOMC(2))=15.254_r8*cff6+70.0_r8
            t(i,j,k,1,iDOMN(2))=t(i,j,k,1,iDOMC(2))*cff10
            t(i,j,k,1,iDOMP(2))=0.0_r8
            t(i,j,k,1,iCDMC(2))=(0.243_r8*cff6+0.055_r8)*cff7
!
! Fecal Initialization.
!
            DO is=1,Nfec
              t(i,j,k,1,iFecC(is))=0.002_r8
              t(i,j,k,1,iFecN(is))=t(i,j,k,1,iFecC(is))*cff11
              t(i,j,k,1,iFecP(is))=t(i,j,k,1,iFecC(is))*cff12
              t(i,j,k,1,iFecF(is))=t(i,j,k,1,iFecC(is))*cff13
              t(i,j,k,1,iFecS(is))=t(i,j,k,1,iFecC(is))*cff11
            END DO
          END DO
        END DO
      END DO
#elif defined BEST_NPZ
!
!-----------------------------------------------------------------------
! This set of initial conditions starts with an empty Bering Sea shelf;
! NO3 is only present at depth, with no NH4 or detritus anywhere.
! Living critters are seeded with only a tiny amount to allow for future
! growth.  This start condition is intended to allow the model domain to
! move towards its own internally-regulated nutrient/benthos steady-
! state
!-----------------------------------------------------------------------
!
      DO i=IstrR,IendR
        DO j=JstrR,JendR

# ifdef IRON_LIMIT
          ! Iron top/bottom values set based on bottom (matches nudging)
          FeSurf = CalcLinearCapped(Feinh(ng), Feinlo(ng), Feoffh(ng), Feofflo(ng), h(i,j))
          FeDeep = CalcLinearCapped(Feinh(ng), Feinhi(ng), Feoffh(ng), Feoffhi(ng), h(i,j))
# endif
          DO k=1,N(ng)
# ifdef IRON_LIMIT

            ! Iron: depth-dependant linear-capped profile (same as nudging)

            t(i,j,k,1,iFe) = CalcLinearCapped(-50.0_r8, FeSurf, -300.0_r8, FeDeep, z_r(i,j,k))
# endif
            ! Nitrate: depth-dependant linear-capped profile (0 above 100, 40 below 300
            ! based very roughly on World Ocean Atlas 2013 January climatological values
            ! at bottom depth in the Bering Sea)

            t(i,j,k,1,iNO3) = CalcLinearCapped(-100.0_r8, 0.0_r8, -300.0_r8, 40.0_r8, z_r(i,j,k))

            ! NH4: 0

            t(i,j,k,1,iNH4) = 0.0_r8

            ! Phytoplankton and zooplankton: constant seed

            t(i,j,k,1,iPhS)  = eps
            t(i,j,k,1,iPhL)  = eps
            t(i,j,k,1,iMZL)  = eps
            t(i,j,k,1,iCop)  = eps
            t(i,j,k,1,iNCaS) = eps
            t(i,j,k,1,iNCaO) = eps
            t(i,j,k,1,iEupS) = eps
            t(i,j,k,1,iEupO) = eps
# ifdef JELLY
            t(i,j,k,1,iJel) = eps
# endif

            ! Detritus: nothing

            t(i,j,k,1,iDet) =  0.0_r8
	          t(i,j,k,1,iDetF) =  0.0_r8

          END DO
# ifdef BENTHIC
          DO k = 1,NBL(ng)

            ! Benthos

            bt(i,j,k,1,iBen) = eps

            ! Benthic detritus

		        bt(i,j,k,1,iDetBen) = 0.0_r8

          END DO
# endif
# ifdef ICE_BIO

          ! Ice: start with no-ice conditions

          it(i,j,1,idice(1))  =  0.0_r8
          it(i,j,1,idice(2))  =  0.0_r8
          it(i,j,1,idice(3))  =  0.0_r8

#  endif
# endif

        END DO
      END DO
#elif defined BIO_GOANPZ
# include "ana_biology_goanpz.h"
#endif
!
!  Report statistics.
!
      DO itrc=1,NBT
        i=idbio(itrc)
        CALL stats_3dfld (ng, tile, iNLM, u3dvar, Stats(itrc),          &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), t(:,:,:,1,i))
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          WRITE (stdout,10) TRIM(Vname(2,idTvar(i)))//': '//            &
     &                      TRIM(Vname(1,idTvar(i))),                   &
     &                      ng, Stats(itrc)%min, Stats(itrc)%max
        END IF
      END DO
!
  10  FORMAT (3x,' ANA_BIOLOGY - ',a,/,19x,                             &
     &        '(Grid = ',i2.2,', Min = ',1p,e15.8,0p,                   &
     &                         ' Max = ',1p,e15.8,0p,')')

      RETURN
      END SUBROUTINE ana_biology_tile
