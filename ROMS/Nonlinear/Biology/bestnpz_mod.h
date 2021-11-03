!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
! 
! Parameters for the BESTNPZ model:
!
!   PARfrac     Fraction of irradiance that is photosynthetically      !
!                 available (PAR) (unitless)                           !
!   k_ext       Clear-water attenuation coefficient (m^-1)             !
!   k_chlA      Chlorophyll attenuation coefficient, factor (m^-1)     !
!   k_chlB      Chlorophyll attenuation coefficient, exponent          !
!                 (unitless)                                           !
!   k_chlC      Other material (CDOM,sediment,etc.) attenuation        !
!                 coefficient (m^-1)                                   !
!   k_sed1      Depth-based attenuation coefficient, factor (m^-1)     !
!   k_sed2      Depth-based attenuation coefficient, exponent          !
!                 (unitless)                                           !
!   xi          Nitrogen:Carbon ratio (mmol N / mg C)                  !
!   ccr         Carbon:Chlorophyll ratio, small phyto (mg C / mg       !
!                 Chl-a)                                               !
!   ccrPhL      Carbon:Chlorophyll ratio, large phyto (mg C / mg       !
!                 Chl-a)                                               !
!   FeC         Fe:Carbon ratio    (2 umol Fe : mol C) (umol Fe / mg   !
!                 C)                                                   !
!   DiS         Doubling rate parameter (d^-1)                         !
!   DiL         Doubling rate parameter (d^-1)                         !
!   DpS         Doubling rate exponent (degC^-1)                       !
!   DpL         Doubling rate exponent (degC^-1)                       !
!   alphaPhS    photosynthetic efficiency (mg C m^2 (mg Chl-a)^-1      !
!                 (E^-1))                                              !
!   alphaPhL    photosynthetic efficiency (mg C m^2 (mg Chl-a)^-1      !
!                 (E^-1))                                              !
!   k1PhS       Half-saturation constant for NO3 limitation            !
!   k1PhL       Half-saturation constant for NO3 limitation            !
!   k2PhS       Half-saturation constant for NH4 limitation            !
!   k2PhL       Half-saturation constant for NH4 limitation            !
!   FeCritPS    Threshold below which PhS is limited (umol Fe m^-3)    !
!   FeCritPL    Threshold below which PhS is limited (umol Fe m^-3)    !
!   kfePhS      Half-saturation constant for Fe (umol m^-3)            !
!   kfePhL      Half-saturation constant for Fe (umol m^-3)            !
!   fpPhSMZL    PhS->MZL  Feeding preference                           !
!   fpPhLMZL    PhL->MZL  Feeding preference                           !
!   fpPhSCop    PhS->Cop  Feeding preference                           !
!   fpPhLCop    PhL->Cop  Feeding preference                           !
!   fpMZLCop    MZL->Cop  Feeding preference                           !
!   fpPhSNCa    PhS->NCa  Feeding preference                           !
!   fpPhLNCa    PhL->NCa  Feeding preference                           !
!   fpMZLNCa    MZL->NCa  Feeding preference                           !
!   fpPhSEup    PhS->Eup  Feeding preference                           !
!   fpPhLEup    PhL->Eup  Feeding preference                           !
!   fpMZLEup    MZL->Eup  Feeding preference                           !
!   fpCopEup    Cop->Eup  Feeding preference                           !
!   fpDetEup    Det->Eup  Feeding preference                           !
!   fpDetEupO   Det->EupO Feeding preference                           !
!   fpCopJel    Cop->Jel  Feeding preference                           !
!   fpNCaJel    NCa->Jel  Feeding preference                           !
!   fpEupJel    Eup->Jel  Feeding preference                           !
!   eMZL        maximum specific ingestion rate (mg C/mg C/d)          !
!   eCop        maximum specific ingestion rate (mg C/mg C/d)          !
!   eNCa        maximum specific ingestion rate (mg C/mg C/d)          !
!   eEup        maximum specific ingestion rate (mg C/mg C/d)          !
!   eJel        maximum specific ingestion rate (mg C/mg C/d)          !
!   Q10MZL      Q10 for growth rate (unitless)                         !
!   Q10Cop      Q10 for growth rate (unitless)                         !
!   Q10NCa      Q10 for growth rate (unitless)                         !
!   Q10Eup      Q10 for growth rate (unitless)                         !
!   Q10Jele     Q10 for growth rate (unitless)                         !
!   Q10MZLT     Temperature coefficient for Q10 (deg. C)               !
!   Q10CopT     Temperature coefficient for Q10 (deg. C)               !
!   Q10NCaT     Temperature coefficient for Q10 (deg. C)               !
!   Q10EupT     Temperature coefficient for Q10 (deg. C)               !
!   Q10JelTe    Temperature coefficient for Q10 (deg. C)               !
!   fMZL        Half-saturation constant for grazing (mg C/m3)         !
!   fCop        Half-saturation constant for grazing (mg C/m3)         !
!   fNCa        Half-saturation constant for grazing (mg C/m3)         !
!   fEup        Half-saturation constant for grazing (mg C/m3)         !
!   fJel        Half-saturation constant for grazing (mg C/m3)         !
!   gammaMZL    Growth efficiency                                      !
!   gammaCop    Growth efficiency                                      !
!   gammaNCa    Growth efficiency                                      !
!   gammaEup    Growth efficiency                                      !
!   gammaJel    Growth efficiency                                      !
!   mPhS        daily linear mortality rate (senescence) (1/d)         !
!   mPhL        daily linear mortality rate (senescence) (1/d)         !
!   mMZL        daily linear mortality rate (1/d)                      !
!   mpredMZL    Daily mortality for Large Microzoo. (1/d/mgC)          !
!   mpredCop    Daily mortality for Copepods (1/d/mgC)                 !
!   mpredNCa    Daily mortality for Neocalanus (1/d/mgC)               !
!   mpredEup    Daily mortality for Euphausiids (1/d/mgC)              !
!   mpredJel    Daily mortality for Large Microzoo. (1/d/mgC)          !
!   wPhS        Sinking rate for Small Phytoplankton (m/d)             !
!   wPhL        Sinking rate for Large Phytoplankton (m/d)             !
!   wDet        Sinking rate for Detritus (m/d)                        !
!   wDetF       Sinking rate for Detritus (m/d)                        !
!   respPhS     Specific respiration rate (d^-1)                       !
!   respPhL     Specific respiration rate (d^-1)                       !
!   respMZL     Specific respiration rate (d^-1)                       !
!   respCop     Specific respiration rate (d^-1)                       !
!   respNCa     Specific respiration rate (d^-1)                       !
!   respEup     Specific respiration rate (d^-1)                       !
!   respJel     Specific respiration rate (d^-1)                       !
!   Q10Jelr     Q10 for respiration rate, jellyfish (degC)             !
!   Q10JelTr    reference temperature for Q10 respiration, jellyfish   !
!                 (1/degC)                                             !
!   KtBm_PhS    temperature coefficient for respiration (1/deg C)      !
!   KtBm_PhL    temperature coefficient for respiration (1/deg C)      !
!   KtBm_MZL    temperature coefficient for respiration (1/deg C)      !
!   ktbmC       temperature coefficient for respiration (1/deg C)      !
!   ktbmN       temperature coefficient for respiration (1/deg C)      !
!   ktbmE       temperature coefficient for respiration (1/deg C)      !
!   TmaxPhS     reference temperature for respiration (degC)           !
!   TmaxPhL     reference temperature for respiration (degC)           !
!   TmaxMZL     reference temperature for respiration (degC)           !
!   TrefC       reference temperature for respiration (degC)           !
!   TrefN       reference temperature for respiration (degC)           !
!   TrefE       reference temperature for respiration (degC)           !
!   Feinlo      inshore/surface (micromol Fe m-3 or nM)                !
!   Feinhi      inshore/deep (micromol Fe m-3 or nM)                   !
!   Feinh       inshore isobath of transition (m)                      !
!   Feofflo     offshore/surface (micromol Fe m-3 or nM)               !
!   Feoffhi     offshore/deep (micromol Fe m-3 or nM)                  !
!   Feoffh      offshore isobath of transition (m)                     !
!   wNCrise     upward velocity , tuned not data (m/day)               !
!   wNCsink     downward velocity , tuned not data (m/day)             !
!   RiseStart   Date NCaO begin to move upward (Day of Year)           !
!   RiseEnd     Date NCaO stop moving upward (Day of Year)             !
!   SinkStart   Date NCaO begin to move downward (Day of Year)         !
!   SinkEnd     Date NCaO stop moving downward (Day of Year)           !
!   RiseStartCM Date NCaS begin to move upward (Day of Year)           !
!   RiseEndCM   Date NCaS stop moving upward (Day of Year)             !
!   SinkStartCM Date NCaS begin to move downward (Day of Year)         !
!   SinkEndCM   Date NCaS stop moving downward (Day of Year)           !
!   Pv0         PON decompositon at 0 deg C (1/d)                      !
!   PvT         Temperature coefficient for remineralization (1/deg C) !
!   Nitr0       Nitrification rate at 0C (1/d)                         !
!   ktntr       Temperature coefficient for nitrification (1/deg C)    !
!   KNH4Nit     Half saturation constant for nitrification (mmolN/m^3) !
!   ToptNtr     Optimal temperature for nitrification (degC)           !
!   q10r        Q10 for growth/feeding and mortality rate (unitless)   !
!   Rup         maximum specific ingestion rate (1/d)                  !
!   KupD        Half-saturation constant for feeding on benthic prey   !
!                 (mg C/m^2)                                           !
!   KupP        Half-saturation constant for feeding on pelagic prey   !
!                 (mg C/m^2)                                           !
!   LupD        Lower threshold for feeding on benthic prey (mg C/m^2) !
!   LupP        Lower threshold for feeding on pelagic prey (mg C/m^2) !
!   Qres        Active metabolic rate (1/d)                            !
!   Rres        Basal metabolism rate (1/d)                            !
!   rmort       linear mortality rate (1/d)                            !
!   eex         fraction of living food excreted (1 - growth           !
!                 efficiency)                                          !
!   eexD        fraction of detrital food excreted                     !
!   prefD       DetBen->Ben feeding preference                         !
!   prefPL      PhL->Ben feeding preference                            !
!   prefPS      PhS->Ben feeding preference                            !
!   T0benr      Reference temperature for growth/feeding rate (degC)   !
!   BenPred     Quadratic mortality rate due to undefined predation    !
!                 (1/mgC/d)                                            !
!   alphaIb     IcePhL Chl-a specific attenuation coefficient (W^-1    !
!                 m^-2)                                                !
!   betaI       IcePhL photosynthetic efficiency (W^-1 m^-2)           !
!   inhib       IcePhL NH4 inhibition on NO3 uptake (m^3/mmol N)       !
!   ksnut1      IcePhL half-saturation constant for NO3 (mmolN/m^3)    !
!   ksnut2      IcePhL half-saturation constant for NH4 (mmolN/m^3)    !
!   mu0         IcePhL maximum growth rate at 0 deg C (1/d)            !
!   R0i         IcePhL respiration rate (1/d)                          !
!   rg0         IcePhL mortality rate at 0 deg C (1/d)                 !
!   rg          IcePhL temperature coefficient for mortality (1/deg C) !
!   annit       IcePhL nitrification factor (1/d)                      !
!   aidz        Ice thickness (m)                                      !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
      integer, allocatable :: idbio(:)  ! Biological tracers
#ifdef BENTHIC
      integer, allocatable :: NBeT(:)   
      integer              :: NBEN      ! Benthic tracers
      integer, allocatable :: idben(:)   
      integer, allocatable :: idBeTvar(:)
      integer, allocatable :: hisBid(:,:)
      integer, allocatable :: avgBid(:,:)
      integer, allocatable :: rstBid(:,:)
#endif
#ifdef ICE_BIO
      integer, allocatable :: NIceT(:)
      integer, allocatable :: NIceLog(:)
      integer              :: NIB
      integer, allocatable :: idice(:)
      integer, allocatable :: idiceLog(:)
#endif
#ifdef DIAGNOSTICS_BIO
      integer, allocatable :: iDbio2(:) ! 2D Biological diagnostics
      integer, allocatable :: iDbio3(:) ! 3D Biological diagnostics
#endif
      
      integer :: iNO3               ! Nitrate
      integer :: iNH4               ! Ammonium
      integer :: iPhS               ! Small Phytoplankton
      integer :: iPhL               ! Large Phytoplankton
      integer :: iMZS               ! Small Microzooplankton
      integer :: iMZL               ! Large Microzooplankton
      integer :: iCop               ! Small Coastal Copepods
      integer :: iNCaS              ! Neocalanus
      integer :: iEupS              ! Euphausiids
      integer :: iNCaO              ! Neocalanus
      integer :: iEupO              ! Euphausiids
#ifdef JELLY
      integer :: iJel               ! Jellfish
#endif
      integer :: iDet               ! Detritus
      integer :: iDetF              ! Fast Sinking Detritus
#ifdef CLIM_ICE_1D
      integer :: i1CI
#endif
#ifdef AKT_3D
      integer ::iAKt3
#endif
#ifdef IRON_LIMIT
      integer :: iFe                ! Iron
#endif
#ifdef CARBON
      integer :: iTIC_              ! Total inorganic carbon
      integer :: iTAlk              ! Total alkalinity
#endif
#ifdef OXYGEN
      integer :: iOxyg              ! Dissolved oxygen concentration
#endif
#ifdef BENTHIC
      integer :: iBen
      integer :: iDetBen
#endif
#ifdef ICE_BIO
!       integer, pointer :: idice(:)  ! Ice tracers
# ifdef CLIM_ICE_1D
!       integer, pointer :: idiceLog(:)  ! Ice tracers
# endif
      integer :: idIcePhL
      integer :: idIceNO3
      integer :: idIceNH4
      integer :: idIceZ
      integer :: idIceLog
#endif
#ifdef DIAGNOSTICS_BIO
      integer :: iilims
      integer :: iiliml
      integer :: inolims
      integer :: inoliml
      integer :: inhlims
      integer :: inhliml
      integer :: ifelims
      integer :: ifeliml
      integer :: iflx_Gpp_NO3_PhS
      integer :: iflx_Gpp_NO3_PhL
      integer :: iflx_Gpp_NH4_PhS
      integer :: iflx_Gpp_NH4_PhL
      integer :: iflx_Gra_PhS_MZL
      integer :: iflx_Gra_PhL_MZL
      integer :: iflx_Ege_MZL_Det
      integer :: iflx_Gra_PhS_Cop
      integer :: iflx_Gra_PhL_Cop
      integer :: iflx_Gra_MZL_Cop
      integer :: iflx_Gra_IPhL_Cop
      integer :: iflx_Ege_Cop_DetF
      integer :: iflx_Gra_PhS_NCaS
      integer :: iflx_Gra_PhL_NCaS
      integer :: iflx_Gra_MZL_NCaS
      integer :: iflx_Gra_IPhL_NCaS
      integer :: iflx_Ege_NCaS_DetF
      integer :: iflx_Gra_PhS_NCaO
      integer :: iflx_Gra_PhL_NCaO
      integer :: iflx_Gra_MZL_NCaO
      integer :: iflx_Gra_IPhL_NCaO
      integer :: iflx_Ege_NCaO_DetF
      integer :: iflx_Gra_PhS_EupS
      integer :: iflx_Gra_PhL_EupS
      integer :: iflx_Gra_MZL_EupS
      integer :: iflx_Gra_Cop_EupS
      integer :: iflx_Gra_IPhL_EupS
      integer :: iflx_Gra_Det_EupS
      integer :: iflx_Gra_DetF_EupS
      integer :: iflx_Ege_EupS_DetF
      integer :: iflx_Gra_PhS_EupO
      integer :: iflx_Gra_PhL_EupO
      integer :: iflx_Gra_MZL_EupO
      integer :: iflx_Gra_Cop_EupO
      integer :: iflx_Gra_IPhL_EupO
      integer :: iflx_Gra_Det_EupO
      integer :: iflx_Gra_DetF_EupO
      integer :: iflx_Ege_EupO_DetF
      integer :: iflx_Gra_Cop_Jel
      integer :: iflx_Gra_EupS_Jel
      integer :: iflx_Gra_EupO_Jel
      integer :: iflx_Gra_NCaS_Jel
      integer :: iflx_Gra_NCaO_Jel
      integer :: iflx_Ege_Jel_DetF
      integer :: iflx_Mor_PhS_Det
      integer :: iflx_Mor_PhL_Det
      integer :: iflx_Mor_MZL_Det
      integer :: iflx_Mor_Cop_DetF
      integer :: iflx_Mor_NCaS_DetF
      integer :: iflx_Mor_EupS_DetF
      integer :: iflx_Mor_NCaO_DetF
      integer :: iflx_Mor_EupO_DetF
      integer :: iflx_Mor_Jel_DetF
      integer :: iflx_Res_PhS_NH4
      integer :: iflx_Res_PhL_NH4
      integer :: iflx_Res_MZL_NH4
      integer :: iflx_Res_Cop_NH4
      integer :: iflx_Res_NCaS_NH4
      integer :: iflx_Res_NCaO_NH4
      integer :: iflx_Res_EupS_NH4
      integer :: iflx_Res_EupO_NH4
      integer :: iflx_Res_Jel_NH4
      integer :: iflx_Rem_Det_NH4
      integer :: iflx_Rem_DetF_NH4
      integer :: iflx_Nit_NH4_NO3
      integer :: iflx_Gra_Det_Ben
      integer :: iflx_Gra_DetF_Ben
      integer :: iflx_Gra_PhS_Ben
      integer :: iflx_Gra_PhL_Ben
      integer :: iflx_Gra_DetB_Ben
      integer :: iflx_Exc_Ben_NH4
      integer :: iflx_Exc_Ben_DetB
      integer :: iflx_Res_Ben_NH4
      integer :: iflx_Mor_Ben_DetB
      integer :: iflx_Rem_DetB_NH4
      integer :: iflx_Gpp_INO3_IPhL
      integer :: iflx_Gpp_INH4_IPhL
      integer :: iflx_Res_IPhL_INH4
      integer :: iflx_Mor_IPhL_INH4
      integer :: iflx_Nit_INH4_INO3
      integer :: iflx_Twi_IPhL_PhL
      integer :: iflx_Twi_INO3_NO3
      integer :: iflx_Twi_INH4_NH4
      integer :: iflx_Ver_PhS_DetB
      integer :: iflx_Ver_PhS_Out
      integer :: iflx_Ver_PhL_DetB
      integer :: iflx_Ver_PhL_Out
      integer :: iflx_Ver_Det_DetB
      integer :: iflx_Ver_Det_Out
      integer :: iflx_Ver_DetF_DetB
      integer :: iflx_Ver_DetF_Out
      integer :: iflx_Ver_NCaO_DetB
      integer :: iflx_Ver_NCaS_DetF
      integer :: iflx_Ver_NCaS_DetB
      integer :: iflx_Frz_PhL_IPhL
      integer :: iflx_Frz_NO3_INO3
      integer :: iflx_Frz_NH4_INH4
      integer :: iflx_Frz_TIC
      integer :: iflx_Frz_Alk
      integer :: iflx_Adv_NO3
      integer :: iflx_Adv_NH4
      integer :: iflx_Adv_PhS
      integer :: iflx_Adv_PhL
      integer :: iflx_Adv_MZL
      integer :: iflx_Adv_Cop
      integer :: iflx_Adv_NCaS
      integer :: iflx_Adv_EupS
      integer :: iflx_Adv_NCaO
      integer :: iflx_Adv_EupO
      integer :: iflx_Adv_Det
      integer :: iflx_Adv_DetF
      integer :: iflx_Adv_Jel
      integer :: iflx_Adv_Fe
      integer :: iflx_Adv_TIC
      integer :: iflx_Adv_Alk
      integer :: iflx_Adv_Oxyg
      integer :: iprod_PhS
      integer :: iprod_PhL
      integer :: iprod_MZL
      integer :: iprod_Cop
      integer :: iprod_NCaS
      integer :: iprod_EupS
      integer :: iprod_NCaO
      integer :: iprod_EupO
      integer :: iprod_Jel
      integer :: iprod_Ben
      integer :: iprod_IcePhL
      integer :: itotprod
      integer :: itotresp
      integer :: itotrem
      integer :: io2flx
      integer :: ico2flx
      integer :: ipco2
      integer :: ibiomem_NO3
      integer :: ibiomem_NH4
      integer :: ibiomem_PhS
      integer :: ibiomem_PhL
      integer :: ibiomem_MZL
      integer :: ibiomem_Cop
      integer :: ibiomem_NCaS
      integer :: ibiomem_EupS
      integer :: ibiomem_NCaO
      integer :: ibiomem_EupO
      integer :: ibiomem_Det
      integer :: ibiomem_DetF
      integer :: ibiomem_Jel
      integer :: ibiomem_Fe
      integer :: ibiomem_TIC
      integer :: ibiomem_Alk
      integer :: ibiomem_Oxyg
      integer :: ipar
#endif
      
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)
      real(r8), allocatable :: PARfrac(:)
      real(r8), allocatable :: k_ext(:)
      real(r8), allocatable :: k_chlA(:)
      real(r8), allocatable :: k_chlB(:)
      real(r8), allocatable :: k_chlC(:)
      real(r8), allocatable :: k_sed1(:)
      real(r8), allocatable :: k_sed2(:)
      real(r8), allocatable :: xi(:)
      real(r8), allocatable :: ccr(:)
      real(r8), allocatable :: ccrPhL(:)
      real(r8), allocatable :: FeC(:)
      real(r8), allocatable :: DiS(:)
      real(r8), allocatable :: DiL(:)
      real(r8), allocatable :: DpS(:)
      real(r8), allocatable :: DpL(:)
      real(r8), allocatable :: alphaPhS(:)
      real(r8), allocatable :: alphaPhL(:)
      real(r8), allocatable :: k1PhS(:)
      real(r8), allocatable :: k1PhL(:)
      real(r8), allocatable :: k2PhS(:)
      real(r8), allocatable :: k2PhL(:)
      real(r8), allocatable :: FeCritPS(:)
      real(r8), allocatable :: FeCritPL(:)
      real(r8), allocatable :: kfePhS(:)
      real(r8), allocatable :: kfePhL(:)
      real(r8), allocatable :: fpPhSMZL(:)
      real(r8), allocatable :: fpPhLMZL(:)
      real(r8), allocatable :: fpPhSCop(:)
      real(r8), allocatable :: fpPhLCop(:)
      real(r8), allocatable :: fpMZLCop(:)
      real(r8), allocatable :: fpPhSNCa(:)
      real(r8), allocatable :: fpPhLNCa(:)
      real(r8), allocatable :: fpMZLNCa(:)
      real(r8), allocatable :: fpPhSEup(:)
      real(r8), allocatable :: fpPhLEup(:)
      real(r8), allocatable :: fpMZLEup(:)
      real(r8), allocatable :: fpCopEup(:)
      real(r8), allocatable :: fpDetEup(:)
      real(r8), allocatable :: fpDetEupO(:)
      real(r8), allocatable :: fpCopJel(:)
      real(r8), allocatable :: fpNCaJel(:)
      real(r8), allocatable :: fpEupJel(:)
      real(r8), allocatable :: eMZL(:)
      real(r8), allocatable :: eCop(:)
      real(r8), allocatable :: eNCa(:)
      real(r8), allocatable :: eEup(:)
      real(r8), allocatable :: eJel(:)
      real(r8), allocatable :: Q10MZL(:)
      real(r8), allocatable :: Q10Cop(:)
      real(r8), allocatable :: Q10NCa(:)
      real(r8), allocatable :: Q10Eup(:)
      real(r8), allocatable :: Q10Jele(:)
      real(r8), allocatable :: Q10MZLT(:)
      real(r8), allocatable :: Q10CopT(:)
      real(r8), allocatable :: Q10NCaT(:)
      real(r8), allocatable :: Q10EupT(:)
      real(r8), allocatable :: Q10JelTe(:)
      real(r8), allocatable :: fMZL(:)
      real(r8), allocatable :: fCop(:)
      real(r8), allocatable :: fNCa(:)
      real(r8), allocatable :: fEup(:)
      real(r8), allocatable :: fJel(:)
      real(r8), allocatable :: gammaMZL(:)
      real(r8), allocatable :: gammaCop(:)
      real(r8), allocatable :: gammaNCa(:)
      real(r8), allocatable :: gammaEup(:)
      real(r8), allocatable :: gammaJel(:)
      real(r8), allocatable :: mPhS(:)
      real(r8), allocatable :: mPhL(:)
      real(r8), allocatable :: mMZL(:)
      real(r8), allocatable :: mpredMZL(:)
      real(r8), allocatable :: mpredCop(:)
      real(r8), allocatable :: mpredNCa(:)
      real(r8), allocatable :: mpredEup(:)
      real(r8), allocatable :: mpredJel(:)
      real(r8), allocatable :: wPhS(:)
      real(r8), allocatable :: wPhL(:)
      real(r8), allocatable :: wDet(:)
      real(r8), allocatable :: wDetF(:)
      real(r8), allocatable :: respPhS(:)
      real(r8), allocatable :: respPhL(:)
      real(r8), allocatable :: respMZL(:)
      real(r8), allocatable :: respCop(:)
      real(r8), allocatable :: respNCa(:)
      real(r8), allocatable :: respEup(:)
      real(r8), allocatable :: respJel(:)
      real(r8), allocatable :: Q10Jelr(:)
      real(r8), allocatable :: Q10JelTr(:)
      real(r8), allocatable :: KtBm_PhS(:)
      real(r8), allocatable :: KtBm_PhL(:)
      real(r8), allocatable :: KtBm_MZL(:)
      real(r8), allocatable :: ktbmC(:)
      real(r8), allocatable :: ktbmN(:)
      real(r8), allocatable :: ktbmE(:)
      real(r8), allocatable :: TmaxPhS(:)
      real(r8), allocatable :: TmaxPhL(:)
      real(r8), allocatable :: TmaxMZL(:)
      real(r8), allocatable :: TrefC(:)
      real(r8), allocatable :: TrefN(:)
      real(r8), allocatable :: TrefE(:)
      real(r8), allocatable :: Feinlo(:)
      real(r8), allocatable :: Feinhi(:)
      real(r8), allocatable :: Feinh(:)
      real(r8), allocatable :: Feofflo(:)
      real(r8), allocatable :: Feoffhi(:)
      real(r8), allocatable :: Feoffh(:)
      real(r8), allocatable :: wNCrise(:)
      real(r8), allocatable :: wNCsink(:)
      real(r8), allocatable :: RiseStart(:)
      real(r8), allocatable :: RiseEnd(:)
      real(r8), allocatable :: SinkStart(:)
      real(r8), allocatable :: SinkEnd(:)
      real(r8), allocatable :: RiseStartCM(:)
      real(r8), allocatable :: RiseEndCM(:)
      real(r8), allocatable :: SinkStartCM(:)
      real(r8), allocatable :: SinkEndCM(:)
      real(r8), allocatable :: Pv0(:)
      real(r8), allocatable :: PvT(:)
      real(r8), allocatable :: Nitr0(:)
      real(r8), allocatable :: ktntr(:)
      real(r8), allocatable :: KNH4Nit(:)
      real(r8), allocatable :: ToptNtr(:)
#ifdef BENTHIC
      real(r8), allocatable :: q10r(:)
      real(r8), allocatable :: Rup(:)
      real(r8), allocatable :: KupD(:)
      real(r8), allocatable :: KupP(:)
      real(r8), allocatable :: LupD(:)
      real(r8), allocatable :: LupP(:)
      real(r8), allocatable :: Qres(:)
      real(r8), allocatable :: Rres(:)
      real(r8), allocatable :: rmort(:)
      real(r8), allocatable :: eex(:)
      real(r8), allocatable :: eexD(:)
      real(r8), allocatable :: prefD(:)
      real(r8), allocatable :: prefPL(:)
      real(r8), allocatable :: prefPS(:)
      real(r8), allocatable :: T0benr(:)
      real(r8), allocatable :: BenPred(:)
#endif
#ifdef ICE_BIO
      real(r8), allocatable :: alphaIb(:)
      real(r8), allocatable :: betaI(:)
      real(r8), allocatable :: inhib(:)
      real(r8), allocatable :: ksnut1(:)
      real(r8), allocatable :: ksnut2(:)
      real(r8), allocatable :: mu0(:)
      real(r8), allocatable :: R0i(:)
      real(r8), allocatable :: rg0(:)
      real(r8), allocatable :: rg(:)
      real(r8), allocatable :: annit(:)
      real(r8), allocatable :: aidz(:)
#endif

      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================
!
!  Local variable declarations
!
      integer :: i, ic
!
!-----------------------------------------------------------------------
!  Determine number of biological tracers.                                                                                                                                                    
!-----------------------------------------------------------------------                                                                                                                      
!  

#  if   (!defined JELLY && !defined IRON_LIMIT && !defined CARBON && !defined OXYGEN)
      NBT = 12
#  elif ( defined JELLY && !defined IRON_LIMIT && !defined CARBON && !defined OXYGEN) || \ 
        (!defined JELLY &&  defined IRON_LIMIT && !defined CARBON && !defined OXYGEN) || \
        (!defined JELLY && !defined IRON_LIMIT && !defined CARBON &&  defined OXYGEN)
      NBT = 13
#  elif ( defined JELLY &&  defined IRON_LIMIT && !defined CARBON && !defined OXYGEN) || \
        (!defined JELLY && !defined IRON_LIMIT &&  defined CARBON && !defined OXYGEN) || \
        ( defined JELLY && !defined IRON_LIMIT && !defined CARBON &&  defined OXYGEN) || \
        (!defined JELLY &&  defined IRON_LIMIT && !defined CARBON &&  defined OXYGEN)
      NBT = 14
#  elif ( defined JELLY && !defined IRON_LIMIT &&  defined CARBON && !defined OXYGEN) || \
        (!defined JELLY &&  defined IRON_LIMIT &&  defined CARBON && !defined OXYGEN) || \
        ( defined JELLY &&  defined IRON_LIMIT && !defined CARBON &&  defined OXYGEN) || \
        (!defined JELLY && !defined IRON_LIMIT &&  defined CARBON &&  defined OXYGEN)
      NBT = 15
#  elif ( defined JELLY &&  defined IRON_LIMIT &&  defined CARBON && !defined OXYGEN) || \
        ( defined JELLY && !defined IRON_LIMIT &&  defined CARBON &&  defined OXYGEN) || \
        (!defined JELLY &&  defined IRON_LIMIT &&  defined CARBON &&  defined OXYGEN)
      NBT = 16
#  elif ( defined JELLY &&  defined IRON_LIMIT &&  defined CARBON &&  defined OXYGEN)
      NBT = 17
# endif
   
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
      NDbio3d=155
      NDbio2d=3
#endif
#ifdef BENTHIC
      NBEN=2
#endif
#ifdef ICE_BIO
      NIB=4
#endif
              
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(PARfrac)) THEN
        allocate ( PARfrac(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_ext)) THEN
        allocate ( k_ext(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_chlA)) THEN
        allocate ( k_chlA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_chlB)) THEN
        allocate ( k_chlB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_chlC)) THEN
        allocate ( k_chlC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_sed1)) THEN
        allocate ( k_sed1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k_sed2)) THEN
        allocate ( k_sed2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(xi)) THEN
        allocate ( xi(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ccr)) THEN
        allocate ( ccr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ccrPhL)) THEN
        allocate ( ccrPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FeC)) THEN
        allocate ( FeC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DiS)) THEN
        allocate ( DiS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DiL)) THEN
        allocate ( DiL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DpS)) THEN
        allocate ( DpS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DpL)) THEN
        allocate ( DpL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alphaPhS)) THEN
        allocate ( alphaPhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alphaPhL)) THEN
        allocate ( alphaPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k1PhS)) THEN
        allocate ( k1PhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k1PhL)) THEN
        allocate ( k1PhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k2PhS)) THEN
        allocate ( k2PhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(k2PhL)) THEN
        allocate ( k2PhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FeCritPS)) THEN
        allocate ( FeCritPS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FeCritPL)) THEN
        allocate ( FeCritPL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kfePhS)) THEN
        allocate ( kfePhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kfePhL)) THEN
        allocate ( kfePhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhSMZL)) THEN
        allocate ( fpPhSMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhLMZL)) THEN
        allocate ( fpPhLMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhSCop)) THEN
        allocate ( fpPhSCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhLCop)) THEN
        allocate ( fpPhLCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpMZLCop)) THEN
        allocate ( fpMZLCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhSNCa)) THEN
        allocate ( fpPhSNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhLNCa)) THEN
        allocate ( fpPhLNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpMZLNCa)) THEN
        allocate ( fpMZLNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhSEup)) THEN
        allocate ( fpPhSEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpPhLEup)) THEN
        allocate ( fpPhLEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpMZLEup)) THEN
        allocate ( fpMZLEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpCopEup)) THEN
        allocate ( fpCopEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpDetEup)) THEN
        allocate ( fpDetEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpDetEupO)) THEN
        allocate ( fpDetEupO(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpCopJel)) THEN
        allocate ( fpCopJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpNCaJel)) THEN
        allocate ( fpNCaJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fpEupJel)) THEN
        allocate ( fpEupJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eMZL)) THEN
        allocate ( eMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eCop)) THEN
        allocate ( eCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eNCa)) THEN
        allocate ( eNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eEup)) THEN
        allocate ( eEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eJel)) THEN
        allocate ( eJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10MZL)) THEN
        allocate ( Q10MZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10Cop)) THEN
        allocate ( Q10Cop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10NCa)) THEN
        allocate ( Q10NCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10Eup)) THEN
        allocate ( Q10Eup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10Jele)) THEN
        allocate ( Q10Jele(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10MZLT)) THEN
        allocate ( Q10MZLT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10CopT)) THEN
        allocate ( Q10CopT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10NCaT)) THEN
        allocate ( Q10NCaT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10EupT)) THEN
        allocate ( Q10EupT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10JelTe)) THEN
        allocate ( Q10JelTe(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fMZL)) THEN
        allocate ( fMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fCop)) THEN
        allocate ( fCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fNCa)) THEN
        allocate ( fNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fEup)) THEN
        allocate ( fEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fJel)) THEN
        allocate ( fJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gammaMZL)) THEN
        allocate ( gammaMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gammaCop)) THEN
        allocate ( gammaCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gammaNCa)) THEN
        allocate ( gammaNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gammaEup)) THEN
        allocate ( gammaEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gammaJel)) THEN
        allocate ( gammaJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mPhS)) THEN
        allocate ( mPhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mPhL)) THEN
        allocate ( mPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mMZL)) THEN
        allocate ( mMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mpredMZL)) THEN
        allocate ( mpredMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mpredCop)) THEN
        allocate ( mpredCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mpredNCa)) THEN
        allocate ( mpredNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mpredEup)) THEN
        allocate ( mpredEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mpredJel)) THEN
        allocate ( mpredJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPhS)) THEN
        allocate ( wPhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPhL)) THEN
        allocate ( wPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wDet)) THEN
        allocate ( wDet(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wDetF)) THEN
        allocate ( wDetF(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respPhS)) THEN
        allocate ( respPhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respPhL)) THEN
        allocate ( respPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respMZL)) THEN
        allocate ( respMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respCop)) THEN
        allocate ( respCop(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respNCa)) THEN
        allocate ( respNCa(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respEup)) THEN
        allocate ( respEup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(respJel)) THEN
        allocate ( respJel(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10Jelr)) THEN
        allocate ( Q10Jelr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q10JelTr)) THEN
        allocate ( Q10JelTr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KtBm_PhS)) THEN
        allocate ( KtBm_PhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KtBm_PhL)) THEN
        allocate ( KtBm_PhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KtBm_MZL)) THEN
        allocate ( KtBm_MZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ktbmC)) THEN
        allocate ( ktbmC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ktbmN)) THEN
        allocate ( ktbmN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ktbmE)) THEN
        allocate ( ktbmE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TmaxPhS)) THEN
        allocate ( TmaxPhS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TmaxPhL)) THEN
        allocate ( TmaxPhL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TmaxMZL)) THEN
        allocate ( TmaxMZL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TrefC)) THEN
        allocate ( TrefC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TrefN)) THEN
        allocate ( TrefN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TrefE)) THEN
        allocate ( TrefE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feinlo)) THEN
        allocate ( Feinlo(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feinhi)) THEN
        allocate ( Feinhi(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feinh)) THEN
        allocate ( Feinh(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feofflo)) THEN
        allocate ( Feofflo(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feoffhi)) THEN
        allocate ( Feoffhi(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Feoffh)) THEN
        allocate ( Feoffh(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wNCrise)) THEN
        allocate ( wNCrise(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wNCsink)) THEN
        allocate ( wNCsink(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RiseStart)) THEN
        allocate ( RiseStart(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RiseEnd)) THEN
        allocate ( RiseEnd(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SinkStart)) THEN
        allocate ( SinkStart(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SinkEnd)) THEN
        allocate ( SinkEnd(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RiseStartCM)) THEN
        allocate ( RiseStartCM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RiseEndCM)) THEN
        allocate ( RiseEndCM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SinkStartCM)) THEN
        allocate ( SinkStartCM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SinkEndCM)) THEN
        allocate ( SinkEndCM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Pv0)) THEN
        allocate ( Pv0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(PvT)) THEN
        allocate ( PvT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nitr0)) THEN
        allocate ( Nitr0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ktntr)) THEN
        allocate ( ktntr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KNH4Nit)) THEN
        allocate ( KNH4Nit(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ToptNtr)) THEN
        allocate ( ToptNtr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(q10r)) THEN
        allocate ( q10r(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Rup)) THEN
        allocate ( Rup(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KupD)) THEN
        allocate ( KupD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KupP)) THEN
        allocate ( KupP(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LupD)) THEN
        allocate ( LupD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LupP)) THEN
        allocate ( LupP(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Qres)) THEN
        allocate ( Qres(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Rres)) THEN
        allocate ( Rres(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rmort)) THEN
        allocate ( rmort(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eex)) THEN
        allocate ( eex(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(eexD)) THEN
        allocate ( eexD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(prefD)) THEN
        allocate ( prefD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(prefPL)) THEN
        allocate ( prefPL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(prefPS)) THEN
        allocate ( prefPS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(T0benr)) THEN
        allocate ( T0benr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(BenPred)) THEN
        allocate ( BenPred(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alphaIb)) THEN
        allocate ( alphaIb(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(betaI)) THEN
        allocate ( betaI(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(inhib)) THEN
        allocate ( inhib(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ksnut1)) THEN
        allocate ( ksnut1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ksnut2)) THEN
        allocate ( ksnut2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(mu0)) THEN
        allocate ( mu0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(R0i)) THEN
        allocate ( R0i(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rg0)) THEN
        allocate ( rg0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rg)) THEN
        allocate ( rg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(annit)) THEN
        allocate ( annit(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(aidz)) THEN
        allocate ( aidz(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
        Dmem(1)=Dmem(1)+REAL(NBT,r8)                                                                                                                                                          
      END IF  
# ifdef BENTHIC
      IF (.not.allocated(idben)) THEN
        allocate ( idben(NBEN) )
        Dmem(1)=Dmem(1)+REAL(NBEN,r8)
      END IF
# endif
# ifdef ICE_BIO
      IF (.not.allocated(idice)) THEN
        allocate ( idben(NIB) )
        Dmem(1)=Dmem(1)+REAL(NIB,r8)
      END IF
# endif
# ifdef DIAGNOSTICS_BIO   
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
        Dmem(1)=Dmem(1)+REAL(NDbio2d,r8)
      END IF
      
      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio2(NDbio3d) )
        Dmem(1)=Dmem(1)+REAL(NDbio3d,r8)
      END IF
#endif
! TODO: idice?

!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
      iNO3=ic+1
      iNH4=ic+2
      iPhS=ic+3
      iPhL=ic+4
      iMZS=ic+5
      iMZL=ic+6
      iCop=ic+7
      iNCaS=ic+8
      iEupS=ic+9
      iNCaO=ic+10
      iEupO=ic+11
      iDet=ic+12
      iDetF=ic+13
      ic = ic+13
# ifdef JELLY
      iJel=ic+1
      ic=ic+1
# endif
# ifdef IRON_LIMIT
      iJel=ic+1
      ic=ic+1
# endif
# ifdef CARBON
      iTIC_=ic+1
      iTAlk=ic+2
      ic=ic+2
# endif
# ifdef OXYGEN
      iOxyg=ic+1
      ic=ic+1
# endif 
# ifdef AKT_3D
      iAKt3=ic+1
      ic=ic+1
# endif

# ifdef DIAGNOSTICS_BIO
      DO i=1,NDbio3d
        iDbio3(i)=i
      END DO
      DO i=1,NDbio2d
        iDbio2(i)=i
      END DO
      
      iilims=1
      iiliml=2
      inolims=3
      inoliml=4
      inhlims=5
      inhliml=6
      ifelims=7
      ifeliml=8
      iflx_Gpp_NO3_PhS=9
      iflx_Gpp_NO3_PhL=10
      iflx_Gpp_NH4_PhS=11
      iflx_Gpp_NH4_PhL=12
      iflx_Gra_PhS_MZL=13
      iflx_Gra_PhL_MZL=14
      iflx_Ege_MZL_Det=15
      iflx_Gra_PhS_Cop=16
      iflx_Gra_PhL_Cop=17
      iflx_Gra_MZL_Cop=18
      iflx_Gra_IPhL_Cop=19
      iflx_Ege_Cop_DetF=20
      iflx_Gra_PhS_NCaS=21
      iflx_Gra_PhL_NCaS=22
      iflx_Gra_MZL_NCaS=23
      iflx_Gra_IPhL_NCaS=24
      iflx_Ege_NCaS_DetF=25
      iflx_Gra_PhS_NCaO=26
      iflx_Gra_PhL_NCaO=27
      iflx_Gra_MZL_NCaO=28
      iflx_Gra_IPhL_NCaO=29
      iflx_Ege_NCaO_DetF=30
      iflx_Gra_PhS_EupS=31
      iflx_Gra_PhL_EupS=32
      iflx_Gra_MZL_EupS=33
      iflx_Gra_Cop_EupS=34
      iflx_Gra_IPhL_EupS=35
      iflx_Gra_Det_EupS=36
      iflx_Gra_DetF_EupS=37
      iflx_Ege_EupS_DetF=38
      iflx_Gra_PhS_EupO=39
      iflx_Gra_PhL_EupO=40
      iflx_Gra_MZL_EupO=41
      iflx_Gra_Cop_EupO=42
      iflx_Gra_IPhL_EupO=43
      iflx_Gra_Det_EupO=44
      iflx_Gra_DetF_EupO=45
      iflx_Ege_EupO_DetF=46
      iflx_Gra_Cop_Jel=47
      iflx_Gra_EupS_Jel=48
      iflx_Gra_EupO_Jel=49
      iflx_Gra_NCaS_Jel=50
      iflx_Gra_NCaO_Jel=51
      iflx_Ege_Jel_DetF=52
      iflx_Mor_PhS_Det=53
      iflx_Mor_PhL_Det=54
      iflx_Mor_MZL_Det=55
      iflx_Mor_Cop_DetF=56
      iflx_Mor_NCaS_DetF=57
      iflx_Mor_EupS_DetF=58
      iflx_Mor_NCaO_DetF=59
      iflx_Mor_EupO_DetF=60
      iflx_Mor_Jel_DetF=61
      iflx_Res_PhS_NH4=62
      iflx_Res_PhL_NH4=63
      iflx_Res_MZL_NH4=64
      iflx_Res_Cop_NH4=65
      iflx_Res_NCaS_NH4=66
      iflx_Res_NCaO_NH4=67
      iflx_Res_EupS_NH4=68
      iflx_Res_EupO_NH4=69
      iflx_Res_Jel_NH4=70
      iflx_Rem_Det_NH4=71
      iflx_Rem_DetF_NH4=72
      iflx_Nit_NH4_NO3=73
      iflx_Gra_Det_Ben=74
      iflx_Gra_DetF_Ben=75
      iflx_Gra_PhS_Ben=76
      iflx_Gra_PhL_Ben=77
      iflx_Gra_DetB_Ben=78
      iflx_Exc_Ben_NH4=79
      iflx_Exc_Ben_DetB=80
      iflx_Res_Ben_NH4=81
      iflx_Mor_Ben_DetB=82
      iflx_Rem_DetB_NH4=83
      iflx_Gpp_INO3_IPhL=84
      iflx_Gpp_INH4_IPhL=85
      iflx_Res_IPhL_INH4=86
      iflx_Mor_IPhL_INH4=87
      iflx_Nit_INH4_INO3=88
      iflx_Twi_IPhL_PhL=89
      iflx_Twi_INO3_NO3=90
      iflx_Twi_INH4_NH4=91
      iflx_Ver_PhS_DetB=92
      iflx_Ver_PhS_Out=93
      iflx_Ver_PhL_DetB=94
      iflx_Ver_PhL_Out=95
      iflx_Ver_Det_DetB=96
      iflx_Ver_Det_Out=97
      iflx_Ver_DetF_DetB=98
      iflx_Ver_DetF_Out=99
      iflx_Ver_NCaO_DetB=100
      iflx_Ver_NCaS_DetF=101
      iflx_Ver_NCaS_DetB=102
      iflx_Frz_PhL_IPhL=103
      iflx_Frz_NO3_INO3=104
      iflx_Frz_NH4_INH4=105
      iprod_PhS=106
      iprod_PhL=107
      iprod_MZL=108
      iprod_Cop=109
      iprod_NCaS=110
      iprod_EupS=111
      iprod_NCaO=112
      iprod_EupO=113
      iprod_Jel=114
      iprod_Ben=115
      iprod_IcePhL=116
      ibiomem_NO3=117
      ibiomem_NH4=118
      ibiomem_PhS=119
      ibiomem_PhL=120
      ibiomem_MZL=121
      ibiomem_Cop=122
      ibiomem_NCaS=123
      ibiomem_EupS=124
      ibiomem_NCaO=125
      ibiomem_EupO=126
      ibiomem_Det=127
      ibiomem_DetF=128
      ibiomem_Jel=129
      ibiomem_Fe=130
      iflx_Adv_NO3=131
      iflx_Adv_NH4=132
      iflx_Adv_PhS=133
      iflx_Adv_PhL=134
      iflx_Adv_MZL=135
      iflx_Adv_Cop=136
      iflx_Adv_NCaS=137
      iflx_Adv_EupS=138
      iflx_Adv_NCaO=139
      iflx_Adv_EupO=140
      iflx_Adv_Det=141
      iflx_Adv_DetF=142
      iflx_Adv_Jel=143
      iflx_Adv_Fe=144
      ipar=145
      itotprod=146
      itotresp=147
      itotrem=148
      iflx_Frz_Alk=149
      
      ico2flx=1
      ipco2=2
      io2flx=3
# endif

      RETURN
      END SUBROUTINE initialize_biology
