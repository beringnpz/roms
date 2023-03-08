      SUBROUTINE biology (ng,tile)
!
!svn $Id$
!************************************************** Hernan G. Arango ***
!  Copyright (c) 2002-2020 The ROMS/TOMS Group        Craig V. Lewis   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!***********************************************************************
!                                                                      !
!  This subroutine implements source/sink equations for the Banas      !
!  model:                                                              !
!                                                                      !
!    Banas NS, Zhang J, Campbell RG, Sambrotto RN, Lomas MW, Sherr E,  !
!    Sherr B, Ashjian C, Stoecker D, Lessard EJ (2016) Spring plankton !
!    dynamics in the Eastern Bering Sea, 1971–2050: Mechanisms of      !
!    interannual variability diagnosed with a numericalmodel. J Geophys!
!    Res 121:3372–3380                                                 !
!                                                                      !
!  It is intended to represent the planktonic food web of the eastern  !
!  Bering Sea shelf.                                                   !
!                                                                      !
!  Model implemented by K. Kearney, 2022.  Any errors in interpreting  !
!  the above paper are my own.  Contact: kelly.kearney@noaa.gov        !
!                                                                      !
!***********************************************************************
!
      USE mod_param
#ifdef DIAGNOSTICS_BIO
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
      USE mod_mixing
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
#include "tile.h"
!
!  Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15, __LINE__, __FILE__)
#endif
      CALL biology_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
#ifdef MASKING
     &                   GRID(ng) % rmask,                              &
#endif
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   FORCES(ng) % srflx,                            &
     &                   MIXING(ng) % Akt,                              &
#ifdef OPTIC_MANIZZA
     &                   OCEAN(ng) % decayW,                            &
#endif
#ifdef DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio3d,                          &
#endif
     &                   OCEAN(ng) % t)

#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15, __LINE__, __FILE__)
#endif
      RETURN
      END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#ifdef MASKING
     &                         rmask,                                   &
#endif
     &                         Hz, z_r, z_w, srflx, AKt,                &
#ifdef OPTIC_MANIZZA
     &                         decayW,                                  &
#endif
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio3d,                                &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
      real(r8), intent(in) :: Akt(LBi:,LBj:,0:,:)
# ifdef OPTIC_MANIZZA
      real(r8), intent(in) :: decayW(LBi:,LBj:,0:,:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Akt(LBi:UBi,LBj:UBj,0:UBk,NAT)
# ifdef OPTIC_MANIZZA
      real(r8), intent(in) :: decayW(LBi:UBi,LBj:UBj,0:UBk,4)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
!
!  Local variable declarations.
!
      integer, parameter :: Nsink = 2

      integer :: Iter, i, ibio, isink, itrc, itrmx, j, k, ks, itime, iTrcMax

      integer, dimension(Nsink) :: idsink

      real(r8), parameter :: eps = 1.0e-16_r8
      real(r8), parameter :: MinVal = 1.0e-6_r8 ! KK TODO: revisit... probably too high

      real(r8) :: cff, cff1, cff2, cff3, dtdays
      real(r8) :: cffL, cffR, cu, dltL, dltR

      real(r8), dimension(Nsink) :: Wbio

      integer, dimension(IminS:ImaxS,N(ng)) :: ksource

      real(r8), dimension(NT(ng),2) :: BioTrc

      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio_old

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC

      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: qc
      real(r8), dimension(IminS:ImaxS,N(ng)) :: PAR

      real(r8) :: E0, maxkappa, Eeff, alpha, Ntot, mu, qP, qZ, qR, I_P

      real(r8), dimension(IminS:ImaxS,N(ng)) :: npp, gra, agg, pmor
      real(r8), dimension(IminS:ImaxS,N(ng)) :: zmor, srem, lrem, nit

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Add biological Source/Sink terms.
!-----------------------------------------------------------------------
!
!  Avoid computing source/sink terms if no biological iterations.
!
      IF (BioIter(ng).le.0) RETURN
!
!  Set time-stepping according to the number of iterations.
!
      dtdays=dt(ng)*sec2day/REAL(BioIter(ng),r8)

!
!  Set vertical sinking indentification vector and vertical sinking
!  velocity vector
!  KK: If you use the sinking routine (see SINK_LOOP, below), you need to
!      set up these two vectors.  idsink is an array of the idbio indices
!      of things that sink, and Wbio is array of the respective sinking
!      velocities (m/s).  Also be sure to update the Nsink parameter
!      (above, in local variable declarations) so these arrays are the
!      right size for your model.
!
      idsink(1)=idets                 ! Small detritus
      idsink(2)=idetl                 ! Large detritus

      Wbio(1)=w_S(ng)                 ! Small detritus, m/d
      Wbio(2)=w_L(ng)                 ! Large detritus, m/d
!
!       Wbio(1)=wDet(ng)
      J_LOOP : DO j=Jstr,Jend
!
!  Compute inverse thickness to avoid repeated divisions.
!  KK: This code is preserved in a lot of models (although most only use
!      the Hz_inv variable) based on the fact that multiplication is
!      faster than division.  These factors are used whenever converting
!      from transport units to tracer units (see note below).  I'm
!      skeptical whether the speed gain outweighs the decrease in human-
!      legibility of code... but oh well, I'll follow the crowd.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
        DO k=1,N(ng)-1
          DO i=Istr,Iend
            Hz_inv2(i,k)=1.0_r8/(Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
        DO k=2,N(ng)-1
          DO i=Istr,Iend
            Hz_inv3(i,k)=1.0_r8/(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
!
!  Restrict biological tracer to be positive definite. If a negative
!  concentration is detected, nitrogen is drawn from the most abundant
!  pool to supplement the negative pools to a lower limit of MinVal
!  which is set to 1E-6 above.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
!
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
            DO itrc=1,NBT
              ibio=idbio(itrc)
              BioTrc(ibio,nstp)=t(i,j,k,nstp,ibio)
              BioTrc(ibio,nnew)=t(i,j,k,nnew,ibio)*Hz_inv(i,k)
            END DO
!
!  Impose positive definite concentrations.
!
            cff2=0.0_r8
            DO itime=1,2
              cff1=0.0_r8
              iTrcMax=idbio(1)
              DO itrc=1,NBT
                ibio=idbio(itrc)
                cff1=cff1+MAX(0.0_r8,MinVal-BioTrc(ibio,itime))
                IF (BioTrc(ibio,itime).gt.BioTrc(iTrcMax,itime)) THEN
                  iTrcMax=ibio
                END IF
                BioTrc(ibio,itime)=MAX(MinVal,BioTrc(ibio,itime))
              END DO
              IF (BioTrc(iTrcMax,itime).gt.cff1) THEN
                BioTrc(iTrcMax,itime)=BioTrc(iTrcMax,itime)-cff1
              END IF
            END DO
!
!  Load biological tracers into local arrays.
!
            DO itrc=1,NBT
              ibio=idbio(itrc)
              Bio_old(i,k,ibio)=BioTrc(ibio,nstp)
              Bio(i,k,ibio)=BioTrc(ibio,nstp)
            END DO
          END DO
        END DO
!
!=======================================================================
!  Start internal iterations
!=======================================================================
!
!  Note: Most variable names reflect the symbols used in Banas et al.,
!        2016 (doi.org/10.1002/2015JC011449).
!
        ITER_LOOP: DO Iter=1,BioIter(ng)

!
!-----------------------------------------------------------------------
!  Source/sink
!-----------------------------------------------------------------------
!
          DO i=Istr,Iend

            ! Light

#ifdef OPTIC_MANIZZA
            E0 = (decayW(i,j,N(ng),3) + decayW(i,j,N(ng),4)) *          &
     &            srflx(i,j) * rho0 * Cp ! W m^-2, surface
#else
            ! TODO: implement Banas Eq. 10?
#endif
            maxkappa = maxval(Akt(i,j,:,itemp))/sec2day  ! diffusivity m d^-1
            DO k=1,N(ng)
#ifdef OPTIC_MANIZZA
              PAR(i,k) = (decayW(i,j,k,3) + decayW(i,j,k,4)) *          &
     &                   srflx(i,j) * rho0 * Cp ! W m^-2
#else
              ! TODO: implement implement Banas Eq. 10?
#endif

              ! Phytoplankton growth parameters

              Eeff = E0 * exp(-att_sw(ng) * sqrt(maxkappa/mu0(ng)))
              alpha = alpha_win(ng) + 0.5*(alpha_sum(ng) - alpha_win(ng)) *  &
     &                  (1 + TANH((Eeff - Ecrit(ng))/deltaE(ng)))

              Ntot = Bio(i,k,ino3) + phi_NH4(ng) * Bio(i,k,inh4)

              mu = (alpha*PAR(i,k)/sqrt(alpha**2*PAR(i,k)**2+mu0(ng)**2)) * &
     &             (Ntot/(kmin(ng) + 2*sqrt(kmin(ng)*Ntot) + Ntot)) * mu0(ng)

              ! Q10 rates

              qP = Q_P(ng) ** (t(i,j,k,nstp,itemp)/10)
              qZ = Q_Z(ng) ** (t(i,j,k,nstp,itemp)/10)
              qR = Q_R(ng) ** (t(i,j,k,nstp,itemp)/10)

              ! Zooplankton grazing on phytoplankton parameters

              I_P = I0(ng) * Bio(i,k,iphyto)/(Kgraz(ng) + Bio(i,k,iphyto))

              ! Intermediate fluxes

              npp( i,k) = qP*mu*Bio(i,k,iphyto)
              gra( i,k) = qZ*I_P*Bio(i,k,izoo)
              agg( i,k) = qP*m_agg(ng)*Bio(i,k,iphyto)**2
              pmor(i,k) = qR*m_P(ng)*Bio(i,k,iphyto)
              zmor(i,k) = qZ*m_Z(ng)*Bio(i,k,izoo)**2
              srem(i,k) = qR*r_remin(ng)*Bio(i,k,idets)
              lrem(i,k) = qR*r_remin(ng)*Bio(i,k,idetl)
              nit( i,k) = qR*r_nitr(ng)*Bio(i,k,inh4)

#ifdef DIAGNOSTICS_BIO
              DiaBio3d(i,j,k,iflxnpp)  = npp( i,k)
              DiaBio3d(i,j,k,iflxgra)  = gra( i,k)
              DiaBio3d(i,j,k,iflxagg)  = agg( i,k)
              DiaBio3d(i,j,k,iflxpmor) = pmor(i,k)
              DiaBio3d(i,j,k,iflxzmor) = zmor(i,k)
              DiaBio3d(i,j,k,iflxsrem) = srem(i,k)
              DiaBio3d(i,j,k,iflxlrem) = lrem(i,k)
              DiaBio3d(i,j,k,iflxnit)  = nit( i,k)
#endif

              ! Apply biomass change

              Bio(i,k,iphyto) = Bio(i,k,iphyto) +                       &
     &                     (  npp(i,k)                                  &
     &                      - gra(i,k)                                  &
     &                      - pmor(i,k)                                 &
     &                      - agg(i,k))*dtdays

              Bio(i,k,izoo  ) = Bio(i,k,izoo  ) +                       &
     &                     (  epsil(ng)*gra(i,k)                        &
     &                      - zmor(i,k))*dtdays

              Bio(i,k,idets ) = Bio(i,k,idets ) +                       &
     &                     (  (1-epsil(ng)-fex(ng))*gra(i,k)            &
     &                      + pmor(i,k)                                 &
     &                      - srem(i,k))*dtdays

              Bio(i,k,idetl ) = Bio(i,k,idetl ) +                       &
     &                     (  agg(i,k) -                                &
     &                      - lrem(i,k))*dtdays

              Bio(i,k,inh4  ) = Bio(i,k,inh4  ) +                       &
     &                     (-(phi_NH4(ng)*Bio(i,k,inh4)/Ntot)*npp(i,k)  &
     &                      + fex(ng)*gra(i,k)                          &
     &                      + srem(i,k)                                 &
     &                      + lrem(i,k)                                 &
     &                      - nit(i,k))*dtdays

              Bio(i,k,ino3  ) = Bio(i,k,ino3  ) +                       &
     &                     (-(Bio(i,k,ino3)/Ntot)*npp(i,k)              &
     &                      + nit(i,k))*dtdays

            END DO
          END DO
!
!-----------------------------------------------------------------------
!  Vertical sinking terms.
!-----------------------------------------------------------------------
!
!  Reconstruct vertical profile of selected biological constituents
!  "Bio(:,:,isink)" in terms of a set of parabolic segments within each
!  grid box. Then, compute semi-Lagrangian flux due to sinking.
!
          SINK_LOOP: DO isink=1,Nsink
            ibio=idsink(isink)
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                qc(i,k)=Bio(i,k,ibio)
              END DO
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
              END DO
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                dltR=Hz(i,j,k)*FC(i,k)
                dltL=Hz(i,j,k)*FC(i,k-1)
                cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                cffR=cff*FC(i,k)
                cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                IF ((dltR*dltL).le.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                cff=(dltR-dltL)*Hz_inv3(i,k)
                dltR=dltR-cff*Hz(i,j,k+1)
                dltL=dltL+cff*Hz(i,j,k-1)
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
                WR(i,k)=(2.0_r8*dltR-dltL)**2
                WL(i,k)=(dltR-2.0_r8*dltL)**2
              END DO
            END DO
            cff=1.0E-14_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
                dltL=MAX(cff,WL(i,k  ))
                dltR=MAX(cff,WR(i,k+1))
                bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                bL(i,k+1)=bR(i,k)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
              bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
              bL(i,N(ng))=qc(i,N(ng))       ! conditions
              bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
              bR(i,1)=bL(i,2)
              bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
              bR(i,1)=bL(i,2)
              bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
              bL(i,2)=qc(i,1)               ! bottom grid boxes are
              bR(i,1)=qc(i,1)               ! re-assumed to be
              bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
            END DO
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                dltR=bR(i,k)-qc(i,k)
                dltL=qc(i,k)-bL(i,k)
                cffR=2.0_r8*dltR
                cffL=2.0_r8*dltL
                IF ((dltR*dltL).lt.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
              END DO
            END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
            cff=dtdays*ABS(Wbio(isink))
            DO k=1,N(ng)
              DO i=Istr,Iend
                FC(i,k-1)=0.0_r8
                WL(i,k)=z_w(i,j,k-1)+cff
                WR(i,k)=Hz(i,j,k)*qc(i,k)
                ksource(i,k)=k
              END DO
            END DO
            DO k=1,N(ng)
              DO ks=k,N(ng)-1
                DO i=Istr,Iend
                  IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                    ksource(i,k)=ks+1
                    FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                  END IF
                END DO
              END DO
            END DO
!
!  Finalize computation of flux: add fractional part.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                ks=ksource(i,k)
                cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                FC(i,k-1)=FC(i,k-1)+                                    &
     &                    Hz(i,j,ks)*cu*                                &
     &                    (bL(i,ks)+                                    &
     &                     cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-              &
     &                         (1.5_r8-cu)*                             &
     &                         (bR(i,ks)+bL(i,ks)-                      &
     &                          2.0_r8*qc(i,ks))))
              END DO
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
              END DO
            END DO

          END DO SINK_LOOP
        END DO ITER_LOOP
!
!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of nutrients
!  when advection causes tracer concentration to go negative.
!-----------------------------------------------------------------------
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff=Bio(i,k,ibio)-Bio_old(i,k,ibio)
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)+cff*Hz(i,j,k)
            END DO
          END DO
        END DO

      END DO J_LOOP

      RETURN
      END SUBROUTINE biology_tile
