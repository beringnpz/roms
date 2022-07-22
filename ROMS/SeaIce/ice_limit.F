#include "cppdefs.h"
      MODULE ice_limit_mod
#if defined ICE_ADVECT || defined ICE_THERMO
!
!============================================== W. Paul Budgell =======!
!  Copyright (c) 2002-2014 ROMS/TOMS Group                                  !
!============================================== Hernan G. Arango ======!
!                                                                      !
!  Limit ice tracer variables, for instance to not allow negative      !
!  ice concentration or thickness.                                     !
!                                                                      !
!======================================================================!
!
      implicit none

      PRIVATE
      PUBLIC ice_limit

      CONTAINS

      SUBROUTINE ice_limit (ng, tile)

      USE mod_param
      USE mod_ice
      USE mod_stepping
#ifdef ICE_BIO
      USE mod_ocean
#endif

      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
      CALL ice_limit_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     liold(ng), linew(ng),                        &
# if defined ICE_BIO
! # if defined ICE_BIO && defined BERING_10K
!      &                     ICE(ng) % IcePhL,                            &
!      &                     ICE(ng) % IceNO3,                            &
!      &                     ICE(ng) % IceNH4,                            &
!      &                     ICE(ng) % IceLog,                            &
     &                     OCEAN(ng) % it,                              &
# endif
     &                     ICE(ng) % ui,                                &
     &                     ICE(ng) % vi,                                &
     &                     ICE(ng) % ai,                                &
     &                     ICE(ng) % hi,                                &
     &                     ICE(ng) % hsn,                               &
     &                     ICE(ng) % ti,                                &
     &                     ICE(ng) % enthalpi                           &
     &                     )
!
      RETURN
      END SUBROUTINE ice_limit
!
!***********************************************************************
      SUBROUTINE ice_limit_tile (ng, tile,                              &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        liold, linew,                             &
# if defined ICE_BIO
     &                        it,                                       &
# endif

     &                        ui, vi, ai, hi, hsn, ti, enthalpi)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_scalars
      USE mod_boundary
!
      USE i2d_bc_mod, ONLY : i2d_bc_tile, i2d_bc_tile_closed
      USE tibc_mod, ONLY : tibc_tile
# ifdef ICE_BIO
      USE mod_biology, ONLY : NIceT
# endif
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(inout) :: liold, linew

# ifdef ASSUMED_SHAPE
#  if defined ICE_BIO
      real(r8), intent(inout) :: it(LBi:,LBj:,:,:)
#  endif
      real(r8), intent(in)    :: ui(LBi:,LBj:,:)
      real(r8), intent(in)    :: vi(LBi:,LBj:,:)
      real(r8), intent(inout) :: ai(LBi:,LBj:,:)
      real(r8), intent(inout) :: hi(LBi:,LBj:,:)
      real(r8), intent(inout) :: hsn(LBi:,LBj:,:)
      real(r8), intent(inout) :: ti(LBi:,LBj:,:)
      real(r8), intent(inout) :: enthalpi(LBi:,LBj:,:)
# else
# if defined ICE_BIO
      real(r8), intent(inout) :: it(LBi:UBi,LBj:UBj,NIceT(ng),2)
#  endif
      real(r8), intent(in)    :: ui(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in)    :: vi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: hsn(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ti(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: enthalpi(LBi:UBi,LBj:UBj,2)
# endif

! Local variable definitions
!
      integer :: i, j, itrc

#include "set_bounds.h"
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          ai(i,j,linew) = MIN(ai(i,j,linew),max_a(ng))
          ai(i,j,linew) = MAX(ai(i,j,linew),0.0_r8)
          hi(i,j,linew) = MAX(hi(i,j,linew),0.0_r8)
          hsn(i,j,linew) = MAX(hsn(i,j,linew),0.0_r8)
          ti(i,j,linew) = MAX(ti(i,j,linew),-70.0_r8)
          IF (ai(i,j,linew) .le. min_a(ng) .or. hi(i,j,linew) .le.      &
     &       0.0_r8) THEN
            ai(i,j,linew) = 0.0_r8
            hi(i,j,linew) = 0.0_r8
            hsn(i,j,linew) = 0.0_r8
          END IF
        END DO
      END DO

# if defined ICE_BIO
      DO j=Jstr,Jend
        DO i=Istr,Iend
          DO itrc=1,NIceT(ng)
            it(i,j,itrc,linew) = MAX(it(i,j,itrc,linew), 0.0_r8)
          END DO
        END DO
      END DO
# endif
!
      CALL i2d_bc_tile (ng, tile, iNLM,                                 &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  liold, linew,                                   &
     &                  BOUNDARY(ng)%ai_west(LBj:UBj),                  &
     &                  BOUNDARY(ng)%ai_east(LBj:UBj),                  &
     &                  BOUNDARY(ng)%ai_north(LBi:UBi),                 &
     &                  BOUNDARY(ng)%ai_south(LBi:UBi),                 &
     &                  ui, vi, ai, LBC(:,isAice,ng))
      CALL i2d_bc_tile (ng, tile, iNLM,                                 &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  liold, linew,                                   &
     &                  BOUNDARY(ng)%hi_west(LBj:UBj),                  &
     &                  BOUNDARY(ng)%hi_east(LBj:UBj),                  &
     &                  BOUNDARY(ng)%hi_north(LBi:UBi),                 &
     &                  BOUNDARY(ng)%hi_south(LBi:UBi),                 &
     &                  ui, vi, hi, LBC(:,isHice,ng))
      CALL i2d_bc_tile (ng, tile, iNLM,                                 &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  liold, linew,                                   &
     &                  BOUNDARY(ng)%hsn_west(LBj:UBj),                 &
     &                  BOUNDARY(ng)%hsn_east(LBj:UBj),                 &
     &                  BOUNDARY(ng)%hsn_north(LBi:UBi),                &
     &                  BOUNDARY(ng)%hsn_south(LBi:UBi),                &
     &                  ui, vi, hsn, LBC(:,isHsno,ng))
      CALL tibc_tile (ng, tile, iNLM,                                   &
     &                LBi, UBi, LBj, UBj,                               &
     &                liold, linew,                                     &
     &                ui, vi, hi, ti, enthalpi)
! # if defined ICE_BIO
!       CALL i2d_bc_tile (ng, tile, iNLM,                                 &
!      &                  LBi, UBi, LBj, UBj,                             &
!      &                  IminS, ImaxS, JminS, JmaxS,                     &
!      &                  liold, linew,                                   &
!      &                  BOUNDARY(ng)%IcePhL_west(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IcePhL_east(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IcePhL_north(LBi:UBi),             &
!      &                  BOUNDARY(ng)%IcePhL_south(LBi:UBi),             &
!      &                  ui, vi, IcePhL, LBC(:,isIcePhL,ng))
!       CALL i2d_bc_tile (ng, tile, iNLM,                                 &
!      &                  LBi, UBi, LBj, UBj,                             &
!      &                  IminS, ImaxS, JminS, JmaxS,                     &
!      &                  liold, linew,                                   &
!      &                  BOUNDARY(ng)%IceNO3_west(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IceNO3_east(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IceNO3_north(LBi:UBi),             &
!      &                  BOUNDARY(ng)%IceNO3_south(LBi:UBi),             &
!      &                  ui, vi, IceNO3, LBC(:,isIceNO3,ng))
!       CALL i2d_bc_tile (ng, tile, iNLM,                                 &
!      &                  LBi, UBi, LBj, UBj,                             &
!      &                  IminS, ImaxS, JminS, JmaxS,                     &
!      &                  liold, linew,                                   &
!      &                  BOUNDARY(ng)%IceNH4_west(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IceNH4_east(LBj:UBj),              &
!      &                  BOUNDARY(ng)%IceNH4_north(LBi:UBi),             &
!      &                  BOUNDARY(ng)%IceNH4_south(LBi:UBi),             &
!      &                  ui, vi, IceNH4, LBC(:,isIceNH4,ng))
#ifdef ICE_BIO
      DO itrc=1,NIceT(ng)
        CALL i2d_bc_tile (ng, tile, iNLM,                                 &
       &                  LBi, UBi, LBj, UBj,                             &
       &                  IminS, ImaxS, JminS, JmaxS,                     &
       &                  liold, linew,                                   &
       &                  BOUNDARY(ng)%it_west(LBj:UBj,itrc),             &
       &                  BOUNDARY(ng)%it_east(LBj:UBj,itrc),             &
       &                  BOUNDARY(ng)%it_north(LBi:UBi,itrc),            &
       &                  BOUNDARY(ng)%it_south(LBi:UBi,itrc),            &
       &                  ui, vi, it(:,:,itrc,:), LBC(:,isIvar(itrc),ng))
!         CALL i2d_bc_tile_closed (ng, tile, iNLM,                        &
!      &                           LBi, UBi, LBj, UBj,                    &
!      &                           IminS, ImaxS, JminS, JmaxS,            &
!      &                           liold, linew,                          &
!      &                           ui, vi, it(:,:,itrc,:))
      END DO
#endif

      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                                 &
     &                            LBi, UBi, LBj, UBj,                     &
     &                            ai(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                                 &
     &                            LBi, UBi, LBj, UBj,                     &
     &                            hi(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                                 &
     &                            LBi, UBi, LBj, UBj,                     &
     &                            hsn(:,:,linew))
        CALL exchange_r2d_tile (ng, tile,                                 &
     &                            LBi, UBi, LBj, UBj,                     &
     &                            ti(:,:,linew))
# if defined ICE_BIO
        DO itrc=1,NIceT(ng)
          CALL exchange_r2d_tile (ng, tile,                               &
       &                          LBi, UBi, LBj, UBj,                     &
       &                          it(:,:,itrc,linew))
        ENDDO
# endif
      END IF
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    ai(:,:,linew), hi(:,:,linew),                 &
     &                    hsn(:,:,linew),ti(:,:,linew))
#  if defined ICE_BIO
      DO itrc=1,NIceT(ng)
        CALL mp_exchange2d (ng, tile, iNLM, 3,                            &
       &                    LBi, UBi, LBj, UBj,                           &
       &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
       &                    it(:,:,itrc,linew))
      ENDDO
#  endif
# endif
      RETURN
      END SUBROUTINE ice_limit_tile
#endif
      END MODULE ice_limit_mod
