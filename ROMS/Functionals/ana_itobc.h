      SUBROUTINE ana_itobc (ng, tile, model)
!
!! svn $Id: ana_cloud.h 75 2007-03-13 13:10:14Z arango $
!!======================================================================
!! Copyright (c) 2002-2009 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!                                                                     !
!=======================================================================
!                                                                      !
!  This routine sets ice biology tracer open boundary conditions using !
!  analytical expressions.                                             !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_ocean
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_itobc_tile (ng, tile, model,                          &
     &                     LBi, UBi, LBj, UBj,                       &
     &                     OCEAN(ng) % it)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(48)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_itobc
!
!***********************************************************************
      SUBROUTINE ana_itobc_tile (ng, tile, model,                    &
     &                           LBi, UBi, LBj, UBj,                 &
     &                           it)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_boundary
      USE mod_grid
      USE mod_scalars
      USE mod_biology, only : NIceT
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: it(LBi:,LBj:,:,:)
#else
      real(r8), intent(in) :: it(LBi:UBi,LBj:UBj,3,NIceT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, j, itrc

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Ice biology tracer open boundary conditions.
!-----------------------------------------------------------------------
!
      IF (ANY(LBC(ieast,isIvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Eastern_Edge(tile)) THEN
        DO itrc=1,NIceT(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%it_east(j,itrc)=0.0_r8
          END DO
        END DO
      END IF

      IF (ANY(LBC(iwest,isIvar(:),ng)%acquire).and.                     &
     &    DOMAIN(ng)%Western_Edge(tile)) THEN
        DO itrc=1,NIceT(ng)
          DO j=JstrT,JendT
            BOUNDARY(ng)%it_west(j,itrc)=0.0_r8
          END DO
        END DO
      END IF

      IF (ANY(LBC(isouth,isIvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Southern_Edge(tile)) THEN
        DO itrc=1,NIceT(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%it_south(i,itrc)=0.0_r8
          END DO
        END DO
      END IF

      IF (ANY(LBC(inorth,isIvar(:),ng)%acquire).and.                    &
     &    DOMAIN(ng)%Northern_Edge(tile)) THEN
        DO itrc=1,NIceT(ng)
          DO i=IstrT,IendT
            BOUNDARY(ng)%it_north(i,itrc)=0.0_r8
          END DO
        END DO
      END IF
      
      RETURN
      END SUBROUTINE ana_itobc_tile
