!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
! 
! Parameters for the TODO:name model:
!
!*****************************************************************************************
! TODO: add parameter documentation here (name, description, units)
!*****************************************************************************************
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
      
!*****************************************************************************************
!     TODO: bio tracer declarations for all state variables and diagnostics
!
!     integer :: iNO3_
!     integer :: iNH4_
!
!*****************************************************************************************
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)
      
!*****************************************************************************************
!     TODO: allocate for all parameters
!
!     real(r8), allocatable :: AttSW(:)              ! 1/m
!     real(r8), allocatable :: AttChl(:)             ! 1/(mg_Chl m2) 
!
!*****************************************************************************************
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
!*****************************************************************************************
!     TODO: add max counter variables
!
!     NBT=12
!#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!     NDbio3d=3
!     NDbio2d=1
!#endif
!*****************************************************************************************
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      
!*****************************************************************************************
!     TODO: Allocate memory for parameters
!
!     IF (.not.allocated(AttSW)) THEN
!       allocate ( AttSW(Ngrids) )
!       Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
!     END IF
!     
!     IF (.not.allocated(AttChl)) THEN
!       allocate ( AttChl(Ngrids) )
!       Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
!     END IF
!
!*****************************************************************************************
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
        Dmem(1)=Dmem(1)+REAL(NBT,r8)                                                                                                                                                          
      END IF  
      
!*****************************************************************************************
!     TODO: Allocate other biological vectors
!
!     IF (.not.allocated(iDbio2)) THEN
!       allocate ( iDbio2(NDbio2d) )
!       Dmem(1)=Dmem(1)+REAL(NDbio2d,r8)
!     END IF
!
!*****************************************************************************************
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
      
!*****************************************************************************************
!     TODO: Initialize specific indices
!
!     iNO3_=ic+1
!     iNH4_=ic+2
!
!*****************************************************************************************
      RETURN
      END SUBROUTINE initialize_biology
