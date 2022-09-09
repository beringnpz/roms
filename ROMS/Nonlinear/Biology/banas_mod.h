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
!************************************************************************
! TODO: add parameter documentation here (name, description, units)
!************************************************************************
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
#endif
#ifdef ICE_BIO
      integer, allocatable :: NIceT(:)
      ! KK: NIB defined in mod_param.F, change there to increase ice bio tracers
!       integer              :: NIB       ! Ice tracers
      integer, allocatable :: idice(:)
      integer, allocatable :: idIceTvar(:)
#endif
#ifdef DIAGNOSTICS_BIO
      integer, allocatable :: iDbio2(:) ! 2D Biological diagnostics
      integer, allocatable :: iDbio3(:) ! 3D Biological diagnostics
#endif
      integer :: iphyto
      integer :: izoo
      integer :: idets
      integer :: idetl
      integer :: inh4
      integer :: ino3
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)
      real(r8), allocatable :: mu0(:)
      real(r8), allocatable :: att_sw(:)
      real(r8), allocatable :: att_p(:)
      real(r8), allocatable :: alpha_win(:)
      real(r8), allocatable :: alpha_sum(:)
      real(r8), allocatable :: Ecrit(:)
      real(r8), allocatable :: deltaE(:)
      real(r8), allocatable :: kmin(:)
      real(r8), allocatable :: phi_NH4(:)
      real(r8), allocatable :: CNratio(:)
      real(r8), allocatable :: chlNratio(:)
      real(r8), allocatable :: m_P(:)
      real(r8), allocatable :: m_agg(:)
      real(r8), allocatable :: I0(:)
      real(r8), allocatable :: K(:)
      real(r8), allocatable :: epsil(:)
      real(r8), allocatable :: fex(:)
      real(r8), allocatable :: m_Z(:)
      real(r8), allocatable :: w_S(:)
      real(r8), allocatable :: w_L(:)
      real(r8), allocatable :: r_remin(:)
      real(r8), allocatable :: r_nitr(:)
      real(r8), allocatable :: Q_P(:)
      real(r8), allocatable :: Q_Z(:)
      real(r8), allocatable :: Q_R(:)
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
!
      NBT=6
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
      NDbio3d=0
      NDbio2d=0
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
      IF (.not.allocated(mu0)) THEN
        allocate ( mu0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(att_sw)) THEN
        allocate ( att_sw(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(att_p)) THEN
        allocate ( att_p(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alpha_win)) THEN
        allocate ( alpha_win(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alpha_sum)) THEN
        allocate ( alpha_sum(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Ecrit)) THEN
        allocate ( Ecrit(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(deltaE)) THEN
        allocate ( deltaE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kmin)) THEN
        allocate ( kmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(phi_NH4)) THEN
        allocate ( phi_NH4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(CNratio)) THEN
        allocate ( CNratio(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(chlNratio)) THEN
        allocate ( chlNratio(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(m_P)) THEN
        allocate ( m_P(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(m_agg)) THEN
        allocate ( m_agg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(I0)) THEN
        allocate ( I0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(K)) THEN
        allocate ( K(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(epsil)) THEN
        allocate ( epsil(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(fex)) THEN
        allocate ( fex(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(m_Z)) THEN
        allocate ( m_Z(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(w_S)) THEN
        allocate ( w_S(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(w_L)) THEN
        allocate ( w_L(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(r_remin)) THEN
        allocate ( r_remin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(r_nitr)) THEN
        allocate ( r_nitr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q_P)) THEN
        allocate ( Q_P(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q_Z)) THEN
        allocate ( Q_Z(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Q_R)) THEN
        allocate ( Q_R(Ngrids) )
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
      IF (.not.allocated(NBeT)) THEN
        allocate ( NBeT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idBeTvar)) THEN
        allocate ( idBeTvar(NBEN) )
        Dmem(1)=Dmem(1)+REAL(NBEN,r8)
      END IF
# endif
# ifdef ICE_BIO
      IF (.not.allocated(idice)) THEN
        allocate ( idice(NIB) )
        Dmem(1)=Dmem(1)+REAL(NIB,r8)
      END IF
      IF (.not.allocated(NIceT)) THEN
        allocate ( NIceT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idIceTvar)) THEN
        allocate ( idIceTvar(NIB) )
        Dmem(1)=Dmem(1)+REAL(NIB,r8)
      END IF
# endif
# ifdef DIAGNOSTICS_BIO
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
        Dmem(1)=Dmem(1)+REAL(NDbio2d,r8)
      END IF

      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
        Dmem(1)=Dmem(1)+REAL(NDbio3d,r8)
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
      iphyto=ic+1
      izoo=ic+2
      idets=ic+3
      idetl=ic+4
      inh4=ic+5
      ino3=ic+6
      ic=ic+6

      RETURN
      END SUBROUTINE initialize_biology
