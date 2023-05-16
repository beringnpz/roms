      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in COBALT ecosystem model input                  !
!  parameters. They are specified in input script "cobalt_bpar.in".    !
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
!  Read in Cobalt biological model parameters.
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
            CASE ('htotal_scale_lo')
              Npts=load_r(Nval, Rval, Ngrids, htotal_scale_lo)
            CASE ('htotal_scale_hi')
              Npts=load_r(Nval, Rval, Ngrids, htotal_scale_hi)
            CASE ('RHO_0')
              Npts=load_r(Nval, Rval, Ngrids, RHO_0)
            CASE ('NKML')
              Npts=load_i(Nval, Rval, Ngrids, NKML)
            CASE ('a_0')
              Npts=load_r(Nval, Rval, Ngrids, a_0)
            CASE ('a_1')
              Npts=load_r(Nval, Rval, Ngrids, a_1)
            CASE ('a_2')
              Npts=load_r(Nval, Rval, Ngrids, a_2)
            CASE ('a_3')
              Npts=load_r(Nval, Rval, Ngrids, a_3)
            CASE ('a_4')
              Npts=load_r(Nval, Rval, Ngrids, a_4)
            CASE ('a_5')
              Npts=load_r(Nval, Rval, Ngrids, a_5)
            CASE ('b_0')
              Npts=load_r(Nval, Rval, Ngrids, b_0)
            CASE ('b_1')
              Npts=load_r(Nval, Rval, Ngrids, b_1)
            CASE ('b_2')
              Npts=load_r(Nval, Rval, Ngrids, b_2)
            CASE ('b_3')
              Npts=load_r(Nval, Rval, Ngrids, b_3)
            CASE ('c_0')
              Npts=load_r(Nval, Rval, Ngrids, c_0)
            CASE ('a1_co2')
              Npts=load_r(Nval, Rval, Ngrids, a1_co2)
            CASE ('a2_co2')
              Npts=load_r(Nval, Rval, Ngrids, a2_co2)
            CASE ('a3_co2')
              Npts=load_r(Nval, Rval, Ngrids, a3_co2)
            CASE ('a4_co2')
              Npts=load_r(Nval, Rval, Ngrids, a4_co2)
            CASE ('a1_o2')
              Npts=load_r(Nval, Rval, Ngrids, a1_o2)
            CASE ('a2_o2')
              Npts=load_r(Nval, Rval, Ngrids, a2_o2)
            CASE ('a3_o2')
              Npts=load_r(Nval, Rval, Ngrids, a3_o2)
            CASE ('a4_o2')
              Npts=load_r(Nval, Rval, Ngrids, a4_o2)
            CASE ('mass_2_n')
              Npts=load_r(Nval, Rval, Ngrids, mass_2_n)
            CASE ('n_2_n_denit')
              Npts=load_r(Nval, Rval, Ngrids, n_2_n_denit)
            CASE ('o2_2_c')
              Npts=load_r(Nval, Rval, Ngrids, o2_2_c)
            CASE ('o2_2_nfix')
              Npts=load_r(Nval, Rval, Ngrids, o2_2_nfix)
            CASE ('o2_2_nh4')
              Npts=load_r(Nval, Rval, Ngrids, o2_2_nh4)
            CASE ('o2_2_nitrif')
              Npts=load_r(Nval, Rval, Ngrids, o2_2_nitrif)
            CASE ('o2_2_no3')
              Npts=load_r(Nval, Rval, Ngrids, o2_2_no3)
            CASE ('k_fed_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_fed_Di)
            CASE ('k_fed_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_fed_Lg)
            CASE ('k_fed_Sm')
              Npts=load_r(Nval, Rval, Ngrids, k_fed_Sm)
            CASE ('k_nh4_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_nh4_Lg)
            CASE ('k_nh4_Sm')
              Npts=load_r(Nval, Rval, Ngrids, k_nh4_Sm)
            CASE ('k_nh4_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_nh4_Di)
            CASE ('k_no3_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_no3_Lg)
            CASE ('k_no3_Sm')
              Npts=load_r(Nval, Rval, Ngrids, k_no3_Sm)
            CASE ('k_no3_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_no3_Di)
            CASE ('k_po4_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_po4_Di)
            CASE ('k_po4_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_po4_Lg)
            CASE ('k_po4_Sm')
              Npts=load_r(Nval, Rval, Ngrids, k_po4_Sm)
            CASE ('k_sio4_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_sio4_Lg)
            CASE ('k_fe_2_n_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_fe_2_n_Di)
            CASE ('k_fe_2_n_Lg')
              Npts=load_r(Nval, Rval, Ngrids, k_fe_2_n_Lg)
            CASE ('k_fe_2_n_Sm')
              Npts=load_r(Nval, Rval, Ngrids, k_fe_2_n_Sm)
            CASE ('fe_2_n_max_Sm')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_max_Sm)
            CASE ('fe_2_n_max_Lg')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_max_Lg)
            CASE ('fe_2_n_max_Di')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_max_Di)
            CASE ('fe_2_n_upt_fac')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_upt_fac)
            CASE ('alpha_Di')
              Npts=load_r(Nval, Rval, Ngrids, alpha_Di)
            CASE ('alpha_Lg')
              Npts=load_r(Nval, Rval, Ngrids, alpha_Lg)
            CASE ('alpha_Sm')
              Npts=load_r(Nval, Rval, Ngrids, alpha_Sm)
            CASE ('kappa_eppley')
              Npts=load_r(Nval, Rval, Ngrids, kappa_eppley)
            CASE ('P_C_max_Di')
              Npts=load_r(Nval, Rval, Ngrids, P_C_max_Di)
            CASE ('P_C_max_Lg')
              Npts=load_r(Nval, Rval, Ngrids, P_C_max_Lg)
            CASE ('P_C_max_Sm')
              Npts=load_r(Nval, Rval, Ngrids, P_C_max_Sm)
            CASE ('thetamax_Di')
              Npts=load_r(Nval, Rval, Ngrids, thetamax_Di)
            CASE ('thetamax_Lg')
              Npts=load_r(Nval, Rval, Ngrids, thetamax_Lg)
            CASE ('thetamax_Sm')
              Npts=load_r(Nval, Rval, Ngrids, thetamax_Sm)
            CASE ('bresp_Di')
              Npts=load_r(Nval, Rval, Ngrids, bresp_Di)
            CASE ('bresp_Lg')
              Npts=load_r(Nval, Rval, Ngrids, bresp_Lg)
            CASE ('bresp_Sm')
              Npts=load_r(Nval, Rval, Ngrids, bresp_Sm)
            CASE ('thetamin')
              Npts=load_r(Nval, Rval, Ngrids, thetamin)
            CASE ('thetamin_nolim')
              Npts=load_r(Nval, Rval, Ngrids, thetamin_nolim)
            CASE ('zpllgr')
              Npts=load_r(Nval, Rval, Ngrids, zpllgr)
            CASE ('gamma_irr_mem')
              Npts=load_r(Nval, Rval, Ngrids, gamma_irr_mem)
            CASE ('gamma_mu_mem')
              Npts=load_r(Nval, Rval, Ngrids, gamma_mu_mem)
            CASE ('k_n_inhib_Di')
              Npts=load_r(Nval, Rval, Ngrids, k_n_inhib_Di)
            CASE ('o2_inhib_Di_pow')
              Npts=load_r(Nval, Rval, Ngrids, o2_inhib_Di_pow)
            CASE ('o2_inhib_Di_sat')
              Npts=load_r(Nval, Rval, Ngrids, o2_inhib_Di_sat)
            CASE ('p_2_n_static')
              Npts=load_i(Nval, Rval, Ngrids, p_2_n_static)
            CASE ('c_2_n')
              Npts=load_r(Nval, Rval, Ngrids, c_2_n)
            CASE ('alk_2_n_denit')
              Npts=load_r(Nval, Rval, Ngrids, alk_2_n_denit)
            CASE ('p_2_n_static_Di')
              Npts=load_r(Nval, Rval, Ngrids, p_2_n_static_Di)
            CASE ('p_2_n_static_Lg')
              Npts=load_r(Nval, Rval, Ngrids, p_2_n_static_Lg)
            CASE ('p_2_n_static_Sm')
              Npts=load_r(Nval, Rval, Ngrids, p_2_n_static_Sm)
            CASE ('si_2_n_static_Lg')
              Npts=load_r(Nval, Rval, Ngrids, si_2_n_static_Lg)
            CASE ('si_2_n_max_Lg')
              Npts=load_r(Nval, Rval, Ngrids, si_2_n_max_Lg)
            CASE ('ca_2_n_arag')
              Npts=load_r(Nval, Rval, Ngrids, ca_2_n_arag)
            CASE ('ca_2_n_calc')
              Npts=load_r(Nval, Rval, Ngrids, ca_2_n_calc)
            CASE ('caco3_sat_max')
              Npts=load_r(Nval, Rval, Ngrids, caco3_sat_max)
            CASE ('q_p_2_n_smz')
              Npts=load_r(Nval, Rval, Ngrids, q_p_2_n_smz)
            CASE ('q_p_2_n_mdz')
              Npts=load_r(Nval, Rval, Ngrids, q_p_2_n_mdz)
            CASE ('q_p_2_n_lgz')
              Npts=load_r(Nval, Rval, Ngrids, q_p_2_n_lgz)
            CASE ('q_p_2_n_bact')
              Npts=load_r(Nval, Rval, Ngrids, q_p_2_n_bact)
            CASE ('agg_Sm')
              Npts=load_r(Nval, Rval, Ngrids, agg_Sm)
            CASE ('agg_Di')
              Npts=load_r(Nval, Rval, Ngrids, agg_Di)
            CASE ('agg_Lg')
              Npts=load_r(Nval, Rval, Ngrids, agg_Lg)
            CASE ('vir_Sm')
              Npts=load_r(Nval, Rval, Ngrids, vir_Sm)
            CASE ('vir_Di')
              Npts=load_r(Nval, Rval, Ngrids, vir_Di)
            CASE ('vir_Lg')
              Npts=load_r(Nval, Rval, Ngrids, vir_Lg)
            CASE ('vir_Bact')
              Npts=load_r(Nval, Rval, Ngrids, vir_Bact)
            CASE ('ktemp_vir')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_vir)
            CASE ('exu_Sm')
              Npts=load_r(Nval, Rval, Ngrids, exu_Sm)
            CASE ('exu_Di')
              Npts=load_r(Nval, Rval, Ngrids, exu_Di)
            CASE ('exu_Lg')
              Npts=load_r(Nval, Rval, Ngrids, exu_Lg)
            CASE ('imax_smz')
              Npts=load_r(Nval, Rval, Ngrids, imax_smz)
            CASE ('imax_mdz')
              Npts=load_r(Nval, Rval, Ngrids, imax_mdz)
            CASE ('imax_lgz')
              Npts=load_r(Nval, Rval, Ngrids, imax_lgz)
            CASE ('ki_smz')
              Npts=load_r(Nval, Rval, Ngrids, ki_smz)
            CASE ('ki_mdz')
              Npts=load_r(Nval, Rval, Ngrids, ki_mdz)
            CASE ('ki_lgz')
              Npts=load_r(Nval, Rval, Ngrids, ki_lgz)
            CASE ('ktemp_smz')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_smz)
            CASE ('ktemp_mdz')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_mdz)
            CASE ('ktemp_lgz')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_lgz)
            CASE ('mu_max_bact')
              Npts=load_r(Nval, Rval, Ngrids, mu_max_bact)
            CASE ('k_ldon_bact')
              Npts=load_r(Nval, Rval, Ngrids, k_ldon_bact)
            CASE ('ktemp_bact')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_bact)
            CASE ('nswitch_smz')
              Npts=load_r(Nval, Rval, Ngrids, nswitch_smz)
            CASE ('nswitch_mdz')
              Npts=load_r(Nval, Rval, Ngrids, nswitch_mdz)
            CASE ('nswitch_lgz')
              Npts=load_r(Nval, Rval, Ngrids, nswitch_lgz)
            CASE ('mswitch_smz')
              Npts=load_r(Nval, Rval, Ngrids, mswitch_smz)
            CASE ('mswitch_mdz')
              Npts=load_r(Nval, Rval, Ngrids, mswitch_mdz)
            CASE ('mswitch_lgz')
              Npts=load_r(Nval, Rval, Ngrids, mswitch_lgz)
            CASE ('smz_ipa_smp')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_smp)
            CASE ('smz_ipa_lgp')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_lgp)
            CASE ('smz_ipa_diaz')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_diaz)
            CASE ('smz_ipa_smz')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_smz)
            CASE ('smz_ipa_mdz')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_mdz)
            CASE ('smz_ipa_lgz')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_lgz)
            CASE ('smz_ipa_bact')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_bact)
            CASE ('smz_ipa_det')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_det)
            CASE ('mdz_ipa_smp')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_smp)
            CASE ('mdz_ipa_lgp')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_lgp)
            CASE ('mdz_ipa_diaz')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_diaz)
            CASE ('mdz_ipa_smz')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_smz)
            CASE ('mdz_ipa_mdz')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_mdz)
            CASE ('mdz_ipa_lgz')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_lgz)
            CASE ('mdz_ipa_bact')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_bact)
            CASE ('mdz_ipa_det')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_det)
            CASE ('lgz_ipa_smp')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_smp)
            CASE ('lgz_ipa_lgp')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_lgp)
            CASE ('lgz_ipa_diaz')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_diaz)
            CASE ('lgz_ipa_smz')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_smz)
            CASE ('lgz_ipa_mdz')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_mdz)
            CASE ('lgz_ipa_lgz')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_lgz)
            CASE ('lgz_ipa_bact')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_bact)
            CASE ('lgz_ipa_det')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_det)
            CASE ('gge_max_smz')
              Npts=load_r(Nval, Rval, Ngrids, gge_max_smz)
            CASE ('gge_max_mdz')
              Npts=load_r(Nval, Rval, Ngrids, gge_max_mdz)
            CASE ('gge_max_lgz')
              Npts=load_r(Nval, Rval, Ngrids, gge_max_lgz)
            CASE ('bresp_smz')
              Npts=load_r(Nval, Rval, Ngrids, bresp_smz)
            CASE ('bresp_mdz')
              Npts=load_r(Nval, Rval, Ngrids, bresp_mdz)
            CASE ('bresp_lgz')
              Npts=load_r(Nval, Rval, Ngrids, bresp_lgz)
            CASE ('gge_max_bact')
              Npts=load_r(Nval, Rval, Ngrids, gge_max_bact)
            CASE ('bresp_bact')
              Npts=load_r(Nval, Rval, Ngrids, bresp_bact)
            CASE ('phi_det_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_det_smz)
            CASE ('phi_det_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_det_mdz)
            CASE ('phi_det_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_det_lgz)
            CASE ('phi_ldon_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldon_smz)
            CASE ('phi_ldon_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldon_mdz)
            CASE ('phi_ldon_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldon_lgz)
            CASE ('phi_ldop_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldop_smz)
            CASE ('phi_ldop_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldop_mdz)
            CASE ('phi_ldop_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldop_lgz)
            CASE ('phi_srdon_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdon_smz)
            CASE ('phi_srdon_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdon_mdz)
            CASE ('phi_srdon_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdon_lgz)
            CASE ('phi_srdop_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdop_smz)
            CASE ('phi_srdop_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdop_mdz)
            CASE ('phi_srdop_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdop_lgz)
            CASE ('phi_sldon_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldon_smz)
            CASE ('phi_sldon_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldon_mdz)
            CASE ('phi_sldon_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldon_lgz)
            CASE ('phi_sldop_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldop_smz)
            CASE ('phi_sldop_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldop_mdz)
            CASE ('phi_sldop_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldop_lgz)
            CASE ('phi_nh4_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_nh4_smz)
            CASE ('phi_nh4_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_nh4_mdz)
            CASE ('phi_nh4_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_nh4_lgz)
            CASE ('phi_po4_smz')
              Npts=load_r(Nval, Rval, Ngrids, phi_po4_smz)
            CASE ('phi_po4_mdz')
              Npts=load_r(Nval, Rval, Ngrids, phi_po4_mdz)
            CASE ('phi_po4_lgz')
              Npts=load_r(Nval, Rval, Ngrids, phi_po4_lgz)
            CASE ('phi_ldon_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldon_vir)
            CASE ('phi_srdon_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdon_vir)
            CASE ('phi_sldon_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldon_vir)
            CASE ('phi_ldop_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_ldop_vir)
            CASE ('phi_srdop_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_srdop_vir)
            CASE ('phi_sldop_vir')
              Npts=load_r(Nval, Rval, Ngrids, phi_sldop_vir)
            CASE ('imax_hp')
              Npts=load_r(Nval, Rval, Ngrids, imax_hp)
            CASE ('ki_hp')
              Npts=load_r(Nval, Rval, Ngrids, ki_hp)
            CASE ('coef_hp')
              Npts=load_r(Nval, Rval, Ngrids, coef_hp)
            CASE ('ktemp_hp')
              Npts=load_r(Nval, Rval, Ngrids, ktemp_hp)
            CASE ('nswitch_hp')
              Npts=load_r(Nval, Rval, Ngrids, nswitch_hp)
            CASE ('mswitch_hp')
              Npts=load_r(Nval, Rval, Ngrids, mswitch_hp)
            CASE ('hp_ipa_smp')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_smp)
            CASE ('hp_ipa_lgp')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_lgp)
            CASE ('hp_ipa_diaz')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_diaz)
            CASE ('hp_ipa_smz')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_smz)
            CASE ('hp_ipa_mdz')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_mdz)
            CASE ('hp_ipa_lgz')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_lgz)
            CASE ('hp_ipa_bact')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_bact)
            CASE ('hp_ipa_det')
              Npts=load_r(Nval, Rval, Ngrids, hp_ipa_det)
            CASE ('hp_phi_det')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_det)
            CASE ('hp_phi_ldon')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_ldon)
            CASE ('hp_phi_ldop')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_ldop)
            CASE ('hp_phi_srdon')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_srdon)
            CASE ('hp_phi_srdop')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_srdop)
            CASE ('hp_phi_sldon')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_sldon)
            CASE ('hp_phi_sldop')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_sldop)
            CASE ('hp_phi_nh4')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_nh4)
            CASE ('hp_phi_po4')
              Npts=load_r(Nval, Rval, Ngrids, hp_phi_po4)
            CASE ('felig_bkg')
              Npts=load_r(Nval, Rval, Ngrids, felig_bkg)
            CASE ('felig_2_don')
              Npts=load_r(Nval, Rval, Ngrids, felig_2_don)
            CASE ('fe_2_n_sed')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_sed)
            CASE ('fe_coast')
              Npts=load_r(Nval, Rval, Ngrids, fe_coast)
            CASE ('alpha_fescav')
              Npts=load_r(Nval, Rval, Ngrids, alpha_fescav)
            CASE ('ffe_sed_max')
              Npts=load_r(Nval, Rval, Ngrids, ffe_sed_max)
            CASE ('beta_fescav')
              Npts=load_r(Nval, Rval, Ngrids, beta_fescav)
            CASE ('remin_eff_fedet')
              Npts=load_r(Nval, Rval, Ngrids, remin_eff_fedet)
            CASE ('ki_fescav')
              Npts=load_r(Nval, Rval, Ngrids, ki_fescav)
            CASE ('io_fescav')
              Npts=load_r(Nval, Rval, Ngrids, io_fescav)
            CASE ('gamma_fescav')
              Npts=load_r(Nval, Rval, Ngrids, gamma_fescav)
            CASE ('kfe_eq_lig_ll')
              Npts=load_r(Nval, Rval, Ngrids, kfe_eq_lig_ll)
            CASE ('kfe_eq_lig_hl')
              Npts=load_r(Nval, Rval, Ngrids, kfe_eq_lig_hl)
            CASE ('k_o2')
              Npts=load_r(Nval, Rval, Ngrids, k_o2)
            CASE ('o2_min')
              Npts=load_r(Nval, Rval, Ngrids, o2_min)
            CASE ('rpcaco3')
              Npts=load_r(Nval, Rval, Ngrids, rpcaco3)
            CASE ('rplith')
              Npts=load_r(Nval, Rval, Ngrids, rplith)
            CASE ('rpsio2')
              Npts=load_r(Nval, Rval, Ngrids, rpsio2)
            CASE ('gamma_ndet')
              Npts=load_r(Nval, Rval, Ngrids, gamma_ndet)
            CASE ('gamma_cadet_arag')
              Npts=load_r(Nval, Rval, Ngrids, gamma_cadet_arag)
            CASE ('gamma_cadet_calc')
              Npts=load_r(Nval, Rval, Ngrids, gamma_cadet_calc)
            CASE ('gamma_sidet')
              Npts=load_r(Nval, Rval, Ngrids, gamma_sidet)
            CASE ('phi_lith')
              Npts=load_r(Nval, Rval, Ngrids, phi_lith)
            CASE ('k_lith')
              Npts=load_r(Nval, Rval, Ngrids, k_lith)
            CASE ('z_sed')
              Npts=load_r(Nval, Rval, Ngrids, z_sed)
            CASE ('k_no3_denit')
              Npts=load_r(Nval, Rval, Ngrids, k_no3_denit)
            CASE ('gamma_srdon')
              Npts=load_r(Nval, Rval, Ngrids, gamma_srdon)
            CASE ('gamma_srdop')
              Npts=load_r(Nval, Rval, Ngrids, gamma_srdop)
            CASE ('gamma_sldon')
              Npts=load_r(Nval, Rval, Ngrids, gamma_sldon)
            CASE ('gamma_sldop')
              Npts=load_r(Nval, Rval, Ngrids, gamma_sldop)
            CASE ('gamma_nitrif')
              Npts=load_r(Nval, Rval, Ngrids, gamma_nitrif)
            CASE ('irr_inhibit')
              Npts=load_r(Nval, Rval, Ngrids, irr_inhibit)
            CASE ('htotal_in')
              Npts=load_r(Nval, Rval, Ngrids, htotal_in)
            CASE ('wsink')
              Npts=load_r(Nval, Rval, Ngrids, wsink)
#ifdef COASTDIAT
            CASE ('k_fed_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_fed_Md)
            CASE ('k_nh4_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_nh4_Md)
            CASE ('k_no3_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_no3_Md)
            CASE ('k_po4_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_po4_Md)
            CASE ('k_sio4_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_sio4_Md)
            CASE ('k_fe_2_n_Md')
              Npts=load_r(Nval, Rval, Ngrids, k_fe_2_n_Md)
            CASE ('fe_2_n_max_Md')
              Npts=load_r(Nval, Rval, Ngrids, fe_2_n_max_Md)
            CASE ('alpha_Md')
              Npts=load_r(Nval, Rval, Ngrids, alpha_Md)
            CASE ('P_C_max_Md')
              Npts=load_r(Nval, Rval, Ngrids, P_C_max_Md)
            CASE ('thetamax_Md')
              Npts=load_r(Nval, Rval, Ngrids, thetamax_Md)
            CASE ('bresp_Md')
              Npts=load_r(Nval, Rval, Ngrids, bresp_Md)
            CASE ('p_2_n_static_Md')
              Npts=load_r(Nval, Rval, Ngrids, p_2_n_static_Md)
            CASE ('si_2_n_static_Md')
              Npts=load_r(Nval, Rval, Ngrids, si_2_n_static_Md)
            CASE ('si_2_n_max_Md')
              Npts=load_r(Nval, Rval, Ngrids, si_2_n_max_Md)
            CASE ('agg_Md')
              Npts=load_r(Nval, Rval, Ngrids, agg_Md)
            CASE ('vir_Md')
              Npts=load_r(Nval, Rval, Ngrids, vir_Md)
            CASE ('exu_Md')
              Npts=load_r(Nval, Rval, Ngrids, exu_Md)
            CASE ('smz_ipa_mdp')
              Npts=load_r(Nval, Rval, Ngrids, smz_ipa_mdp)
            CASE ('mdz_ipa_mdp')
              Npts=load_r(Nval, Rval, Ngrids, mdz_ipa_mdp)
            CASE ('lgz_ipa_mdp')
              Npts=load_r(Nval, Rval, Ngrids, lgz_ipa_mdp)
#endif
#ifdef COASTAL_ATTEN
            CASE ('k_sed1')
              Npts=load_r(Nval, Rval, Ngrids, k_sed1)
            CASE ('k_sed2')
              Npts=load_r(Nval, Rval, Ngrids, k_sed2)
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
              Npts=load_l(Nval, Cval, NBEN, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBEN
                  i=idBeTvar(idben(itrc))
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef ICE_BIO
            CASE ('Hout(idIceTvar)')
              Npts=load_l(Nval, Cval, NIB, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NIB
                  i=idIceTvar(idice(itrc))
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
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
#ifdef ICE_BIO
            CASE ('Aout(idIceTvar)')
              Npts=load_l(Nval, Cval,NIB, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NIB
                  i=idIceTvar(idice(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
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
            CASE ('Dout(ichl)')
              IF (iDbio3(ichl).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ichl)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ichl)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico3_ion)')
              IF (iDbio3(ico3_ion).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ico3_ion)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ico3_ion)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ihtotal)')
              IF (iDbio3(ihtotal).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ihtotal)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ihtotal)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iirr_mem)')
              IF (iDbio3(iirr_mem).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iirr_mem)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iirr_mem)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iirr_mix)')
              IF (iDbio3(iirr_mix).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iirr_mix)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iirr_mix)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iirr_inst)')
              IF (iDbio3(iirr_inst).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iirr_inst)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iirr_inst)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico3_sol_calc)')
              IF (iDbio3(ico3_sol_calc).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ico3_sol_calc)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ico3_sol_calc)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico3_sol_arag)')
              IF (iDbio3(ico3_sol_arag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ico3_sol_arag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ico3_sol_arag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ife_bulk_flx)')
              IF (iDbio3(ife_bulk_flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ife_bulk_flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ife_bulk_flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iomega_cadet_calc)')
              IF (iDbio3(iomega_cadet_calc).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iomega_cadet_calc)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iomega_cadet_calc)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iomega_cadet_arag)')
              IF (iDbio3(iomega_cadet_arag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iomega_cadet_arag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iomega_cadet_arag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iswdk)')
              IF (iDbio3(iswdk).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iswdk)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iswdk)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imu_mem_sm)')
              IF (iDbio3(imu_mem_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(imu_mem_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(imu_mem_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imu_mem_di)')
              IF (iDbio3(imu_mem_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(imu_mem_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(imu_mem_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imu_mem_lg)')
              IF (iDbio3(imu_mem_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(imu_mem_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(imu_mem_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iagg_lim_sm)')
              IF (iDbio3(iagg_lim_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iagg_lim_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iagg_lim_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iagg_lim_di)')
              IF (iDbio3(iagg_lim_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iagg_lim_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iagg_lim_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iagg_lim_lg)')
              IF (iDbio3(iagg_lim_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iagg_lim_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iagg_lim_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_di)')
              IF (iDbio3(iaggloss_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iaggloss_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iaggloss_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_sm)')
              IF (iDbio3(iaggloss_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iaggloss_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iaggloss_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_lg)')
              IF (iDbio3(iaggloss_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iaggloss_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iaggloss_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ivirloss_di)')
              IF (iDbio3(ivirloss_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ivirloss_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ivirloss_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ivirloss_sm)')
              IF (iDbio3(ivirloss_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ivirloss_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ivirloss_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ivirloss_lg)')
              IF (iDbio3(ivirloss_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ivirloss_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ivirloss_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_di)')
              IF (iDbio3(izloss_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(izloss_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(izloss_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_sm)')
              IF (iDbio3(izloss_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(izloss_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(izloss_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_lg)')
              IF (iDbio3(izloss_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(izloss_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(izloss_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(idef_fe_sm)')
              IF (iDbio3(idef_fe_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(idef_fe_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(idef_fe_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(idef_fe_di)')
              IF (iDbio3(idef_fe_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(idef_fe_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(idef_fe_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(idef_fe_lg)')
              IF (iDbio3(idef_fe_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(idef_fe_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(idef_fe_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifelim_sm)')
              IF (iDbio3(ifelim_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifelim_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifelim_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifelim_di)')
              IF (iDbio3(ifelim_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifelim_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifelim_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifelim_lg)')
              IF (iDbio3(ifelim_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifelim_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifelim_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3lim_sm)')
              IF (iDbio3(ino3lim_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ino3lim_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ino3lim_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3lim_di)')
              IF (iDbio3(ino3lim_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ino3lim_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ino3lim_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3lim_lg)')
              IF (iDbio3(ino3lim_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ino3lim_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ino3lim_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inh4lim_sm)')
              IF (iDbio3(inh4lim_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inh4lim_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inh4lim_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inh4lim_di)')
              IF (iDbio3(inh4lim_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inh4lim_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inh4lim_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inh4lim_lg)')
              IF (iDbio3(inh4lim_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inh4lim_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inh4lim_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipo4lim_sm)')
              IF (iDbio3(ipo4lim_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ipo4lim_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ipo4lim_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipo4lim_di)')
              IF (iDbio3(ipo4lim_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ipo4lim_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ipo4lim_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipo4lim_lg)')
              IF (iDbio3(ipo4lim_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ipo4lim_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ipo4lim_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ichl_di)')
              IF (iDbio3(ichl_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ichl_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ichl_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iC_2_chl_di)')
              IF (iDbio3(iC_2_chl_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iC_2_chl_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iC_2_chl_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ichl_sm)')
              IF (iDbio3(ichl_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ichl_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ichl_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iC_2_chl_sm)')
              IF (iDbio3(iC_2_chl_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iC_2_chl_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iC_2_chl_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ichl_lg)')
              IF (iDbio3(ichl_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ichl_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ichl_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iC_2_chl_lg)')
              IF (iDbio3(iC_2_chl_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iC_2_chl_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iC_2_chl_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipCO2)')
              IF (iDbio3(ipCO2).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ipCO2)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ipCO2)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# ifdef COASTDIAT
            CASE ('Dout(imu_mem_md)')
              IF (iDbio3(imu_mem_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(imu_mem_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(imu_mem_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iagg_lim_md)')
              IF (iDbio3(iagg_lim_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iagg_lim_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iagg_lim_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ivirloss_md)')
              IF (iDbio3(ivirloss_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ivirloss_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ivirloss_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_md)')
              IF (iDbio3(izloss_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(izloss_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(izloss_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(idef_fe_md)')
              IF (iDbio3(idef_fe_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(idef_fe_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(idef_fe_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifelim_md)')
              IF (iDbio3(ifelim_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifelim_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifelim_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3lim_md)')
              IF (iDbio3(ino3lim_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ino3lim_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ino3lim_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inh4lim_md)')
              IF (iDbio3(inh4lim_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inh4lim_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inh4lim_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipo4lim_md)')
              IF (iDbio3(ipo4lim_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ipo4lim_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ipo4lim_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ichl_md)')
              IF (iDbio3(ichl_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ichl_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ichl_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iC_2_chl_md)')
              IF (iDbio3(iC_2_chl_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iC_2_chl_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iC_2_chl_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
            CASE ('Dout(inpp_sm)')
              IF (iDbio3(inpp_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inpp_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inpp_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# ifdef COASTDIAT
            CASE ('Dout(inpp_md)')
              IF (iDbio3(inpp_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inpp_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inpp_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif 
            CASE ('Dout(inpp_lg)')
              IF (iDbio3(inpp_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inpp_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inpp_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inpp_di)')
              IF (iDbio3(inpp_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(inpp_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(inpp_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifratio)')
              IF (iDbio3(ifratio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ifratio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ifratio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_smz)')
              IF (iDbio3(iprod_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_mdz)')
              IF (iDbio3(iprod_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_lgz)')
              IF (iDbio3(iprod_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iprod_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iprod_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_no3_nsm)')
              IF (iDbio3(iflx_gpp_no3_nsm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_no3_nsm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_no3_nsm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_no3_nlg)')
              IF (iDbio3(iflx_gpp_no3_nlg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_no3_nlg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_no3_nlg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_no3_ndi)')
              IF (iDbio3(iflx_gpp_no3_ndi).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_no3_ndi)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_no3_ndi)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_nh4_nsm)')
              IF (iDbio3(iflx_gpp_nh4_nsm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_nh4_nsm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_nh4_nsm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_nh4_nlg)')
              IF (iDbio3(iflx_gpp_nh4_nlg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_nh4_nlg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_nh4_nlg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_nh4_ndi)')
              IF (iDbio3(iflx_gpp_nh4_ndi).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_nh4_ndi)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_nh4_ndi)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gpp_n2_ndi)')
              IF (iDbio3(iflx_gpp_n2_ndi).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gpp_n2_ndi)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gpp_n2_ndi)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_ldon_bac)')
              IF (iDbio3(iflx_gra_ldon_bac).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_ldon_bac)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_ldon_bac)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_rem_ldon_nh4)')
              IF (iDbio3(iflx_rem_ldon_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_rem_ldon_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_rem_ldon_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nsm_nsmz)')
              IF (iDbio3(iflx_gra_nsm_nsmz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nsm_nsmz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nsm_nsmz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_bac_nsmz)')
              IF (iDbio3(iflx_gra_bac_nsmz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_bac_nsmz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_bac_nsmz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_ndi_nmdz)')
              IF (iDbio3(iflx_gra_ndi_nmdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_ndi_nmdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_ndi_nmdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nlg_nmdz)')
              IF (iDbio3(iflx_gra_nlg_nmdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nlg_nmdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nlg_nmdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nsmz_nmdz)')
              IF (iDbio3(iflx_gra_nsmz_nmdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nsmz_nmdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nsmz_nmdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_ndi_nlgz)')
              IF (iDbio3(iflx_gra_ndi_nlgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_ndi_nlgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_ndi_nlgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nlg_nlgz)')
              IF (iDbio3(iflx_gra_nlg_nlgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nlg_nlgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nlg_nlgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nmdz_nlgz)')
              IF (iDbio3(iflx_gra_nmdz_nlgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nmdz_nlgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nmdz_nlgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nsmz_hip)')
              IF (iDbio3(iflx_gra_nsmz_hip).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nsmz_hip)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nsmz_hip)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nmdz_hip)')
              IF (iDbio3(iflx_gra_nmdz_hip).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nmdz_hip)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nmdz_hip)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_gra_nlgz_hip)')
              IF (iDbio3(iflx_gra_nlgz_hip).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_gra_nlgz_hip)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_gra_nlgz_hip)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nsmz_ndet)')
              IF (iDbio3(iflx_ege_nsmz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nsmz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nsmz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nsmz_sldon)')
              IF (iDbio3(iflx_ege_nsmz_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nsmz_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nsmz_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nsmz_ldon)')
              IF (iDbio3(iflx_ege_nsmz_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nsmz_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nsmz_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nsmz_srdon)')
              IF (iDbio3(iflx_ege_nsmz_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nsmz_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nsmz_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nmdz_ndet)')
              IF (iDbio3(iflx_ege_nmdz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nmdz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nmdz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nmdz_sldon)')
              IF (iDbio3(iflx_ege_nmdz_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nmdz_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nmdz_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nmdz_ldon)')
              IF (iDbio3(iflx_ege_nmdz_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nmdz_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nmdz_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nmdz_srdon)')
              IF (iDbio3(iflx_ege_nmdz_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nmdz_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nmdz_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nlgz_ndet)')
              IF (iDbio3(iflx_ege_nlgz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nlgz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nlgz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nlgz_sldon)')
              IF (iDbio3(iflx_ege_nlgz_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nlgz_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nlgz_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nlgz_ldon)')
              IF (iDbio3(iflx_ege_nlgz_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nlgz_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nlgz_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nlgz_srdon)')
              IF (iDbio3(iflx_ege_nlgz_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nlgz_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nlgz_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_hip_ndet)')
              IF (iDbio3(iflx_ege_hip_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_hip_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_hip_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_agg_ndi_ndet)')
              IF (iDbio3(iflx_agg_ndi_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_agg_ndi_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_agg_ndi_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_agg_nlg_ndet)')
              IF (iDbio3(iflx_agg_nlg_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_agg_nlg_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_agg_nlg_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_agg_nsm_ndet)')
              IF (iDbio3(iflx_agg_nsm_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_agg_nsm_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_agg_nsm_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_ndi_ldon)')
              IF (iDbio3(iflx_vir_ndi_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_ndi_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_ndi_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nlg_ldon)')
              IF (iDbio3(iflx_vir_nlg_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nlg_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nlg_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nsm_ldon)')
              IF (iDbio3(iflx_vir_nsm_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nsm_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nsm_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_bac_ldon)')
              IF (iDbio3(iflx_vir_bac_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_bac_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_bac_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_ndi_sldon)')
              IF (iDbio3(iflx_vir_ndi_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_ndi_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_ndi_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nlg_sldon)')
              IF (iDbio3(iflx_vir_nlg_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nlg_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nlg_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nsm_sldon)')
              IF (iDbio3(iflx_vir_nsm_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nsm_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nsm_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_bac_sldon)')
              IF (iDbio3(iflx_vir_bac_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_bac_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_bac_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_ndi_srdon)')
              IF (iDbio3(iflx_vir_ndi_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_ndi_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_ndi_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nlg_srdon)')
              IF (iDbio3(iflx_vir_nlg_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nlg_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nlg_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_nsm_srdon)')
              IF (iDbio3(iflx_vir_nsm_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_nsm_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_nsm_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_vir_bac_srdon)')
              IF (iDbio3(iflx_vir_bac_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_vir_bac_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_vir_bac_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_exu_ndi_ldon)')
              IF (iDbio3(iflx_exu_ndi_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_exu_ndi_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_exu_ndi_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_exu_nlg_ldon)')
              IF (iDbio3(iflx_exu_nlg_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_exu_nlg_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_exu_nlg_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_exu_nsm_ldon)')
              IF (iDbio3(iflx_exu_nsm_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_exu_nsm_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_exu_nsm_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nsmz_nh4)')
              IF (iDbio3(iflx_ege_nsmz_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nsmz_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nsmz_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nmdz_nh4)')
              IF (iDbio3(iflx_ege_nmdz_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nmdz_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nmdz_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_nlgz_nh4)')
              IF (iDbio3(iflx_ege_nlgz_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_nlgz_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_nlgz_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_nsmz_ndet)')
              IF (iDbio3(iflx_mor_nsmz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_nsmz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_nsmz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_nmdz_ndet)')
              IF (iDbio3(iflx_mor_nmdz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_nmdz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_nmdz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_nlgz_ndet)')
              IF (iDbio3(iflx_mor_nlgz_ndet).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_nlgz_ndet)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_nlgz_ndet)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_bac_ldon)')
              IF (iDbio3(iflx_mor_bac_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_bac_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_bac_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_bac_sldon)')
              IF (iDbio3(iflx_mor_bac_sldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_bac_sldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_bac_sldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_mor_bac_srdon)')
              IF (iDbio3(iflx_mor_bac_srdon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_mor_bac_srdon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_mor_bac_srdon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_ege_hip_nh4)')
              IF (iDbio3(iflx_ege_hip_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_ege_hip_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_ege_hip_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_rem_ndet_nh4)')
              IF (iDbio3(iflx_rem_ndet_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_rem_ndet_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_rem_ndet_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_dnit_no3_nh4)')
              IF (iDbio3(iflx_dnit_no3_nh4).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_dnit_no3_nh4)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_dnit_no3_nh4)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_nit_nh4_no3)')
              IF (iDbio3(iflx_nit_nh4_no3).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_nit_nh4_no3)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_nit_nh4_no3)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_rem_sldon_ldon)')
              IF (iDbio3(iflx_rem_sldon_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_rem_sldon_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_rem_sldon_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iflx_rem_srdon_ldon)')
              IF (iDbio3(iflx_rem_srdon_ldon).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iflx_rem_srdon_ldon)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iflx_rem_srdon_ldon)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(icased)')
              IF (iDbio2(icased).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(icased)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(icased)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(icadet_arag_btf)')
              IF (iDbio2(icadet_arag_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(icadet_arag_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(icadet_arag_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(icadet_calc_btf)')
              IF (iDbio2(icadet_calc_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(icadet_calc_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(icadet_calc_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(indet_btf)')
              IF (iDbio2(indet_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(indet_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(indet_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipdet_btf)')
              IF (iDbio2(ipdet_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipdet_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipdet_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(isidet_btf)')
              IF (iDbio2(isidet_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(isidet_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(isidet_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ialk_btf)')
              IF (iDbio2(ialk_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ialk_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ialk_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(idic_btf)')
              IF (iDbio2(idic_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(idic_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(idic_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ifed_btf)')
              IF (iDbio2(ifed_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ifed_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ifed_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inh4_btf)')
              IF (iDbio2(inh4_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(inh4_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(inh4_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3_btf)')
              IF (iDbio2(ino3_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ino3_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ino3_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(io2_btf)')
              IF (iDbio2(io2_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(io2_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(io2_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipo4_btf)')
              IF (iDbio2(ipo4_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipo4_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipo4_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(isio4_btf)')
              IF (iDbio2(isio4_btf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(isio4_btf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(isio4_btf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imxl_depth)')
              IF (iDbio2(imxl_depth).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(imxl_depth)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(imxl_depth)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imxl_level)')
              IF (iDbio2(imxl_level).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(imxl_level)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(imxl_level)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ialpha)')
              IF (iDbio2(ialpha).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ialpha)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ialpha)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico2star)')
              IF (iDbio2(ico2star).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ico2star)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ico2star)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipco2surf)')
              IF (iDbio2(ipco2surf).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipco2surf)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipco2surf)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ico2_flx)')
              IF (iDbio2(ico2_flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ico2_flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ico2_flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(io2_flx)')
              IF (iDbio2(io2_flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(io2_flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(io2_flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iironsed_flx)')
              IF (iDbio2(iironsed_flx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iironsed_flx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iironsed_flx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(inpp_100)')
              IF (iDbio2(inpp_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(inpp_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(inpp_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imesozoo_200)')
              IF (iDbio2(imesozoo_200).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(imesozoo_200)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(imesozoo_200)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_sm)')
              IF (iDbio2(iprod_n_100_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_n_100_sm)')
              IF (iDbio2(iaggloss_n_100_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iaggloss_n_100_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iaggloss_n_100_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_sm)')
              IF (iDbio2(izloss_n_100_sm).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_sm)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_sm)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_lg)')
              IF (iDbio2(iprod_n_100_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_n_100_lg)')
              IF (iDbio2(iaggloss_n_100_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iaggloss_n_100_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iaggloss_n_100_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_lg)')
              IF (iDbio2(izloss_n_100_lg).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_lg)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_lg)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_di)')
              IF (iDbio2(iprod_n_100_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_n_100_di)')
              IF (iDbio2(iaggloss_n_100_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iaggloss_n_100_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iaggloss_n_100_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_di)')
              IF (iDbio2(izloss_n_100_di).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_di)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_di)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_smz)')
              IF (iDbio2(iprod_n_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iingest_n_100_smz)')
              IF (iDbio2(iingest_n_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iingest_n_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iingest_n_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_smz)')
              IF (iDbio2(izloss_n_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ihploss_n_100_smz)')
              IF (iDbio2(ihploss_n_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ihploss_n_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ihploss_n_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_ndet_100_smz)')
              IF (iDbio2(iprod_ndet_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_ndet_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_ndet_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_mdz)')
              IF (iDbio2(iprod_n_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iingest_n_100_mdz)')
              IF (iDbio2(iingest_n_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iingest_n_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iingest_n_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_mdz)')
              IF (iDbio2(izloss_n_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ihploss_n_100_mdz)')
              IF (iDbio2(ihploss_n_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ihploss_n_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ihploss_n_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_ndet_100_mdz)')
              IF (iDbio2(iprod_ndet_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_ndet_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_ndet_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_lgz)')
              IF (iDbio2(iprod_n_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iingest_n_100_lgz)')
              IF (iDbio2(iingest_n_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iingest_n_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iingest_n_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_lgz)')
              IF (iDbio2(izloss_n_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ihploss_n_100_lgz)')
              IF (iDbio2(ihploss_n_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ihploss_n_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ihploss_n_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_ndet_100_lgz)')
              IF (iDbio2(iprod_ndet_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_ndet_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_ndet_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_n_100_bact)')
              IF (iDbio2(iprod_n_100_bact).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_bact)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_bact)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_bact)')
              IF (iDbio2(izloss_n_100_bact).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_bact)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_bact)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(imesozooprod_200)')
              IF (iDbio2(imesozooprod_200).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(imesozooprod_200)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(imesozooprod_200)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iuptake_din_100)')
              IF (iDbio2(iuptake_din_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iuptake_din_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iuptake_din_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iuptake_no3_n2_100)')
              IF (iDbio2(iuptake_no3_n2_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iuptake_no3_n2_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iuptake_no3_n2_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_mesozoo_100)')
              IF (iDbio2(iprod_mesozoo_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_mesozoo_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_mesozoo_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iz_ratio_100)')
              IF (iDbio2(iz_ratio_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iz_ratio_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iz_ratio_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ipe_ratio_100)')
              IF (iDbio2(ipe_ratio_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipe_ratio_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipe_ratio_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(if_ratio_100)')
              IF (iDbio2(if_ratio_100).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(if_ratio_100)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(if_ratio_100)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_don_100_smz)')
              IF (iDbio2(iprod_don_100_smz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_don_100_smz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_don_100_smz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_don_100_mdz)')
              IF (iDbio2(iprod_don_100_mdz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_don_100_mdz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_don_100_mdz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iprod_don_100_lgz)')
              IF (iDbio2(iprod_don_100_lgz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_don_100_lgz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_don_100_lgz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ino3denit_sed)')
              IF (iDbio2(ino3denit_sed).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ino3denit_sed)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ino3denit_sed)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(ijno3denit_wc_vint)')
              IF (iDbio2(ijno3denit_wc_vint).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ijno3denit_wc_vint)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ijno3denit_wc_vint)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# ifdef COASTDIAT
            CASE ('Dout(iprod_n_100_md)')
              IF (iDbio2(iprod_n_100_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iprod_n_100_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iprod_n_100_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iaggloss_n_100_md)')
              IF (iDbio2(iaggloss_n_100_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iaggloss_n_100_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iaggloss_n_100_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(izloss_n_100_md)')
              IF (iDbio2(izloss_n_100_md).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(izloss_n_100_md)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(izloss_n_100_md)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
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
            WRITE (out,80) htotal_scale_lo(ng), 'htotal_scale_lo',      &
     &            '? (None)'
            WRITE (out,80) htotal_scale_hi(ng), 'htotal_scale_hi',      &
     &            '? (None)'
            WRITE (out,80) RHO_0(ng), 'RHO_0',                          &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,70) NKML(ng), 'NKML',                            &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_0(ng), 'a_0',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_1(ng), 'a_1',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_2(ng), 'a_2',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_3(ng), 'a_3',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_4(ng), 'a_4',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a_5(ng), 'a_5',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) b_0(ng), 'b_0',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) b_1(ng), 'b_1',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) b_2(ng), 'b_2',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) b_3(ng), 'b_3',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) c_0(ng), 'c_0',                              &
     &            'coefficients for O2 saturation (N/A)'
            WRITE (out,80) a1_co2(ng), 'a1_co2',                        &
     &            'Compute the Schmidt number of CO2 in seawater (N/A)'
            WRITE (out,80) a2_co2(ng), 'a2_co2',                        &
     &            'Compute the Schmidt number of CO2 in seawater (N/A)'
            WRITE (out,80) a3_co2(ng), 'a3_co2',                        &
     &            'Compute the Schmidt number of CO2 in seawater (N/A)'
            WRITE (out,80) a4_co2(ng), 'a4_co2',                        &
     &            'Compute the Schmidt number of CO2 in seawater (N/A)'
            WRITE (out,80) a1_o2(ng), 'a1_o2',                          &
     &            'Compute the Schmidt number of O2 in seawater (N/A)'
            WRITE (out,80) a2_o2(ng), 'a2_o2',                          &
     &            'Compute the Schmidt number of O2 in seawater (N/A)'
            WRITE (out,80) a3_o2(ng), 'a3_o2',                          &
     &            'Compute the Schmidt number of O2 in seawater (N/A)'
            WRITE (out,80) a4_o2(ng), 'a4_o2',                          &
     &            'Compute the Schmidt number of O2 in seawater (N/A)'
            WRITE (out,80) mass_2_n(ng), 'mass_2_n',                    &
     &            'Stoichiometry (g mol N-1)'
            WRITE (out,80) n_2_n_denit(ng), 'n_2_n_denit',              &
     &            'Stoichiometry (mol N NO3 mol N org-1)'
            WRITE (out,80) o2_2_c(ng), 'o2_2_c',                        &
     &            'Stoichiometry (mol O2 mol C-1)'
            WRITE (out,80) o2_2_nfix(ng), 'o2_2_nfix',                  &
     &            'Stoichiometry (mol O2 mol N-1)'
            WRITE (out,80) o2_2_nh4(ng), 'o2_2_nh4',                    &
     &            'Stoichiometry (mol O2 mol N-1)'
            WRITE (out,80) o2_2_nitrif(ng), 'o2_2_nitrif',              &
     &            'Stoichiometry (mol O2 mol N-1)'
            WRITE (out,80) o2_2_no3(ng), 'o2_2_no3',                    &
     &            'Stoichiometry (mol O2 mol N-1)'
            WRITE (out,80) k_fed_Di(ng), 'k_fed_Di',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fed kg-1)'
            WRITE (out,80) k_fed_Lg(ng), 'k_fed_Lg',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fed kg-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_fed_Md(ng), 'k_fed_Md',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fed kg-1)'
# endif
            WRITE (out,80) k_fed_Sm(ng), 'k_fed_Sm',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fed kg-1)'
            WRITE (out,80) k_nh4_Lg(ng), 'k_nh4_Lg',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NH4 kg-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_nh4_Md(ng), 'k_nh4_Md',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NH4 kg-1)'
# endif
            WRITE (out,80) k_nh4_Sm(ng), 'k_nh4_Sm',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NH4 kg-1)'
            WRITE (out,80) k_nh4_Di(ng), 'k_nh4_Di',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NH4 kg-1)'
            WRITE (out,80) k_no3_Lg(ng), 'k_no3_Lg',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NO3 kg-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_no3_Md(ng), 'k_no3_Md',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NO3 kg-1)'
# endif
            WRITE (out,80) k_no3_Sm(ng), 'k_no3_Sm',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NO3 kg-1)'
            WRITE (out,80) k_no3_Di(ng), 'k_no3_Di',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol NO3 kg-1)'
            WRITE (out,80) k_po4_Di(ng), 'k_po4_Di',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol PO4 kg-1)'
            WRITE (out,80) k_po4_Lg(ng), 'k_po4_Lg',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol PO4 kg-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_po4_Md(ng), 'k_po4_Md',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol PO4 kg-1)'
# endif
            WRITE (out,80) k_po4_Sm(ng), 'k_po4_Sm',                    &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol PO4 kg-1)'
            WRITE (out,80) k_sio4_Lg(ng), 'k_sio4_Lg',                  &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol SiO4 kg-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_sio4_Md(ng), 'k_sio4_Md',                  &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol SiO4 kg-1)'
# endif
            WRITE (out,80) k_fe_2_n_Di(ng), 'k_fe_2_n_Di',              &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
            WRITE (out,80) k_fe_2_n_Lg(ng), 'k_fe_2_n_Lg',              &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
# ifdef COASTDIAT
            WRITE (out,80) k_fe_2_n_Md(ng), 'k_fe_2_n_Md',              &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
# endif
            WRITE (out,80) k_fe_2_n_Sm(ng), 'k_fe_2_n_Sm',              &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
            WRITE (out,80) fe_2_n_max_Sm(ng), 'fe_2_n_max_Sm',          &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
# ifdef COASTDIAT
            WRITE (out,80) fe_2_n_max_Md(ng), 'fe_2_n_max_Md',          &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
# endif
            WRITE (out,80) fe_2_n_max_Lg(ng), 'fe_2_n_max_Lg',          &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
            WRITE (out,80) fe_2_n_max_Di(ng), 'fe_2_n_max_Di',          &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
            WRITE (out,80) fe_2_n_upt_fac(ng), 'fe_2_n_upt_fac',        &
     &            'Nutrient Limitation Parameters (phytoplankton) (mol Fe mol N-1)'
            WRITE (out,80) alpha_Di(ng), 'alpha_Di',                    &
     &            'Phytoplankton light limitation/growth rate (g C g Chl-1 m2 W-1 s-1)'
            WRITE (out,80) alpha_Lg(ng), 'alpha_Lg',                    &
     &            'Phytoplankton light limitation/growth rate (g C g Chl-1 m2 W-1 s-1)'
# ifdef COASTDIAT
            WRITE (out,80) alpha_Md(ng), 'alpha_Md',                    &
     &            'Phytoplankton light limitation/growth rate (g C g Chl-1 m2 W-1 s-1)'
# endif
            WRITE (out,80) alpha_Sm(ng), 'alpha_Sm',                    &
     &            'Phytoplankton light limitation/growth rate (g C g Chl-1 m2 W-1 s-1)'
            WRITE (out,80) kappa_eppley(ng), 'kappa_eppley',            &
     &            'Phytoplankton light limitation/growth rate (deg C-1)'
            WRITE (out,80) P_C_max_Di(ng), 'P_C_max_Di',                &
     &            'Phytoplankton light limitation/growth rate (s-1)'
            WRITE (out,80) P_C_max_Lg(ng), 'P_C_max_Lg',                &
     &            'Phytoplankton light limitation/growth rate (s-1)'
# ifdef COASTDIAT
            WRITE (out,80) P_C_max_Md(ng), 'P_C_max_Md',                &
     &            'Phytoplankton light limitation/growth rate (s-1)'
# endif
            WRITE (out,80) P_C_max_Sm(ng), 'P_C_max_Sm',                &
     &            'Phytoplankton light limitation/growth rate (s-1)'
            WRITE (out,80) thetamax_Di(ng), 'thetamax_Di',              &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
            WRITE (out,80) thetamax_Lg(ng), 'thetamax_Lg',              &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
# ifdef COASTDIAT
            WRITE (out,80) thetamax_Md(ng), 'thetamax_Md',              &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
# endif
            WRITE (out,80) thetamax_Sm(ng), 'thetamax_Sm',              &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
            WRITE (out,80) bresp_Di(ng), 'bresp_Di',                    &
     &            'Phytoplankton light limitation/growth rate (sec-1)'
            WRITE (out,80) bresp_Lg(ng), 'bresp_Lg',                    &
     &            'Phytoplankton light limitation/growth rate (sec-1)'
# ifdef COASTDIAT
            WRITE (out,80) bresp_Md(ng), 'bresp_Md',                    &
     &            'Phytoplankton light limitation/growth rate (sec-1)'
# endif
            WRITE (out,80) bresp_Sm(ng), 'bresp_Sm',                    &
     &            'Phytoplankton light limitation/growth rate (sec-1)'
            WRITE (out,80) thetamin(ng), 'thetamin',                    &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
            WRITE (out,80) thetamin_nolim(ng), 'thetamin_nolim',        &
     &            'Phytoplankton light limitation/growth rate (g Chl g C-1)'
            WRITE (out,80) zpllgr(ng), 'zpllgr',                        &
     &            'Phytoplankton light limitation/growth rate (dimensionless)'
            WRITE (out,80) gamma_irr_mem(ng), 'gamma_irr_mem',          &
     &            'Phytoplankton light limitation/growth rate (s-1)'
            WRITE (out,80) gamma_mu_mem(ng), 'gamma_mu_mem',            &
     &            'Phytoplankton aggregation rate (s-1)'
            WRITE (out,80) k_n_inhib_Di(ng), 'k_n_inhib_Di',            &
     &            'Nitrogen fixation inhibition parameters (mol NO3 kg-1)'
            WRITE (out,80) o2_inhib_Di_pow(ng), 'o2_inhib_Di_pow',      &
     &            'Nitrogen fixation inhibition parameters (mol O2-1 m3)'
            WRITE (out,80) o2_inhib_Di_sat(ng), 'o2_inhib_Di_sat',      &
     &            'Nitrogen fixation inhibition parameters (mol O2 kg-1)'
            WRITE (out,70) p_2_n_static(ng), 'p_2_n_static',            &
     &            'Other stoichiometry (N/A)'
            WRITE (out,80) c_2_n(ng), 'c_2_n',                          &
     &            'Other stoichiometry (N/A)'
            WRITE (out,80) alk_2_n_denit(ng), 'alk_2_n_denit',          &
     &            'Other stoichiometry (eq. alk mol NO3-1)'
            WRITE (out,80) p_2_n_static_Di(ng), 'p_2_n_static_Di',      &
     &            'Other stoichiometry (mol P mol N-1)'
            WRITE (out,80) p_2_n_static_Lg(ng), 'p_2_n_static_Lg',      &
     &            'Other stoichiometry (mol P mol N-1)'
# ifdef COASTDIAT
            WRITE (out,80) p_2_n_static_Md(ng), 'p_2_n_static_Md',      &
     &            'Other stoichiometry (mol P mol N-1)'
# endif
            WRITE (out,80) p_2_n_static_Sm(ng), 'p_2_n_static_Sm',      &
     &            'Other stoichiometry (mol P mol N-1)'
            WRITE (out,80) si_2_n_static_Lg(ng), 'si_2_n_static_Lg',    &
     &            'Other stoichiometry (mol Si mol N-1)'
            WRITE (out,80) si_2_n_max_Lg(ng), 'si_2_n_max_Lg',          &
     &            'Other stoichiometry (mol Si mol N-1)'
# ifdef COASTDIAT
            WRITE (out,80) si_2_n_static_Md(ng), 'si_2_n_static_Md',    &
     &            'Other stoichiometry (mol Si mol N-1)'
            WRITE (out,80) si_2_n_max_Md(ng), 'si_2_n_max_Md',          &
     &            'Other stoichiometry (mol Si mol N-1)'
# endif
            WRITE (out,80) ca_2_n_arag(ng), 'ca_2_n_arag',              &
     &            'Other stoichiometry (mol Ca mol N-1)'
            WRITE (out,80) ca_2_n_calc(ng), 'ca_2_n_calc',              &
     &            'Other stoichiometry (mol Ca mol N-1)'
            WRITE (out,80) caco3_sat_max(ng), 'caco3_sat_max',          &
     &            'Other stoichiometry (dimensionless)'
            WRITE (out,80) q_p_2_n_smz(ng), 'q_p_2_n_smz',              &
     &            'Zooplankton Stoichiometry - presently static (mol P mol N-1)'
            WRITE (out,80) q_p_2_n_mdz(ng), 'q_p_2_n_mdz',              &
     &            'Zooplankton Stoichiometry - presently static (mol P mol N-1)'
            WRITE (out,80) q_p_2_n_lgz(ng), 'q_p_2_n_lgz',              &
     &            'Zooplankton Stoichiometry - presently static (mol P mol N-1)'
            WRITE (out,80) q_p_2_n_bact(ng), 'q_p_2_n_bact',            &
     &            'Bacteria Stoichiometry - presently static (mol P mol N-1)'
            WRITE (out,80) agg_Sm(ng), 'agg_Sm',                        &
     &            'Phytoplankton aggregation (s-1 (mole N kg)-1)'
            WRITE (out,80) agg_Di(ng), 'agg_Di',                        &
     &            'Phytoplankton aggregation (s-1 (mole N kg)-1)'
            WRITE (out,80) agg_Lg(ng), 'agg_Lg',                        &
     &            'Phytoplankton aggregation (s-1 (mole N kg)-1)'
# ifdef COASTDIAT
            WRITE (out,80) agg_Md(ng), 'agg_Md',                        &
     &            'Phytoplankton aggregation (s-1 (mole N kg)-1)'
# endif
            WRITE (out,80) vir_Sm(ng), 'vir_Sm',                        &
     &            'Phytoplankton and bacterial losses to viruses (mole N kg)-1)'
            WRITE (out,80) vir_Di(ng), 'vir_Di',                        &
     &            'Phytoplankton and bacterial losses to viruses (mole N kg)-1)'
            WRITE (out,80) vir_Lg(ng), 'vir_Lg',                        &
     &            'Phytoplankton and bacterial losses to viruses (mole N kg)-1)'
# ifdef COASTDIAT
            WRITE (out,80) vir_Md(ng), 'vir_Md',                        &
     &            'Phytoplankton and bacterial losses to viruses (mole N kg)-1)'
# endif
            WRITE (out,80) vir_Bact(ng), 'vir_Bact',                    &
     &            'Phytoplankton and bacterial losses to viruses (mole N kg)-1)'
            WRITE (out,80) ktemp_vir(ng), 'ktemp_vir',                  &
     &            'Phytoplankton and bacterial losses to viruses (C-1)'
            WRITE (out,80) exu_Sm(ng), 'exu_Sm',                        &
     &            'Phytoplankton losses to exudation (dimensionless)'
            WRITE (out,80) exu_Di(ng), 'exu_Di',                        &
     &            'Phytoplankton losses to exudation (dimensionless)'
            WRITE (out,80) exu_Lg(ng), 'exu_Lg',                        &
     &            'Phytoplankton losses to exudation (dimensionless)'
# ifdef COASTDIAT
            WRITE (out,80) exu_Md(ng), 'exu_Md',                        &
     &            'Phytoplankton losses to exudation (dimensionless)'
# endif
            WRITE (out,80) imax_smz(ng), 'imax_smz',                    &
     &            'Zooplankton ingestion parameterization and temperature dependence (s-1)'
            WRITE (out,80) imax_mdz(ng), 'imax_mdz',                    &
     &            'Zooplankton ingestion parameterization and temperature dependence (s-1)'
            WRITE (out,80) imax_lgz(ng), 'imax_lgz',                    &
     &            'Zooplankton ingestion parameterization and temperature dependence (s-1)'
            WRITE (out,80) ki_smz(ng), 'ki_smz',                        &
     &            'Zooplankton ingestion parameterization and temperature dependence (moles N kg-1)'
            WRITE (out,80) ki_mdz(ng), 'ki_mdz',                        &
     &            'Zooplankton ingestion parameterization and temperature dependence (moles N kg-1)'
            WRITE (out,80) ki_lgz(ng), 'ki_lgz',                        &
     &            'Zooplankton ingestion parameterization and temperature dependence (moles N kg-1)'
            WRITE (out,80) ktemp_smz(ng), 'ktemp_smz',                  &
     &            'Zooplankton ingestion parameterization and temperature dependence (C-1)'
            WRITE (out,80) ktemp_mdz(ng), 'ktemp_mdz',                  &
     &            'Zooplankton ingestion parameterization and temperature dependence (C-1)'
            WRITE (out,80) ktemp_lgz(ng), 'ktemp_lgz',                  &
     &            'Zooplankton ingestion parameterization and temperature dependence (C-1)'
            WRITE (out,80) mu_max_bact(ng), 'mu_max_bact',              &
     &            'Bacterial growth and uptake parameters (s-1)'
            WRITE (out,80) k_ldon_bact(ng), 'k_ldon_bact',              &
     &            'Bacterial growth and uptake parameters (mol ldon kg-1)'
            WRITE (out,80) ktemp_bact(ng), 'ktemp_bact',                &
     &            'Bacterial growth and uptake parameters (C-1)'
            WRITE (out,80) nswitch_smz(ng), 'nswitch_smz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) nswitch_mdz(ng), 'nswitch_mdz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) nswitch_lgz(ng), 'nswitch_lgz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mswitch_smz(ng), 'mswitch_smz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mswitch_mdz(ng), 'mswitch_mdz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mswitch_lgz(ng), 'mswitch_lgz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_smp(ng), 'smz_ipa_smp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# ifdef COASTDIAT
            WRITE (out,80) smz_ipa_mdp(ng), 'smz_ipa_mdp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# endif
            WRITE (out,80) smz_ipa_lgp(ng), 'smz_ipa_lgp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_diaz(ng), 'smz_ipa_diaz',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_smz(ng), 'smz_ipa_smz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_mdz(ng), 'smz_ipa_mdz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_lgz(ng), 'smz_ipa_lgz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_bact(ng), 'smz_ipa_bact',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) smz_ipa_det(ng), 'smz_ipa_det',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_smp(ng), 'mdz_ipa_smp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# ifdef COASTDIAT
            WRITE (out,80) mdz_ipa_mdp(ng), 'mdz_ipa_mdp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# endif
            WRITE (out,80) mdz_ipa_lgp(ng), 'mdz_ipa_lgp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_diaz(ng), 'mdz_ipa_diaz',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_smz(ng), 'mdz_ipa_smz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_mdz(ng), 'mdz_ipa_mdz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_lgz(ng), 'mdz_ipa_lgz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_bact(ng), 'mdz_ipa_bact',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) mdz_ipa_det(ng), 'mdz_ipa_det',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_smp(ng), 'lgz_ipa_smp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# ifdef COASTDIAT
            WRITE (out,80) lgz_ipa_mdp(ng), 'lgz_ipa_mdp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
# endif
            WRITE (out,80) lgz_ipa_lgp(ng), 'lgz_ipa_lgp',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_diaz(ng), 'lgz_ipa_diaz',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_smz(ng), 'lgz_ipa_smz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_mdz(ng), 'lgz_ipa_mdz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_lgz(ng), 'lgz_ipa_lgz',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_bact(ng), 'lgz_ipa_bact',            &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) lgz_ipa_det(ng), 'lgz_ipa_det',              &
     &            'Zooplankton switching and prey preference parameters (dimensionless)'
            WRITE (out,80) gge_max_smz(ng), 'gge_max_smz',              &
     &            'Zooplankton bioenergetics (dimensionless)'
            WRITE (out,80) gge_max_mdz(ng), 'gge_max_mdz',              &
     &            'Zooplankton bioenergetics (dimensionless)'
            WRITE (out,80) gge_max_lgz(ng), 'gge_max_lgz',              &
     &            'Zooplankton bioenergetics (dimensionless)'
            WRITE (out,80) bresp_smz(ng), 'bresp_smz',                  &
     &            'Zooplankton bioenergetics (s-1)'
            WRITE (out,80) bresp_mdz(ng), 'bresp_mdz',                  &
     &            'Zooplankton bioenergetics (s-1)'
            WRITE (out,80) bresp_lgz(ng), 'bresp_lgz',                  &
     &            'Zooplankton bioenergetics (s-1)'
            WRITE (out,80) gge_max_bact(ng), 'gge_max_bact',            &
     &            'Bacterial bioenergetics (dimensionless)'
            WRITE (out,80) bresp_bact(ng), 'bresp_bact',                &
     &            'Bacterial bioenergetics (s-1)'
            WRITE (out,80) phi_det_smz(ng), 'phi_det_smz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_det_mdz(ng), 'phi_det_mdz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_det_lgz(ng), 'phi_det_lgz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldon_smz(ng), 'phi_ldon_smz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldon_mdz(ng), 'phi_ldon_mdz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldon_lgz(ng), 'phi_ldon_lgz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldop_smz(ng), 'phi_ldop_smz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldop_mdz(ng), 'phi_ldop_mdz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldop_lgz(ng), 'phi_ldop_lgz',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdon_smz(ng), 'phi_srdon_smz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdon_mdz(ng), 'phi_srdon_mdz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdon_lgz(ng), 'phi_srdon_lgz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdop_smz(ng), 'phi_srdop_smz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdop_mdz(ng), 'phi_srdop_mdz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdop_lgz(ng), 'phi_srdop_lgz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldon_smz(ng), 'phi_sldon_smz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldon_mdz(ng), 'phi_sldon_mdz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldon_lgz(ng), 'phi_sldon_lgz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldop_smz(ng), 'phi_sldop_smz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldop_mdz(ng), 'phi_sldop_mdz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldop_lgz(ng), 'phi_sldop_lgz',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_nh4_smz(ng), 'phi_nh4_smz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_nh4_mdz(ng), 'phi_nh4_mdz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_nh4_lgz(ng), 'phi_nh4_lgz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_po4_smz(ng), 'phi_po4_smz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_po4_mdz(ng), 'phi_po4_mdz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_po4_lgz(ng), 'phi_po4_lgz',              &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldon_vir(ng), 'phi_ldon_vir',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdon_vir(ng), 'phi_srdon_vir',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldon_vir(ng), 'phi_sldon_vir',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_ldop_vir(ng), 'phi_ldop_vir',            &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_srdop_vir(ng), 'phi_srdop_vir',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) phi_sldop_vir(ng), 'phi_sldop_vir',          &
     &            'Partitioning of zooplankton ingestion to other compartments (dimensionless)'
            WRITE (out,80) imax_hp(ng), 'imax_hp',                      &
     &            'Parameters for unresolved higher predators (s-1)'
            WRITE (out,80) ki_hp(ng), 'ki_hp',                          &
     &            'Parameters for unresolved higher predators (mol N kg-1)'
            WRITE (out,80) coef_hp(ng), 'coef_hp',                      &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) ktemp_hp(ng), 'ktemp_hp',                    &
     &            'Parameters for unresolved higher predators (C-1)'
            WRITE (out,80) nswitch_hp(ng), 'nswitch_hp',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) mswitch_hp(ng), 'mswitch_hp',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_smp(ng), 'hp_ipa_smp',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_lgp(ng), 'hp_ipa_lgp',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_diaz(ng), 'hp_ipa_diaz',              &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_smz(ng), 'hp_ipa_smz',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_mdz(ng), 'hp_ipa_mdz',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_lgz(ng), 'hp_ipa_lgz',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_bact(ng), 'hp_ipa_bact',              &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_ipa_det(ng), 'hp_ipa_det',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_det(ng), 'hp_phi_det',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_ldon(ng), 'hp_phi_ldon',              &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_ldop(ng), 'hp_phi_ldop',              &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_srdon(ng), 'hp_phi_srdon',            &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_srdop(ng), 'hp_phi_srdop',            &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_sldon(ng), 'hp_phi_sldon',            &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_sldop(ng), 'hp_phi_sldop',            &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_nh4(ng), 'hp_phi_nh4',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) hp_phi_po4(ng), 'hp_phi_po4',                &
     &            'Parameters for unresolved higher predators (dimensionless)'
            WRITE (out,80) felig_bkg(ng), 'felig_bkg',                  &
     &            'Iron chemistry (mol Fe kg-1)'
            WRITE (out,80) felig_2_don(ng), 'felig_2_don',              &
     &            'Iron chemistry (mol Fe mol N-1)'
            WRITE (out,80) fe_2_n_sed(ng), 'fe_2_n_sed',                &
     &            'Iron chemistry (mol Fe mol N-1)'
            WRITE (out,80) fe_coast(ng), 'fe_coast',                    &
     &            'Iron chemistry (mol Fe m kg-1 s-1)'
            WRITE (out,80) alpha_fescav(ng), 'alpha_fescav',            &
     &            'Iron chemistry (sec-1)'
            WRITE (out,80) ffe_sed_max(ng), 'ffe_sed_max',              &
     &            'Iron chemistry (micromol Fe m-2 d-1)'
            WRITE (out,80) beta_fescav(ng), 'beta_fescav',              &
     &            'Iron chemistry (mol N-1 sec-1)'
            WRITE (out,80) remin_eff_fedet(ng), 'remin_eff_fedet',      &
     &            'Iron chemistry (unitless)'
            WRITE (out,80) ki_fescav(ng), 'ki_fescav',                  &
     &            'Iron chemistry (watts m-2)'
            WRITE (out,80) io_fescav(ng), 'io_fescav',                  &
     &            'Iron chemistry (watts m-2)'
            WRITE (out,80) gamma_fescav(ng), 'gamma_fescav',            &
     &            'Iron chemistry (watts m-2)'
            WRITE (out,80) kfe_eq_lig_ll(ng), 'kfe_eq_lig_ll',          &
     &            'Iron chemistry (mol lig-1 kg)'
            WRITE (out,80) kfe_eq_lig_hl(ng), 'kfe_eq_lig_hl',          &
     &            'Iron chemistry (mol lig-1 kg)'
            WRITE (out,80) k_o2(ng), 'k_o2',                            &
     &            'Remineralization (mol O2 kg-1)'
            WRITE (out,80) o2_min(ng), 'o2_min',                        &
     &            'Remineralization (mol O2 kg-1)'
            WRITE (out,80) rpcaco3(ng), 'rpcaco3',                      &
     &            'Remineralization (mol N mol Ca-1)'
            WRITE (out,80) rplith(ng), 'rplith',                        &
     &            'Remineralization (mol N g lith-1)'
            WRITE (out,80) rpsio2(ng), 'rpsio2',                        &
     &            'Remineralization (mol N mol Si-1)'
            WRITE (out,80) gamma_ndet(ng), 'gamma_ndet',                &
     &            'Remineralization (s-1)'
            WRITE (out,80) gamma_cadet_arag(ng), 'gamma_cadet_arag',    &
     &            'Remineralization (s-1)'
            WRITE (out,80) gamma_cadet_calc(ng), 'gamma_cadet_calc',    &
     &            'Remineralization (s-1)'
            WRITE (out,80) gamma_sidet(ng), 'gamma_sidet',              &
     &            'Remineralization (s-1)'
            WRITE (out,80) phi_lith(ng), 'phi_lith',                    &
     &            'Remineralization (kg mol-1)'
            WRITE (out,80) k_lith(ng), 'k_lith',                        &
     &            'Remineralization (s-1)'
            WRITE (out,80) z_sed(ng), 'z_sed',                          &
     &            'Remineralization (m)'
            WRITE (out,80) k_no3_denit(ng), 'k_no3_denit',              &
     &            'Remineralization (mol NO3 kg-1)'
            WRITE (out,80) gamma_srdon(ng), 'gamma_srdon',              &
     &            'Dissolved Organic Material (s-1)'
            WRITE (out,80) gamma_srdop(ng), 'gamma_srdop',              &
     &            'Dissolved Organic Material (s-1)'
            WRITE (out,80) gamma_sldon(ng), 'gamma_sldon',              &
     &            'Dissolved Organic Material (s-1)'
            WRITE (out,80) gamma_sldop(ng), 'gamma_sldop',              &
     &            'Dissolved Organic Material (s-1)'
            WRITE (out,80) gamma_nitrif(ng), 'gamma_nitrif',            &
     &            'Nitrification (s-1)'
            WRITE (out,80) irr_inhibit(ng), 'irr_inhibit',              &
     &            'Nitrification (m2 W-1)'
            WRITE (out,80) htotal_in(ng), 'htotal_in',                  &
     &            '? (N/A)'
            WRITE (out,80) wsink(ng), 'wsink',                          &
     &            'Sinking velocity of detritus (m s-1)'
            WRITE (out,80) k_sed1(ng), 'k_sed1',                        &
     &            'Depth-based attenuation coefficient, factor (m^-1)'
            WRITE (out,80) k_sed2(ng), 'k_sed2',                        &
     &            'Depth-based attenuation coefficient, exponent (unitless)'
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
            END IF
#ifdef BENTHIC
              DO itrc=1,NBEN
                i=idben(itrc)
                IF (Hout(idBeTvar(i),ng)) WRITE (out,120)               &
   &                Hout(idBeTvar(i),ng), 'Hout(idBeTvar)',             &
   &                'Write out benthic tracer ', i, TRIM(Vname(1,idBeTvar(i)))
              END DO
#endif
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
            END IF
#endif
#ifdef DIAGNOSTICS_TS
            IF ((nDIA(ng).gt.0).and.ANY(Dout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTrate),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTrate)',               &
     &                'Write out rate of change of tracer ', itrc,      &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iThadv)',               &
     &                'Write out horizontal advection, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTxadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTxadv)',               &
     &                'Write out horizontal X-advection, tracer ', itrc,&
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTyadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTyadv)',               &
     &                'Write out horizontal Y-advection, tracer ', itrc,&
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTvadv)',               &
     &                'Write out vertical advection, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined TS_DIF2 || defined TS_DIF4
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iThdif)',               &
     &                'Write out horizontal diffusion, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(i,iTxdif),ng))                          &
     &            WRITE (out,120) .TRUE., 'Dout(iTxdif)',               &
     &                'Write out horizontal X-diffusion, tracer ', itrc,&
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTydif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTydif)',               &
     &                'Write out horizontal Y-diffusion, tracer ', itrc,&
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTsdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTsdif)',               &
     &                'Write out horizontal S-diffusion, tracer ', itrc,&
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
#  endif
# endif
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTvdif)',               &
     &                'Write out vertical diffusion, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
            END IF
#endif
#ifdef DIAGNOSTICS_BIO
            IF (nDIA(ng).gt.0) THEN
              IF (NDbio2d.gt.0) THEN
                DO itrc=1,NDbio2d
                  i=iDbio2(itrc)
                  IF (Dout(i,ng)) WRITE (out,130)                       &
     &                Dout(i,ng), 'Dout(iDbio2)',                       &
     &                'Write out diagnostics for', TRIM(Vname(1,i))
                END DO
              END IF
              DO itrc=1,NDbio3d
                i=iDbio3(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                         &
     &              Dout(i,ng), 'Dout(iDbio3)',                         &
     &              'Write out diagnostics for', TRIM(Vname(1,i))
              END DO
            END IF
#endif
          END IF
        END DO
      END IF
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
  60  FORMAT (/,/,' COBALT Model Parameters, Grid: ',i2.2,        &
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