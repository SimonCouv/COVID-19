proc run_polygenic {} {

  ########
  # set up trait and covariates
  # house.gz matrix [n x n] of household effects (1 if same household) is automatically generated upon 'load pedigree'
  ########

  load pedigree data/solar_pedigree.ped
  load phenotypes data/pheno_tmp.phen
  trait pheno
  covariate age sex
  outdir results/solar/tmp_output

  ########
  # run polygenic
  ########

  house
  polygenic -screen

  return ""
}

proc run_polygenic_bmi {} {

  ########
  # set up trait and covariates
  # house.gz matrix [n x n] of household effects (1 if same household) is automatically generated upon 'load pedigree'
  ########

  load pedigree data/solar_pedigree.ped
  load phenotypes data/pheno_tmp.phen
  trait pheno
  covariate age sex bmi
  outdir results/solar/tmp_output

  ########
  # run polygenic
  ########

  house
  polygenic -screen

  return ""
}
