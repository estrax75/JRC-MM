pro l3time_unc, daysNumber, data_in, idx_third, distance, mean_field, std_mean, nfield, splitDims, faparMean, cloudtype=cloudtype

  ;            /* Compute vegetation index and related fields, mean, standard deviation,  number of samples and file number */
  ;            /* Old calculation of selected point, only FPAR taken into account */
  ;            /* ComputeFPAR (NbFiles,line,nelem,elem1,elem2,p_FPAR,out_bad_ui8,p_flag,FPAR,MeanFPAR,DevFPAR,NbFPAR,FPARNb); */
  ;            /* New calculation, rectR and NIR taken into account */
  daysNumber, data_in, idx_third, distance, mean_field, std_mean, nfield, splitDims, faparMean, cloudtype=cloudtype

  for i=0, do begin
    ComputeFPARwRectUncert(p_FPAR,   p_brf_r,   p_brf_n,
    p_FPAR_u, p_brf_r_u, p_brf_n_u,
    out_bad_ui8,
    p_flag,
    FPAR,   brf_r,   brf_n,
    FPAR_u, brf_r_u, brf_n_u,
    DevFPAR, DevRectR, DevRectNIR,
    NbFPAR, FPARNb);
  endfor
end
