pro ComputeFPARwRectUncert, daysNumber, $
  p_FAPAR, $     ; Input FPAR */
  p_brf_r, $     ; Input Rect R */
  p_brf_n, $     ; Input Rect NIR */
  p_FAPAR_u, $   ; Input FPAR */
  p_brf_r_u, $   ; Input Rect R */
  p_brf_n_u, $   ; Input Rect NIR */
  badvalue, $                 ; Input bad value */
  p_flag, $      ; Input flag */
  outFAPAR, $                   ; Output selected FPAR */
  outbrf_r, $                   ; Output selected rect R */
  outbrf_n, $                   ; Output selected rect NIR */
  outFAPAR_u, $                 ; Output selected FPAR */
  outbrf_r_u, $                 ; Output selected rect R */
  outbrf_n_u, $                 ; Output selected rect NIR */
  outdev_FAPAR, $               ; Output stdev FPAR */
  outdev_brf_r, $               ; Output stdev rect R */
  outdev_brf_n, $               ; Output stdev rect NIR */
  outp_DayAvailable, $               ; Output nb of values used */
  outp_DayIndex               ; Output file number selected */
;  outp_NbValue, $               ; Output nb of values used */
;    outp_ValueNb               ; Output file number selected */

  xval=0. & xval1=0. & xval2=0.

  i=0 & f=0
  elem =0
  num =0 & num2=0
  ind=0 & ind1=0 & ind2=0

  ;flag;
  indexMDthres=0
  indexFPARnonzero=0
  indexrectRNIRnonzero=0
  selectedIndex=0 & secondIndex=0

  ; Local vectors */
  ;fpar=0d & rred=0d & rnir=0d;
  ;fpar_u=0d & rred_u=0d & rnir_u=0d;
  distance=0d
  v_dummy=0d

  Npoints=0 & MinMDindex=0 & OriginalIndex=0

  std_fpar = 0d & std_rred = 0d & std_rnir = 0d & dummy = 0d;
  ContinueWithOutliers = 0;
  InvalidFpar = 0;
  out_bad = -1;

  ; Initialization */
  ; ----------------------------------------------------------------- */
  ; loop over the number of elements */
  ; ToDo flagCheck
  ;//printf("RectR: %g, Rect NIR: %g, FPAR: %g\n", rectR[f], rectNIR[f], fpar[f]);

  ;if (flag[f] < ITHRES_CLDICE && flag[f] > ITHRES_LAND) { ; flag values for valid FPAR */
  ; should be changed ng ??      */
  ;if (flag[f] != IFLAG_NEGRECT && (fpar[f]) == badvalue) {
  ; printf ("WARNING: ComputeFPAR - FPAR-flag inconsistent for"
  ;" f=%d at line=%d, elem=%d; flag=%d\n",f,line,elem,flag[f]); */
  ;flag[f] = out_bad_ui8;

  ; =============================================== */
  ;       BEGINNING OF SELECTION ALGORITHM          */
  ; =============================================== */

  ; Count how many points (i.e. days) we have with a valid, non-zero FPAR */
  NFparNonZero = 0;
  NrectRNIRnotBad = 0;
  ;ToDo FlagCheck
  indexrectRNIRnonzero[NrectRNIRnotBad] = i;
  NrectRNIRnotBad++;
  ;; new/to check
  NFparNonZero = 0;
  NrectRNIRnotBad = 0;
  ; cut off Nan
  validMask=finite(p_FAPAR) and finite(p_brf_r[*]) and finite(p_brf_r[*])
  goodIndexes=where(validMask eq 1, validMaskCount)
  if validMaskCount eq 0 then return
  ;create Soil mask
  idxMaskSoil=where(p_FAPAR[goodIndexes] ge 0.0 and $
    p_brf_r[goodIndexes] gt 0.0 and p_brf_r[goodIndexes] lt 1.0 and $
    p_brf_n[goodIndexes] gt 0.0 and p_brf_n[goodIndexes] lt 1.0)
  validMaskSoil=validMask*0
  validMaskSoil[goodIndexes[idxMaskSoil]]=1
  validMaskSoil=validMask*validMaskSoil
  ;create Veg mask
  idxMaskVeg=where(p_FAPAR[goodIndexes] gt 0.0 and $
    p_brf_r[goodIndexes] gt 0.0 and p_brf_r[goodIndexes] lt 1.0 and $
    p_brf_n[goodIndexes] gt 0.0 and p_brf_n[goodIndexes] lt 1.0)
  validMaskVeg=validMask*0
  validMaskVeg[goodIndexes[idxMaskVeg]]=1
  validMaskVeg=validMask*validMaskVeg
  ;final Vegetation Mask
  idx_maskVeg = where(validMaskVeg eq 1 and p_flag eq 0, countVeg)
  ;final Soil Mask
  idx_maskSoil = where(validMaskSoil eq 1 and (p_flag eq 4 or p_flag eq 5), countSoil)
  ;window,3
  ;tvscl, congrid(diff, 36,720)
  ; matrix approach
  ;if countVeg gt 0 then dayVeg[idx_maskVeg]=dayVeg[idx_maskVeg]+one[idx_maskVeg]
  ;
  NFparNonZero=countVeg
  ;NFparNonZeroIdx= where(p_FAPAR[*] ne out_bad_ui8 AND p_FAPAR[*] gt 0, NFparNonZero)
  NrectRNIRnotBad=countSoil
  ;NrectRNIRnotBadIdx= where(p_brf_r[*] ne out_bad_ui16 AND p_brf_r[*] ne out_bad_ui16, NrectRNIRnotBad)
  ; C Version Start
  ;  for i = 0, daysNumber-1 do begin
  ;    if ((fpar[i] ne out_bad_ui8) AND (fpar[i] gt 0)) then begin
  ;      indexFPARnonzero[NFparNonZero] = i;
  ;      NFparNonZero++;
  ;    endif
  ;    if ((rred[i] ne out_bad_i16) and (rnir[i] ne out_bad_i16)) then begin
  ;      indexrectRNIRnonzero[NrectRNIRnotBad] = i;
  ;      NrectRNIRnotBad++;
  ;    endif
  ;  endfor
  ; C Version End
  ;;

  ;
  ;* Note: if FPAR is valid ALSO RED and NIR are valid!!!!
  ;*/

  ; Cases handled :
  ;1. Valid FAPAR for more than 3 points:
  ;Find distance for each point.
  ;Continue (later) to reject the most distant points.
  ;2. Valid FPAR for only 2 points: Select as representative
  ;the point with the maximum FPAR of the two.
  ;3. Valid FPAR for only 1 point: Select this point.
  ;4. No valid FPAR points, more than 3 valid rectR
  ;and rectNIR points: Find distance, and continue
  ;later to reject outliers
  ;5. No valid FPAR points, only two valid rectR and rectNIR
  ;points: Select the one with the maximum rectNIR
  ;6. No valid FPAR: Only one with valid rectR - rectNIR:
  ;Select this point. */
  if NFparNonZero eq 0 and NrectRNIRnotBad eq 0 then return
  if NFparNonZero gt 2 then begin
    ; 1. If we have 3 points or more with a valid non-zero FPAR,
    ;Use FPAR and Rect R and NIR and find the distance
    ;of each day data-point: Call with 3 vectors */

    fpar = p_FAPAR[idx_maskVeg];
    rred = p_brf_r[idx_maskVeg];
    rnir = p_brf_n[idx_maskVeg];

    fpar_u = p_FAPAR_u[idx_maskVeg]
    rred_u = p_brf_r_u[idx_maskVeg]
    rnir_u = p_brf_n_u[idx_maskVeg]

    outMinMDindex=FindMinEuclDistanceUncert( $
      fpar,   rred,   rnir,$
      fpar_u, rred_u, rnir_u,$
      NFparNonZero, 3,$
      outDistance,$
      outStd_fpar, outStd_red, outStd_nir,$
      outMinMDindex);

    OriginalIndex = idx_maskVeg[outMinMDindex];
    ;    print, '*****list*****'
    ;    print, p_FAPAR
    ;    print, '*****selected******'
    ;    print, p_FAPAR[OriginalIndex]
    ;    print, '*****************'

    outFAPAR = p_FAPAR[OriginalIndex];;
    outBrf_r = p_brf_r[OriginalIndex];
    outBrf_n = p_brf_n[OriginalIndex];

    outFAPAR_u = p_FAPAR_u[OriginalIndex];;
    outBrf_r = p_brf_r_u[OriginalIndex];
    outBrf_n = p_brf_n_u[OriginalIndex];

    outDev_FAPAR = outStd_fpar;
    outDev_brf_r = outStd_red;
    outDev_brf_n = outStd_nir;

    outP_ValueNb = OriginalIndex; /* ORIGINAL */
    outP_NbValue = NFparNonZero;

    outp_DayAvailable=NFparNonZero
    outp_DayIndex=OriginalIndex
    ContinueWithOutliers = 1;
    InvalidFpar = 0;
  endif

  if (NFparNonZero eq 2) then begin
    ;2. Valid FPAR for only 2 points: Select as representative
    ;the point with the maximum FPAR of the two.
    selectedIndex = idx_maskVeg[0];
    secondIndex   = idx_maskVeg[1];

    if (p_FAPAR[selectedIndex] lt p_FAPAR[secondIndex]) then begin
      selectedIndex = idx_maskVeg[1];
      secondIndex   = idx_maskVeg[0];
    endif

    FAPAR = p_FAPAR[selectedIndex];
    brf_r = p_brf_r[selectedIndex];
    brf_n = p_brf_n[selectedIndex];

    FAPAR_u = p_FAPAR_u[selectedIndex];
    brf_r_u = p_brf_r_u[selectedIndex];
    brf_n_u = p_brf_n_u[selectedIndex];

    dev_FAPAR = (0.707 * abs(p_FAPAR[selectedIndex] - p_FAPAR[secondIndex]));
    dev_brf_r = (0.707 * abs(p_brf_r[selectedIndex] - p_brf_r[secondIndex]));
    dev_brf_n = (0.707 * abs(p_brf_n[selectedIndex] - p_brf_n[secondIndex]));

    outFAPAR = FAPAR;;
    outBrf_r = brf_r;
    outBrf_n = brf_n;

    outFAPAR_u = FAPAR_u;;
    outBrf_r = brf_r_u;
    outBrf_n = brf_n_u;

    outDev_FAPAR = dev_FAPAR;
    outDev_brf_r = dev_brf_r;
    outDev_brf_n = dev_brf_n;

    outP_ValueNb = selectedIndex; /* ORIGINAL */
    outP_NbValue = 2;

    outp_DayIndex=selectedIndex
    outp_DayAvailable=NFparNonZero
    ContinueWithOutliers = 0;
    return
  endif
  if (NFparNonZero eq 1) then begin
    ;3. Valid FPAR for only 1 point: Select this point.
    fpar = p_FAPAR[idx_maskVeg];
    rred = p_brf_r[idx_maskVeg];
    rnir = p_brf_n[idx_maskVeg];

    fpar_u = p_FAPAR_u[idx_maskVeg]
    rred_u = p_brf_r_u[idx_maskVeg]
    rnir_u = p_brf_n_u[idx_maskVeg]

    OriginalIndex=idx_maskVeg
    
    outFAPAR = p_FAPAR[OriginalIndex];;
    outBrf_r = p_brf_r[OriginalIndex];
    outBrf_n = p_brf_n[OriginalIndex];

    outFAPAR_u = p_FAPAR_u[OriginalIndex];;
    outBrf_r_u = p_brf_r_u[OriginalIndex];
    outBrf_n_u = p_brf_n_u[OriginalIndex];

    outDev_FAPAR = !VALUES.F_NAN;
    outDev_brf_r = !VALUES.F_NAN;
    outDev_brf_n = !VALUES.F_NAN;

    outP_ValueNb = OriginalIndex; /* ORIGINAL */
    outP_NbValue = 1;
    outp_DayIndex=OriginalIndex
    outp_DayAvailable=NFparNonZero

    return
  endif
  if (NrectRNIRnotBad gt 2 and NFparNonZero eq 0) then begin
    ; 4. NO points with a non-zero FPAR:
    ;use only the Rect R and NIR (entire) and find the distance
    ;of each data point: Call with 2 vectors - the third is
    ;a dummy, not used */

    Npoints = 0;
    ; oly valid idx (bare soil)
    fpar = p_FAPAR[idx_maskSoil];
    rred = p_brf_r[idx_maskSoil];
    rnir = p_brf_n[idx_maskSoil];

    fpar_u = p_FAPAR_u[idx_maskSoil];
    rred_u = p_brf_r_u[idx_maskSoil];
    rnir_u = p_brf_n_u[idx_maskSoil];

    outMinMDindex=FindMinEuclDistanceUncert($
      v_dummy, rred,   rnir,$
      v_dummy, rred_u, rnir_u,$
      Npoints, 2,$
      outDistance, outDummy, outStd_rred, outStd_rnir,$
      outMinMDindex);

    OriginalIndex = indexrectRNIRnonzero[outMinMDindex];

    outFAPAR = p_FAPAR[OriginalIndex];;
    outBrf_r = p_brf_r[OriginalIndex];
    outBrf_n = p_brf_n[OriginalIndex];

    outFAPAR_u = !VALUES.F_NAN;;
    outBrf_r_u = p_brf_r_u[OriginalIndex];
    outBrf_n_u = p_brf_n_u[OriginalIndex];

    outDev_FAPAR = !VALUES.F_NAN;;
    outDev_brf_r = outStd_rred;
    outDev_brf_n = outStd_rnir;

    outP_ValueNb = OriginalIndex; /* ORIGINAL */
    outP_NbValue = NrectRNIRnotBad;

    outp_DayIndex=OriginalIndex
    outp_DayAvailable=NrectRNIRnotBad
    ContinueWithOutliers = 1;
    InvalidFpar = 1;
  endif
  if (NrectRNIRnotBad eq 2 and NFparNonZero eq 0) then begin
    ;5. No valid FPAR points, only two valid rectR and rectNIR
    ;points: Select the one with the maximum rectNIR
    selectedIndex = idx_maskSoil[0];
    secondIndex   = idx_maskSoil[1];

    if (p_brf_n[selectedIndex] lt p_brf_n[secondIndex]) then begin
      selectedIndex = idx_maskSoil[1];
      secondIndex   = idx_maskSoil[0];
    endif

    OriginalIndex=selectedIndex
    
    FAPAR = p_FAPAR[OriginalIndex];
    brf_r = p_brf_r[OriginalIndex];
    brf_n = p_brf_n[OriginalIndex];

    FAPAR_u = p_FAPAR_u[OriginalIndex];
    brf_r_u = p_brf_r_u[OriginalIndex];
    brf_n_u = p_brf_n_u[OriginalIndex];

    dev_FAPAR = (0.707 * abs(p_FAPAR[selectedIndex] - p_FAPAR[secondIndex]));
    dev_brf_r = (0.707 * abs(p_brf_r[selectedIndex] - p_brf_r[secondIndex]));
    dev_brf_n = (0.707 * abs(p_brf_n[selectedIndex] - p_brf_n[secondIndex]));

    p_ValueNb = selectedIndex;
    p_NbValue = NFparNonZero;

    outFAPAR = FAPAR;
    outBrf_r = brf_r;
    outBrf_n = brf_n;

    outFAPAR_u = FAPAR_u;
    outBrf_r_u = brf_r_u;
    outBrf_n_u = brf_n_u;

    outDev_FAPAR = dev_FAPAR;
    outDev_brf_r = dev_brf_r;
    outDev_brf_n = dev_brf_n;

    outP_ValueNb = idx_maskSoil; /* ORIGINAL */
    outP_NbValue = 2;

    outp_DayIndex=selectedIndex
    outp_DayAvailable=NrectRNIRnotBad
    ContinueWithOutliers = 0;
    return
  endif
  if (NrectRNIRnotBad eq 1 and NFparNonZero eq 0) then begin
    ;6. No valid FPAR: Only one with valid rectR - rectNIR:
    ;Select this point. */
    fpar = p_FAPAR[idx_maskSoil];
    rred = p_brf_r[idx_maskSoil];
    rnir = p_brf_n[idx_maskSoil];

    fpar_u = p_FAPAR_u[idx_maskSoil]
    rred_u = p_brf_r_u[idx_maskSoil]
    rnir_u = p_brf_n_u[idx_maskSoil]

    OriginalIndex=idx_maskSoil

    outFAPAR = fpar;
    outBrf_r = rred;
    outBrf_n = rnir;

    outDev_FAPAR = fpar_u;
    outDev_brf_r = rred_u;
    outDev_brf_n = rnir_u;

    outDev_FAPAR = !VALUES.F_NAN;
    outDev_brf_r = !VALUES.F_NAN;
    outDev_brf_n = !VALUES.F_NAN;

    outP_ValueNb = OriginalIndex; /* ORIGINAL */
    outP_NbValue = 1;
    outp_DayIndex=OriginalIndex
    outp_DayAvailable=NrectRNIRnotBad

    return
  endif
  if ContinueWithOutliers then begin
    threshold = (InvalidFpar) ? 2.30 : (3.53)^2;

    ;/* In either case, count the points with a distance of more than 3.53 squared */
    ;int NPdistthres = 0;
    prev=n_elements(outDistance)
    outLayerIdx=where(outDistance lt threshold, count)
    ;/* Use all 3 vectors, if used previously */
    ;/* Repeat the point count, for clarity. So sue me. */
    ;/* Select the points, but also re-index the data */
    ; no outliers: done
    if count eq prev then return
    fpar1 = fpar[outLayerIdx];
    rred1 = rred[outLayerIdx];
    rnir1 = rnir[outLayerIdx];
    fpar_u1 = fpar_u[outLayerIdx];
    rred_u1 = rred_u[outLayerIdx];
    rnir_u1 = rnir_u[outLayerIdx];
    indexMDthres = outLayerIdx;

    type = InvalidFpar eq 1 ? 2 : 3
    outMinMDindex=FindMinEuclDistanceUncert( $
      fpar1,   rred1,   rnir1,$
      fpar_u1, rred_u1, rnir_u1,$
      count, type,$
      outDistance1,$
      outStd_fpar1, outStd_rred1, outStd_rnir1,$
      outMinMDindex1);

    OriginalIndex = indexMDthres[outMinMDindex1];
    outFAPAR = p_FAPAR[OriginalIndex];
    outBrf_r = p_brf_r[OriginalIndex];
    outBrf_n = p_brf_n[OriginalIndex];

    outFAPAR = p_FAPAR_u[OriginalIndex];
    outBrf_r = p_brf_r_u[OriginalIndex];
    outBrf_n = p_brf_n[OriginalIndex];

    outDev_FAPAR = outStd_fpar1;
    outDev_brf_r = outStd_rred1;
    outDev_brf_n = outStd_rnir1;

    outP_ValueNb = OriginalIndex; /* ORIGINAL */
    outP_NbValue = count;
    outp_DayIndex=OriginalIndex
    outp_DayAvailable=count

    ;    print, '*****list(OL)***'
    ;    print, p_FAPAR
    ;    print, '**selected(OL)**'
    ;    print, p_FAPAR[OriginalIndex]
    ;    print, '*****************'
    return

  endif

end