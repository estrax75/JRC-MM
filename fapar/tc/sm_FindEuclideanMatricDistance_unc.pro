PRO sm_FindEuclideanMatricDistance_unc, daysNumber, data_in, idx_third, distance, mean_field, std_mean, nfield, splitDims
  ;
  ;
  ;
  ; inputs:
  ; data_in : daily data
  ; position of of valid points > 3
  ; nfield: 3 if vegetation - 2 if bare soil
  ;
  ; option: add weight with uncertainties
  ;
  ; outputs:
  ; distance : normalized euclidean distance
  ; mean_field: average over the valid dates
  ; std_mean: mean standard deviation over the valid dates
  ;
  tt=[0, daysNumber]
  ; save memory version
  okpix=N_elements(idx_third)
  ; compute the mean and std of the fapar and red/nir channels
  sm_call_mean_3_unc1, daysNumber, data_in, mean_field, std_mean, std_field, nfield, splitDims
  ;
  ; compute the distance
  ;  help, mean_field
  ;  help, std_mean
  ;  help, std_field
  ;  stop

  distance=fltarr(tt(1),splitDims[0],splitDims[1], /NO)
  pesi=fltarr(tt(1),splitDims[0],splitDims[1])
  ;
  ;help, distance
  ;stop
  distance[*,*,*]=100.0
  buf=fltarr(splitDims[0],splitDims[1])
  one=buf
  one(*,*)=1.0
  ;
  ; MM 22/9/2016
  bufRed=std_mean.red[*,*]
  bufNir=std_mean.nir[*,*]
  if nfield eq 3 then begin
    bufFapar=std_mean.temp[*,*]
  endif
  ;
  if nfield eq 3 then begin
    ;
    ; compute the total sigma ????
    ;
    fpar_w=fltarr(splitDims[0],splitDims[1])
    red_w=fltarr(splitDims[0],splitDims[1])
    nir_w=fltarr(splitDims[0],splitDims[1])
    ;
    N_ele=fltarr(splitDims[0],splitDims[1])
    ;
    for t=0, tt(1)-1 do begin
      ; cut off NaN
      validMask=finite(finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*)) and finite(data_in(t).fapar(*,*)))
   
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(data_in(t).fapar[goodIndexes] gt 0.0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_maskVeg = where(validMaskVeg eq 1 and data_in(t).jrc_flag(*,*) eq 0, countVeg)
   
      if idx_maskVeg(0) GE 0 THEN BEGIN
        fpar_w(idx_maskVeg)=fpar_w(idx_maskVeg)+data_in(t).sigma(idx_maskVeg)
        red_w(idx_maskVeg)=red_w(idx_maskVeg)+data_in(t).sigma_red(idx_maskVeg)
        nir_w(idx_maskVeg)=nir_w(idx_maskVeg)+data_in(t).sigma_nir(idx_maskVeg)
        N_ele(idx_maskVeg)=N_ele(idx_maskVeg)+one(idx_maskVeg)
      endif
       
    endfor
    print, red_w(0,2430)
    print, total(data_in(*).sigma_red(0,2430), /NAN)
    stop
    ;

    ;stop
    ;idx_non_nul=where(N_ele GE 3)
    ;fpar_w(idx_non_nul)=fpar_w(idx_non_nul) ;/N_ele(idx_non_nul)
    ;red_w(idx_non_nul)=red_w(idx_non_nul)   ;/N_ele(idx_non_nul)
    ;nir_w(idx_non_nul)=nir_w(idx_non_nul)   ;/N_ele(idx_non_nul)
    ;
    ;idx_nul=where(N_ele LT 3)
    ;IF IDX_NUL(0) GE 0 THEN BEGIN
    ;  fpar_w(idx_nul)=30.0
    ;  RED_w(idx_nul)=30.0
    ;  NIR_w(idx_nul)=30.0
   ; ENDIF
    ; stop
    ; if data_in(t).sigma (red,nir) = 0. alora fpar/data_in(t).sigma = 1.0
    ;
    ;print, fpar_w(0,2430), red_w(0,2430), nir_w(0,2430)
    ;
  ;  stop
    for t=0, tt(1)-1 do begin
      bufStdRed=reform(std_field.red(t,*,*))
      bufStdNir=reform(std_field.nir(t,*,*))
      bufStdFapar=reform(std_field.fapar(t,*,*))
      buf(*,*)=100.0
      ;
      ; first case all none nul
      ;
      idx_ca=where(bufRed gt 0.0 and bufNir gt 0.0 and bufFapar gt 0.0 and $
        bufStdRed gt 0.0 and  bufStdNir gt 0.0 and  bufStdFapar gt 0.0, foundElements, compl=badca)

      weightRed=data_in(t).sigma_red
      weightNir=data_in(t).sigma_nir
      weightFapar=data_in(t).sigma
      ;
      weightRed[*,*]=!VALUES.F_NAN
      weightNir[*,*]=!VALUES.F_NAN
      weightFapar[*,*]=!VALUES.F_NAN
      ;print, '1', weightRed(0,2430)

      if idx_ca(0) ge 0 then begin

        idxRed=where(data_in(t).sigma_red(idx_ca) gt 0. and red_w(idx_ca) GT 0.0, countFapar, compl=maxDistRedIdx)
        idxNir=where(data_in(t).sigma_nir(idx_ca) gt 0. and nir_w(idx_ca) GT 0.0, countFapar, compl=maxDistNirIdx)
        idxFapar=where(data_in(t).sigma(idx_ca) gt 0.  and fpar_w(idx_ca) GT 0.0, countFapar, compl=maxDistNirFapar)

        ; sigma on this day <> 0, apply
        if idxred(0) ge 0 then weightRed[idx_ca[idxRed]]=1./red_w(idx_ca[idxRed])/data_in(t).sigma_red(idx_ca[idxRed]) else weightRed(idxca(maxDistRedIdx))=!VALUES.F_NAN
        if idxnir(0) ge 0 then weightNir[idx_ca[idxNir]]=1./nir_w(idx_ca[idxNir])/data_in(t).sigma_nir(idx_ca[idxNir]) else weightnir(idxca(maxDistNirIdx))=!VALUES.F_NAN
        if idxfapar(0) ge 0 then weightFapar[idx_ca[idxFapar]]=1./fpar_w(idx_ca[idxFapar])/data_in(t).sigma(idx_ca[idxFapar]) else weightRed(idxca(maxDistNirFapar))=!VALUES.F_NAN
       print,'first print day =', t
       print, t, red_w(0,2430), data_in(t).sigma_red(0,2430), weightRed(0,2430)
       print, t, nir_w(0,2430), data_in(t).sigma_nir(0,2430), weightNir(0,2430)
       print, t, fpar_w(0,2430), data_in(t).sigma(0,2430), weightFAPAR(0,2430)
  ;     stop
      
      ;print, '2', weightRed(0,2430)

      ;stop
      ; sigma on this day = 0, force to 0
      idxRed2=where(data_in(t).sigma_red(idx_ca) EQ 0.)
      idxNir2=where(data_in(t).sigma_nir(idx_ca) EQ 0.)
      idxFapar2=where(data_in(t).sigma(idx_ca) EQ 0.)

      if idxRed2(0) GE 0 then weightRed[idx_ca[idxRed2]]=1.
      if idxNir2(0) GE 0 then weightNir[idx_ca[idxNir2]]=1.
      if idxFapar2(0) GE 0 then weightFapar[idx_ca[idxFapar2]]=1.
      ;print, '3', weightRed(0,2430)
      ;
      ;STOP
      ; original computation of distance
      ;if foundElements gt 0 then begin
        print, 'case 0'
        print, 'before', buf(0,2430)
        ;
        buf(idx_ca)= sqrt( $
          (bufStdRed(idx_ca)*weightRed(idx_ca))^2/bufRed(idx_ca)^2 +$
          (bufStdNir(idx_ca)*weightNir(idx_ca))^2/bufNir(idx_ca)^2 +$
          (bufStdFapar(idx_ca)*weightFapar(idx_ca))^2/bufFapar(idx_ca)^2 )
              print, 'here 0'
              print, t, bufStdRed(0,2430), weightRed(0,2430), bufRed(0,2430)
              print, t, bufStdNir(0,2430), weightNir(0,2430), bufNir(0,2430)
              print, t, bufStdFapar(0,2430), weightFapar(0,2430), bufFapar(0,2430)
        print, 'after', buf(0,2430)

     
      endif 
      ;
      ;
      idxRed=where(bufRed eq 0.0 and bufStdRed  ge 0.0 and data_in(t).sigma_red gt 0.0 and red_w GT 0.0,count12)
      idxNir=where(bufNir eq 0.0 and bufStdNir  ge 0.0 and data_in(t).sigma_nir gt 0.0 and nir_w GT 0.0,count22)
      idxFapar=where(bufFapar eq 0.0 and bufStdFapar  ge 0.0 and data_in(t).sigma gt 0.0 and fpar_w GT 0.0, count33)
      ;
      
      if idxred(0) ge 0 then weightRed[idxRed]=1./red_w(idxRed)/data_in(t).sigma_red(idxRed) 
      if idxnir(0) ge 0 then weightNir[idxNir]=1./nir_w(idxNir)/data_in(t).sigma_nir(idxNir) 
      if idxfapar(0) ge 0 then weightFapar[idxFapar]=1./fpar_w(idxFapar)/data_in(t).sigma(idxFapar) 
      ;
      ;
      ;print, '# std = 0:', count1, count2, count3 ; ng 2016
      if count12 gt 0 then begin      ; ng 2016
        
        print, 'case 1'
        print, 'before', buf(0,2430)
        buf(idxred)= sqrt($
          (bufStdNir(idxred)*weightNir[idxred])^2/bufNir(idxred)^2 +$
          (bufStdFapar(idxred)*weightFapar(idxred))^2/bufFapar(idxred)^2 )
        print, t, bufStdRed(0,2430), weightRed(0,2430), bufRed(0,2430)
        print, t, bufStdNir(0,2430), weightNir(0,2430), bufNir(0,2430)
        print, t, bufStdFapar(0,2430), weightFapar(0,2430), bufFapar(0,2430)
        print, 'after', buf(0,2430)
      endif
      if count22 gt 0 then begin      ; ng 2016
        print, 'case 2'
        print, 'before', buf(0,2430)
        buf(idxNir)= sqrt($
          (bufStdRed(idxNir)*weightRed(idxNir))^2/bufRed(idxNir)^2 +$
          (bufStdFapar(idxNir)*weightFapar(idxNir))^2/bufFapar(idxNir)^2 )
        print, t, bufStdRed(0,2430), weightRed(0,2430), bufRed(0,2430)
        print, t, bufStdNir(0,2430), weightNir(0,2430), bufNir(0,2430)
        print, t, bufStdFapar(0,2430), weightFapar(0,2430), bufFapar(0,2430)
        print, 'after', buf(0,2430)
      endif
      if count33 gt 0 then begin      ; ng 2016
        print, 'case 3'
        print, 'before', buf(0,2430)
        buf(idxFapar)= sqrt($
          (bufStdRed(idxFapar)*weightRed(idxFapar))^2/bufRed(idxFapar)^2 +$
          (bufStdNir(idxFapar)*weightNir(idxFapar))^2/bufNir(idxFapar)^2 )
        print, t, bufStdRed(0,2430), weightRed(0,2430), bufRed(0,2430)
        print, t, bufStdNir(0,2430), weightNir(0,2430), bufNir(0,2430)
        print, t, bufStdFapar(0,2430), weightFapar(0,2430), bufFapar(0,2430)
        print, 'after', buf(0,2430)
      endif 
      ;
     ; endif
      ;
      ;
      distance[t,*,*]=buf(*,*)
      pesi[t,*,*]=1./weightFapar(*,*)
    endfor
    in_cloud=cgi_map_bitwise_flag(data_in[*].ltdr_flag(0,2430),1)
    print, in_cloud
    print, '****daily analysis******'
    print, 'red:', data_in(*).red(0,2430), 'red sigma:', data_in(*).sigma_red(0,2430)
    print, 'nir:', data_in(*).nir(0,2430), 'nir sigma:', data_in(*).sigma_red(0,2430)
    print, 'fapar', data_in(*).fapar(0,2430), 'fapar sigma', data_in(*).sigma(0,2430)
    print, 'distance', distance(*,0,2430)
    print, 'ltdr cloud detection:', in_cloud
    print, '****end daily analysis******'
    print, '****NG print***'
    print, distance(*,0,2430)
    print, data_in(*).sigma(0,2430)
    print, pesi[*,0,2430]
    print, '****end NG print***'
    stop
    ;   stop

  endif else begin

    red_w=fltarr(splitDims[0],splitDims[1])
    nir_w=fltarr(splitDims[0],splitDims[1])
    N_ele=fltarr(splitDims[0],splitDims[1])
    ;
    for t=0, tt(1)-1 do begin
      ; cut off NaN
      ;; check
      ;      validMask=finite(finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*)))
      ;      goodIndexes=where(validMask eq 1)
      ;
      ;      idxMaskVeg=where(data_in(t).fapar[goodIndexes] eq 0.0 and $
      ;        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
      ;        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      ;; find right mask...
      validMask=finite(finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*)))
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(data_in(t).fapar[goodIndexes] gt 0.0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_maskVeg = where(validMaskVeg eq 1 and data_in(t).jrc_flag(*,*) eq 0, countVeg)
      ;;
      red_w(idxMaskveg)=red_w(idxMaskVeg)+data_in(t).sigma_red(idxMaskVeg)
      nir_w(idxMaskveg)=nir_w(idxMaskVeg)+data_in(t).sigma_nir(idxMaskVeg)
      N_ele(idxMaskveg)=N_ele(idxMaskveg)+one(idxMaskveg)
    endfor
    idx_non_nul=where(N_ele GE 3)
    red_w(idx_non_nul)=red_w(idx_non_nul)/N_ele(idx_non_nul)
    nir_w(idx_non_nul)=nir_w(idx_non_nul)/N_ele(idx_non_nul)
    ;

    for t=0, tt(1)-1 do begin
      bufStdRed=reform(std_field.red(t,*,*))
      bufStdNir=reform(std_field.nir(t,*,*))
      buf(*)=100.0
      ;
      ; first case none nul
      ;
      idx_ca=where(bufRed gt 0.0 and bufNir gt 0.0 and $
        bufStdRed gt 0.0 and  bufStdNir gt 0.0, validDataCount)
      ;   stop
      ;;
      weightRed=data_in(t).sigma_red
      weightNir=data_in(t).sigma_nir

      idxRed=where(data_in(t).sigma_red(idx_ca) gt 0, countFapar, compl=goodRed)
      idxNir=where(data_in(t).sigma_nir(idx_ca) gt 0, countFapar, compl=goodNir)

      ; sigma on this day <> 0, apply
      weightRed[idx_ca[idxRed]]=1./red_w(idx_ca[idxRed])/data_in(t).sigma_red(idx_ca[idxRed])
      weightNir[idx_ca[idxNir]]=1./nir_w(idx_ca[idxNir])/data_in(t).sigma_nir(idx_ca[[idxNir]])

      ; sigma on this day = 0, force to 0
      weightRed[idx_ca[goodRed]]=1.
      weightNir[idx_ca[goodNir]]=1.
      ;;
      if validDataCount gt 0 then begin
        buf(idx_ca)= sqrt( $
          (bufStdRed(idx_ca)*weightRed(idx_ca))^2/bufRed(idx_ca)^2 +$
          (bufStdNir(idx_ca)*weightNir(idx_ca))^2/bufNir(idx_ca)^2)
      endif
      idx_caRed=where(bufRed eq 0.0 and bufStdRed ge 0.0, count1)
      idx_caNir=where(bufNir eq 0.0 and bufStdNir ge 0.0, count2)
      print, '# std = 0 for soil pixels ', count1, count2; ng 2016
      if count1 gt 0 then begin      ; ng 2016
        buf(idx_caRed)= sqrt($
          (bufStdNir(idx_caRed)*weightNir(idx_caRed))^2/bufNir(idx_caRed)^2 )
      endif
      if count2 gt 0 then begin      ; ng 2016
        buf(idx_caNir)= sqrt($
          (bufStdRed(idx_caNir)*weightRed(idx_caNir))^2/bufRed(idx_caNir)^2)
      endif
      ;
      ; second case if one is equal to zero ... i.e no change during the period
      ;
      distance[t,*,*]=buf
    endfor
    ;stop
  endelse

  ; stop
END
