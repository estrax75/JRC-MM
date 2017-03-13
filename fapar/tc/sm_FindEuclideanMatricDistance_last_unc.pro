PRO sm_FindEuclideanMatricDistance_last_unc, daysNumber, data_in, idx_third, distance, mean_field, std_mean, nfield, splitDims
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
  ; save memory version
  ; okpix=N_elements(idx_third)
  ; compute the mean and std of the fapar and red/nir channels
  ;sm_call_mean_3_unc, daysNumber, data_in, mean_field, std_mean, std_field, sigmasum1, sigmasum2, sigmasum3, nfield, splitDims
  ;if nfield eq 2 then stop
  sm_call_mean_3_unc, daysNumber, data_in, mean_field, std_mean, std_field, weightSumRed, weightSumNir, weightSumFapar, nfield, splitDims
  ;stop
  idx_check = 964418
  ;print, mean_field.fapar(idx_check), mean_field.red(idx_check), mean_field.nir(idx_check)
  ;stop
  ;
  ;print, std_mean.temp(idx_check), std_mean.red(idx_check), std_mean.nir(idx_check)
  ;stop
  ;
  ; compute the distance
  ;  help, mean_field
  ;  help, std_mean
  ;  help, std_field
  distance=fltarr(daysNumber,splitDims[0],splitDims[1], /NO)
  ;
  ;help, distance
  ;stop
  distance[*,*,*]=!VALUES.F_NAN
  buf=fltarr(splitDims[0],splitDims[1])
  ; MM 22/9/2016
  buf1=std_mean.red
  buf2=std_mean.nir
  if nfield eq 3 then begin
    buf3=std_mean.temp
  endif
  ;
  if nfield eq 3 then begin

    for t=0, daysNumber-1 do begin
      buf4=reform(std_field.red(t,*,*))
      buf5=reform(std_field.nir(t,*,*))
      buf6=reform(std_field.fapar(t,*,*))
      buf(*)=!VALUES.F_NAN
      ;
      ; first case none nul
      ;
      idx_ca=where(buf1 gt 0.0 and buf2 gt 0.0 and buf3 gt 0.0 and $
        buf4 gt 0.0 and  buf5 gt 0.0 and  buf6 gt 0.0, foundElements)
      ;
      ; original computation of distance
      if foundElements gt 0 then begin
        buf(idx_ca)= sqrt( $
          buf4(idx_ca)^2/buf1(idx_ca)^2 * (1./data_in(t).sigma_red(idx_ca))/weightSumRed(idx_ca)+$
          buf5(idx_ca)^2/buf2(idx_ca)^2 * (1./data_in(t).sigma_nir(idx_ca))/weightSumNir(idx_ca)+$
          buf6(idx_ca)^2/buf3(idx_ca)^2 * (1./data_in(t).sigma(idx_ca))/weightSumFapar(idx_ca))
        ;          buf4(idx_ca)^2/buf1(idx_ca)^2 * data_in(t).sigma_red(idx_ca)/sigmasum1(idx_ca)+$
        ;          buf5(idx_ca)^2/buf2(idx_ca)^2 * data_in(t).sigma_nir(idx_ca)/sigmasum2(idx_ca)+$
        ;          buf6(idx_ca)^2/buf3(idx_ca)^2 * data_in(t).sigma(idx_ca)/sigmasum3(idx_ca))
;        print,  'sigma red:', data_in(t).sigma_red(idx_ca(0))
;        print, 'w red:', (1./data_in[t].sigma_red[idx_ca[0]])/weightSumRed[idx_ca[0]]
;        print, 'distance red:', buf4(idx_ca(0))^2/buf1(idx_ca(0))^2 * (1./data_in(t).sigma_red(idx_ca(0)))/weightSumRed(idx_ca(0))
;        print,  'sigma nir:', data_in(t).sigma_nir(idx_ca(0))
;        print, 'w nir:', (1./data_in[t].sigma_nir[idx_ca[0]])/weightSumNir[idx_ca[0]]
;        print, 'distance nir:', buf5(idx_ca(0))^2/buf2(idx_ca(0))^2 * (1./data_in(t).sigma_nir(idx_ca(0)))/weightSumNir(idx_ca(0))
;        print,  'sigma fapar:', data_in(t).sigma(idx_ca(0))
;        print, 'w fapar:', (1./data_in[t].sigma[idx_ca[0]])/weightSumFapar[idx_ca[0]]
;        print, 'distance fapar:', buf6(idx_ca(0))^2/buf3(idx_ca(0))^2 * (1./data_in(t).sigma(idx_ca(0)))/weightSumFapar(idx_ca(0))
        ;if data_in(t).sigma(idx_ca(0)) ne 1 then stop
        ;if data_in(t).sigma(idx_ca(0)) gt 0 and data_in(t).sigma(idx_ca(0)) lt 1 and finite(data_in(t).sigma(idx_ca(0))) eq 1 then stop
      endif
      idx_ca1=where(buf1 eq 0.0 and buf4  ge 0.0, count1)
      idx_ca2=where(buf2 eq 0.0 and buf5  ge 0.0, count2)
      idx_ca3=where(buf3 eq 0.0 and buf6  ge 0.0, count3)
      ;
      idx_check = 964418
      ;print,buf(idx_check)
      ;stop
      print, '# std = 0:', count1, count2, count3 ; ng 2016
      if count1 gt 0 then begin      ; ng 2016
        buf(idx_ca1)= sqrt($
          buf5(idx_ca1)^2/buf2(idx_ca1)^2 * (1./data_in(t).sigma_nir(idx_ca1))/weightSumNir(idx_ca1)+$
          buf6(idx_ca1)^2/buf3(idx_ca1)^2 * (1./data_in(t).sigma(idx_ca1))/weightSumFapar(idx_ca1))
      endif
      if count2 gt 0 then begin      ; ng 2016
        buf(idx_ca2)= sqrt($
          buf4(idx_ca2)^2/buf1(idx_ca2)^2 * (1./data_in(t).sigma_red(idx_ca2))/weightSumRed(idx_ca2)+$
          buf6(idx_ca2)^2/buf3(idx_ca2)^2 * (1./data_in(t).sigma(idx_ca2))/weightSumFapar(idx_ca2))
      endif
      if count3 gt 0 then begin      ; ng 2016
        buf(idx_ca3)= sqrt($
          buf4(idx_ca3)^2/buf1(idx_ca3)^2 * (1./data_in(t).sigma_red(idx_ca3))/weightSumRed(idx_ca3) +$
          buf5(idx_ca3)^2/buf2(idx_ca3)^2 * (1./data_in(t).sigma_nir(idx_ca3))/weightSumNir(idx_ca3))
      endif
      distance[t,*,*]=buf

    endfor
    ;  print, distance(*,0,2430)
    ;  print, data_in(*).sigma(0,2430)
    ;  stop
    ;   in_cloud=cgi_map_bitwise_flag(data_in[*].ltdr_flag(0,2430),1)
    ;   print, in_cloud
    ;   stop
  endif else begin
    for t=0, daysNumber-1 do begin
      buf4=reform(std_field.red(t,*,*))
      buf5=reform(std_field.nir(t,*,*))
      buf(*)=!VALUES.F_NAN
      ;
      ; first case none nul
      ;
      idx_ca=where(buf1 gt 0.0 and buf2 gt 0.0 and $
        buf4 gt 0.0 and  buf5 gt 0.0, validDataCount)
      ;   stop
      if validDataCount gt 0 then begin
        buf(idx_ca)= sqrt( $
          buf4(idx_ca)^2/buf1(idx_ca)^2 +$
          buf5(idx_ca)^2/buf2(idx_ca)^2)
      endif
      idx_ca1=where(buf1 eq 0.0 and buf4 ge 0.0, count1)
      idx_ca2=where(buf2 eq 0.0 and buf5 ge 0.0, count2)
      print, '# std = 0 for soil pixels ', count1, count2; ng 2016
      if count1 gt 0 then begin      ; ng 2016
        buf(idx_ca1)= sqrt($
          buf5(idx_ca1)^2/buf2(idx_ca1)^2 )
      endif
      if count2 gt 0 then begin      ; ng 2016
        buf(idx_ca2)= sqrt($
          buf4(idx_ca2)^2/buf1(idx_ca2)^2)
      endif

      ; second case if one is equal to zero ... i.e no change during the period

      distance[t,*,*]=buf
    endfor
    ; stop
  endelse
  ; stop
END
