Pro sm_call_mean_3_unc,  daysNumber, data_in, mean_field, std_mean, std_field, nfield, splitDims
  ;
  if n_elements(splitDims) ne 2 then splitDims=[7200,3600]
  mean_field={red:fltarr(splitDims[0],splitDims[1]), nir:fltarr(splitDims[0],splitDims[1]), fapar:fltarr(splitDims[0],splitDims[1])}
  buf1=fltarr(splitDims[0],splitDims[1])
  buf2=fltarr(splitDims[0],splitDims[1])
  bufRedsigma=fltarr(splitDims[0],splitDims[1])
  bufNirsigma=fltarr(splitDims[0],splitDims[1])

  if nfield eq 3 then begin
    buf3=fltarr(splitDims[0],splitDims[1])
    bufFaparSigma=fltarr(splitDims[0],splitDims[1])
  endif
  tt=[0,daysNumber]
  num_used=bytarr(splitDims[0],splitDims[1])
  one=num_used
  one(*,*)=1
  ; compute the average for each field, i.e red nir and fapar ...
  if nfield eq 3 then begin

    for t=0, tt(1)-1 do begin
      ; cut off NaN
      validMask=finite(data_in(t).fapar(*,*)) and finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*))
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(data_in(t).fapar[goodIndexes] gt 0.0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg
      idx_c = where(validMaskVeg eq 1 and data_in(t).flag(*,*) eq 0, countVeg)

      ; previous (without NaN)
      ;      idx_cOld= where((data_in(t).red gt 0. and data_in(t).red lt 1.) and $
      ;        (data_in(t).nir gt 0. and data_in(t).nir lt 1.) and $
      ;        (data_in(t).fapar gt 0. and data_in(t).fapar le 1.) and $
      ;        (data_in(t).flag eq 0), countVegOld)
      ;      if countVegOld ne countVeg then stop
      if idx_c(0) ge 0 then begin
        ; sigma weight based on sum (of all the valid sigma...)
        ;buf1(idx_c)= data_in(t).red(idx_c)+buf1(idx_c)
        buf1(idx_c)= (data_in(t).red(idx_c)*data_in(t).sigma_red(idx_c))+buf1(idx_c)
        bufRedSigma(idx_c)= data_in(t).sigma_red(idx_c)+bufRedSigma(idx_c)
        ;buf2(idx_c)= data_in(t).nir(idx_c)+buf2(idx_c)
        buf2(idx_c)= data_in(t).nir(idx_c)*data_in(t).sigma_nir(idx_c)+buf2(idx_c)
        bufNirSigma(idx_c)= data_in(t).sigma_nir(idx_c)+bufNirSigma(idx_c)
        ;buf3(idx_c)= data_in(t).fapar(idx_c)+buf3(idx_c)
        buf3(idx_c)= data_in(t).fapar(idx_c)*data_in(t).sigma(idx_c)+buf3(idx_c)
        ;sigma_fapar
        bufFaparSigma(idx_c)= data_in(t).sigma(idx_c)+bufFaparSigma(idx_c)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
      endif
    endfor
  endif else begin
    for t=0, tt(1)-1 do begin
      validMask=finite(data_in(t).fapar(*,*)) and finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*))
      goodIndexes=where(validMask eq 1)

      idxMaskSoil=where(data_in(t).fapar[goodIndexes] eq 0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      validMaskSoil=validMask*validMaskSoil
      idx_c= where(validMaskSoil eq 1 and (data_in(t).flag eq 4 or data_in(t).flag eq 5), countSoil)
      ; previous check without NaN
      ;      idx_cOld= where((data_in(t).red gt 0. and data_in(t).red lt 1.) and $
      ;        (data_in(t).nir gt 0. and data_in(t).nir lt 1.) and $
      ;        (data_in(t).fapar eq 0.) and $
      ;        (data_in(t).flag eq 4 or data_in(t).flag eq 5), countSoilOld)
      ;      if countSoilOld ne countSoil then stop
      ;      a=where((data_in(t).red gt 0. and data_in(t).red lt 1.), redc)
      ;      b=where((data_in(t).nir gt 0. and data_in(t).nir lt 1.), nirc)
      ;      c=where(data_in(t).fapar eq 0., faparc)
      ;      d=where(data_in(t).flag eq 4 or data_in(t).flag eq 5, flagc)
      if idx_c(0) ge 0 then begin
        buf1(idx_c)= data_in(t).red(idx_c)+buf1(idx_c)
        buf2(idx_c)= data_in(t).nir(idx_c)+buf2(idx_c)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
      endif
    endfor
  endelse
  ; vgt/fapar valid
  if nfield eq 3 then begin
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0. and buf3 gt 0.)   ; ng 2016
    if idx_ok(0) ge 0 then begin
      ; version without sigma...
      buf1(idx_ok)=buf1(idx_ok)/float(num_used(idx_ok))
      buf2(idx_ok)=buf2(idx_ok)/float(num_used(idx_ok))
      buf3(idx_ok)=buf3(idx_ok)/float(num_used(idx_ok))
      ; compute average of sigma(s)
      bufRedSigma(idx_ok)=bufRedSigma(idx_ok)/float(num_used(idx_ok))
      bufNirSigma(idx_ok)=bufNirSigma(idx_ok)/float(num_used(idx_ok))
      bufFaparSigma(idx_ok)=bufFaparSigma(idx_ok)/float(num_used(idx_ok))
      ; sigma weight based on sum...
    endif
    idx_nok=where(num_used le 2b or buf1 le 0.0 or buf2 le 0.0 or buf3 le 0.0)   ; ng 2016
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
      buf3(idx_nok)=-1.0
      bufRedSigma(idx_nok)=-1
      bufNirSigma(idx_nok)=-1
      bufFaparSigma(idx_nok)=-1
    endif
  endif else begin
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0.)  ;ng 2016
    ; bare soil / no fapar
    if idx_ok(0) ge 0 then begin
      buf1(idx_ok)=buf1(idx_ok)/float(num_used(idx_ok))
      buf2(idx_ok)=buf2(idx_ok)/float(num_used(idx_ok))
      bufRedSigma(idx_ok)=bufRedSigma(idx_ok)/float(num_used(idx_ok))
      bufNirSigma(idx_ok)=bufNirSigma(idx_ok)/float(num_used(idx_ok))
    endif
    idx_nok=where(num_used le 2b or buf1 lt 0.0 or buf2 lt 0.0)  ; ng 2016
    ; no data here (water/invalid)
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
      bufRedSigma(idx_nok)=-1.
      bufNirSigma(idx_nok)=-1.
    endif
  endelse
  if n_elements(buf3) ne 0 then faparMean=buf3
  mean_field(*,*).red=buf1(*,*)
  mean_field(*,*).nir=buf2(*,*)
  if nfield eq 3 then mean_field.fapar=buf3(*,*)
  ;window,0
  ;plot, mean_field.fapar(idx_ok), psym=1
  ;window, 0, xsize=splitDims(0), ysize=splitDims(1)/3.
  ;tvscl, congrid(buf1(*,*), splitDims(0), splitDims(1)/3.)
  ;stop
  initValues=fltarr(tt(1),splitDims[0],splitDims[1])
  initValues[*]=-1.0
  ; compute the standard deviation for each field, i.e red nir and fapar ...
  std_field={red:initValues, nir:initValues, fapar:initValues}
  if nfield eq 3 then begin
    for t=0, tt(1)-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      buf3=fltarr(splitDims[0],splitDims[1])
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf3(*,*)=-1.0
      buf=fltarr(splitDims[0],splitDims[1])
      buff=fltarr(splitDims[0],splitDims[1])
      buf(*,*)=data_in(t).red(*,*)
      buff=mean_field.red(*,*)
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and data_in(t).flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idx=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and data_in(t).flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      if idx(0) ge 0 then begin
        ; Here apply a weight factor, based on sigma...
        buf(idx)=data_in(t).red[idx]*(data_in[t].sigma_red[idx]/bufRedSigma[idx])
        buff[idx]=temporary(mean_field.red[idx])*temporary(bufRedSigma[idx])
        buf1(idx) = abs(buf(idx)-buff(idx))
      endif
      buf=data_in(t).nir(*,*)
      buff(*,*)=mean_field.nir(*,*)
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and data_in(t).flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idx=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).flag eq 0) and num_used ge 3b, countBuf) ;ng 2016 )
      if idx(0) ge 0 then begin
        ; Here apply a weight factor, based on sigma...
        buf(idx)=data_in(t).nir[idx]*(data_in[t].sigma_nir[idx]/bufNirSigma[idx])
        buff(idx)=mean_field.nir(idx)*bufNirSigma[idx]
        buf2[idx] = abs(buf(idx)-buff(idx))
      endif
      buf=data_in(t).fapar(*,*)
      buff(*,*)=mean_field.fapar(*,*)
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and data_in(t).flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ;idx=where((buf gt 0. and buf le 1.0) and (buff gt 0. and buff le 1.) and (data_in(t).flag eq 0) and num_used ge 3b, countBuf) ;ng 2016
      if idx(0) ge 0 then begin
        ; Here apply a weight factor, based on sigma...
        buf=data_in(t).fapar[idx]*(data_in[t].sigma[idx]/bufFaparSigma[idx])
        buff[idx]=mean_field.fapar[idx]*bufFaparSigma[idx]
        buf3[idx]=abs(buf(idx)-buff(idx))
      endif
      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
      std_field.fapar(t,*,*)=buf3
    endfor
  endif else begin
    for t=0, tt(1)-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf=fltarr(splitDims[0],splitDims[1])
      buff=fltarr(splitDims[0],splitDims[1])
      buf=data_in(t).red
      buff=mean_field.red
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and (data_in(t).flag eq 4 or data_in(t).flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; Previious version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).flag eq 4 or data_in(t).flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld ne countBuf then stop
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))
      buf=data_in(t).nir
      buff=mean_field(*,*).nir
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and (data_in(t).flag eq 4 or data_in(t).flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).flag eq 4 or data_in(t).flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld-countBuf gt 1 then stop
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))
      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
    endfor
  endelse
  ;window, /free
  ;plot,
  ;
  ; compute the mean of the standard deviation ...
  ; now split...
  std_mean={red:fltarr(splitDims[0],splitDims[1]), nir:fltarr(splitDims[0],splitDims[1]), temp:fltarr(splitDims[0],splitDims[1])}
  num_used_1=bytarr(splitDims[0],splitDims[1])
  num_used_2=bytarr(splitDims[0],splitDims[1])
  if nfield eq 3 then num_used_3=bytarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  buf1=fltarr(splitDims[0],splitDims[1])
  buf2=fltarr(splitDims[0],splitDims[1])
  if nfield eq 3 then buf3=fltarr(splitDims[0],splitDims[1])
  for t=0, tt(1)-1 do begin
    buf=std_field.red(t,*,*)
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf1(idx_ca)=(buf(idx_ca)^2)+buf1(idx_ca)
      num_used_1(idx_ca)  = num_used_1(idx_ca)+one(idx_ca)
    endif
    buf=std_field.nir(t,*,*)
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf2(idx_ca)=(buf(idx_ca)^2)+buf2(idx_ca)
      num_used_2(idx_ca)  = num_used_2(idx_ca)+one(idx_ca)
    endif
    if nfield eq 3 then begin
      buf(*,*)=std_field.fapar(t,*,*)
      idx_ca=where(buf ge 0.)
      if idx_ca(0) ge 0 then begin
        buf3(idx_ca)=(buf(idx_ca)^2)+buf3(idx_ca)
        num_used_3(idx_ca)  = num_used_3(idx_ca)+one(idx_ca)
      endif
    endif
  endfor
  idx_ok=where(num_used_1 ge 3b) ; ng 2016
  if idx_ok(0) ge 0 then buf1(idx_ok)=sqrt(buf1(idx_ok)/float(num_used_1(idx_ok)-1))
  idx_ok=where(num_used_2 ge 3b) ; ng 2016
  if idx_ok(0) ge 0 then buf2(idx_ok)=sqrt(buf2(idx_ok)/float(num_used_2(idx_ok)-1))
  if nfield eq 3 then begin
    idx_ok=where(num_used_3 ge 3b) ; ng 2016
    if idx_ok(0) ge 0 then buf3(idx_ok)=sqrt(buf3(idx_ok)/float(num_used_3(idx_ok)-1))
  endif
  ;erase
  ;window,0
  ;plot, num_used_1 , num_used_2, psym =2
  ;stop
  idx_nok=where(num_used le 2b)  ; ng 2016
  if idx_nok(0) ge 0 then begin
    buf1(idx_nok)=-1.0
    buf2(idx_nok)=-1.0
    if nfield eq 3 then buf3(idx_nok)=-1.0
  endif
  std_mean.red[*,*]=buf1[*,*]
  std_mean.nir[*,*]=buf2[*,*]
  if nfield eq 3 then begin
    std_mean.temp[*,*]=buf3[*,*]
    checkZeroes=where(std_mean.temp[*,*] eq 0, countZeroes)
    print,'Find # points with Standard Deviation Mean FAPAR = 0 ', countZeroes
  endif
  ;plot, mean_field(*,*).red, mean_field(*,*).nir, psym=1
  ;oplot, mean_field(*,*).nir, psym=4
  ;oplot, mean_field(*,*).fapar, psym=6
  ; stop
  window, 0, title='fapar'
  yMinMax=[0.0, 1.0]
  plot, data_in(0).fapar(0,1950:2100), yr=yMinMax, min=0.01, psym = 3
  for t=0, 9 do oplot, data_in(t).fapar(0,1950:2100), col=t*20, psym = 2, min=0.01
  oplot, mean_field.fapar(0,1950:2100), line = 0, min=0.01, thick=2.5
  for t=0, 9 do oplot, mean_field.fapar(0,1950:2100)-std_field.fapar(t,0,1950:2100), col=t*20, max=0.6
  for t=0, 9 do oplot, mean_field.fapar(0,1950:2100)+std_field.fapar(t,0,1950:2100), col=t*20, min=0.01
  oplot, std_mean.temp(0,1950:2100), min=0.01, col=250, thick=2.5
  oplot, mean_field.fapar(0,1950:2100)+ std_mean.temp(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.6
  oplot, mean_field.fapar(0,1950:2100)- std_mean.temp(0,1950:2100), min=0.01, col=250, thick=1.5

  window, 1, title='red'
  plot, data_in(0).red(0,1950:2100), yr=yMinMax, min=0.01, psym = 3
  for t=0, 9 do oplot, data_in(t).red(0,1950:2100), col=t*20, psym = 2, min=0.01
  oplot, mean_field.red(0,1950:2100), line = 0, min=0.01, thick=2.5
  for t=0, 9 do oplot, mean_field.red(0,1950:2100)-std_field.red(t,0,1950:2100), col=t*20, max=0.6
  for t=0, 9 do oplot, mean_field.red(0,1950:2100)+std_field.red(t,0,1950:2100), col=t*20, min=0.01
  oplot, std_mean.red(0,1950:2100), min=0.01, col=250, thick=2.5
  oplot, mean_field.red(0,1950:2100)+ std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.6
  oplot, mean_field.red(0,1950:2100)- std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5

  window, 2, title='nir'
  plot, data_in(0).nir(0,1950:2100), yr=yMinMax, min=0.01, psym = 3
  for t=0, 9 do oplot, data_in(t).nir(0,1950:2100), col=t*20, psym = 2, min=0.01
  oplot, mean_field.nir(0,1950:2100), line = 0, min=0.01, thick=2.5
  for t=0, 9 do oplot, mean_field.nir(0,1950:2100)-std_field.red(t,0,1950:2100), col=t*20, max=0.6
  for t=0, 9 do oplot, mean_field.nir(0,1950:2100)+std_field.red(t,0,1950:2100), col=t*20, min=0.01
  oplot, std_mean.nir(0,1950:2100), min=0.01, col=250, thick=2.5
  oplot, mean_field.nir(0,1950:2100)+ std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.6
  oplot, mean_field.nir(0,1950:2100)- std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5
  stop
end
;===================================================================================================
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
  sm_call_mean_3_unc, daysNumber, data_in, mean_field, std_mean, std_field, nfield, splitDims
  ;
  ; compute the distance
;  help, mean_field
;  help, std_mean
;  help, std_field
;  stop
  distance=fltarr(tt(1),splitDims[0],splitDims[1], /NO)
  ;
  ;help, distance
  ;stop
  distance[*]=100.0
  buf=fltarr(splitDims[0],splitDims[1])
  ;
  ; MM 22/9/2016
  buf1=std_mean.red[*,*]
  buf2=std_mean.nir[*,*]
  if nfield eq 3 then begin
    buf3=std_mean.temp[*,*]
  endif
  ;
  if nfield eq 3 then begin

    for t=0, tt(1)-1 do begin
      buf4=reform(std_field.red(t,*,*))
      buf5=reform(std_field.nir(t,*,*))
      buf6=reform(std_field.fapar(t,*,*))
      buf(*)=100.0
      ;
      ; first case none nul
      ;
      idx_ca=where(buf1 gt 0.0 and buf2 gt 0.0 and buf3 gt 0.0 and $
        buf4 gt 0.0 and  buf5 gt 0.0 and  buf6 gt 0.0, foundElements)
      ;
      ; original computation of distance
      if foundElements gt 0 then begin
        buf(idx_ca)= sqrt( $
          buf4(idx_ca)^2/buf1(idx_ca)^2 +$
          buf5(idx_ca)^2/buf2(idx_ca)^2 +$
          buf6(idx_ca)^2/buf3(idx_ca)^2 )
      endif
      idx_ca1=where(buf1 eq 0.0 and buf4  ge 0.0, count1)
      idx_ca2=where(buf2 eq 0.0 and buf5  ge 0.0, count2)
      idx_ca3=where(buf3 eq 0.0 and buf6  ge 0.0, count3)
      ;
      print, '# std = 0:', count1, count2, count3 ; ng 2016
      if count1 gt 0 then begin      ; ng 2016
        buf(idx_ca1)= sqrt($
          buf5(idx_ca1)^2/buf2(idx_ca1)^2 +$
          buf6(idx_ca1)^2/buf3(idx_ca1)^2 )
      endif
      if count2 gt 0 then begin      ; ng 2016
        buf(idx_ca2)= sqrt($
          buf4(idx_ca2)^2/buf1(idx_ca2)^2 +$
          buf6(idx_ca2)^2/buf3(idx_ca2)^2 )
      endif
      if count3 gt 0 then begin      ; ng 2016
        buf(idx_ca3)= sqrt($
          buf4(idx_ca3)^2/buf1(idx_ca3)^2 +$
          buf5(idx_ca3)^2/buf2(idx_ca3)^2 )
      endif
      distance[t,*,*]=buf
    endfor
  endif else begin
    for t=0, tt(1)-1 do begin
      buf4=reform(std_field.red(t,*,*))
      buf5=reform(std_field.nir(t,*,*))
      buf(*)=100.0
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
      ;
      ; second case if one is equal to zero ... i.e no change during the period
      ;
      distance[t,*,*]=buf
    endfor
    ;stop
  endelse
  ; stop
END
