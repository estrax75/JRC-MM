Pro sm_call_mean_3_unc1,  daysNumber, data_in, mean_field, std_mean, std_field, nfield, splitDims, faparMean, cloudtype=cloudtype
  ;
  if n_elements(splitDims) ne 2 then splitDims=[7200,3600]
  mean_field={red:fltarr(splitDims[0],splitDims[1]), nir:fltarr(splitDims[0],splitDims[1]), fapar:fltarr(splitDims[0],splitDims[1])}
  buf1=dblarr(splitDims[0],splitDims[1])
  buf2=dblarr(splitDims[0],splitDims[1])
  buf11=dblarr(splitDims[0],splitDims[1])
  buf21=dblarr(splitDims[0],splitDims[1])

  ;bufRedsigma=fltarr(splitDims[0],splitDims[1])
  ;bufNirsigma=fltarr(splitDims[0],splitDims[1])

  if nfield eq 3 then buf3=dblarr(splitDims[0],splitDims[1])
  if nfield eq 3 then buf31=dblarr(splitDims[0],splitDims[1])

  ;    buf3=fltarr(splitDims[0],splitDims[1])
  ;    bufFaparSigma=fltarr(splitDims[0],splitDims[1])
  ;  endif
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
      idx_c = where(validMaskVeg eq 1 and data_in(t).jrc_flag(*,*) eq 0 and data_in(t).fapar gt 0. , countVeg)

      ; previous (without NaN)
      ;      idx_cOld= where((data_in(t).red gt 0. and data_in(t).red lt 1.) and $
      ;        (data_in(t).nir gt 0. and data_in(t).nir lt 1.) and $
      ;        (data_in(t).fapar gt 0. and data_in(t).fapar le 1.) and $
      ;        (data_in(t).jrc_flag eq 0), countVegOld)
      ;      if countVegOld ne countVeg then stop
      if idx_c(0) ge 0 then begin
        ;  sigma weight (of all the valid sigma...)
        ;check data_in(t).sigma(*,*)=one(*,*)
        ;print,'T-1', buf3(0,2430), buf31(0,2430)
     
        buf1(idx_c)= data_in(t).red(idx_c)*1./data_in(t).sigma_red(idx_c)+buf1(idx_c)
        buf2(idx_c)= data_in(t).nir(idx_c)*1./data_in(t).sigma_nir(idx_c)+buf2(idx_c)  
        buf3(idx_c)= data_in(t).fapar(idx_c)*1./data_in(t).sigma(idx_c)+buf3(idx_c)
        ;
        buf11(idx_c)=1./data_in(t).sigma_red(idx_c)+buf11(idx_c)
        buf21(idx_c)=1./data_in(t).sigma_nir(idx_c)+buf21(idx_c)
        buf31(idx_c)=1./data_in(t).sigma(idx_c)+buf31(idx_c)
        
        ;print, t, data_in(t).fapar(0,2430), data_in(t).sigma(0,2430)
        print, buf3(0,2430), buf31(0,2430)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
        ;print, num_used(0,2430)
      endif
    endfor
    ;stop
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
      idx_c= where(validMaskSoil eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5), countSoil)
      ; previous check without NaN
      ;      idx_cOld= where((data_in(t).red gt 0. and data_in(t).red lt 1.) and $
      ;        (data_in(t).nir gt 0. and data_in(t).nir lt 1.) and $
      ;        (data_in(t).fapar eq 0.) and $
      ;        (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5), countSoilOld)
      ;      if countSoilOld ne countSoil then stop
      ;      a=where((data_in(t).red gt 0. and data_in(t).red lt 1.), redc)
      ;      b=where((data_in(t).nir gt 0. and data_in(t).nir lt 1.), nirc)
      ;      c=where(data_in(t).fapar eq 0., faparc)
      ;      d=where(data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5, flagc)
      if idx_c(0) ge 0 then begin
        buf1(idx_c)= data_in(t).red(idx_c)+buf1(idx_c)
        buf2(idx_c)= data_in(t).nir(idx_c)+buf2(idx_c)
        buf11(idx_c)=buf11(idx_c)+1./data_in(t).sigma_red(idx_c)
        buf21(idx_c)=buf21(idx_c)+1./data_in(t).sigma_nir(idx_c)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
      endif
    endfor
  endelse
  ; vgt/fapar valid
  if nfield eq 3 then begin
    ;stop
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0. and buf3 gt 0.)   ; ng 2016
    ;stop
    if idx_ok(0) ge 0 then begin
      ; version with sigma...
      
      buf1(idx_ok)=buf1(idx_ok)/buf11(idx_ok)
      buf2(idx_ok)=buf2(idx_ok)/buf21(idx_ok)
      buf3(idx_ok)=buf3(idx_ok)/buf31(idx_ok)
      print,buf3(0,2430), num_used(0,2430)
      stop
      ;
      ;num_used(idx_c)=num_used(idx_c)+one(idx_c)
      ;print, num_used(0,2430)
      ; compute average of sigma(s)
      ;bufRedSigma(idx_ok)=bufRedSigma(idx_ok)/float(num_used(idx_ok))
      ;bufNirSigma(idx_ok)=bufNirSigma(idx_ok)/float(num_used(idx_ok))
      ;bufFaparSigma(idx_ok)=bufFaparSigma(idx_ok)/float(num_used(idx_ok))
      ; sigma weight based on sum...
    endif
    idx_nok=where(num_used le 2b or buf1 le 0.0 or buf2 le 0.0 or buf3 le 0.0)   ; ng 2016
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
      buf3(idx_nok)=-1.0
      ;bufRedSigma(idx_nok)=-1
      ;bufNirSigma(idx_nok)=-1
      ;bufFaparSigma(idx_nok)=-1
    endif
  endif else begin
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0.)  ;ng 2016
    ; bare soil / no fapar
    if idx_ok(0) ge 0 then begin
      buf1(idx_ok)=buf1(idx_ok)/buf11(idx_ok)
      buf2(idx_ok)=buf2(idx_ok)/buf21(idx_ok)
      ;buf3(idx_ok)=buf3(idx_ok)/float(num_used(idx_ok))
      ;bufRedSigma(idx_ok)=bufRedSigma(idx_ok)/float(num_used(idx_ok))
      ;bufNirSigma(idx_ok)=bufNirSigma(idx_ok)/float(num_used(idx_ok))
      ;;bufFaparSigma(idx_ok)=bufFaparSigma(idx_ok)/float(num_used(idx_ok))
    endif
    idx_nok=where(num_used le 2b or buf1 lt 0.0 or buf2 lt 0.0)  ; ng 2016
    ; no data here
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
      ;bufRedSigma(idx_nok)=-1.
      ;bufNirSigma(idx_nok)=-1.
      ;;bufFaparSigma(idx_nok)=-1.
    endif
  endelse
  
  if n_elements(buf3) ne 0 then faparMean=buf3
  ;tvscl, faparMean
  ;stop
  mean_field.red=buf1
  mean_field.nir=buf2
  if nfield eq 3 then mean_field.fapar=buf3
  ;
  ;window,0
  ;plot, mean_field.fapar(idx_ok), psym=1
  initValues=fltarr(tt(1),splitDims[0],splitDims[1])
  initValues[*]=-1.0
  ; compute the standard deviation for each field, i.e red nir and fapar ...
  ;
  std_field={red:initValues, nir:initValues, fapar:initValues}
  if nfield eq 3 then begin
    for t=0, tt(1)-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      buf3=fltarr(splitDims[0],splitDims[1])
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf3(*,*)=-1.0
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

      idx=where(validMaskVeg eq 1 and data_in(t).jrc_flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idx=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and data_in(t).jrc_flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))

      buf=reform(data_in(t).nir)
      buff=reform(mean_field.nir)
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and data_in(t).jrc_flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idx=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).jrc_flag eq 0) and num_used ge 3b, countBuf) ;ng 2016 )
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))

      ;
      buf=data_in(t).fapar
      buff=mean_field.fapar
    ;  stop
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and data_in(t).jrc_flag eq 0 and num_used ge 3b, countBuf) ;ng 2016
      ;idx=where((buf gt 0. and buf le 1.0) and (buff gt 0. and buff le 1.) and (data_in(t).jrc_flag eq 0) and num_used ge 3b, countBuf) ;ng 2016
      if idx(0) ge 0 then buf3(idx)  = abs(buf(idx)-buff(idx))
      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
      std_field.fapar(t,*,*)=buf3
    endfor
    ;   stop
  endif else begin
    for t=0, tt(1)-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf=fltarr(splitDims[0],splitDims[1])
      buff=fltarr(splitDims[0],splitDims[1])
      ;
      buf=data_in(t).red
      buff=mean_field.red
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      ; cut off NaN
      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; Previous version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld ne countBuf then stop
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))
      ;
      buf=data_in(t).nir
      buff=mean_field.nir
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      idx=where(validMaskVeg eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld-countBuf gt 1 then stop
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))
      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
      ;  stop
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
    buf=reform(std_field.red(t,*,*))
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf1(idx_ca)=(buf(idx_ca)^2)+buf1(idx_ca)
      num_used_1(idx_ca)  = num_used_1(idx_ca)+one(idx_ca)
    endif
    buf=reform(std_field.nir(t,*,*))
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf2(idx_ca)=(buf(idx_ca)^2)+buf2(idx_ca)
      num_used_2(idx_ca)  = num_used_2(idx_ca)+one(idx_ca)
    endif
    if nfield eq 3 then begin
      buf=reform(std_field.fapar(t,*,*))
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
  ;
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
  ;
  ;stop
  ;plot, mean_field(*,*).red, mean_field(*,*).nir, psym=1
  ;oplot, mean_field(*,*).nir, psym=4
  ;oplot, mean_field(*,*).fapar, psym=6













  ;  if n_elements(cloudtype) eq 1 then begin
  ;    ;device, decomposed=0
  ;    ;faparcolor
  ;    ;titles=['mask bit 1 or 2 (cloudy/shadow cloud)', 'fapar - mask bit 1 (cloudy)', 'fapar - mask bit 2 (shadow cloud)', 'fapar - no mask']
  ;    ;fNames=['both', 'only_cloudy', 'only_shadow_cloud', 'no_mask']
  ;    ;tempDir='E:\mariomi\Documents\projects\ldtr\data\pics\avhrr\'
  ;    ;; fapar
  ;    ;window, 0, title='fapar - '+titles[cloudtype]
  ;    ;nday=n_elements(data_in)
  ;    all_day_data=reform(data_in(*).fapar(0,1950:2100))
  ;    meandata=reform(mean_field.fapar(0,1950:2100))
  ;    stddata=reform(std_field.fapar(*,0,1950:2100))
  ;    stdmean=reform(std_mean.temp(0,1950:2100))
  ;    save, all_day_data, meandata, stddata, stdmean, filename='fpa_'+strcompress(cloudtype, /REMOVE)+'.sav'
  ;    all_day_data=reform(data_in(*).red(0,1950:2100))
  ;    meandata=reform(mean_field.red(0,1950:2100))
  ;    stddata=reform(std_field.red(*,0,1950:2100))
  ;    stdmean=reform(std_mean.red(0,1950:2100))
  ;    save, all_day_data, meandata, stddata, stdmean, filename='red_'+strcompress(cloudtype, /REMOVE)+'.sav'
  ;    all_day_data=reform(data_in(*).nir(0,1950:2100))
  ;    meandata=reform(mean_field.nir(0,1950:2100))
  ;    stddata=reform(std_field.nir(*,0,1950:2100))
  ;    stdmean=reform(std_mean.nir(0,1950:2100))
  ;    save, all_day_data, meandata, stddata, stdmean, filename='nir_'+strcompress(cloudtype, /REMOVE)+'.sav'
  ;;    plot, data_in(0).fapar(0,1950:2100), yr=[0.,0.6], min=0.01, psym = 3, max=0.9
  ;;    ;for t=0, nday-1 do oplot, data_in(t).fapar(0,1950:2100), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;    ;for t=0, nday-1 do oplot, mean_field.fapar(0,1950:2100)+std_field.fapar(t,0,1950:2100), col=fix(float(t)*255/nday), min=0.01
  ;;    nData=float(n_elements(data_in(0).fapar(0,1950:2100)))
  ;;    for t=0, nday-1 do begin
  ;;      oplot, data_in(t).fapar(0,1950:2100), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;      oplot, mean_field.fapar(0,1950:2100)+std_field.fapar(t,0,1950:2100), col=fix(float(t)*255/nday), min=0.01
  ;
  ;;      plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
  ;;      xyouts, .05, 1.*t/nday, string(t+1, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
  ;;    endfor
  ;;    device, decomposed=1
  ;;    oplot, std_mean.temp(0,1950:2100), min=0.01, col=0l, thick=2.5
  ;;    oplot, mean_field.fapar(0,1950:2100), line = 0, min=0.01, thick=2.5, color=255l*255*255
  ;;    oplot, mean_field.fapar(0,1950:2100)+ std_mean.temp(0,1950:2100), min=0.01, color=255l*255*255, thick=1.5, max=0.9, linestyle=3
  ;;    oplot, mean_field.fapar(0,1950:2100)- std_mean.temp(0,1950:2100), min=0.01, color=255l*255*255, thick=1.5, linestyle=3
  ;;    device, decomposed=0
  ;;    save, all_day_data, meandata, stddata, stdmean, filename='fpa.sav'
  ;;    all_day_data=0 & meandata=0 & stddata=0 & stdmean=0
  ;;    restore, filename='fpa.sav'
  ;;    window, 1, title='fapar - '+titles[cloudtype]
  ;;    plot, reform(all_day_data[*,0]), yr=[0.,0.6], min=0.01, psym = 3, max=0.9
  ;;    nData=float(n_elements(all_day_data[*,0]))
  ;;    nday=float(n_elements(all_day_data[0,*]))
  ;;    for t=0, nday-1 do begin
  ;;      oplot, reform(all_day_data[*,t]), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;      oplot, reform(meandata)+reform(stddata[t]), col=fix(float(t)*255/nday), min=0.01
  ;;      plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
  ;;      xyouts, .05, 1.*t/nday, string(t+1, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
  ;;    endfor
  ;;    device, decomposed=1
  ;;    oplot, reform(stdmean), min=0.01, col=0l, thick=2.5
  ;;    oplot, reform(meandata), line = 0, min=0.01, thick=2.5, color=255l*255*255
  ;;    oplot, reform(meandata)+ reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, max=0.9, linestyle=3
  ;;    oplot, reform(meandata)- reform(stdmean), min=0.01, col=255l*255*255, thick=1.5, linestyle=3
  ;;    device, decomposed=0
  ;;
  ;;    plotimg=tvrd(true=1)
  ;;    fName=tempDir+'fapar_'+fNames[cloudtype]+'.png'
  ;;    write_png,fName,plotimg
  ;;    ;; b1 / red
  ;;    window, 1, title='red - '+titles[cloudtype]
  ;;    plot, data_in(0).red(0,1950:2100), yr=[0.,0.6], min=0.01, psym = 3, max=0.9
  ;;    for t=0, nday-1 do begin
  ;;      oplot, data_in(t).red(0,1950:2100), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;      oplot, mean_field.red(0,1950:2100)+std_field.red(t,0,1950:2100), col=fix(float(t)*255/nday), min=0.01
  ;;      plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
  ;;      xyouts, .05, 1.*t/nday, string(t+1, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
  ;;    endfor
  ;;    oplot, mean_field.red(0,1950:2100), line = 0, min=0.01, thick=2.5
  ;;    oplot, std_mean.red(0,1950:2100), min=0.01, col=250, thick=2.5
  ;;    oplot, mean_field.red(0,1950:2100)+ std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.9
  ;;    oplot, mean_field.red(0,1950:2100)- std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5
  ;;    plotimg=tvrd(true=1)
  ;;    fName=tempDir+'red_'+fNames[cloudtype]+'.png'
  ;;    write_png,fName,plotimg
  ;;    ;; b2 / nir
  ;;    window, 2, title='nir - '+titles[cloudtype]
  ;;    plot, data_in(0).nir(0,1950:2100), yr=[0.,0.6], min=0.01, psym = 3, max=0.9
  ;;    ;for t=0, nday-1 do oplot, data_in(t).fapar(0,1950:2100), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;    ;for t=0, nday-1 do oplot, mean_field.fapar(0,1950:2100)+std_field.fapar(t,0,1950:2100), col=fix(float(t)*255/nday), min=0.01
  ;;    for t=0, nday-1 do begin
  ;;      oplot, data_in(t).nir(0,1950:2100), col=fix(float(t)*255/nday), psym = 2, min=0.01
  ;;      oplot, mean_field.nir(0,1950:2100)+std_field.nir(t,0,1950:2100), col=fix(float(t)*255/nday), min=0.01
  ;;      plots, [0., .05], [1.*t/nday,1.*t/nday], /NORM, col=fix(float(t)*255/nday), thick=4.
  ;;      xyouts, .05, 1.*t/nday, string(t, format='(I02)'), /NORM, col=fix(float(t)*255/nday), charsize=1.2, ALIGN=1.;fix(float(t)*255/nday)
  ;;    endfor
  ;;    oplot, mean_field.nir(0,1950:2100), line = 0, min=0.01, thick=2.5
  ;;    oplot, std_mean.nir(0,1950:2100), min=0.01, col=250, thick=2.5
  ;;    oplot, mean_field.nir(0,1950:2100)+ std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.9
  ;;    oplot, mean_field.nir(0,1950:2100)- std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5
  ;;    plotimg=tvrd(true=1)
  ;;    fName=tempDir+'nir_'+fNames[cloudtype]+'.png'
  ;;    write_png,fName,plotimg
  ;
  ;  endif

  ;  window, 1, title='red'
  ;  plot, data_in(0).red(0,1950:2100), yr=[0.,0.6], min=0.01, psym = 3
  ;  for t=0, 9 do oplot, data_in(t).red(0,1950:2100), col=t*20, psym = 2, min=0.01
  ;  oplot, mean_field.red(0,1950:2100), line = 0, min=0.01, thick=2.5
  ;  for t=0, 9 do oplot, mean_field.red(0,1950:2100)-std_field.red(t,0,1950:2100), col=t*20, max=0.6
  ;  for t=0, 9 do oplot, mean_field.red(0,1950:2100)+std_field.red(t,0,1950:2100), col=t*20, min=0.01
  ;  oplot, std_mean.red(0,1950:2100), min=0.01, col=250, thick=2.5
  ;  oplot, mean_field.red(0,1950:2100)+ std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.6
  ;  oplot, mean_field.red(0,1950:2100)- std_mean.red(0,1950:2100), min=0.01, col=250, thick=1.5
  ;
  ;  window, 2, title='nir'
  ;  plot, data_in(0).nir(0,1950:2100), yr=[0.,0.6], min=0.01, psym = 3
  ;  for t=0, 9 do oplot, data_in(t).nir(0,1950:2100), col=t*20, psym = 2, min=0.01
  ;  oplot, mean_field.nir(0,1950:2100), line = 0, min=0.01, thick=2.5
  ;  for t=0, 9 do oplot, mean_field.nir(0,1950:2100)-std_field.red(t,0,1950:2100), col=t*20, max=0.6
  ;  for t=0, 9 do oplot, mean_field.nir(0,1950:2100)+std_field.red(t,0,1950:2100), col=t*20, min=0.01
  ;  oplot, std_mean.nir(0,1950:2100), min=0.01, col=250, thick=2.5
  ;  oplot, mean_field.nir(0,1950:2100)+ std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5, max=0.6
  ;  oplot, mean_field.nir(0,1950:2100)- std_mean.nir(0,1950:2100), min=0.01, col=250, thick=1.5
  ;  stop
end
