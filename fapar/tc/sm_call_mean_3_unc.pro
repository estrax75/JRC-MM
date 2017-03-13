;Pro sm_call_mean_3_unc,  daysNumber, data_in, mean_field, std_mean, std_field, sigmasum1, sigmasum2, sigmasum3, nfield, splitDims
Pro sm_call_mean_3_unc,  daysNumber, data_in, mean_field, std_mean, std_field, weightSumRed, weightSumNir, weightSumFapar, nfield, splitDims
  ;
  if n_elements(splitDims) ne 2 then splitDims=[7200,3600]
  mean_field={red:fltarr(splitDims[0],splitDims[1]), nir:fltarr(splitDims[0],splitDims[1]), fapar:fltarr(splitDims[0],splitDims[1])}
  buf1=dblarr(splitDims[0],splitDims[1])
  buf2=dblarr(splitDims[0],splitDims[1])
  weightSumRed=dblarr(splitDims[0],splitDims[1])
  weightSumNir=dblarr(splitDims[0],splitDims[1])
  ;
  ;sigmasum1=dblarr(splitDims[0],splitDims[1])
  ;sigmasum2=dblarr(splitDims[0],splitDims[1])

  if nfield eq 3 then buf3=dblarr(splitDims[0],splitDims[1])
  if nfield eq 3 then weightSumFapar=dblarr(splitDims[0],splitDims[1])
  ;if nfield eq 3 then sigmasum3=dblarr(splitDims[0],splitDims[1])

   
  ; define weight as total(sigma) / sigma_i
   
   
  num_used=bytarr(splitDims[0],splitDims[1])
  one=num_used
  one(*,*)=1
  ; compute the average for each field, i.e red nir and fapar ...
  if nfield eq 3 then begin

    for t=0, daysNumber-1 do begin
      ; cut off NaN
      validMask=finite(data_in(t).fapar(*,*)) and finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*))
      goodIndexes=where(validMask eq 1)

      idxMaskVeg=where(data_in(t).fapar[goodIndexes] gt 0.0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg
      ;
      ;
      ;   validMaskVege did include fapar = 0.0
      ;
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

        ;buf1(idx_c)= data_in(t).red(idx_c)*1./data_in(t).sigma_red(idx_c)+buf1(idx_c)
        ;buf2(idx_c)= data_in(t).nir(idx_c)*1./data_in(t).sigma_nir(idx_c)+buf2(idx_c)
        ;buf3(idx_c)= data_in(t).fapar(idx_c)*1./data_in(t).sigma(idx_c)+buf3(idx_c)
        ; 20170210: FC suggestions: sigma squared?
        buf1(idx_c)+= data_in(t).red(idx_c)*1./data_in(t).sigma_red(idx_c);+buf1(idx_c)
        buf2(idx_c)+= data_in(t).nir(idx_c)*1./data_in(t).sigma_nir(idx_c);+buf2(idx_c)
        buf3(idx_c)+= data_in(t).fapar(idx_c)*1./data_in(t).sigma(idx_c);+buf3(idx_c)
        ;
        weightSumRed(idx_c)+=1./data_in(t).sigma_red(idx_c);+weightSumRed(idx_c)
        weightSumNir(idx_c)+=1./data_in(t).sigma_nir(idx_c);+weightSumNir(idx_c)
        weightSumFapar(idx_c)+=1./data_in(t).sigma(idx_c);+weightSumFapar(idx_c)
         ;
;        sigmasum1(idx_c)=data_in(t).sigma_red(idx_c)+sigmasum1(idx_c)
;        sigmasum2(idx_c)=data_in(t).sigma_nir(idx_c)+sigmasum2(idx_c)
;        sigmasum3(idx_c)=data_in(t).sigma(idx_c)+sigmasum3(idx_c)

        ;print, t, data_in(t).fapar(0,2430), data_in(t).sigma(0,2430)
        ;print, buf3(0,2430), buf31(0,2430)
        num_used(idx_c)+=one(idx_c);+num_used(idx_c)
        ;print, num_used(0,2430)
      endif
    endfor
    ;stop
  endif else begin
    ;
    ; soil
    ;
    for t=0, daysNumber-1 do begin
      validMask=finite(data_in(t).fapar(*,*)) and finite(data_in(t).red(*,*)) and finite(data_in(t).nir(*,*))
      goodIndexes=where(validMask eq 1)

      idxMaskSoil=where(data_in(t).fapar[goodIndexes] eq 0 and $
        data_in(t).red[goodIndexes] gt 0.0 and data_in(t).red[goodIndexes] lt 1.0 and $
        data_in(t).nir[goodIndexes] gt 0.0 and data_in(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      validMaskSoil=validMask*validMaskSoil
      ;
      ; NG: verifica che fapar = 0.0
      ;
      ;
      idx_c= where(validMaskSoil eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and data_in(t).fapar eq 0.0, countSoil)
      ;stop
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
      if countSoil gt 0 then begin
        ; before
        ;        buf1(idx_c)= data_in(t).red(idx_c)+buf1(idx_c)
        ;        buf2(idx_c)= data_in(t).nir(idx_c)+buf2(idx_c)
        ;        buf11(idx_c)=buf11(idx_c)+1./data_in(t).sigma_red(idx_c)
        ;        buf21(idx_c)=buf21(idx_c)+1./data_in(t).sigma_nir(idx_c)
        ; new
        ; 20170210: FC suggestions: sigma squared?
        buf1(idx_c)+= data_in(t).red(idx_c)*1./data_in(t).sigma_red(idx_c);+buf1(idx_c)
        buf2(idx_c)+= data_in(t).nir(idx_c)*1./data_in(t).sigma_nir(idx_c);+buf2(idx_c)
        ;
        weightSumRed(idx_c)+=1./data_in(t).sigma_red(idx_c);+weightSumRed(idx_c)
        weightSumNir(idx_c)+=1./data_in(t).sigma_nir(idx_c);+weightSumNir(idx_c)
        num_used(idx_c)+=one(idx_c);num_used(idx_c)
      endif
    endfor
    ;    stop
  endelse
  ; vgt/fapar valid
  if nfield eq 3 then begin
    ;stop
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0.0 and buf3 gt 0.0)   ; ng 2016
    ;stop
    if idx_ok(0) ge 0 then begin
      ; version with sigma...

      buf1(idx_ok)=buf1(idx_ok)/weightSumRed(idx_ok)
      buf2(idx_ok)=buf2(idx_ok)/weightSumNir(idx_ok)
      buf3(idx_ok)=buf3(idx_ok)/weightSumFapar(idx_ok)
      ; print,buf3(0,2430), num_used(0,2430)
      ; stop
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
      ; MM think about use Nan...
      buf1(idx_nok)=-1.0 ;!VALUES.F_NAN
      buf2(idx_nok)=-1.0 ;!VALUES.F_NAN
      buf3(idx_nok)=-1.0 ;!VALUES.F_NAN
    endif
  endif else begin
    idx_ok=where(num_used ge 3b and buf1 gt 0.0 and buf2 gt 0.)  ;ng 2016
    ; bare soil / no fapar
    if idx_ok(0) ge 0 then begin
      buf1(idx_ok)=buf1(idx_ok)/weightSumRed(idx_ok)
      buf2(idx_ok)=buf2(idx_ok)/weightSumNir(idx_ok)
      ;buf3(idx_ok)=buf3(idx_ok)/float(num_used(idx_ok))
      ;bufRedSigma(idx_ok)=bufRedSigma(idx_ok)/float(num_used(idx_ok))
      ;bufNirSigma(idx_ok)=bufNirSigma(idx_ok)/float(num_used(idx_ok))
      ;;bufFaparSigma(idx_ok)=bufFaparSigma(idx_ok)/float(num_used(idx_ok))
    endif
    idx_nok=where(num_used le 2b or buf1 lt 0.0 or buf2 lt 0.0)  ; ng 2016
    ; no data here
    if idx_nok(0) ge 0 then begin
      ; MM think about use Nan...
      buf1(idx_nok)=-1.0 ;!VALUES.F_NAN
      buf2(idx_nok)=-1.0 ;!VALUES.F_NAN
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
  initValues=fltarr(daysNumber,splitDims[0],splitDims[1])
  ; MM think about use Nan...
  initValues[*]=-1.0 ;!VALUES.F_NAN
  ; compute the standard deviation for each field, i.e red nir and fapar ...
  ;
  std_field={red:initValues, nir:initValues, fapar:initValues}
  if nfield eq 3 then begin
    for t=0, daysNumber-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      buf3=fltarr(splitDims[0],splitDims[1])
      ; MM think about use Nan...
      buf1(*,*)=-1.0 ;!VALUES.F_NAN
      buf2(*,*)=-1.0 ;!VALUES.F_NAN
      buf3(*,*)=-1.0 ;!VALUES.F_NAN
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
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))  ;*data_in(t).sigma_red(idx)/buf11(idx)

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
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx)) ;*data_in(t).sigma_nir(idx)/buf21(idx)

      ;
;      stop ;sto pstop sto pstop
      buf(idx)=data_in(t).fapar(idx) ;/data_in(t).sigma(idx)
;
;
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
      if idx(0) ge 0 then buf3(idx)  = abs(buf(idx)-buff(idx))      ;*data_in(t).sigma(idx)/buf31(idx)
      idx_check = 964418
    ;  print, t, buf1(idx_check), buf2(idx_check), buf3(idx_check), buf(idx_check) ;,  sigmasum3(idx_check)/data_in(t).sigma(idx_check)
    ;  print,mean_field.red(idx_check), mean_field.nir(idx_check), mean_field.fapar(idx_check)
      ;stop

      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
      std_field.fapar(t,*,*)=buf3

    endfor
  ;  stop
    ;stop
    ;   stop
  endif else begin
    for t=0, daysNumber-1 do begin
      buf1=fltarr(splitDims[0],splitDims[1])
      buf2=fltarr(splitDims[0],splitDims[1])
      ; MM think about use Nan...
      buf1(*,*)=-1.0 ;VALUES.F_NAN
      buf2(*,*)=-1.0 ;VALUES.F_NAN
      buf=fltarr(splitDims[0],splitDims[1])
      buff=fltarr(splitDims[0],splitDims[1])
      ;
      buf=data_in(t).red
      buff=mean_field.red
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      ; cut off out-of-range
      idxMaskSoil=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      validMaskSoil=validMask*validMaskSoil

      idx=where(validMaskSoil eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; Previous version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld ne countBuf then stop
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx)) ;*data_in(t).sigma_red(idx)*buf11(idx)
      ;if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))
      ;
      buf=data_in(t).nir
      buff=mean_field.nir
      ; cut off NaN
      validMask=finite(buf) and finite(buff)
      goodIndexes=where(validMask eq 1)
      ; cut off out-of-range
      idxMaskSoil=where(buf[goodIndexes] gt 0.0 and buf[goodIndexes] lt 1.0 and $
        buff[goodIndexes] gt 0.0 and buff[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      validMaskSoil=validMask*validMaskSoil

      idx=where(validMaskSoil eq 1 and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBuf) ;ng 2016
      ; previous version without NaN
      ;idxOld=where((buf gt 0. and buf lt 1.0) and (buff gt 0. and buff lt 1.) and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5) and num_used ge 3b, countBufOld) ; ng 2016
      ;if countBufOld-countBuf gt 1 then stop
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))  ;*data_in(t).sigma_nir(idx)*buf21(idx)
      ;if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))
      std_field.red(t,*,*)=buf1
      std_field.nir(t,*,*)=buf2
      ;  stop
    endfor
  endelse
  ;window, /free
  ;plot,
  ;
  ; compute the mean of the standard deviation ...
  ; BUT REMOVE THE weighting FACTOR FOR ALL DAY
  ;
  ; now split...
  std_mean={red:fltarr(splitDims[0],splitDims[1]), nir:fltarr(splitDims[0],splitDims[1]), temp:fltarr(splitDims[0],splitDims[1])}
  num_used_1=bytarr(splitDims[0],splitDims[1])
  num_used_2=bytarr(splitDims[0],splitDims[1])
  if nfield eq 3 then num_used_3=bytarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  buf1=fltarr(splitDims[0],splitDims[1])
  buf2=fltarr(splitDims[0],splitDims[1])
  if nfield eq 3 then buf3=fltarr(splitDims[0],splitDims[1])
  for t=0, daysNumber-1 do begin
    buf=reform(std_field.red(t,*,*))
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf1(idx_ca)+=(buf(idx_ca))^2;+buf1(idx_ca)
     ; buf1(idx_ca)=(buf(idx_ca)/(data_in(t).sigma_red(idx_ca)*buf11(idx_ca)))^2+buf1(idx_ca)
      num_used_1(idx_ca)  += one(idx_ca);+num_used_1(idx_ca)
    endif
    buf=reform(std_field.nir(t,*,*))
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf2(idx_ca)+=(buf(idx_ca))^2;+buf2(idx_ca)
      ;buf2(idx_ca)=(buf(idx_ca)/(data_in(t).sigma_nir(idx_ca)*buf21(idx_ca)))^2+buf2(idx_ca)
      num_used_2(idx_ca)  += one(idx_ca);+num_used_2(idx_ca)
    endif
    if nfield eq 3 then begin
      buf=reform(std_field.fapar(t,*,*))
      idx_ca=where(buf ge 0.)
      if idx_ca(0) ge 0 then begin
        ;   print, t, buf(0,2430),  buf(0,2430)*data_in(t).sigma(0,2430)*buf31(0,2430)
        buf3(idx_ca)+=(buf(idx_ca))^2;+buf3(idx_ca)
        ;buf3(idx_ca)=(buf(idx_ca)/(data_in(t).sigma(idx_ca)*buf31(idx_ca)))^2+buf3(idx_ca)
        num_used_3(idx_ca) += one(idx_ca);num_used_3(idx_ca)
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
    ; MM think about use Nan...
    buf1(idx_nok)=-1.0 ; !VALUES.F_NAN
    buf2(idx_nok)=-1.0 ; !VALUES.F_NAN
    if nfield eq 3 then buf3(idx_nok)=-1.0
  endif
  std_mean.red[*,*]=buf1[*,*]
  std_mean.nir[*,*]=buf2[*,*]
  if nfield eq 3 then begin
    std_mean.temp[*,*]=buf3[*,*]
    ;print, std_mean.temp(0,2430),  std_mean.red(0,2430), std_mean.nir(0,2430)
    ;  stop
    checkZeroes=where(std_mean.temp[*,*] eq 0, countZeroes)
    print,'Find # points with Standard Deviation Mean FAPAR = 0 ', countZeroes
  endif
  
end
