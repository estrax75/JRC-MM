Pro call_mean_3, mean_field, std_mean, std_field, nfield
  ;
  COMMON bigData 

  mean_field=fltarr(nfield,7200,3600)
  ;
  buf1=fltarr(7200,3600)
  buf2=fltarr(7200,3600)
  ;

  if nfield eq 3 then buf3=fltarr(7200,3600)
  ;
  tt=expectedDays
  ;
  num_used=bytarr(7200,3600)
  one=num_used
  one(*,*)=1
  ;
  ;
  ; compute the average for each field, i.e red nir and fapar ...
  ;
  if nfield eq 3 then begin

    for t=0, tt-1 do begin
      idx_c= where(data_day(t).red gt 0. and data_day(t).red lt 1. and $
        data_day(t).nir gt 0. and data_day(t).nir lt 1. and $
        data_day(t).fapar gt 0. and data_day(t).fapar le 1. and $
        data_day(t).flag eq 0.)

      if idx_c(0) ge 0 then begin
        buf1(idx_c)= data_day(t).red(idx_c)+buf1(idx_c)
        buf2(idx_c)= data_day(t).nir(idx_c)+buf2(idx_c)
        buf3(idx_c)= data_day(t).fapar(idx_c)+buf3(idx_c)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
      endif
    endfor
  endif else begin
    for t=0, tt-1 do begin
      idx_c= where(data_day(t).red gt 0. and data_day(t).red lt 1. and $
        data_day(t).nir gt 0. and data_day(t).nir lt 1. and $
        data_day(t).fapar eq 0. and $
        data_day(t).flag eq 4.)

      if idx_c(0) ge 0 then begin
        buf1(idx_c)= data_day(t).red(idx_c)+buf1(idx_c)
        buf2(idx_c)= data_day(t).nir(idx_c)+buf2(idx_c)
        num_used(idx_c)=num_used(idx_c)+one(idx_c)
      endif
    endfor
  endelse
  ;
  if nfield eq 3 then begin
    idx_ok=where(num_used ge 1 and buf1 gt 0.0 and buf2 gt 0. and buf3 gt 0.)
    if idx_ok(0) ge 0 then begin
      buf1(idx_ok)=buf1(idx_ok)/float(num_used(idx_ok))
      buf2(idx_ok)=buf2(idx_ok)/float(num_used(idx_ok))
      buf3(idx_ok)=buf3(idx_ok)/float(num_used(idx_ok))
    endif
    idx_nok=where(num_used lt 1.0 or buf1 le 0.0 or buf2 le 0.0 or buf3 le 0.0)
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
      buf3(idx_nok)=-1.0
    endif
  endif else begin
    idx_ok=where(num_used ge 1 and buf1 gt 0.0 and buf2 gt 0.)
    if idx_ok(0) ge 0 then begin
      buf1(idx_ok)=buf1(idx_ok)/float(num_used(idx_ok))
      buf2(idx_ok)=buf2(idx_ok)/float(num_used(idx_ok))
    endif
    idx_nok=where(num_used lt 1.0 or buf1 lt 0.0 or buf2 lt 0.0)
    if idx_nok(0) ge 0 then begin
      buf1(idx_nok)=-1.0
      buf2(idx_nok)=-1.0
    endif
  endelse
  ;
  ;
  ;
  mean_field(0,*,*)=buf1(*,*)
  mean_field(1,*,*)=buf2(*,*)
  if nfield eq 3 then mean_field(2,*,*)=buf3(*,*)
  ;
  ;
  ; compute the standard deviation for each field, i.e red nir and fapar ...
  ;
  std_field=fltarr(nfield,tt,7200,3600)
  std_field(*,*,*,*)=-1.0
  ;
  ;
  if nfield eq 3 then begin
    for t=0, expectedDays-1 do begin
      buf1=fltarr(7200,3600)
      buf2=fltarr(7200,3600)
      buf3=fltarr(7200,3600)
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf3(*,*)=-1.0
      buf=fltarr(7200,3600)
      buff=fltarr(7200,3600)
      ;
      buf(*,*)=data_day(t).red(*,*)
      buff(*,*)=mean_field(0,*,*)
      idx=where(buf gt 0. and buf lt 1.0 and buff gt 0. and buff lt 1. and data_day(t).flag eq 0.)
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))
      ;
      buf=data_day(t).nir
      buff(*,*)=mean_field(1,*,*)
      idx=where(buf gt 0. and buf lt 1.0 and buff gt 0. and buff lt 1.  and data_day(t).flag eq 0.)
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))
      ;
      buf=data_day(t).fapar
      buff(*,*)=mean_field(2,*,*)
      idx=where(buf gt 0. and buf le 1.0 and buff gt 0. and buff le 1. and data_day(t).flag eq 0.)
      if idx(0) ge 0 then buf3(idx)  = abs(buf(idx)-buff(idx))
      ;
      std_field(0,t,*,*)=buf1
      std_field(1,t,*,*)=buf2
      std_field(2,t,*,*)=buf3
    endfor
  endif else begin
    for t=0, expectedDays-1 do begin
      buf1=fltarr(7200,3600)
      buf2=fltarr(7200,3600)
      buf1(*,*)=-1.0
      buf2(*,*)=-1.0
      buf=fltarr(7200,3600)
      buff=fltarr(7200,3600)
      ;
      buf(*,*)=data_day(t).red
      buff(*,*)=mean_field(0,*,*)
      idx=where(buf gt 0. and buf lt 1.0 and buff gt 0. and buff lt 1. and data_day(t).flag eq 4.)
      if idx(0) ge 0 then buf1(idx) = abs(buf(idx)-buff(idx))
      ;
      buf=data_day(t).nir
      buff(*,*)=mean_field(1,*,*)
      idx=where(buf gt 0. and buf lt 1.0 and buff gt 0. and buff lt 1. and data_day(t).flag eq 4.)
      if idx(0) ge 0 then buf2(idx)= abs(buf(idx)-buff(idx))
      ;
      std_field(0,t,*,*)=buf1
      std_field(1,t,*,*)=buf2
    endfor
  endelse
  ;
  ;
  ; compute the mean of the standard deviation ...
  ;
  std_mean=fltarr(nfield,7200,3600)
  ;
  num_used_1=bytarr(7200,3600)
  num_used_2=bytarr(7200,3600)
  if nfield eq 3 then num_used_3=bytarr(7200,3600)
  ;
  buf=fltarr(7200,3600)
  ;
  buf1=fltarr(7200,3600)
  buf2=fltarr(7200,3600)
  if nfield eq 3 then buf3=fltarr(7200,3600)
  ;
  for t=0, expectedDays-1 do begin
    buf(*,*)=std_field(0,t,*,*)^2
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf1(idx_ca)=buf(idx_ca)+buf1(idx_ca)
      num_used_1(idx_ca)  = num_used_1(idx_ca)+one(idx_ca)
    endif
    buf(*,*)=std_field(1,t,*,*)^2
    idx_ca=where(buf ge 0.)
    if idx_ca(0) ge 0 then begin
      buf2(idx_ca)=buf(idx_ca)+buf2(idx_ca)
      num_used_2(idx_ca)  = num_used_2(idx_ca)+one(idx_ca)
    endif
    if nfield eq 3 then begin
      buf(*,*)=std_field(2,t,*,*)^2
      idx_ca=where(buf ge 0.)
      if idx_ca(0) ge 0 then begin
        buf3(idx_ca)=buf(idx_ca)+buf3(idx_ca)
        num_used_3(idx_ca)  = num_used_3(idx_ca)+one(idx_ca)
      endif
    endif
  endfor
  ;
  ;
  ;
  idx_ok=where(num_used_1 ge 2.0)
  if idx_ok(0) ge 0 then buf1(idx_ok)=sqrt(buf1(idx_ok)/float(num_used_1(idx_ok)-1))
  idx_ok=where(num_used_2 ge 2.0)
  if idx_ok(0) ge 0 then buf2(idx_ok)=sqrt(buf2(idx_ok)/float(num_used_2(idx_ok)-1))
  if nfield eq 3 then begin
    idx_ok=where(num_used_3 ge 2.0)
    if idx_ok(0) ge 0 then buf3(idx_ok)=sqrt(buf3(idx_ok)/float(num_used_3(idx_ok)-1))
  endif
  ;
  idx_nok=where(num_used le 1.0)
  if idx_nok(0) ge 0 then begin
    buf1(idx_nok)=-1.0
    buf2(idx_nok)=-1.0
    if nfield eq 3 then buf3(idx_nok)=-1.0
  endif
  ;
  std_mean(0,*,*)=buf1(*,*)
  std_mean(1,*,*)=buf2(*,*)
  if nfield eq 3 then std_mean(2,*,*)=buf3(*,*)

end
;===================================================================================================
;


PRO FindEuclideanMatricDistance, idx_third, distance, mean_field, std_mean, nfield

  COMMON bigData 

  ;
  ;
  ;
  ; inputs:
  ;	data_in : daily data
  ;	position of of valid points > 3
  ;	nfield: 3 if vegetation - 2 if bare soil
  ;
  ; option: add weight with uncertainties
  ;
  ; outputs:
  ;	distance : normalized euclidean distance
  ;	mean_field: average over the valid dates
  ;	std_mean: mean standard deviation over the valid dates
  ;
  ;
  ;
  okpix=N_elements(idx_third)
  ;
  ; compute the mean and std of the fapar and red/nir channels
  ;
  ;
  call_mean_3,  mean_field,  std_mean, std_field, nfield
  ;
  ; compute the distance
  ;
  distance=fltarr(expectedDays,okpix)
  ;
  distance(*,*)=100.0
  ;
  buf=fltarr(okpix)
  ;
  buf1=fltarr(okpix)
  buf2=fltarr(okpix)
  if nfield eq 3 then buf3=fltarr(okpix)
  ;
  buf4=fltarr(okpix)
  buf5=fltarr(okpix)
  if nfield eq 3 then buf6=fltarr(okpix)
  ;
  img=fltarr(7200,3600)
  img(*,*)=std_mean(0,*,*)
  buf1=img(idx_third)
  img(*,*)=std_mean(1,*,*)
  buf2=img(idx_third)
  if nfield eq 3 then begin
    img(*,*)=std_mean(2,*,*)
    buf3=img(idx_third)
  endif
  ;
  if nfield eq 3 then begin

    for t=0, expectedDays-1 do begin
      img=fltarr(7200,3600)
      img=std_field(0,t,*,*)
      buf4(*)=img(idx_third)
      img=std_field(1,t,*,*)
      buf5(*)=img(idx_third)
      img=std_field(2,t,*,*)
      buf6(*)=img(idx_third)
      buf(*)=100.0
      ;
      ; first case none nul
      ;
      idx_ca=where(buf1 gt 0.0 and buf2 gt 0.0 and buf3 gt 0.0 and $
        buf4 gt 0.0 and  buf5 gt 0.0 and  buf6 gt 0.0)
      ;
      ;
      if idx_ca(0) ge 0 then begin
        buf(idx_ca)= sqrt( $
          buf4(idx_ca)^2/buf1(idx_ca)^2 +$
          buf5(idx_ca)^2/buf2(idx_ca)^2 +$
          buf6(idx_ca)^2/buf3(idx_ca)^2 )
      endif
      ;
      ;
      ; second case if one is equal to zero ... i.e no change during the period
      ;
      idx_ca1=where(buf1 eq 0.0 and buf4  ge 0.0 )
      idx_ca2=where(buf2 eq 0.0 and buf5  ge 0.0 )
      idx_ca3=where(buf3 eq 0.0 and buf6  ge 0.0 )
      ;
      ;if idx_ca1(0) ge 0 or idx_ca2(0) ge 0 or idx_ca3(0) ge 0  then stop
      ;
      ;
      ;
      ;
      distance(t,*)=buf(*)
    endfor
  endif else begin
    for t=0, expectedDays-1 do begin
      img=fltarr(7200,3600)
      img=std_field(0,t,*,*)
      buf4=img(idx_third)
      img=std_field(1,t,*,*)
      buf5=img(idx_third)
      buf(*)=100.0
      ;
      ; first case none nul
      ;
      idx_ca=where(buf1 gt 0.0 and buf2 gt 0.0 and $
        buf4 gt 0.0 and  buf5 gt 0.0)
      ;
      ;
      if idx_ca(0) ge 0 then begin
        buf(idx_ca)= sqrt( $
          buf4(idx_ca)^2/buf1(idx_ca)^2 +$
          buf5(idx_ca)^2/buf2(idx_ca)^2)
      endif
      ;
      ;
      ; second case if one is equal to zero ... i.e no change during the period
      ;
      idx_ca1=where(buf1 eq 0.0 and buf4  ge 0.0 )
      idx_ca2=where(buf2 eq 0.0 and buf5  ge 0.0 )
      ;
      ;if idx_ca1(0) ge 0 or idx_ca2(0) ge 0 then stop
      ;
      ;
      ;
      ;
      distance(t,*)=buf(*)
    endfor
  endelse
  ;stop
  ;
END
