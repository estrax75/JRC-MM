Pro sm_make_tc_distance_eu_vegetation_m_unc_last, dayVeg, daysNumber, data_in, idx_doIt, day, meandat, std_mean, nfield, index_2, splitDims
  ;
  ; input
  ; daysNumber : total number of days (files)
  ; data_in : data for each days
  ; data_in : daily data (at least 3 days contain valid value)
  ; index : number of observation
  ; idx_doIt : position where index ge 3

  ; nfield : 3 over vegetation pixels
  ; nfield : 2 over bare soil
  ;
  ; output:
  ; day : the day to be used in the time composite
  ; index_2 : number of observation after outliers
  ; meandat : temporal deviation over the period (only valid pixels after outliers)
  ;
  ; compute the distance
  ;help, idx_doIT
  ;stop
  print, 'new sigma'
  ;if nfield eq 2 then stop
  sm_FindEuclideanMatricDistance_last_unc, daysNumber, data_in, idx_doIt, distance, meandat, std_mean, nfield, splitDims
  ;
  ; remove outliers using a threshold (different if we use fapar or not)
  ;
  if nfield eq 3 then thres = 7.915
  if nfield eq 2 then thres = 5.991
  ; this setting coming from SeaWifs l3time C-Code
  ;  if nfield eq 3 then thres = 3.53
  ;  if nfield eq 2 then thres = 2.30
  ;
  ;=================================================================
  ;
  ; re-compute number of days
  ;
  ;========================================================================
  index_2=bytarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  index_2(*,*)=0b
  out_2=index_2
  one=index_2
  one(*,*)=1b

  idx_check = 964418
  ;print, data_in(*).jrc_flag(idx_check)
  ;print, data_in(*).fapar(idx_check)

  for t=0, daysNumber-1 do begin
    buf=reform(distance[t,*,*])
    ;print, t, buf(idx_check)
    ;stop
    ; MM & NG 22/09/2016
    ; check for NaN
    ; MM: 6 feb version (without NaN check...)
    ;idx_valid = where(buf[validIdx] le thres and buf[validIdx] ge 0.0, validCount); and distance(t,*,*) lt 50.0)
    ; MM: 7 feb version (with NaN check...)
    validIdx=where(finite(buf), ValidCount)
    ;stop
    prevExcept=0
    !EXCEPT=0
    idx_valid = where(buf(validIdx) le thres and dayveg(validIdx) GE 3, validCount, complement=outidx); and distance(t,*,*) lt 50.0)
    idx_bad = where(buf(validIdx) gt thres and dayveg(validIdx) GE 3, invalidCount)
    a=check_math()
    !EXCEPT=prevExcept
    ;print, t, validCount, invalidCount
    if validCount gt 0 then index_2(validIdx(idx_valid))=index_2(validIdx(idx_valid))+one(validIdx(idx_valid))
    if invalidCount gt 0 then out_2(validIdx(idx_bad))=out_2(validIdx(idx_bad))+one(validIdx(idx_bad))
    if invalidCount gt 0 then data_in(t).jrc_flag(validIdx(idx_bad))=11.0
    ; print, t, index_2(0,2430), out_2(0,2430), dayveg(0,2430), buf(0,2430)

    ;stop
    ; check our distance vs StdDev
    ;idx_valid = where(buf le std_mean.temp and std_mean.temp gt 0, validCount); and distance(t,*,*) lt 50.0)
    ; mark and remove outliers using flag value 1 (invalid)
    ; MM: 6 feb version (without NaN check...)
    ; if nfield eq 2 then idx_bad_mask = where(buf gt thres and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5), outliersCount)
    ; if nfield eq 3 then idx_bad_mask = where(buf gt thres and data_in(t).jrc_flag eq 0, outliersCount)
    ; MM: 7 feb version (with NaN check...)
    ;novalidIdx=where(finite(buf) ne 1, NaNCount)
    ;if nfield eq 2 then begin
    ;
    ;
    ;endif
    ; if nfield eq 3 then begin
    ; idx_bad_mask = where(buf gt thres and buf ne 100.0 and data_in(t).jrc_flag eq 0 and dayveg(*,*) GE 3, outliersCount)
    ;  if outliersCount gt 0 then idx_bad_mask=idx_bad_mask
    ;  print, min(buf[idx_bad_mask]), max(buf[idx_bad_mask]), data_in(t).sigma(idx_bad_mask(0)), dayVeg(idx_bad_mask(0))
    ;  stop
    ; endif
    ;    window, 1, xsize= 220, ysize =360, title='in'
    ;    tvscl, congrid(index_2, 36,360)
    ;    window, 2, xsize= 220, ysize =360, title='out'
    ;    tvscl, congrid(out_2, 36,360)

    ;if NaNCount gt 0 then data_in(t).jrc_flag[novalidIdx]=11
    ;if outliersCount gt 0 then begin
    ;  data_in(t).jrc_flag(idx_bad_mask)=1
    ;endif
    ;stop
    ;  print, NaNCount
    ;  print, distance(t,0,2430), thres, data_in(t).jrc_flag(0,2430)
  endfor
  ;  print, min(index_2), max(index_2)
  ;  print, min(out_2), max(out_2)
  ;  stop
  ;
  idx_check = 964418

  ;print, dayVeg(idx_check), index_2(idx_check), data_in(*).fapar(idx_check)
  ;for t=0, 9 do print, distance[t,idx_check]
  ; stop
  ; print, NaNCount
  ;stop
  if ~keyword_set(SKIP_OUTLIERS) then begin
    if nfield eq 3 then idx_remake=where(index_2 ge 3, complement=saveIndexes)
    if nfield eq 2 then idx_remake=where(index_2 ge 3)
    ; Use DelIdlVar to save memory
    DelidlVar, buf
    DelidlVar, one
    DelidlVar, idx_bad_mask
    if idx_remake(0) ge 0 then begin
      print, 'Remake after Outliers out'
      ;  help, idx_remake
      ;   stop
      sm_FindEuclideanMatricDistance_last_unc, daysNumber, data_in, idx_remake, distanceRes, meandatRes, std_meanRes, nfield, splitDims
      ; stop
      distance[*,idx_remake]=distanceRes[*,idx_remake]
      DelidlVar, distanceRes
      meandat.red(idx_remake)=meandatRes.red(idx_remake)
      meandat.nir(idx_remake)=meandatRes.nir(idx_remake)
      meandat.fapar(idx_remake)=meandatRes.fapar(idx_remake)
      DelidlVar, meandatRes
      std_mean.red(idx_remake)=std_meanRes.red(idx_remake)
      std_mean.nir(idx_remake)=std_meanRes.nir(idx_remake)
      std_mean.temp(idx_remake)=std_meanRes.temp(idx_remake)
      DelidlVar, std_meanRes
    endif
  endif
  ; look for day of minimum distance
  day=bytarr(splitDims[0],splitDims[1])
  day(*,*)=255
  day1=day
  min_val=fltarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  ;
  ; take the first as minimum value
  ;
  min_val(*,*)=10000.0
  min_val1=min_val
  ; MM: 20161028: check use of idx_remake
  sigmaSelected=reform(distance[0,*,*])*0
  bestDistance=sigmaSelected+8
  ;  testData=reform(data_in(0).fapar)
  ;  testData1=reform(data_in(0).sigma)
  ;  aa=where(finite(testData) and testData gt 0 and testData1 ne 1)
  ;  idx=array_indices(testData, aa)
  ;  print,  distance[*,idx[1,250],idx[1,250]]
  ;  print,  data_in(*).sigma(idx[0,250],idx[1,250])
  ;  print,  data_in(*).fapar(idx[0,250],idx[1,250])
  for t=daysNumber-1, 0, -1 do begin
    ; MM 20161028: subscribe distance with idx_remake???
    buf=reform((distance[t,*,*]))
    sigmas=reform(data_in(t).sigma)
    ;validIdx=where(finite(buf), ValidCount)

    ;idx_mask = where(finite(buf) and buf(validIdx) lt min_val(validIdx) and index_2(validIdx) GE 3, count)
    idx_mask = where(finite(buf) and buf lt min_val and index_2 GE 3, count)
    prevExcept=!EXCEPT
    !EXCEPT=0
    idx_mask1 = where(finite(buf) and buf lt min_val and index_2 GE 3 and sigmas ne 1, count1)
    a=check_math()
    !EXCEPT=prevExcept
    if count1 gt 0 then begin
      min_val1(idx_mask1)=buf(idx_mask1)
      day1(idx_mask)=data_in(t).day
    endif
    if count gt 0 then begin
      ;min_val(validIdx(idx_mask))=buf(validIdx(idx_mask))
      ;day(validIdx(idx_mask))=data_in(t).day
      min_val(idx_mask)=buf(idx_mask)
      day(idx_mask)=data_in(t).day
      sigmaSelected(idx_mask)=data_in(t).sigma(idx_mask)
      highSigma=where(sigmaSelected eq 1, cc)
      meaningSigma=where(sigmaSelected ne 1 and sigmaSelected gt 0, cc1)
      tempTV=sigmaSelected*0
      tempTV[highSigma]=1
      bestDistance[idx_mask]=buf[idx_mask]
;      print, 'Selected (sigma 1)#', cc
;      print, 'Selected (sigma ne 1)#', cc1
;      print, 'min distance (with sigma 1)', min(min_val[where(min_val lt 8)], /NAN)
;      print, 'max distance (with sigma 1)', max(min_val[where(min_val lt 8)], /NAN)
;      print, 'min distance (without sigma 1)', min(min_val1[where(min_val1 lt 8)], /NAN)
;      print, 'max distance (without sigma 1)', max(min_val1[where(min_val1 lt 8)], /NAN)
      ;tvscl, congrid(sigmaSelected, 72, 360), /NAN
      ;window, 1, xsize=150, ysize=400, xpos=0, ypos=0
      ;tvscl, congrid(tempTV, 72, 360), /NAN
      ;tvscl, congrid(tempTV, 72, 360), /NAN
      ;pic2=BYTSCL(congrid(bestDistance, 72, 360), /NAN, max=8, min=0)
      ;window, 2, xsize=150, ysize=400, xpos=500, ypos=0
      ;tv, pic2, /NAN
    endif
  endfor
  highsigmadist=where(data_in(t).sigma eq 1)
  ;stop
  ;print, data_in(t).sigma
  ;newFlagCube=data_in(*).jrc_flag
  ;stop
  print,'find minimum distance day with sigma new...'
  idx_check = 964418
  ;print, dayVeg(idx_check), index_2(idx_check), data_in(*).fapar(idx_check)
  ;for t=0, 9 do print, distance[t,idx_check]
  ;stop
  ;data_in(t).jrc_flag(idx_bad_mask)=0
  ;
END
