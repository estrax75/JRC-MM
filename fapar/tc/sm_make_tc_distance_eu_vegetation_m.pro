Pro sm_make_tc_distance_eu_vegetation_m, daysNumber, data_in, idx_doIt, day, meandat, std_mean, nfield, index_2, splitDims
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
  print, 'no sigma'
  dims=[7200,3600]
  sm_FindEuclideanMatricDistance, daysNumber, data_in, idx_doIt, distance, meandat, std_mean, nfield, splitDims
  ;
  ;
  ; remove outliers
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
  one=index_2
  one(*,*)=1b
  tt=[0, daysNumber]
  ;try to avoid indexing/masking...
  if ~keyword_set(SKIP_OUTLIERS) then begin
    for t=0, tt(1)-1 do begin
      buf=reform(distance[t,*,*])
      ;stop
      ; MM & NG 22/09/2016
      ; check for NaN
      ; MM: 6 feb version (without NaN check...)
      ;idx_valid = where(buf[validIdx] le thres and buf[validIdx] ge 0.0, validCount); and distance(t,*,*) lt 50.0)
      ; MM: 7 feb version (with NaN check...)
      validIdx=where(finite(buf), ValidCount)
      idx_valid = where(buf[validIdx] le thres and buf[validIdx] ge 0.0, validCount); and distance(t,*,*) lt 50.0)
      idx_valid=validIdx[idx_valid]
      if validCount gt 0 then index_2(idx_valid)=index_2(idx_valid)+one(idx_valid)
      ; check our distance vs StdDev
      ;idx_valid = where(buf le std_mean.temp and std_mean.temp gt 0, validCount); and distance(t,*,*) lt 50.0)
      ; mark and remove outliers using flag value 1 (invalid)
      ; MM: 6 feb version (without NaN check...)
      ; if nfield eq 2 then idx_bad_mask = where(buf gt thres and (data_in(t).jrc_flag eq 4 or data_in(t).jrc_flag eq 5), outliersCount)
      ; if nfield eq 3 then idx_bad_mask = where(buf gt thres and data_in(t).jrc_flag eq 0, outliersCount)
      ; MM: 7 feb version (with NaN check...)
      validIdx=where(finite(buf) ne 1, NaNCount)
      if nfield eq 2 then begin
        idx_bad_mask = where(buf[idx_valid] gt thres and (data_in(t).jrc_flag[idx_valid] eq 4 or data_in(t).jrc_flag[idx_valid] eq 5), outliersCount)
        if outliersCount gt 0 then idx_bad_mask=idx_valid[idx_bad_mask]
      endif
      if nfield eq 3 then begin
        idx_bad_mask = where(buf[idx_valid] gt thres and data_in(t).jrc_flag[idx_valid] eq 0, outliersCount)
        if outliersCount gt 0 then idx_bad_mask=idx_valid[idx_bad_mask]
      endif
      if NaNCount gt 1 then data_in(t).jrc_flag[nanIdx]=1
      if outliersCount gt 1 then data_in(t).jrc_flag(idx_bad_mask)=1
    endfor
    ;
    ;stop
    if nfield eq 3 then idx_remake=where(data_in.jrc_flag eq 0 and index_2 ge 3, complement=saveIndexes)
    if nfield eq 2 then idx_remake=where((data_in.jrc_flag eq 4 or data_in.jrc_flag eq 5) and index_2 ge 3)
    ; Use DelIdlVar to save memory
    DelidlVar, buf
    DelidlVar, one
    DelidlVar, idx_bad_mask
    if idx_remake(0) ge 0 then begin
      print, 'Remake after Outliers out'
      sm_FindEuclideanMatricDistance, daysNumber, data_in, idx_remake, distanceRes, meandatRes, std_meanRes, nfield, splitDims
      distance(*,idx_remake)=distanceRes(*,idx_remake)
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
  min_val=fltarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  ;
  ; take the first as minimum value
  ;
  min_val(*,*)=10000.0
  ; MM: 20161028: check use of idx_remake
  for t=tt(1)-1, 0, -1 do begin
    buf(*,*) = 11000.0
    ; MM 20161028: subscribe distance with idx_remake???
    buf=reform((distance[t,*,*]))
    idx_mask = where(buf(*,*) lt min_val(*,*) and buf(*,*) lt 100, count)
    if count gt 0 then begin
      min_val(idx_mask)=buf(idx_mask)
      day(idx_mask)=data_in(t).day
    endif
  endfor
  print,'find minimum distance day ...'

  ;
END
