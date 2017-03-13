Pro sm_make_tc_distance_eu_vegetation_m_w_unc, daysNumber, data_in, idx_doIt, day, meandat, std_mean, nfield, index_2, splitDims, outDist, UNC=UNC, BIG_THRES=BIG_THRES
  ;
  ; input
  ; daysNumber : total number of days (files)
  ; data_in : data for each days
  ; idx_doIt : position where index ge 3
  ; nfield : 3 over vegetation pixels (consider fapar value) / 2 over bare soil pixels (ignore fapar value)
  ;
  ; output:
  ; day : the day to be used in the time composite
  ; index_2 : valid observations after outliers
  ; meandat : temporal deviation over the period (only valid pixels after outliers)
  ;
  ; compute the distance
  ;
  ;
  ;
  ;aa=where(data_in[0].FLAG eq 4 or data_in[0].FLAG eq 5, cc)
  ;help, idx_doIT
  ;stop
  print, 'with sigma'
  sm_FindEuclideanMatricDistance_unc, daysNumber, data_in, idx_doIt, distance, meandat, std_mean, nfield, splitDims
  ;aa=where(data_in[0].FLAG eq 4 or data_in[0].FLAG eq 5, cc)
  ;
  ;
  ;
  ;window,11, xsize=720*2, ysize=360*2, title='Mean fapar after'
  ;faparcolor
  ;img=bytarr(7200,3600)
  ;img(*,*)=meandat(2,*,*)*250.0
  ;tvscl, reverse(congrid(img,720*2,360*2),2)
  ;
  ;
  ; remove outliers ; check the numbers
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
  ;tt=size(data_in.day)
  tt=[0, daysNumber]
  ;try to avoid indexing/masking...
  ;tt=[0,n_elements(data_in)]
  for t=0, tt(1)-1 do begin
    ;buf(*,*)=-1.0
    ;buf(idx_doIt)=distance(t,*,*)
    buf=reform(distance[t,*,*])
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
      idx_bad_mask=idx_valid[idx_bad_mask]
    endif
    if nfield eq 3 then begin
      idx_bad_mask = where(buf[idx_valid] gt thres and data_in(t).jrc_flag[idx_valid] eq 0, outliersCount)
      idx_bad_mask=idx_valid[idx_bad_mask]
    endif
    if NaNCount gt 1 then data_in(t).jrc_flag[nanIdx]=1
    if outliersCount gt 1 then data_in(t).jrc_flag(idx_bad_mask)=1
  endfor
  ;
  ;stop
  if nfield eq 3 then idx_remake=where(data_in.jrc_flag eq 0 and index_2 ge 3, complement=saveIndexes)
  if nfield eq 2 then idx_remake=where((data_in.jrc_flag eq 4 or data_in.jrc_flag eq 5) and index_2 ge 3)
  ;
  DelidlVar, buf
  DelidlVar, one
  DelidlVar, idx_bad_mask
  if idx_remake(0) ge 0 then begin
    print, 'Remake with sigma after Outliers out'
    sm_FindEuclideanMatricDistance_unc, daysNumber, data_in, idx_remake, distanceRes, meandatRes, std_meanRes, nfield, splitDims
    ;stop
    distance(*,idx_remake)=distanceRes(*,idx_remake)
    DelidlVar, distanceRes
    meandat.red(idx_remake)=meandatRes.red(idx_remake)
    meandat.nir(idx_remake)=meandatRes.nir(idx_remake)
    meandat.fapar(idx_remake)=meandatRes.fapar(idx_remake)
    DelidlVar, meandatRes
    ;help, std_mean, std_meanRes
    std_mean.red(idx_remake)=std_meanRes.red(idx_remake)
    std_mean.nir(idx_remake)=std_meanRes.nir(idx_remake)
    std_mean.temp(idx_remake)=std_meanRes.temp(idx_remake)


    DelidlVar, std_meanRes

    ;meandat=temporary(meandatRes)
    ;std_mean=temporary(std_meanRes)
    ;save, distance, filename='new_distance_.sav', /compress
    ;save, meandat, filename='new_meandat_.sav', /compress
    ;save, std_mean, filename='new_std_mean_.sav', /compress
    ;    restore, 'distance.sav'
    ;    resDist=distanceRes-distance
    ;    restore, 'meandat.sav'
    ;    resMean=meandatRes-meandat
    ;    restore, 'std_mean.sav'
    ;    resStdDev=std_meanRes-std_mean
    ;distance(*,saveIndexes)=saveDistance
    ;meandat(*,saveIndexes)=saveMean
    ;std_mean(*,saveIndexes)=savemean_std_mean
    ;    help, distance_2
    ;    stop
    ;distance(*,idx_remake)=distance_2
    ;distance(*,idx_remake)=temporary(distance_2(*,idx_remake)); MM 20161028: subscribe???
    ;meandat(*,idx_remake)=temporary(meandat2(*,idx_remake))
    ;std_mean(*,idx_remake)=std_mean2(*,idx_remake)
    ; MM 22/9/2016
    ;std_mean.red[idx_remake]=temporary(std_mean2.red[idx_remake])
    ;std_mean.nir[idx_remake]=temporary(std_mean2.nir[idx_remake])
    ;std_mean.temp[idx_remake]=temporary(std_mean2.temp[idx_remake])
  endif
  ;
  ;
  ;faparcolor
  ;window,12, xsize=72*2, ysize=360*2, title='mean fapar after outliers out'
  ;tvscl, congrid(img,72,360)
  ;img=bytarr(splitDims[0],splitDims[1])
  ;if nfield eq 3 then img(*,*)=meandat(2,*,*)*250.0
  ;if nfield eq 2 then img(*,*)=meandat(0,*,*)*250.0
  ;tvscl, reverse(congrid(img,720*2,360*2),2)
  ;tvscl, congrid(img,720*2,360*2)
  ;
  ; look for day of minimum distance
  ;
  day=bytarr(splitDims[0],splitDims[1])
  day(*,*)=255
  min_val=fltarr(splitDims[0],splitDims[1])
  buf=fltarr(splitDims[0],splitDims[1])
  ;
  ; take the first as minimum value
  ;
  min_val(*,*)=10000.0
  ;
  ;
  ; MM: 20161028: check use of idx_remake
  for t=tt(1)-1, 0, -1 do begin
    buf(*,*) = 11000.0

    ;buf(idx_remake)=distance(t,*)
    ; MM 20161028: subscribe distance with idx_remake???
    ;buf(idx_remake)=distance(t,idx_remake)
    buf=reform(distance[t,*,*])

    idx_mask = where(buf(*,*) lt min_val(*,*) and buf(*,*) lt 100, count)

    if count gt 0 then begin
      min_val(idx_mask)=buf(idx_mask)
      day(idx_mask)=data_in(t).day
      ;aa=where(day ne 255, count)
      ;print, count
      ;tvscl, congrid(day, 72, 360)
      ;tempD=where(day eq 255, complement=rev)
      ;dd=day
      ;dd[tempD]=1
      ;dd[rev]=100
      ;tvscl, congrid(dd, 72, 360)
      ;print, t, count

      ;print, idx_mask(0), min_val(idx_mask(0)), t, day(idx_mask(0))

    endif
  endfor
  print,'find minimum distance day ...'

  ;
END