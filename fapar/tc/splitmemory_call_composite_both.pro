;
;ldtr == cloud (remove data)
PRO sm_call_composite_both, daysNumber, data_day_f, data_tc, nSlice, dataDim, $
  CLOUDTYPE=CLOUDTYPE, APPLY_HIGH_SIGMA=APPLY_HIGH_SIGMA, REMOVE_CLOUD=REMOVE_CLOUD, SIGMA_WEIGHTED=SIGMA_WEIGHTED, $
  PIXELS_PROCESS=PIXELS_PROCESS, CSV=CSV, NC=NC

  ;
  ;
  ;
  ; data_day = products for each day and each pixels
  ;
  ; data_tc = time-composite results
  ;
  ; test pixel position

  INT_NAN=2^15
  DATA_RANGE=[0., 1.]
  DATA_NAN=!VALUES.F_NAN

  ; only with a valid filename there is a valid day...
  validIdx=where(data_day_f.fname ne '', count)
  data_day_t=data_day_f[validIdx]
  daysNumber=count
  print, 'In the both time composite program ...'
  print, 'daysNumber', daysnumber
  ;
  dims=dataDim
  prevflag=bytarr(dims[0],dims[1])

  data_tc= getDataTCStruct1(dims[0],dims[1], /INIT)

  ;==========================================================================================
  ;
  ; look for vegetated pixels
  ;
  ; count the number of dates where we have valid pixels over vegetation land
  xSplitDim=dims[0]/nSlice
  ySplitDim=dims[1]; full dim,

  ; for a faster test set a specific slice here...
  startSlice=0;2
  endSlice=nSlice;2
  ; good test: vertical slice half of total slices shows "Center Europe and Africa"
  ;  startSlice=10;/20
  ;  endSlice=11;/20
  if nSlice gt 5 then begin
    print, '***********TEST**************'
    startSlice =5;/10
    endSlice=6;/10
  endif

  print, '******************'
  print, 'tiles sequence: from', startSlice+1,'  to:', endSlice
  print, '******************'
  for slice=startSlice, endSlice-1 do begin ;nSlice-1 do begin
    subXStart=slice*xSplitDim & subXEnd=(slice+1)*xSplitDim-1
    subYStart=0 & subYEnd=dims[1]-1

    ; only for test
    data_day_split=getDataDayStruct(xSplitDim, ySplitDim)
    data_tc_split=getDataTCStruct(xSplitDim, ySplitDim, /INIT)

    data_day_split=replicate(data_day_split, daysNumber)

    print, 'reading day from: ', 0, 'to: ', daysNumber
    print, '...'
    resFlags=0
    for t=0, daysNumber-1 do begin   ; ng ++

      print, 'reading day...', t+1, '/', daysNumber
      ;      if data_day_t[t].fid gt 0 then faparData=read_AVHRR_FAPAR(data_day_t[t].fDir, data_day_t[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_t[t].fid, /FULL) $
      ;      else faparData=read_AVHRR_FAPAR(data_day_t[t].fDir, data_day_t[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=fid, /FULL)

      FOUND=0
      faparData=0
      if ~keyword_set(PIXELS_PROCESS) then begin
        if data_day_t[t].fid gt 0 then faparData=read_AVHRR(data_day_t[t].fDir, data_day_t[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_t[t].fid, /FULL) $
        else faparData=read_AVHRR(data_day_t[t].fDir, data_day_t[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=fid, /FULL)
      endif else begin
        faparData=read_AVHRR(data_day_t[t].fDir, data_day_t[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_t[t].fid, $
          PIXELS_PROCESS=PIXELS_PROCESS, CSV=CSV, NC=NC, /FULL)
      endelse
      print, data_day_t[t].fDir, data_day_t[t].fName
      print, 'faparData.fapar, faparData.red, faparData.nir, faparData.flag' 
      print, faparData.fapar[0], faparData.red[0], faparData.nir[0], faparData.flag[0]
      print, 'done'
      data_day_split[t].valid=0
      if keyword_set(FOUND) then begin

        data_day_split[t].sigma=faparData.sigma
        data_day_split[t].red=faparData.red
        data_day_split[t].sigma_red=faparData.sigma_red
        data_day_split[t].nir=faparData.nir
        data_day_split[t].sigma_nir=faparData.sigma_nir
        data_day_split[t].ltdr_flag=faparData.qa
        data_day_split[t].ts=fapardata.ts
        data_day_split[t].tv=faparData.tv
        data_day_split[t].phi=faparData.phi
        ;

        ;
        ; remove clouds from data...
        ; original data doesn't apply cloud detection from LTDR
        ; for Time Composition try to tune LTDR and sigma weight factor
        countCloud=0
        if cloudtype le 2 then begin
          ; 1 means totally cloud
          checkCloud1=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,1)
          ; 2 means partially cloud
          checkCloud2=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,2)
          if cloudtype eq 0 then cloudNaN=where(checkCloud1 eq 1 or checkCloud2 eq 1, countCloud)
          if cloudtype eq 1 then cloudNaN=where(checkCloud1 eq 1, countCloud)
          if cloudtype eq 2 then cloudNaN=where(checkCloud2 eq 1, countCloud)
        endif

        if countCloud gt 0 then begin
          ; ng + mm
          ;if cnt gt 0 then faparData.flag[cloudNaN[changeIdxs]]=2 ; force cloud code for fapar and soil
          ;leave original values, but flag cloud AND/OR change sigma values
          changeIdxs=where(faparData.flag[cloudNaN] eq 4 or faparData.flag[cloudNaN] eq 5 or faparData.flag[cloudNaN] eq 6 or faparData.flag[cloudNaN] eq 0, cnt)
          if cnt gt 0 then begin
            if Keyword_set(APPLY_HIGH_SIGMA) eq 1 then begin
              ;stop
              faparData.sigma[cloudNaN[changeIdxs]]=1.0 ; faparData.sigma[cloudNaN[changeIdxs]]*5.0
              faparData.sigma_red[cloudNaN[changeIdxs]]=1.0 ; faparData.sigma_red[cloudNaN[changeIdxs]]*5.0
              faparData.sigma_nir[cloudNaN[changeIdxs]]=1.0 ; faparData.sigma_nir[cloudNaN[changeIdxs]]*5.0
              ;stop
            endif
            if keyword_set(REMOVE_CLOUD) then faparData.flag[cloudNaN[changeIdxs]]=2
            ;if keyword_set(sigma_correction) eq 1 then faparData.flag[cloudNaN[changeIdxs]]=2 ; force cloud code for fapar and soil
            ; test: discard always the clouds
            ;faparData.flag[cloudNaN[changeIdxs]]=2
          endif
        endif
        ;; Set fixed sigma to compare SWF "weighted-by-sigma" algorithm with the "basic" one
        ;data_day_split[t].sigma=0.00001
        ;data_day_split[t].sigma_red=0.00001
        ;data_day_split[t].sigma_nir=0.00001
        ;;
        data_day_split[t].fapar=faparData.fapar
        data_day_split[t].red=faparData.red
        data_day_split[t].nir=faparData.nir
        data_day_split[t].sigma=faparData.sigma
        data_day_split[t].sigma_red=faparData.sigma_red
        data_day_split[t].sigma_nir=faparData.sigma_nir
        data_day_split[t].toc_red=faparData.toc_red
        data_day_split[t].toc_nir=faparData.toc_nir

        flagMatrix=fapardata.flag
        data_day_split[t].jrc_flag=flagMatrix
        resFlags=[resFlags,flagMatrix[UNIQ(flagMatrix, SORT(flagMatrix))]]
        flagMatrix=0
        checkCloud1=-1 & checkCloud2=-1
        ;.day sets to position of file in the (sub)sequence ([1..8/9/10/11] for decade [1..28/29/30/31] for monthly), use doy???
        data_day_split[t].day=t
        ;    idxsoil=where(data_day_split[t].jrc_flag(*,*) eq 4.0)
        ;    print, data_day_split[t].sigma(idxsoil(0:5))
        ;       stop
        ; .valid eq 1 means a good file
        data_day_split[t].valid=1
      endif else begin
        print, '**********missing source!!!!***********'
      endelse
    endfor
    ;stop
    ;
    print, 'data_day_split.fapar, data_day_split.red, data_day_split.nir, data_day_split.jrc_flag'
    print, data_day_split.fapar[0], data_day_split.red[0], data_day_split.nir[0], data_day_split.jrc_flag[0]

    resFlags=resFlags[UNIQ(resFlags, SORT(resFlags))]
    print, 'input flags list:', resFlags
    vIdxs=where(data_day_split.valid eq 1, dNumber)
    daysNumber=dNumber
    data_day_split=data_day_split[vIdxs]

    dayVeg=bytarr(xSplitDim,ySplitDim) & dayVeg[*,*]=0
    one=dayVeg & one[*,*]=1

    for t=0, daysNumber-1 do begin
      ; cut off Nan (soil & vegetation)
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)

      ;create Soil mask
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] eq 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      validMaskSoil=validMask*validMaskSoil

      ;create Veg mask
      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      validMaskVeg=validMask*validMaskVeg

      ;final Veg Mask
      prevExcept=!EXcept
      !EXcept=0
      idx_maskVeg = where(validMaskVeg eq 1 and data_day_split(t).jrc_flag(*,*) eq 0 and data_day_split(t).fapar(*,*) gt 0.0, countVeg)
      a=check_math()
      !EXcept=prevExcept
      ;final Soil Mask
      idx_maskSoil = where(validMaskSoil eq 1 and (data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5), countSoil)
      if countVeg gt 0 then dayVeg[idx_maskVeg]=dayVeg[idx_maskVeg]+one[idx_maskVeg]
    endfor
    idx_third = where(dayVeg ge 3, complement=flagNan)

    if keyword_set(SIGMA_WEIGHTED) then begin
      ;
      sm_make_tc_distance_eu_vegetation_m_unc_last, dayVeg, daysNumber, data_day_split, idx_third, day, meandat, std_mean, 3, index_2, [xSplitDim, ySplitDim];, SKIP_OUTLIERS=SKIP_OUTLIERS
    endif else begin
      sm_make_tc_distance_eu_vegetation_m, daysNumber, data_day_split, idx_third, day, meandat, std_mean, 3, index_2, [xSplitDim, ySplitDim];, SKIP_OUTLIERS=SKIP_OUTLIERS
    endelse
    ;index_2 is new vegetation indexes (number of days)

    newDayVeg=index_2
    for j=0, daysNumber-1 do begin
      idx = where(data_day_split(j).jrc_flag eq 11)
      data_day_split(j).jrc_flag(idx) = 0
    endfor
    ;print, data_day_split(*).jrc_flag(0, 2430)
    ;stop

    ;plot, index_2, dayVeg, psym =1
    ;stop
    idx=where(index_2 eq 0, countOldVeg)
    if countOldVeg gt 1 then newDayVeg(idx)=dayVeg(idx) 
    
    ;print, newDayVeg(idx(0))
    ;stop
    ; remember that data_day_split(*).jrc_flag doesn't change outside "sm_make_tc_distance_eu_vegetation_m_unc_last".
    for t =0, daysNumber-1  do begin      ; ng ++
      ; MM & NG 22/09/2016
      idx_t=where(day eq t and newDayVeg ge 3, countThreeDays)
      if idx_t(0) ge 0 then begin
        print, 'countThreeDay (vegetation) for day: ', t, countThreeDays
        ;print, data_day_split(t).jrc_flag(idx_t(0))
        ;print, data_day_split(*).jrc_flag(idx_t(0))
        ;print, data_day_split(*).fapar(idx_t(0)), data_day_split(*).sigma(idx_t(0))
        ;print, newDayVeg(idx_t(0)), dayVeg(idx_t(0))
        ;
        ;stop
        data_tc_split.nday[idx_t]=newDayVeg[idx_t]
        data_tc_split.red(idx_t)= data_day_split(t).red(idx_t)
        data_tc_split.nir(idx_t)= data_day_split(t).nir(idx_t)
        data_tc_split.fapar(idx_t)= data_day_split(t).fapar(idx_t)
        ;
        data_tc_split.sigma_red(idx_t)= data_day_split(t).sigma_red(idx_t)
        data_tc_split.sigma_nir(idx_t)= data_day_split(t).sigma_nir(idx_t)
        data_tc_split.sigma(idx_t)= data_day_split(t).sigma(idx_t)
        data_tc_split.jrc_flag(idx_t)= data_day_split(t).jrc_flag(idx_t)
        data_tc_split.ltdr_flag(idx_t)= data_day_split(t).ltdr_flag(idx_t)
        data_tc_split.day(idx_t) = day(idx_t)
        ; MM & NG 22/9/2016
        data_tc_split.toc_red(idx_t)= data_day_split(t).toc_red(idx_t)
        data_tc_split.toc_nir(idx_t) = data_day_split(t).toc_nir(idx_t)
      endif
    endfor
    data_tc_split.dev_red_temp = std_mean.red
    data_tc_split.dev_nir_temp = std_mean.nir
    data_tc_split.dev_temp = std_mean.temp

    ;stop
    ; tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ; If only One date
    ; associated values for the only dates
    for t=0, daysNumber-1 do begin     ;   ng ++

      idx_time = where((data_day_split(t).jrc_flag eq 0) and (newDayVeg eq 1), countSingleDay)

      print, 'singleDay for day: ', t, countSingleDay
      if countSingleDay gt 0 then begin
        ;print, 'countOneDay (vegetation) for one day: ', t, countsingleDay
        ;print, data_day_split(t).jrc_flag(idx_time(0))
        ;print, data_day_split(*).jrc_flag(idx_time(0))
        ;print, newDayVeg(idx_time(0)), dayVeg(idx_time(0))
        ;print, data_day_split(t).fapar(idx_time(0))
        ;stop
        data_tc_split.nday(idx_time)=1
        data_tc_split.day(idx_time)=data_day_split(t).day
        ;
        data_tc_split.red(idx_time)=data_day_split(t).red(idx_time)
        data_tc_split.nir(idx_time)=data_day_split(t).nir(idx_time)
        data_tc_split.fapar(idx_time)=data_day_split(t).fapar(idx_time)
        data_tc_split.toc_red(idx_time)= data_day_split(t).toc_red(idx_time)
        data_tc_split.toc_nir(idx_time)= data_day_split(t).toc_nir(idx_time)
        ;
        data_tc_split.jrc_flag(idx_time)=data_day_split(t).jrc_flag(idx_time)
        data_tc_split.ltdr_flag(idx_time)=data_day_split(t).ltdr_flag(idx_time)
        ;
        data_tc_split.sigma_red(idx_time)= data_day_split(t).sigma_red(idx_time)
        data_tc_split.sigma_nir(idx_time)= data_day_split(t).sigma_nir(idx_time)
        data_tc_split.sigma(idx_time)= data_day_split(t).sigma(idx_time)
        ;
        data_tc_split.dev_red_temp(idx_time)=0.
        data_tc_split.dev_nir_temp(idx_time)=0.
        data_tc_split.dev_temp(idx_time)=0.
      endif
    endfor
    ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ; If only two dates
    ; associated values for the only dates
    ;
    idx_two = where(newDayVeg eq 2, countThreeDays)
    fapar_two=fltarr(xSplitDim,dims[1])
    for t=0, daysNumber-1  do begin      ; ng ++
      print, 'countThreeDay (vegetation) for two days: ', t, countThreeDays
      ;print, data_day_split(t).jrc_flag(idx_t(0))
      ;print, data_day_split(*).jrc_flag(idx_t(0))
      ;print, newDayVeg(idx_t(0))

      buf=data_day_split(t).jrc_flag
      buf1=data_day_split(t).fapar

      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      ; check where fapar(t) is not Nan AND jrc_flag = 0
      idxMaskVeg=where(buf[goodIndexes] eq 0 and (buf1[goodIndexes] gt 0.0))
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      ; Only two days AND previous condition
      idx_time = where(validMaskVeg eq 1 and newDayVeg eq 2 and buf eq 0, coundTwoDays)

      print, 'DoubleDay for day: ', t, coundTwoDays
      if coundTwoDays gt 0 then begin
        idx_lp= where(buf1(idx_time) gt fapar_two(idx_time))
        if idx_lp(0) ge 0 then begin
          fapar_two(idx_time(idx_lp))=buf1(idx_time(idx_lp))
          data_tc_split.nday(idx_time(idx_lp))=2
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
          ;
          data_tc_split.fapar(idx_time(idx_lp)) = fapar_two(idx_time(idx_lp))
          data_tc_split.red(idx_time(idx_lp))=data_day_split(t).red(idx_time(idx_lp))
          data_tc_split.nir(idx_time(idx_lp))=data_day_split(t).nir(idx_time(idx_lp))
          data_tc_split.toc_red(idx_time(idx_lp))= data_day_split(t).toc_red(idx_time(idx_lp))
          data_tc_split.toc_nir(idx_time(idx_lp))= data_day_split(t).toc_nir(idx_time(idx_lp))
          ;
          data_tc_split.sigma(idx_time(idx_lp))= data_day_split(t).sigma(idx_time(idx_lp))
          data_tc_split.sigma_red(idx_time(idx_lp))= data_day_split(t).sigma_red(idx_time(idx_lp))
          data_tc_split.sigma_nir(idx_time(idx_lp))= data_day_split(t).sigma_nir(idx_time(idx_lp))
          ;
          data_tc_split.jrc_flag(idx_time(idx_lp))= data_day_split(t).jrc_flag(idx_time(idx_lp))
          data_tc_split.ltdr_flag(idx_time(idx_lp))= data_day_split(t).ltdr_flag(idx_time(idx_lp))
        endif
      endif
    endfor
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    for t=0, daysNumber-1  do begin        ; NG ++
      ; MM & NG 22/9/2016
      ; check where fapar(t) is not Nan AND jrc_flag = 0
      buf=data_day_split(t).jrc_flag
      buf1=data_day_split(t).fapar

      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      ; check where fapar(t) is not Nan AND jrc_flag = 0
      idxMaskVeg=where(buf[goodIndexes] eq 0 and (buf1[goodIndexes] gt 0.0))
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      ;final Veg Mask
      validMaskVeg=validMask*validMaskVeg
      ; Only two days AND previous condition
      idx_ok = where(validMaskVeg eq 1 and newDayVeg eq 2 and (data_tc_split.day ne t) and data_day_split(t).jrc_flag eq 0, countDay)

      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
      endif
    endfor
    ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;print,'Finish vegetation ....'
    ;==========================================================================================
    ; look for bare soil  pixels
    ;
    ; count the number of date where we have valid pixels over vegetation land
    indexBareSoil=bytarr(xSplitDim,dims[1])
    indexBareSoil(*,*)=0
    one=indexBareSoil
    one(*,*)=1
    for t=0, daysNumber-1  do begin        ;  ng ++
      ; Cut off NaN from original data
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] eq 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil
      ;final Soil Mask
      idx_masks = where(validMaskSoil eq 1 and (data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5), countSoil)
      if idx_masks(0) ge 0 then indexBareSoil(idx_masks)=indexBareSoil(idx_masks)+one(idx_masks)
    endfor
    ;==========================================================================================
    ; More than two dates
    ; associated values for the number of dates is bigger than 3
    ;if indexBareSoil[0] gt 0 and indexBareSoil[0] lt 3 then stop
    idx_thirds = where(indexBareSoil ge 3, complement=flagNan)
    print, '# soil pixels more than 3 times', N_elements(idx_thirds)
    ;stop
    ;==========================================================================================
    if keyword_set(SIGMA_WEIGHTED) then begin
      sm_make_tc_distance_eu_vegetation_m_unc_last, indexBareSoil, daysNumber, data_day_split, idx_thirds, days, meandats, std_means, 2, index_2s, [xSplitDim, ySplitDim]
    endif else begin
      sm_make_tc_distance_eu_vegetation_m, daysNumber, data_day_split, idx_thirds, days, meandats, std_means, 2, index_2s, [xSplitDim, ySplitDim]
    endelse
    
    soilFewDays=where(indexBareSoil lt 3, countSoilFD)
    if countSoilFD gt 0 then index_2s[soilFewDays]=indexBareSoil[soilFewDays] 
    newDaySoil=index_2s
    
    vgt=where(newDayVeg gt 0)
    ; force soil to 0 when at least 1 vgt was previously found
    newDaySoil[vgt]=0

    ;bareSday=data_tc_split.red
    ;bareSday[*]=0

    for t=0, daysNumber-1 do begin        ; ng ++
      idx_t=where((newDaySoil ge 3) and $
        ((data_day_split(t).jrc_flag eq 4) or (data_day_split(t).jrc_flag eq 5)) and $
        days eq t, countThreeDays)
      print, 'countThreeDay (bare soil) for day: ', t, countThreeDays
      ;print, data_day_split(t).jrc_flag(idx_t(0))
      ;print, data_day_split(*).jrc_flag(idx_t(0))
      ;print, index_2(idx_t(0))
      ;stop
      if countThreeDays ne 0 then begin
        data_tc_split.nday(idx_t)=newDaySoil[idx_t]
        data_tc_split.day(idx_t) = days(idx_t)
        ;
        data_tc_split.red(idx_t)= data_day_split(t).red(idx_t)
        data_tc_split.nir(idx_t)= data_day_split(t).nir(idx_t)
        data_tc_split.fapar(idx_t)= data_day_split(t).fapar(idx_t)
        data_tc_split.toc_red(idx_t)= data_day_split(t).toc_red(idx_t)
        data_tc_split.toc_nir(idx_t)= data_day_split(t).toc_nir(idx_t)
        ;
        data_tc_split.jrc_flag(idx_t)= data_day_split(t).jrc_flag(idx_t)
        data_tc_split.ltdr_flag(idx_t)= data_day_split(t).ltdr_flag(idx_t)
        ;
        data_tc_split.sigma_red(idx_t)= data_day_split(t).sigma_red(idx_t)
        data_tc_split.sigma_nir(idx_t)= data_day_split(t).sigma_nir(idx_t)
        data_tc_split.sigma(idx_t)= data_day_split(t).sigma(idx_t)
        ; MM & NG 22/9/2016
        data_tc_split.dev_red_temp(idx_t)= std_means.red[idx_t]
        data_tc_split.dev_nir_temp(idx_t)= std_means.nir[idx_t]
        data_tc_split.dev_temp(idx_t)= std_means.temp[idx_t]
        ;bareSday(idx_t)=1.
      endif
      ;tvscl, congrid(reform(bareSday), 72, 360)
    endfor
    ;stop
    ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;==========================================================================================
    ; If only One date
    ; associated values for the only dates
    ; MM & NG 23/9/2016
    idx_ones = where(newDaySoil eq 1)
    ones=0b*data_tc_split.day+1
    for t=0, daysNumber-1  do begin          ;  ng ++
      ; MM & NG 22/9/2016
      idx_time = where((newDaySoil eq 1) and $
        ((data_day_split(t).jrc_flag eq 4) or (data_day_split(t).jrc_flag eq 5)), countSingleDay)
      print, 'countSingleDay (bare soil) for day: ', t, countSingleDay
      ;print, data_day_split(t).jrc_flag(idx_ones(idx_time(0)))
      ;print, data_day_split(*).jrc_flag(idx_ones(idx_time(0)))
      ;stop
      if countSingleDay gt 0 then begin
        data_tc_split.nday(idx_ones(idx_time))=1
        data_tc_split.red(idx_ones(idx_time))=data_day_split(t).red(idx_ones(idx_time))
        data_tc_split.nir(idx_ones(idx_time))=data_day_split(t).nir(idx_ones(idx_time))
        data_tc_split.toc_red(idx_ones(idx_time))=data_day_split(t).toc_red(idx_ones(idx_time))
        data_tc_split.toc_nir(idx_ones(idx_time))=data_day_split(t).toc_nir(idx_ones(idx_time))
        data_tc_split.fapar(idx_ones(idx_time))=data_day_split(t).fapar(idx_ones(idx_time))
        data_tc_split.sigma(idx_ones(idx_time))=data_day_split(t).sigma(idx_ones(idx_time))
        data_tc_split.sigma_red(idx_ones(idx_time))=data_day_split(t).sigma_red(idx_ones(idx_time))
        data_tc_split.sigma_nir(idx_ones(idx_time))=data_day_split(t).sigma_nir(idx_ones(idx_time))
        data_tc_split.jrc_flag(idx_ones(idx_time))=data_day_split(t).jrc_flag(idx_ones(idx_time))
        data_tc_split.ltdr_flag(idx_ones(idx_time))=data_day_split(t).ltdr_flag(idx_ones(idx_time))
        data_tc_split.day(idx_ones(idx_time))=data_day_split(t).day(idx_ones(idx_time))
        ;break
        ;bareSday(idx_ones(idx_time))=1.
      endif
      ;tvscl, congrid(reform(bareSday), 72, 360)
    endfor
    ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    data_tc_split.dev_red_temp(idx_ones)=0.
    data_tc_split.dev_nir_temp(idx_ones)=0.
    data_tc_split.dev_temp(idx_ones)=0.
    ;==========================================================================================
    ;
    ; If  two dates
    idx_two = where((newDaySoil eq 2) and $
      data_tc_split.jrc_flag eq 5 or data_tc_split.jrc_flag eq 4)
    nir_two=fltarr(xSplitDim,dims[1])
    for t=0, daysNumber-1  do begin      ; ng ++
      buf=reform(data_day_split(t).jrc_flag)
      buf1=reform(data_day_split(t).nir)
      ;idx_time = where(buf eq 0 and buf1 gt 0.0 and indexs eq 2 or index_2s eq 2)
      ; MM & NG 22/9/2016
      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      idxMaskSoil=where(buf1[goodIndexes] gt 0.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask (for this day)
      validMaskSoil=validMask*validMaskSoil

      ;final Soil Mask AND check if actually soil (jrc_flag)
      idx_time = where(newDaySoil eq 2 and $
        (buf eq 4 or buf eq 5) and $
        validMaskSoil eq 1, countTwoDays)
      print, 'DoubleDay (bare soil) for day: ', t, coundTwoDays
      if countTwoDays gt 0 then begin
        idx_lp= where(buf1(idx_time) gt nir_two(idx_time))
        ;print, data_day_split(t).jrc_flag(idx_time(idx_lp(0)))
        ;print, data_day_split(*).jrc_flag(idx_time(idx_lp(0)))
        ;stop
        if idx_lp(0) ge 0 then begin
          nir_two(idx_time(idx_lp))=buf1(idx_time(idx_lp))
          data_tc_split.fapar(idx_time(idx_lp)) = data_day_split(t).fapar(idx_time(idx_lp))
          data_tc_split.red(idx_time(idx_lp))=data_day_split(t).red(idx_time(idx_lp))
          data_tc_split.nir(idx_time(idx_lp))= nir_two(idx_time(idx_lp))
          ;
          data_tc_split.sigma_red(idx_time(idx_lp))= data_day_split(t).sigma_red(idx_time(idx_lp))
          data_tc_split.sigma_nir(idx_time(idx_lp))= data_day_split(t).sigma_nir(idx_time(idx_lp))
          data_tc_split.sigma(idx_time(idx_lp))= data_day_split(t).sigma(idx_time(idx_lp))
          ;
          data_tc_split.toc_red(idx_time(idx_lp))= data_day_split(t).toc_red(idx_time(idx_lp))
          data_tc_split.toc_nir(idx_time(idx_lp))= data_day_split(t).toc_nir(idx_time(idx_lp))
          ;
          data_tc_split.jrc_flag(idx_time(idx_lp))= data_day_split(t).jrc_flag(idx_time(idx_lp))
          data_tc_split.ltdr_flag(idx_time(idx_lp))= data_day_split(t).ltdr_flag(idx_time(idx_lp))
          ;
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
          data_tc_split.nday(idx_time(idx_lp))=2
          ;bareSday(idx_time(idx_lp))=1.
        endif
      endif
      DelIdlVar, buf
      DelIdlVar, buf1
    endfor
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    tempFlag=data_tc_split.jrc_flag
    notAssignedFlag=15
    prevExcept=!EXCEPT
    !EXCEPT=0
    notAssigned1=where(data_tc_split.jrc_flag eq notAssignedFlag and data_tc_split.fapar gt 0., notAssignedCount)
    a=check_math()
    prevExcept=!EXCEPT
    if notAssignedCount gt 0 then stop
    nCloudIceMx=0b*data_tc_split.jrc_flag
    nSeaMx=nCloudIceMx

    for t=0, daysNumber-1 do begin       ;  ng ++
      ; MM & NG 22/9/2016
      idx_ok=where((newDaySoil eq 2) and $
        (data_day_split(t).jrc_flag eq 4 or data_day_split(t).jrc_flag eq 5) and $
        (data_day_split(t).fapar eq 0.0) and data_tc_split.day ne t)
      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
        ;      stop
      endif
      thisDayIndexes=where(data_tc_split.day eq data_day_split[t].day, count)
      if count ne 0 then begin
        data_tc_split.ts[thisDayIndexes]=data_day_split[t].ts[thisDayIndexes]
        data_tc_split.tv[thisDayIndexes]=data_day_split[t].tv[thisDayIndexes]
        data_tc_split.phi[thisDayIndexes]=data_day_split[t].phi[thisDayIndexes]
        ;
        data_tc_split.jrc_flag[thisDayIndexes]=data_day_split[t].jrc_flag[thisDayIndexes]
        data_tc_split.ltdr_flag[thisDayIndexes]=data_day_split[t].ltdr_flag[thisDayIndexes]
        ;
        data_tc_split.toc_red[thisDayIndexes]=data_day_split[t].toc_red[thisDayIndexes]
        data_tc_split.toc_nir[thisDayIndexes]=data_day_split[t].toc_nir[thisDayIndexes]
        ;
        data_tc_split.fapar(thisDayIndexes)=data_day_split(t).fapar(thisDayIndexes)
        data_tc_split.sigma(thisDayIndexes)=data_day_split(t).sigma(thisDayIndexes)
        data_tc_split.sigma_red(thisDayIndexes)=data_day_split(t).sigma_red(thisDayIndexes)
        data_tc_split.sigma_nir(thisDayIndexes)=data_day_split(t).sigma_nir(thisDayIndexes)
      endif
      idxSea=where(data_day_split(t).jrc_flag eq 3, cntSea)
      idxCloudIce=where(data_day_split(t).jrc_flag eq 2, cntCloudIce)
      if cntCloudIce gt 0 then nSeaMx[idxSea]=nseaMx[idxSea]+ones[idxSea]
      if cntSea gt 0 then nCloudIceMx[idxCloudIce]=nCloudIceMx[idxCloudIce]+ones[idxCloudIce]
    endfor

    ; Set best flag available for tc (only when not assigned by previous steps) "flag eq notAssignedFlag:
    tempFlag=data_tc_split.jrc_flag
    notAssigned=where(data_tc_split.jrc_flag eq notAssignedFlag, notAssignedCount)

    ; MM: test setting...
    ;notAssignedCount=0
    if notAssignedCount gt 1 then begin
      print, '**Flag = ', notAssignedFlag, '!!! (not assigned value)***'
      ;at least one pixel classified as sea/water...
      idxSea=where(nSeaMx ne 0 and data_tc_split.jrc_flag eq notAssignedFlag, cntSea)
      ;at least one pixel classified as cloud/ice...
      ;idxCloudIce=where(nCloudIceMx ne 0 and data_tc_split.jrc_flag eq notAssignedFlag, cntCloudIce)
      choosenDay=-1
      if cntSea gt 0 then begin
        for pix=0, cntSea-1 do begin
          ; lowest flag values... different decoding table
          flagList=data_day_split[*].jrc_flag[idxSea[pix]]
          unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
          ; work around to map best flag using C/Seawifs approach
          mapFlagList=mapFaparFlag(flagList)
          selectedFlag=min(mapFlagList)
          selectedDay=(where(selectedFlag eq mapFlagList))[0]
          ; come back to AVHRR flag decoding
          selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
          ;
          data_tc_split.jrc_flag[idxSea[pix]]=selectedFlag
          data_tc_split.ltdr_flag[idxSea[pix]] = data_day_split[selectedDay].ltdr_flag[idxSea[pix]]
          ;
          data_tc_split.fapar[idxSea[pix]] = data_day_split[selectedDay].fapar[idxSea[pix]]
          data_tc_split.red[idxSea[pix]]=data_day_split[selectedDay].red[idxSea[pix]]
          data_tc_split.nir[idxSea[pix]]= data_day_split[selectedDay].nir[idxSea[pix]]
          ;
          data_tc_split.day[idxSea[pix]]=data_day_split[selectedDay].day
          data_tc_split.nday[idxSea[pix]]=1
          ;
          data_tc_split.sigma_red[idxSea[pix]]= data_day_split[selectedDay].sigma_red[idxSea[pix]]
          data_tc_split.sigma_nir[idxSea[pix]]= data_day_split[selectedDay].sigma_nir[idxSea[pix]]
          data_tc_split.sigma[idxSea[pix]]= data_day_split[selectedDay].sigma[idxSea[pix]]
          ;
          data_tc_split.ts[idxSea[pix]]=data_day_split[selectedDay].ts[idxSea[pix]]
          data_tc_split.tv[idxSea[pix]]=data_day_split[selectedDay].tv[idxSea[pix]]
          data_tc_split.phi[idxSea[pix]]=data_day_split[selectedDay].phi[idxSea[pix]]
          data_tc_split.toc_red[idxSea[pix]]=data_day_split[selectedDay].toc_red[idxSea[pix]]
          data_tc_split.toc_nir[idxSea[pix]]=data_day_split[selectedDay].toc_nir[idxSea[pix]]
          ;  stop
        endfor
      endif
      nSeaMx[*]=0
      ;window, 1
      ;tvscl, congrid(data_tc_split.jrc_flag, 36, 360)
      ;aa=where(data_tc_split.jrc_flag eq 255, cnt255)
      ;if cnt255 ne 0 then stop
      ;=ARRAY_INDICES(data_tc_split.jrc_flag, cntCloudIce)
      ;temp=data_tc_split.jrc_flag
      ;temp[*]=0
      ;temp[cntCloudIce]=1
      ;vIdx=where(data_tc_split.jrc_flag eq 4 or data_tc_split.jrc_flag eq 5 or data_tc_split.jrc_flag eq 6 or data_tc_split.jrc_flag eq 0, cc)
      ;vIdx1=where(data_tc_split.jrc_flag eq 4 or data_tc_split.jrc_flag eq 5 or data_tc_split.jrc_flag eq 6 or data_tc_split.jrc_flag eq 0 or temp eq 0, cc1)
      ; MM: test setting...
      ;cntCloudIce=0
      if cntCloudIce gt 0 then begin
        print, 'clouds:', cntCloudIce
        for pix=0, cntCloudIce-1 do begin
          ; just for display test recycle nSeaMx variable
          checkDone=where(data_tc_split.jrc_flag[idxCloudIce[pix]] eq 4 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 5 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 6 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 3 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 0, jump)
          ; jump means: pixel is assigned, do nothing, skip to the next pixel...
          if jump eq 1 then continue
          nSeaMx[idxCloudIce[pix]]=1
          flagList=data_day_split[*].jrc_flag[idxCloudIce[pix]]
          unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
          ; work around to map best flag using C/Seawifs approach
          mapFlagList=mapFaparFlag(flagList)
          notBad=where(mapFlagList ne 255, cnt)
          if cnt gt 0 then selectedFlag=max(mapFlagList[notBad]) else selectedFlag=255
          selectedDay=(where(selectedFlag eq mapFlagList))[0]
          ; come back to AVHRR flag decoding
          selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
          ;print, 'choose:', selectedFlag
          ;print, 'from:', unikFlags
          data_tc_split.jrc_flag[idxCloudIce[pix]] = selectedFlag
          data_tc_split.ltdr_flag[idxCloudIce[pix]] = data_day_split[selectedDay].ltdr_flag[idxCloudIce[pix]]
          data_tc_split.fapar[idxCloudIce[pix]] = data_day_split[selectedDay].fapar[idxCloudIce[pix]]
          ;
          data_tc_split.day[idxCloudIce[pix]] = data_day_split[selectedDay].day
          data_tc_split.nday[idxCloudIce[pix]] = 1
          ;
          data_tc_split.red[idxCloudIce[pix]]=data_day_split[selectedDay].red[idxCloudIce[pix]]
          data_tc_split.nir[idxCloudIce[pix]]= data_day_split[selectedDay].nir[idxCloudIce[pix]]
          data_tc_split.sigma_red[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_red[idxCloudIce[pix]]
          data_tc_split.sigma_nir[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_nir[idxCloudIce[pix]]
          data_tc_split.sigma[idxCloudIce[pix]]= data_day_split[selectedDay].sigma[idxCloudIce[pix]]
          ;
          data_tc_split.toc_red[idxCloudIce[pix]]= data_day_split[selectedDay].toc_red[idxCloudIce[pix]]
          data_tc_split.toc_nir[idxCloudIce[pix]]= data_day_split[selectedDay].toc_nir[idxCloudIce[pix]]
          ;
          data_tc_split.ts[idxCloudIce[pix]]=data_day_split[selectedDay].ts[idxCloudIce[pix]]
          data_tc_split.tv[idxCloudIce[pix]]=data_day_split[selectedDay].tv[idxCloudIce[pix]]
          data_tc_split.phi[idxCloudIce[pix]]=data_day_split[selectedDay].phi[idxCloudIce[pix]]
        endfor
      endif
      notAssigned=where(data_tc_split.jrc_flag eq notAssignedFlag, notAssignedCount)
      ; MM: test setting...
      ;notAssignedCount=0
      for jj=0, notAssignedCount-1 do begin
        fList=data_day_split[*].jrc_flag[notAssigned[jj]]
        unikFlags=fList[UNIQ(fList, SORT(fList))]
        data_tc_split.jrc_flag[notAssigned[jj]]=min(unikFlags)
        ;print, 'undetermined flag-->', min(unikFlags)
      endfor
    endif
    waterIdxs=where(data_tc_split.jrc_flag[*,*] eq 3, waterCount)
    if waterCount gt 0 then begin
      data_tc_split.day[waterIdxs]=255
      ;data_tc_split.nday[waterIdxs]=255
    endif

    NanDay=where(data_tc_split.day ne 255, validCountDay)
    ; Add 1 to update 0-based day "index": more readable field (think about set doy instead)
    if validCountDay gt 0 then data_tc_split.day=data_tc_split.day+1

    print, 'compute slice...', slice+1, '/', nSlice

    ; try use efficiently subscription (no subscript on left side, but only starting index)
    prevflag[subXStart:subXEnd, subYStart:subYEnd]=tempFlag
    data_tc.nday[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.nday[*,*]
    data_tc.day[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.day[*,*]

    data_tc.dev_red_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_red_temp[*,*]
    data_tc.dev_nir_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_nir_temp[*,*]
    data_tc.dev_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_temp[*,*]

    data_tc.sigma[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma[*,*]
    data_tc.sigma_red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma_red[*,*]
    data_tc.sigma_nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma_nir[*,*]
    data_tc.jrc_flag[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.jrc_flag[*,*]
    data_tc.ltdr_flag[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.ltdr_flag[*,*]
    data_tc.fapar[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.fapar[*,*]

    data_tc.nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.nir[*,*]
    data_tc.red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.red[*,*]
    ;only for deep check test
    data_tc.ts[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.ts[*,*]
    data_tc.tv[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.tv[*,*]
    data_tc.phi[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.phi[*,*]
    data_tc.toc_red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.toc_red[*,*]
    data_tc.toc_nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.toc_nir[*,*]
    ;  print, data_tc.fapar(0,1370)
    ;  print, data_tc(*).fapar(180,1370)
    ;  print, data_tc(*).fapar(220,1370)
    ; print, data_tc(*).fapar(0,1611)
    ;   stop
  endfor
  print, '**********************'
  print, data_day_split.jrc_flag
  print, data_tc.red
  print, data_tc.nir
  print, '**********************'

end
;========================================================================================================
