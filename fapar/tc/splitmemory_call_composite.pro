;
;ldtr == cloud (remove data)
PRO sm_call_composite, daysNumber, data_day_f, data_tc, nSlice, prevFlag=prevFlag, cloudtype=cloudtype, sigma_correction=sigma_correction, forceLTDRCloud=forceLTDRCloud
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
  print, 'In the time composite program ...'
  ;
  ; !!!!!!!!!!!!!!!!
  ; NG 2016: YOU HAVE TO CHANGE THIS AS IF THERE IS MISSING DAILY FILE THE DAY WILL NOT CORRESPOND TO THE T ...
  ;         I suggest that you add and save the DOY to replace in to DAY fiels in the output files
  ;
  ;
  tt=[0, daysNumber-1]    ; ng ++
  print, 'daysNumber', daysnumber
  ;
  prevflag=bytarr(dims[0],dims[1])
  
  data_tc= getDataTCStruct1(dims[0],dims[1], /INIT)

  ;==========================================================================================
  ;
  ; look for vegetated pixels
  ;
  ; count the number of dates where we have valid pixels over vegetation land
  xSplitDim=dims[0]/nSlice
  ySplitDim=dims[1]; full dim,

  tt=[0, daysNumber-1]    ; ng ++
  ;pixel_position=[460, 1680]
  ; for a faster test set a specific slice here...
  startSlice=0;2
  endSlice=nSlice;2
  ; good test: vertical slice half of total slices shows "Center Europe and Africa"
  ;startSlice=9;2
  ;endSlice=12;2
  for slice=startSlice, endSlice-1 do begin ;nSlice-1 do begin
    subXStart=slice*xSplitDim & subXEnd=(slice+1)*xSplitDim-1
    subYStart=0 & subYEnd=dims[1]-1

    ; only for test
    data_day_split=getDataDayStruct(xSplitDim, ySplitDim)

    data_tc_split=getDataTCStruct(xSplitDim, ySplitDim, /INIT)

    finalCloud=data_tc_split.toc_nir
    finalCloud[*,*]=0

    data_day_split=replicate(data_day_split, daysNumber)

    print, 'reading day from: ', tt[0]+1, 'to: ', tt[1]+1
    print, '...'
    resFlags=0
    for t=0, tt[1] do begin   ; ng ++

      print, 'reading day...', t+1, '/', tt[1]+1
      if data_day_f[t].fid gt 0 then faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_f[t].fid, /FULL) $
      else faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=fid, /FULL)
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

        ; remove clouds from data...
        ;titles=['fapar - mask bit 1 or 2 (cloudy/shadow cloud)', 'fapar - mask bit 1 (cloudy', 'fapar - mask bit 2 (shadow cloud)', 'fapar - no mask']
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
          ;faparData.fapar[cloudNaN]=!VALUES.F_NAN
          ;faparData.nir[cloudNaN]=!VALUES.F_NAN
          ;faparData.red[cloudNaN]=!VALUES.F_NAN
          ;faparData.toc_nir[cloudNaN]=!VALUES.F_NAN
          ;faparData.toc_red[cloudNaN]=!VALUES.F_NAN
          ; ng 2016: set a "fake" cloud to flag out fapar values
          ; ng 2017: set a big sigma to flag out fapar values
          ; ng + mm
          ;if cnt gt 0 then faparData.flag[cloudNaN[changeIdxs]]=2 ; force cloud code for fapar and soil
          ;leave original values, but flag cloud AND/OR cheange sigma values
          changeIdxs=where(faparData.flag[cloudNaN] eq 4 or faparData.flag[cloudNaN] eq 5 or faparData.flag[cloudNaN] eq 6 or faparData.flag[cloudNaN] eq 0, cnt)
          if cnt gt 0 then begin
            if n_elements(sigma_correction) eq 1 then faparData.sigma[cloudNaN[changeIdxs]]=faparData.sigma[cloudNaN[changeIdxs]]*sigma_correction
            if keyword_set(forceLTDRCloud) then faparData.flag[cloudNaN[changeIdxs]]=2 ; force cloud code for fapar and soil
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
        data_day_split[t].toc_red=faparData.toc_red
        data_day_split[t].toc_nir=faparData.toc_nir
        flagMatrix=fapardata.flag
        data_day_split[t].jrc_flag=flagMatrix
        resFlags=[resFlags,flagMatrix[UNIQ(flagMatrix, SORT(flagMatrix))]]
        flagMatrix=0
        checkCloud1=-1 & checkCloud2=-1
        ;.day sets to position of file in the (sub)sequence ([1..8/9/10/11] for decade [1..28/29/30/31] for monthly), use doy???
        data_day_split[t].day=t
        ;array=faparData.jrc_flag
        ;print, array[UNIQ(array, SORT(array))]
        ;test flag...
        if keyword_set(test_pics) then begin
          loadct,12
          tvlct,r,g,b, /get
          ;8Invalid
          ;9Invalid
          ;3Pixel is over water1 = yes, 0 = no
          ;2Pixel contains cloud shadow1 = yes, 0 = no
          ;1Pixel is cloudy1 = yes, 0 = no
          red = [0,1,0.8,0,0.00,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
          gre = [0,0,0.0,0,1.00,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
          blu = [0,0,0.0,1,1.00,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
          TVLCT, red*255, gre*255, blu*255
          tvlct,r,g,b, /get
          destFlag=(data_day_split[t].ltdr_flag)*0
          cloud1=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,1)
          idx=where(cloud1 eq 1, cnt)
          destFlag[idx]=1
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_cloud1_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          cloud2=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,2)
          idx=where(cloud2 eq 1, cnt)
          destFlag[idx]=2
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_cloud2_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag, 2), red=r,gre=g,blu=b

          water=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,3)
          idx=where(water eq 1, cnt)
          destFlag[idx]=3
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_water_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          invalid1=cgi_map_bitwise_flag(data_day_split[t].ltdr_flag,8)
          idx=where(invalid1 eq 1, cnt)
          destFlag[idx]=4
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_invalid1_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          invalid2=CGI_MAP_BITWISE_FLAG(data_day_split[t].ltdr_flag,9)
          idx=where(invalid2 eq 1, cnt)
          destFlag[idx]=4
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_invalid2_day'+strcompress(t+1, /remove)+'.tiff', reverse(destflag,2), red=r,gre=g,blu=b

          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_day'+strcompress(t+1, /remove)+'.tiff', reverse(bytscl(faparData.ltdr_flag, min=0, max=25600),2), red=r,gre=g,blu=b
          red = [0,1,1,0,0.66,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
          gre = [0,0,0,0,0.66,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
          blu = [0,0,1,1,0.66,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
          TVLCT, red*255, gre*255, blu*255
          tvlct,r,g,b, /get
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/JRC_day'+strcompress(t+1, /remove)+'.tiff', reverse(faparData.jrc_flag,2), red=r,gre=g,blu=b
        endif
        ; .valid eq 1 means a good file
        data_day_split[t].valid=1
      endif
    endfor
    ;TOCnir=reform(data_day_split[*].toc_nir[0,1950:2100])
    ;TOCred=reform(data_day_split[*].toc_red[0,1950:2100])
    ;nir=reform(data_day_split[*].nir[0,1950:2100])
    ;red=reform(data_day_split[*].red[0,1950:2100])
    ;save, filename='tocdata_199906_CT'+strcompress(cloudType, /REMOVE)+'.sav', TOCnir, TOCred
    ;save, filename='brfdata_199906_CT'+strcompress(cloudType, /REMOVE)+'.sav', nir, red

    resFlags=resFlags[UNIQ(resFlags, SORT(resFlags))]
    print, 'input flags list:', resFlags
    vIdxs=where(data_day_split.valid eq 1, dNumber)
    daysNumber=dNumber
    data_day_split=data_day_split[vIdxs]
    tt=[0, daysNumber-1]
    waterMask=data_day_split[0].jrc_flag*0

    dayVeg=bytarr(xSplitDim,ySplitDim) & dayVeg[*,*]=0
    one=dayVeg & one[*,*]=1

    ;window,1,xsize=360, ysize=360, title='flag 1'
    ;array=data_tc_split.jrc_flag
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    pixel_position=[460, 1680]
    for t=0, tt(1) do begin
      ; cut off Nan
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)
      ;create Soil mask
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] ge 0.0 and $
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
      idx_maskVeg = where(validMaskVeg eq 1 and data_day_split(t).jrc_flag(*,*) eq 0, countVeg)
      ;final Soil Mask
      idx_maskSoil = where(validMaskSoil eq 1 and (data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5), countSoil)
      ; previous version (without Nan)
      ;      idx_maskVeg = where(data_day_split(t).fapar(*,*) gt 0.0 and data_day_split(t).jrc_flag(*,*) eq 0 and  $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0, count1)
      ;      idx_maskBareSoil = where((data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5) and $
      ;        data_day_split(t).fapar(*,*) ge 0.0 and $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0, count1)
      ;window,3
      ;tvscl, congrid(diff, 36,720)
      if countVeg gt 0 then dayVeg[idx_maskVeg]=dayVeg[idx_maskVeg]+one[idx_maskVeg]
      ;if countBSoil gt 0 then indexVeg[idx_maskVeg]=indexVeg[idx_maskVeg]+one[idx_maskVeg]
      ;if idx_maskBareSoil(0) ge 0 then indexBareSoil(idx_maskBareSoil)=indexBareSoil(idx_maskBareSoil)+one(idx_maskBareSoil)
    endfor
    ; More than two dates
    ;
    ; associated values for the number of date is bigger or equal to 3
    ;
    idx_third = where(dayVeg ge 3, complement=flagNan)
    ;2580, 1720
    sm_make_tc_distance_eu_vegetation_m, daysNumber, data_day_split, idx_third, day, meandat, std_mean, 3, index_2, [xSplitDim, ySplitDim], faparMean, cloudtype=cloudtype
    for t =0 , tt(1)  do begin      ; ng ++
      ; MM & NG 22/09/2016
      idx_t=where(day eq t and index_2 ge 3)
      if idx_t(0) ge 0 then begin
        data_tc_split.nday[idx_t]=dayVeg[idx_t]
        data_tc_split.red(idx_t)= data_day_split(t).red(idx_t)
        data_tc_split.nir(idx_t)= data_day_split(t).nir(idx_t)
        data_tc_split.fapar(idx_t)= data_day_split(t).fapar(idx_t)
        ;
        data_tc_split.sigma_red(idx_t)= data_day_split(t).sigma_red(idx_t)
        data_tc_split.sigma_nir(idx_t)= data_day_split(t).sigma_nir(idx_t)
        data_tc_split.sigma(idx_t)= data_day_split(t).sigma(idx_t)
        ;;wrongIndex=where(data_day_split(t).jrc_flag(idx_t) eq 21, countWrong)
        ;if countWrong ne 0 then stop
        data_tc_split.jrc_flag(idx_t)= data_day_split(t).jrc_flag(idx_t)
        data_tc_split.ltdr_flag(idx_t)= data_day_split(t).ltdr_flag(idx_t)
        ;overwriteCheck=where(data_tc_split.day(idx_t) ne 255, overWriteCount)
        ;if overWriteCount ne 0 then stop
        data_tc_split.day(idx_t) = day(idx_t)
        ; MM & NG 22/9/2016
        data_tc_split.toc_red(idx_t)= data_day_split(t).toc_red(idx_t)
        data_tc_split.toc_nir(idx_t) = data_day_split(t).toc_nir(idx_t)
        ; data_tc_split.dev_temp not time-dependent
      endif
      tv, congrid(reform(data_tc_split.fapar[*,*]), 72, 360)
      ;meandatFapar=meandat[2,*,*]
      ;      checkConsistency=where(data_tc_split.day(*) lt 255 and $
      ;        (data_tc_split.fapar(*) lt (MEANDATFAPAR(*)+std_mean.temp(*)) and $
      ;        data_tc_split.fapar(*) gt (meandatFapar(idx_t)-std_mean.temp(*))), consistencyCount)
    endfor
    data_tc_split.dev_red_temp= std_mean.red
    data_tc_split.dev_nir_temp= std_mean.nir
    data_tc_split.dev_temp= std_mean.temp
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ; If only One date
    ; associated values for the only dates
    idx_one=where(dayVeg eq 1 or index_2 eq 1)
    ;totDay=data_day_split[0].jrc_flag*0
    ;
    for t=0, tt(1) do begin     ;   ng ++
      validMask=finite(data_day_split(t).fapar)
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_time = where(validMaskVeg eq 1 and (data_day_split(t).jrc_flag eq 0) and (dayVeg eq 1 or index_2 eq 1), countSingleDay)

      ;      idx_timeOld = where((data_day_split(t).jrc_flag eq 0) and (data_day_split(t).fapar gt 0.0) and (dayVeg eq 1 or index_2 eq 1), countSingleDayOld)
      ;      if countSingleDay ne countSingleDayOld then stop
      print, 'singleDay for day: ', t, countSingleDay
      if countSingleDay gt 0 then begin
        data_tc_split.nday(idx_time)=1
        data_tc_split.red(idx_time)=data_day_split(t).red(idx_time)
        data_tc_split.nir(idx_time)=data_day_split(t).nir(idx_time)
        data_tc_split.fapar(idx_time)=data_day_split(t).fapar(idx_time)
        ;wrongIndex=where(data_day_split(t).jrc_flag(idx_time) eq 21, countWrong)
        ;if countWrong ne 0 then stop
        data_tc_split.jrc_flag(idx_time)=data_day_split(t).jrc_flag(idx_time)
        data_tc_split.ltdr_flag(idx_time)=data_day_split(t).ltdr_flag(idx_time)
        data_tc_split.sigma_red(idx_time)= data_day_split(t).sigma_red(idx_time)
        data_tc_split.sigma_nir(idx_time)= data_day_split(t).sigma_nir(idx_time)
        data_tc_split.sigma(idx_time)= data_day_split(t).sigma(idx_time)
        data_tc_split.toc_red(idx_time)= data_day_split(t).toc_red(idx_time)
        data_tc_split.toc_nir(idx_time)= data_day_split(t).toc_nir(idx_time)
        ;overwriteCheck=where(data_tc_split.day(idx_time) ne 255, overWriteCount)
        ;if overWriteCount then stop
        data_tc_split.day(idx_time)=data_day_split(t).day

      endif
    endfor
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    data_tc_split.dev_red_temp(idx_one)=0.
    data_tc_split.dev_nir_temp(idx_one)=0.
    data_tc_split.dev_temp(idx_one)=0.
    ; If only two dates

    ; associated values for the only dates
    ;
    idx_two = where(dayVeg eq 2 or index_2 eq 2)
    fapar_two=fltarr(xSplitDim,dims[1])
    for t=0, tt(1)  do begin      ; ng ++

      buf=data_day_split(t).jrc_flag
      buf1=data_day_split(t).fapar

      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(buf[goodIndexes] eq 0 and (buf1[goodIndexes] gt 0.0))
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_time = where(validMaskVeg eq 1 and (dayVeg eq 2 or index_2 eq 2) and buf eq 0, coundTwoDays)

      ;idx_timeOld = where((buf eq 0) and (buf1 gt 0.0) and (dayVeg eq 2 or index_2 eq 2), coundTwoDaysOld)
      ;if coundTwoDays ne coundTwoDaysOld then stop
      print, 'DoubleDay for day: ', t, coundTwoDays
      if coundTwoDays gt 0 then begin
        idx_lp= where(buf1(idx_time) gt fapar_two(idx_time))
        if idx_lp(0) ge 0 then begin
          fapar_two(idx_time(idx_lp))=buf1(idx_time(idx_lp))
          data_tc_split.nday(idx_time(idx_lp))=2
          data_tc_split.fapar(idx_time(idx_lp)) = fapar_two(idx_time(idx_lp))
          data_tc_split.red(idx_time(idx_lp))=data_day_split(t).red(idx_time(idx_lp))
          data_tc_split.nir(idx_time(idx_lp))=data_day_split(t).nir(idx_time(idx_lp))
          data_tc_split.toc_red(idx_time(idx_lp))= data_day_split(t).toc_red(idx_time(idx_lp))
          data_tc_split.toc_nir(idx_time(idx_lp))= data_day_split(t).toc_nir(idx_time(idx_lp))
          data_tc_split.sigma(idx_time(idx_lp))= data_day_split(t).sigma(idx_time(idx_lp))
          data_tc_split.sigma_red(idx_time(idx_lp))= data_day_split(t).sigma_red(idx_time(idx_lp))
          data_tc_split.sigma_nir(idx_time(idx_lp))= data_day_split(t).sigma_nir(idx_time(idx_lp))
          ;wrongIndex=where(data_day_split(t).jrc_flag(idx_time(idx_lp)) eq 21, countWrong)
          ;if countWrong ne 0 then stop
          data_tc_split.jrc_flag(idx_time(idx_lp))= data_day_split(t).jrc_flag(idx_time(idx_lp))
          data_tc_split.ltdr_flag(idx_time(idx_lp))= data_day_split(t).ltdr_flag(idx_time(idx_lp))
          ;overwriteCheck=where(data_tc_split.day(idx_time(idx_lp)) ne 255, overWriteCount)
          ;if overWriteCount then stop
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
        endif
      endif
    endfor
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    for t=0, tt(1)  do begin        ; NG ++
      ;idx_ok=where(data_day_split(t).jrc_flag eq 0 and data_day_split(t).fapar gt 0.0 and index eq 2 and data_tc_split.day ne t)
      ; MM & NG 22/9/2016
      validMask=finite(data_day_split(t).fapar(*,*))
      goodIndexes=where(validMask eq 1)
      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_ok = where(validMaskVeg eq 1 and (dayVeg eq 2 or index_2 eq 2) and (data_tc_split.day ne t) and data_day_split(t).jrc_flag eq 0, countDay)

      ;      idx_okOld=where((data_day_split(t).jrc_flag eq 0) and (data_day_split(t).fapar gt 0.0) and (dayVeg eq 2 or index_2 eq 2) and (data_tc_split.day ne t), countDayOld)
      ;      if countDay ne countDayOld then stop
      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
      endif
    endfor
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;print,'Finish vegetation ....'
    ;==========================================================================================
    ; look for bare soil  pixels
  ;
    ; count the number of date where we have valid pixels over vegetation land
    indexBareSoil=bytarr(xSplitDim,dims[1])
    indexBareSoil(*,*)=0
    one=indexBareSoil
    one(*,*)=1
    for t=0, tt(1)  do begin        ;  ng ++
      ; Cut off NaN from original data
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] ge 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil
      ;final Soil Mask
      idx_masks = where(validMaskSoil eq 1 and (data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5), countSoil)
      ;      idx_masksOld = where(data_day_split(t).fapar eq 0 and $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0 and data_day_split(t).jrc_flag(*,*) eq 4 or $
      ;        data_day_split(t).jrc_flag(*,*) eq 5, countSoilOld)                                                                                       ; ng 2016
      ;      if countSoilOld ne countSoil then stop
      if idx_masks(0) ge 0 then indexBareSoil(idx_masks)=indexBareSoil(idx_masks)+one(idx_masks)
    endfor
    array=data_tc_split.jrc_flag
    ;==========================================================================================
    ; More than two dates
    ; associated values for the number of dates is bigger than 3
    idx_thirds = where(indexBareSoil ge 3, complement=flagNan)
    print, '# soil pixels more than 3 times', N_elements(idx_thirds)
    ;stop
    ;==========================================================================================
    sm_make_tc_distance_eu_vegetation_m, daysNumber, data_day_split, idx_thirds, days, meandats, std_means, 2, index_2s, [xSplitDim, ySplitDim]
    array=data_tc_split.jrc_flag
    bareSday=data_tc_split.red
    bareSday[*]=0

    for t=0 , tt(1) do begin        ; ng ++
      idx_t1=where(days eq t and index_2s ge 3, countThreeDays1)
      idx_t=where(days eq t and index_2s ge 3 and (data_day_split(t).jrc_flag eq 4 or $
        data_day_split(t).jrc_flag eq 5) and data_tc_split.jrc_flag ne 0, countThreeDays)
      print, 'countThreeDay (bare soil) for day: ', t, countThreeDays
      if countThreeDays ne 0 then begin
        data_tc_split.nday(idx_t)=index_2s[idx_t]
        data_tc_split.red(idx_t)= data_day_split(t).red(idx_t)
        data_tc_split.nir(idx_t)= data_day_split(t).nir(idx_t)
        data_tc_split.fapar(idx_t)= data_day_split(t).fapar(idx_t)
        ;wrongIndex=where(data_day_split(t).jrc_flag(idx_t) eq 21, countWrong)
        ;if countWrong ne 0 then stop
        data_tc_split.jrc_flag(idx_t)= data_day_split(t).jrc_flag(idx_t)
        data_tc_split.ltdr_flag(idx_t)= data_day_split(t).ltdr_flag(idx_t)
        ;overwriteCheck=where(data_tc_split.day(idx_t) ne 255, overWriteCount)
        ;if overWriteCount then stop
        data_tc_split.day(idx_t) = days(idx_t)
        ;
        data_tc_split.toc_red(idx_t)= data_day_split(t).toc_red(idx_t)
        data_tc_split.toc_nir(idx_t)= data_day_split(t).toc_nir(idx_t)
        data_tc_split.sigma_red(idx_t)= data_day_split(t).sigma_red(idx_t)
        data_tc_split.sigma_nir(idx_t)= data_day_split(t).sigma_nir(idx_t)
        data_tc_split.sigma(idx_t)= data_day_split(t).sigma(idx_t)
        ; MM & NG 22/9/2016
        data_tc_split.dev_red_temp(idx_t)= std_means.red[idx_t]
        data_tc_split.dev_nir_temp(idx_t)= std_means.nir[idx_t]
        data_tc_split.dev_temp(idx_t)= std_means.temp[idx_t]
        bareSday(idx_t)=1.
      endif
      tvscl, congrid(reform(bareSday), 72, 360)
    endfor
    ;stop
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;==========================================================================================
    ; If only One date
    ; associated values for the only dates
    ; MM & NG 23/9/2016
    idx_ones = where(((indexBareSoil eq 1) or (index_2s eq 1)) and data_tc_split.jrc_flag ne 0)
    ones=0b*data_tc_split.day+1
    for t=0, tt(1)  do begin          ;  ng ++
      ; MM & NG 22/9/2016
      idx_time = where((data_day_split(t).jrc_flag(idx_ones) eq 4) or (data_day_split(t).jrc_flag(idx_ones) eq 5) , countSingleDay)
      print, 'countSingleDay (bare soil) for day: ', t, countSingleDay
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
        ;wrongIndex=where(data_day_split(t).jrc_flag(idx_ones(idx_time)) eq 21, countWrong)
        ;if countWrong ne 0 then stop
        data_tc_split.jrc_flag(idx_ones(idx_time))=data_day_split(t).jrc_flag(idx_ones(idx_time))
        data_tc_split.ltdr_flag(idx_ones(idx_time))=data_day_split(t).ltdr_flag(idx_ones(idx_time))
        ;overwriteCheck=where(data_tc_split.day(idx_ones(idx_time)) ne 255, overWriteCount)
        ;if overWriteCount then stop
        data_tc_split.day(idx_ones(idx_time))=data_day_split(t).day(idx_ones(idx_time))
        bareSday(idx_ones(idx_time))=1.
      endif
      ;tvscl, congrid(reform(bareSday), 72, 360)
    endfor
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    data_tc_split.dev_red_temp(idx_ones)=0.
    data_tc_split.dev_nir_temp(idx_ones)=0.
    data_tc_split.dev_temp(idx_ones)=0.
    ;==========================================================================================
    ;
    ; If  two dates
    idx_two = where(indexBareSoil eq 2 or index_2s eq 2 and data_tc_split.jrc_flag ne 0)
    nir_two=fltarr(xSplitDim,dims[1])
    for t=0, tt(1)  do begin      ; ng ++
      buf=reform(data_day_split(t).jrc_flag)
      buf1=reform(data_day_split(t).nir)
      ;idx_time = where(buf eq 0 and buf1 gt 0.0 and indexs eq 2 or index_2s eq 2)
      ; MM & NG 22/9/2016
      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      idxMaskSoil=where(buf1[goodIndexes] gt 0.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil

      ;final Soil Mask
      idx_time = where(validMaskSoil eq 1 and (buf eq 4 or buf eq 5) and data_tc_split.jrc_flag ne 0 and (indexBareSoil eq 2 or index_2s eq 2), countTwoDays)
      ;idx_timeOld = where((buf eq 4 or buf eq 5) and data_tc_split.jrc_flag ne 0 and (buf1 gt 0.0) and (indexBareSoil eq 2 or index_2s eq 2), countTwoDaysOld)
      print, 'DoubleDay (bare soil) for day: ', t, coundTwoDays
      ;if countTwoDaysOld ne  countTwoDays then stop
      if countTwoDays gt 0 then begin
        idx_lp= where(buf1(idx_time) gt nir_two(idx_time))
        if idx_lp(0) ge 0 then begin
          nir_two(idx_time(idx_lp))=buf1(idx_time(idx_lp))
          data_tc_split.nday(idx_time(idx_lp))=2
          data_tc_split.fapar(idx_time(idx_lp)) = data_day_split(t).fapar(idx_time(idx_lp))
          data_tc_split.red(idx_time(idx_lp))=data_day_split(t).red(idx_time(idx_lp))
          data_tc_split.nir(idx_time(idx_lp))= nir_two(idx_time(idx_lp))
          data_tc_split.sigma_red(idx_time(idx_lp))= data_day_split(t).sigma_red(idx_time(idx_lp))
          data_tc_split.sigma_nir(idx_time(idx_lp))= data_day_split(t).sigma_nir(idx_time(idx_lp))
          data_tc_split.sigma(idx_time(idx_lp))= data_day_split(t).sigma(idx_time(idx_lp))
          data_tc_split.toc_red(idx_time(idx_lp))= data_day_split(t).toc_red(idx_time(idx_lp))
          data_tc_split.toc_nir(idx_time(idx_lp))= data_day_split(t).toc_nir(idx_time(idx_lp))
          ;wrongIndex=where(data_day_split(t).jrc_flag(idx_time(idx_lp)) eq 21, countWrong)
          ;if countWrong ne 0 then stop
          data_tc_split.jrc_flag(idx_time(idx_lp))= data_day_split(t).jrc_flag(idx_time(idx_lp))
          data_tc_split.ltdr_flag(idx_time(idx_lp))= data_day_split(t).ltdr_flag(idx_time(idx_lp))
          ;overwriteCheck=where(data_tc_split.day(idx_time(idx_lp)) ne 255, overWriteCount)
          ;if overWriteCount then stop
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
          bareSday(idx_time(idx_lp))=1.
        endif
      endif
      DelIdlVar, buf
      DelIdlVar, buf1
    endfor
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    tempFlag=data_tc_split.jrc_flag
    notAssigned1=where(data_tc_split.jrc_flag eq notAssignedFlag and data_tc_split.fapar gt 0., notAssignedCount)
    if notAssignedCount gt 0 then stop
    nCloudIceMx=0b*data_tc_split.jrc_flag
    nSeaMx=nCloudIceMx

    for t=0, tt(1)   do begin       ;  ng ++
      ; MM & NG 22/9/2016
      ;fill jrc_flag (composite) with water ONLY if fapar is lt 0 (invalid, never computed)
      idx_water=where((data_day_split(t).jrc_flag eq 3 and data_tc_split.fapar lt 0.0), checkWater)
      if checkWater ne 0 then data_tc_split.jrc_flag[idx_water]=3
      idx_ok=where((data_day_split(t).jrc_flag eq 4 or data_day_split(t).jrc_flag eq 5) and (data_day_split(t).fapar eq 0.0) and (indexBareSoil eq 2 or index_2s eq 2) and data_tc_split.day ne t)
      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
      endif
      thisDayIndexes=where(data_tc_split.day eq data_day_split[t].day, count)
      if count ne 0 then begin
        data_tc_split.ts[thisDayIndexes]=data_day_split[t].ts[thisDayIndexes]
        data_tc_split.tv[thisDayIndexes]=data_day_split[t].tv[thisDayIndexes]
        data_tc_split.phi[thisDayIndexes]=data_day_split[t].phi[thisDayIndexes]
        data_tc_split.jrc_flag[thisDayIndexes]=data_day_split[t].jrc_flag[thisDayIndexes]
        data_tc_split.ltdr_flag[thisDayIndexes]=data_day_split[t].ltdr_flag[thisDayIndexes]
        data_tc_split.toc_red[thisDayIndexes]=data_day_split[t].toc_red[thisDayIndexes]
        data_tc_split.toc_nir[thisDayIndexes]=data_day_split[t].toc_nir[thisDayIndexes]
      endif
      idxSea=where(data_day_split(t).jrc_flag eq 3, cntSea)
      idxCloudIce=where(data_day_split(t).jrc_flag eq 2, cntCloudIce)
      if cntCloudIce gt 0 then nSeaMx[idxSea]=nseaMx[idxSea]+ones[idxSea]
      if cntSea gt 0 then nCloudIceMx[idxCloudIce]=nCloudIceMx[idxCloudIce]+ones[idxCloudIce]
    endfor
    ;    restore, filename='fpa_'+strcompress(cloudtype, /REMOVE)+'.sav'
    ;    restore, filename='red_'+strcompress(cloudtype, /REMOVE)+'.sav'
    ;    restore, filename='nir_'+strcompress(cloudtype, /REMOVE)+'.sav'
    ;    meandata=reform(mean_field.nir(0,1950:2100))
    ;    stddata=reform(std_field.nir(*,0,1950:2100))
    ;    stdmean=reform(std_mean.nir(0,1950:2100))

    ; Set best flag available for tc (only when not assigned by previous steps) flag eq notAssignedFlag
    tempFlag=data_tc_split.jrc_flag
    notAssigned=where(data_tc_split.jrc_flag eq notAssignedFlag, notAssignedCount)
    aa=where(data_tc_split.jrc_flag eq 255, nancnt)
    if notAssignedCount gt 1 then begin
      print, '**Flag = ', notAssignedFlag, '!!! (not assigned value)***'
      ;at least one pixel classified as sea/water...
      idxSea=where(nSeaMx ne 0 and data_tc_split.jrc_flag eq notAssignedFlag, cntSea)
      ;at least one pixel classified as cloud/ice...
      ; aggiungere flag ldtr  afare per natale
      ;
      ;      idxCloudIce=where(nCloudIceMx ne 0 and data_tc_split.jrc_flag eq notAssignedFlag, cntCloudIce)
      choosenDay=-1
      if cntSea gt 0 then begin
        for pix=0, cntSea-1 do begin
          ; lowest flag values... different decoding table
          flagList=data_day_split[*].jrc_flag[idxSea[pix]]
          unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
          ;if n_elements(unikFlags) ne 1 then stop
          ; work around to map best flag using C/Seawifs approach
          mapFlagList=mapFaparFlag(flagList)
          selectedFlag=min(mapFlagList)
          selectedDay=(where(selectedFlag eq mapFlagList))[0]
          selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
          ; come back to AVHRR flag decoding
          data_tc_split.jrc_flag[idxSea[pix]]=selectedFlag
          ;aa=where(data_tc_split.jrc_flag eq 255, cnt255)
          ;if cnt255 ne 0 then stop
          data_tc_split.ltdr_flag[idxSea[pix]] = data_day_split[selectedDay].ltdr_flag[idxSea[pix]]
          data_tc_split.fapar[idxSea[pix]] = data_day_split[selectedDay].fapar[idxSea[pix]]
          data_tc_split.red[idxSea[pix]]=data_day_split[selectedDay].red[idxSea[pix]]
          ;data_tc_split.day[idxSea[pix]]=data_day_split[selectedDay].day
          data_tc_split.day[idxSea[pix]]=255
          data_tc_split.nday[idxSea[pix]]=255
          data_tc_split.nir[idxSea[pix]]= data_day_split[selectedDay].nir[idxSea[pix]]
          data_tc_split.sigma_red[idxSea[pix]]= data_day_split[selectedDay].sigma_red[idxSea[pix]]
          data_tc_split.sigma_nir[idxSea[pix]]= data_day_split[selectedDay].sigma_nir[idxSea[pix]]
          data_tc_split.sigma[idxSea[pix]]= data_day_split[selectedDay].sigma[idxSea[pix]]
          data_tc_split.toc_red[idxSea[pix]]= data_day_split[selectedDay].toc_red[idxSea[pix]]
          data_tc_split.toc_nir[idxSea[pix]]= data_day_split[selectedDay].toc_nir[idxSea[pix]]
          data_tc_split.ts[idxSea[pix]]=data_day_split[selectedDay].ts[idxSea[pix]]
          data_tc_split.tv[idxSea[pix]]=data_day_split[selectedDay].tv[idxSea[pix]]
          data_tc_split.phi[idxSea[pix]]=data_day_split[selectedDay].phi[idxSea[pix]]
          data_tc_split.toc_red[idxSea[pix]]=data_day_split[selectedDay].toc_red[idxSea[pix]]
          data_tc_split.toc_nir[idxSea[pix]]=data_day_split[selectedDay].toc_nir[idxSea[pix]]
        endfor
      endif
      nSeaMx[*]=0
      ;window, 1
      ;tvscl, congrid(data_tc_split.jrc_flag, 36, 360)
      aa=where(data_tc_split.jrc_flag eq 255, cnt255)
      if cnt255 ne 0 then stop
      ;=ARRAY_INDICES(data_tc_split.jrc_flag, cntCloudIce)
      ;temp=data_tc_split.jrc_flag
      ;temp[*]=0
      ;temp[cntCloudIce]=1
      ;vIdx=where(data_tc_split.jrc_flag eq 4 or data_tc_split.jrc_flag eq 5 or data_tc_split.jrc_flag eq 6 or data_tc_split.jrc_flag eq 0, cc)
      ;vIdx1=where(data_tc_split.jrc_flag eq 4 or data_tc_split.jrc_flag eq 5 or data_tc_split.jrc_flag eq 6 or data_tc_split.jrc_flag eq 0 or temp eq 0, cc1)
      if cntCloudIce gt 0 then begin
        print, 'clouds:', cntCloudIce
        for pix=0, cntCloudIce-1 do begin
          ; just for display test recycle nSeaMx variable
          checkDone=where(data_tc_split.jrc_flag[idxCloudIce[pix]] eq 4 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 5 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 6 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 0 or data_tc_split.jrc_flag[idxCloudIce[pix]] eq 0, jump)
          if jump eq 1 then continue
          nSeaMx[idxCloudIce[pix]]=1
          flagList=data_day_split[*].jrc_flag[idxCloudIce[pix]]
          unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
          ;aa=where(unikFlags eq 21,c)
          ;if n_elements(unikFlags) ne 1 and c eq 0 then stop
          ; work around to map best flag using C/Seawifs approach
          mapFlagList=mapFaparFlag(flagList)
          notBad=where(mapFlagList ne 255, cnt)
          if cnt gt 0 then selectedFlag=max(mapFlagList[notBad]) else selectedFlag=255
          selectedDay=(where(selectedFlag eq mapFlagList))[0]
          selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
          ; come back to AVHRR flag decoding
          ;if selectedFlag ne 1 then begin
          data_tc_split.jrc_flag[idxCloudIce[pix]] = selectedFlag
          ;print, 'choose:', selectedFlag
          ;print, 'from:', unikFlags
          data_tc_split.ltdr_flag[idxCloudIce[pix]] = data_day_split[selectedDay].ltdr_flag[idxCloudIce[pix]]
          data_tc_split.fapar[idxCloudIce[pix]] = data_day_split[selectedDay].fapar[idxCloudIce[pix]]
          ;data_tc_split.day[idxCloudIce[pix]] = data_day_split[selectedDay].day
          data_tc_split.day[idxCloudIce[pix]] = 255
          data_tc_split.nday[idxCloudIce[pix]] = 255
          data_tc_split.red[idxCloudIce[pix]]=data_day_split[selectedDay].red[idxCloudIce[pix]]
          data_tc_split.nir[idxCloudIce[pix]]= data_day_split[selectedDay].nir[idxCloudIce[pix]]
          data_tc_split.sigma_red[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_red[idxCloudIce[pix]]
          data_tc_split.sigma_nir[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_nir[idxCloudIce[pix]]
          data_tc_split.sigma[idxCloudIce[pix]]= data_day_split[selectedDay].sigma[idxCloudIce[pix]]
          data_tc_split.toc_red[idxCloudIce[pix]]= data_day_split[selectedDay].toc_red[idxCloudIce[pix]]
          data_tc_split.toc_nir[idxCloudIce[pix]]= data_day_split[selectedDay].toc_nir[idxCloudIce[pix]]
          data_tc_split.ts[idxCloudIce[pix]]=data_day_split[selectedDay].ts[idxCloudIce[pix]]
          data_tc_split.tv[idxCloudIce[pix]]=data_day_split[selectedDay].tv[idxCloudIce[pix]]
          data_tc_split.phi[idxCloudIce[pix]]=data_day_split[selectedDay].phi[idxCloudIce[pix]]
          ;endif
        endfor
      endif
      notAssigned=where(data_tc_split.jrc_flag eq notAssignedFlag, notAssignedCount)
      for jj=0, n_elements(notAssigned)-1 do begin
        fList=data_day_split[*].jrc_flag[notAssigned[jj]]
        unikFlags=fList[UNIQ(fList, SORT(fList))]
        ;if n_elements(unikFlags) ne 1 then print, 'warning'
        ;if unikFlags[0] ne 1 then print, 'warning'
        data_tc_split.jrc_flag[notAssigned[jj]]=min(unikFlags)
        ;print, 'undetermined flag-->', min(unikFlags)
      endfor
    endif
    waterIdxs=where(data_tc_split.jrc_flag[*,*] eq 3, waterCount)
    if waterCount gt 0 then begin
      data_tc_split.day[waterIdxs]=255
      ;data_tc_split.nday[waterIdxs]=255
    endif

    noFaparIdxs=where((data_tc_split.jrc_flag[*,*] ne 0) and (data_tc_split.jrc_flag[*,*] ne 4) and (data_tc_split.jrc_flag[*,*] ne 5), noFaparCount)
    if noFaparCount gt 0 then begin
      data_tc_split.nday[noFaparIdxs]=255
    endif
    NanDay=where(data_tc_split.day ne 255, validCountDay)
    if validCountDay gt 0 then data_tc_split.day=data_tc_split.day+1

    noDayIdxs=where(data_tc_split.tv eq INT_NAN, noDayCount)
    if noDayCount gt 0 then begin
      data_tc_split.day[noDayIdxs]=255
    endif

    ; test daily behaviour

    ;    diffDay=dd-data_tc_split.day
    ;    window, 1
    ;    tvscl, congrid(reform(data_tc_split.day), 72, 360)
    ;    window, 2
    ;    tvscl, congrid(reform(diffDay), 72, 360)
    ;      for jj=0, notAssignedCount-1 do begin
    ;        strangeReasonIdx=where((data_day_split[*].jrc_flag[notAssigned[jj]] ne 1) and (data_day_split[*].jrc_flag[notAssigned[jj]] ne 2), strangereasonCount)
    ;        if strangereasonCount gt 1 then begin
    ;          print, data_tc_split.fapar[notAssigned[jj]]
    ;          print, data_day_split[*].fapar[notAssigned[jj]]
    ;          print, data_day_split[*].jrc_flag[notAssigned[jj]]
    ;        endif
    ;      endfor
    ;      print, '**Flag = ',notAssignedFlag,'!!! (end)***'
    ;    endif


    ;array=data_tc_split.jrc_flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,12,xsize=360, ys 360)
    ;print, array[UNIQ(array, SORT(array))];

    ;tvscl, congrid(data_tc_split.jrc_flag, 72, 360)
    ;data_tc_split.nday(idx_two)=2
    ;data_tc_split.jrc_flag(idx_two)=4
    ;    bareSday=data_tc_split.jrc_flag
    ;    aa=where(bareSday eq 4)
    ;    bareSday[*]=0
    ;    bareSday[aa]=1
    ;    tvscl, congrid(reform(bareSday), 72, 360)
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;tvscl, congrid(reform(nSeaMx), 72, 360)
    ;    window, 10, xsize=360, ysize=360, title='day - 4'
    ;    tvscl, congrid(data_tc_split.day, 72, 360)
    ;    window, 11, xsize=360, ysize=360, title='fapar - 4'
    ;    tvscl, congrid(data_tc_split.fapar, 72, 360)
    ;    window, 12, xsize=360, ysize=360, title='nday - 4'
    ;    tvscl, congrid(data_tc_split.nday, 72, 360)

    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->7',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,7, xsize=72*3, ysize=360*3, title='final'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    print, 'compute slice...', slice+1, '/', nSlice
    ; Add 1 to update 0-based day "index": more readable field (think about set doy instead)

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
    data_tc.faparMean[subXStart:subXEnd, subYStart:subYEnd]=faparMean
    ;end test

  endfor
  ;===================================================================================================
  ;tNames=tag_names(data_tc)

  ; check Sahara mistery...
  ;data_tc.fapar[3500:3700, 2100]=0.5
  ;saharaIndex=where(data_tc.fapar eq 0 and data_tc.jrc_flag eq 6, countSahara)

end
;========================================================================================================
