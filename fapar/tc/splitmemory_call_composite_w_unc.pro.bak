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
    ;buf(idx_doIt)=distance(t,*)
    buf=reform(distance[t,*,*])
    ;stop
    ; MM & NG 22/09/2016
    idx_valid = where(buf le thres and buf ge 0.0, validCount); and distance(t,*,*) lt 50.0)
    ; check our distance vs StdDev
    ;idx_valid = where(buf le std_mean.temp and std_mean.temp gt 0, validCount); and distance(t,*,*) lt 50.0)
    if validCount gt 0 then index_2(idx_valid)=index_2(idx_valid)+one(idx_valid)
    ; remove outliers using flag 21
    if nfield eq 2 then idx_bad_mask = where(buf gt thres and (data_in(t).flag eq 4 or data_in(t).flag eq 5), outliersCount)
    if nfield eq 3 then idx_bad_mask = where(buf gt thres and data_in(t).flag eq 0, outliersCount)
    vld=where(buf lt 100)
    ;if max(buf[vld]) gt thres then stop
    ;if nfield eq 2 then idx_bad_mask = where(buf gt thres and (data_in(t).flag eq 4 or data_in(t).flag eq 5), outliersCount)
    ;if nfield eq 3 then idx_bad_mask = where(buf gt std_mean.temp and data_in(t).flag eq 0, outliersCount)
    if outliersCount gt 1 then data_in(t).flag(idx_bad_mask)=21.0
    ; remove mask ONLY to work on bare soil (We need original daily flag values...)...
    ;if idx_bad_mask(0) ge 0 and nfield eq 2 then data_in(t).flag(idx_bad_mask)=21.0
    ;if idx_bad_mask(0) ge 0 then print, data_in(t).fapar(idx_bad_mask(0)), t
  endfor
  ;
  ;stop
  if nfield eq 3 then idx_remake=where(data_in.flag eq 0 and index_2 ge 3, complement=saveIndexes)
  if nfield eq 2 then idx_remake=where((data_in.flag eq 4 or data_in.flag eq 5) and index_2 ge 3)
  ;
  DelidlVar, buf
  DelidlVar, one
  DelidlVar, idx_bad_mask
  if idx_remake(0) ge 0 then begin
    print, 'Remake after Outliers out'
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
    buf=reform((distance[t,*,*]))


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
;
;
PRO sm_call_composite_w_unc, daysNumber, data_day_f, data_tc, nSlice, prevflag=prevflag, cloudtype=cloudtype
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
  DATA_NAN=-1

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
  ;tt=size(data_day.day)
  tt=[0, daysNumber-1]    ; ng ++
  print, 'daysNumber', daysnumber
  ;
  ;
  ;  data_tc= {day: bytarr(7200,3600), $
  ;    nday: bytarr(7200,3600), $
  ;    fapar: fltarr(7200,3600), $
  ;    dev_temp: fltarr(7200,3600), $
  ;    sigma: fltarr(7200,3600), $
  ;    red: fltarr(7200,3600), $
  ;    dev_red_temp: fltarr(7200,3600), $
  ;    sigma_red:fltarr(7200,3600), $
  ;    nir: fltarr(7200,3600), $
  ;    dev_nir_temp: fltarr(7200,3600), $
  ;    sigma_nir: fltarr(7200,3600), $
  ;    qa: intarr(7200,3600), $
  ;    flag: bytarr(7200,3600)}

  ;only for test
  prevflag=bytarr(7200,3600)
  data_tc= {day: bytarr(7200,3600), $
    nday: bytarr(7200,3600), $
    fapar: fltarr(7200,3600), $
    dev_temp: fltarr(7200,3600), $
    sigma: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    dev_red_temp: fltarr(7200,3600), $
    sigma_red:fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    dev_nir_temp: fltarr(7200,3600), $
    sigma_nir: fltarr(7200,3600), $
    qa: intarr(7200,3600), $
    flag: bytarr(7200,3600), $
    ts: fltarr(7200,3600), $
    tv: fltarr(7200,3600), $
    toc_red: fltarr(7200,3600), $
    toc_nir: fltarr(7200,3600), $
    faparMean: fltarr(7200,3600)}
  ;end test

  data_tc.fapar[*,*]=DATA_NAN
  data_tc.red[*,*]=DATA_NAN
  data_tc.nir[*,*]=DATA_NAN

  data_tc.dev_red_temp[*,*]=DATA_NAN
  data_tc.dev_nir_temp[*,*]=DATA_NAN
  data_tc.dev_temp[*,*]=DATA_NAN

  data_tc.sigma[*,*]=DATA_NAN
  data_tc.sigma_red[*,*]=DATA_NAN
  data_tc.sigma_nir[*,*]=DATA_NAN

  ;only for test
  data_tc.tv[*,*]=INT_NAN
  data_tc.ts[*,*]=INT_NAN
  data_tc.toc_red[*,*]=DATA_NAN
  data_tc.toc_nir[*,*]=DATA_NAN
  ;end test
  ;
  ; initiate flag to sea mask
  ; MM & NC 22/09/2016 flagging water
  ;data_tc.flag(*,*)=6
  ; initialize to unvalid
  data_tc.flag[*,*]=1
  ; day may be 0 (the first); 255 means no data
  data_tc.day[*,*]=255
  ; nday equal 0 means no data.
  data_tc.nday[*,*]=0
  ;
  ;==========================================================================================
  ;
  ; look for vegetated pixels
  ;
  ; count the number of dates where we have valid pixels over vegetation land
  ;
  ;
  xSplitDim=7200/nSlice
  ySplitDim=3600; full dim,

  tt=[0, daysNumber-1]    ; ng ++
  pixel_position=[460, 1680]
  ;720*3
  ;slBasedPix=[pixel_position[0]-(720*3), pixel_position[1]]
  ;print, SLBASEDPIX

  ;startSlice=0;2
  ;endSlice=nSlice;2
  startSlice=10;2
  endSlice=11;2

  for slice=startSlice, endSlice-1 do begin ;nSlice-1 do begin
    ; good test: vertical slice #5 (on 10) shows "Europe and Africa"
    ;slice=

    subXStart=slice*xSplitDim & subXEnd=(slice+1)*xSplitDim-1
    subYStart=0 & subYEnd=3600-1

    ; initialize this slice (overwriting previous...)
    ;data_day_split={  day: bytarr(xSplitDim,ySplitDim), $
    ;  data_day_split={  day: 0b, $
    ;    ;nday: bytarr(xSplitDim,ySplitDim), $
    ;    fapar: fltarr(xSplitDim,ySplitDim), $
    ;    ;dev_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma: fltarr(xSplitDim,ySplitDim), $
    ;    red: fltarr(xSplitDim,ySplitDim), $
    ;    ;dev_red_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma_red:fltarr(xSplitDim,ySplitDim), $
    ;    nir: fltarr(xSplitDim,ySplitDim), $
    ;    ;dev_nir_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma_nir: fltarr(xSplitDim,ySplitDim), $
    ;    flag: bytarr(xSplitDim,ySplitDim), $
    ;    qa: intarr(xSplitDim,ySplitDim), $
    ;    ts: intarr(xSplitDim,ySplitDim), $
    ;    tv: intarr(xSplitDim,ySplitDim), $
    ;    valid:0}

    ; only for test
    data_day_split={  day: 0b, $
      ;nday: bytarr(xSplitDim,ySplitDim), $
      fapar: fltarr(xSplitDim,ySplitDim), $
      ;dev_temp: fltarr(xSplitDim,ySplitDim), $
      sigma: fltarr(xSplitDim,ySplitDim), $
      red: fltarr(xSplitDim,ySplitDim), $
      ;dev_red_temp: fltarr(xSplitDim,ySplitDim), $
      sigma_red:fltarr(xSplitDim,ySplitDim), $
      nir: fltarr(xSplitDim,ySplitDim), $
      ;dev_nir_temp: fltarr(xSplitDim,ySplitDim), $
      sigma_nir: fltarr(xSplitDim,ySplitDim), $
      flag: bytarr(xSplitDim,ySplitDim), $
      qa: intarr(xSplitDim,ySplitDim), $
      ts: fltarr(xSplitDim,ySplitDim), $
      tv: fltarr(xSplitDim,ySplitDim), $
      toc_red: fltarr(xSplitDim,ySplitDim), $
      toc_nir: fltarr(xSplitDim,ySplitDim), $
      valid:0}
    ; end test

    ;  data_tc_split={  day: bytarr(xSplitDim,ySplitDim), $
    ;    nday: bytarr(xSplitDim,ySplitDim), $
    ;    fapar: fltarr(xSplitDim,ySplitDim), $
    ;    dev_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma: fltarr(xSplitDim,ySplitDim), $
    ;    red: fltarr(xSplitDim,ySplitDim), $
    ;    dev_red_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma_red:fltarr(xSplitDim,ySplitDim), $
    ;    nir: fltarr(xSplitDim,ySplitDim), $
    ;    dev_nir_temp: fltarr(xSplitDim,ySplitDim), $
    ;    sigma_nir: fltarr(xSplitDim,ySplitDim), $
    ;    flag: bytarr(xSplitDim,ySplitDim), $
    ;    qa: intarr(xSplitDim,ySplitDim), $
    ;    ts: intarr(xSplitDim,ySplitDim), $
    ;    tv: intarr(xSplitDim,ySplitDim), $
    ;    valid:0}

    ; only for test
    data_tc_split={  day: bytarr(xSplitDim,ySplitDim), $
      nday: bytarr(xSplitDim,ySplitDim), $
      fapar: fltarr(xSplitDim,ySplitDim), $
      dev_temp: fltarr(xSplitDim,ySplitDim), $
      sigma: fltarr(xSplitDim,ySplitDim), $
      red: fltarr(xSplitDim,ySplitDim), $
      dev_red_temp: fltarr(xSplitDim,ySplitDim), $
      sigma_red:fltarr(xSplitDim,ySplitDim), $
      nir: fltarr(xSplitDim,ySplitDim), $
      dev_nir_temp: fltarr(xSplitDim,ySplitDim), $
      sigma_nir: fltarr(xSplitDim,ySplitDim), $
      flag: bytarr(xSplitDim,ySplitDim), $
      qa: intarr(xSplitDim,ySplitDim), $
      ts: fltarr(xSplitDim,ySplitDim), $
      tv: fltarr(xSplitDim,ySplitDim), $
      toc_red: fltarr(xSplitDim,ySplitDim), $
      toc_nir: fltarr(xSplitDim,ySplitDim), $
      valid:0}
    ; end test
    notAssignedFlag=15
    data_tc_split.flag=notAssignedFlag ; init to not-assigned flag coding....
    data_tc_split.fapar[*,*]=DATA_NAN
    data_tc_split.red[*,*]=DATA_NAN
    data_tc_split.nir[*,*]=DATA_NAN

    data_tc_split.dev_red_temp[*,*]=DATA_NAN
    data_tc_split.dev_nir_temp[*,*]=DATA_NAN
    data_tc_split.dev_temp[*,*]=DATA_NAN

    data_tc_split.sigma[*,*]=DATA_NAN
    data_tc_split.sigma_red[*,*]=DATA_NAN
    data_tc_split.sigma_nir[*,*]=DATA_NAN

    data_tc_split.day[*,*]=255
    data_tc_split.nday[*,*]=0

    ; only for test
    data_tc_split.ts[*,*]=INT_NAN
    data_tc_split.tv[*,*]=INT_NAN
    data_tc_split.toc_red[*,*]=DATA_NAN
    data_tc_split.toc_nir[*,*]=DATA_NAN
    ; end test

    data_day_split=replicate(data_day_split, daysNumber)

    print, 'reading day from: ', tt[0]+1, 'to: ', tt[1]+1
    print, '...'
    resFlags=0
    for t=0, tt[1] do begin   ; ng ++

      ;fInfo=file_info('testData10.sav')
      ;if fInfo.size gt 10 then begin
      ;  restore, 'testData10.sav'
      ;  break
      ;endif
      ;    fInfo=file_info('testData5.sav')
      ;    if fInfo.size gt 10 then begin
      ;      restore, 'testData5.sav'
      ;      break
      ;    endif

      ;restore, data_day_f[t].data_file
      ; only for test add "FULL" keyword
      print, 'reading day...', t+1, '/', tt[1]+1
      if data_day_f[t].fid gt 0 then faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=data_day_f[t].fid, /FULL) $
      else faparData=read_AVHRR_FAPAR(data_day_f[t].fDir, data_day_f[t].fName, FOUND=FOUND, /APPLY, offset=[subXStart, 0], count=[xSplitDim, ySplitDim], fid=fid, /FULL)
      print, 'done'

      ;if fid ne -1 then data_day_f[t].fid=fid
      data_day_split[t].valid=0
      ;count just one time how many water pixels
      if keyword_set(FOUND) then begin

        data_day_split[t].fapar=faparData.fapar
        data_day_split[t].sigma=faparData.sigma
        data_day_split[t].red=faparData.red
        data_day_split[t].sigma_red=faparData.sigma_red
        data_day_split[t].nir=faparData.nir
        data_day_split[t].sigma_nir=faparData.sigma_nir
        flagMatrix=fapardata.flag
        data_day_split[t].flag=flagMatrix
        resFlags=[resFlags,flagMatrix[UNIQ(flagMatrix, SORT(flagMatrix))]]
        flagMatrix=0
        data_day_split[t].qa=faparData.qa
        data_day_split[t].ts=fapardata.ts
        data_day_split[t].tv=faparData.tv
        data_day_split[t].toc_red=fapardata.toc_red
        data_day_split[t].toc_nir=faparData.toc_nir
        data_day_split[t].day=t
        ;array=faparData.flag
        ;print, array[UNIQ(array, SORT(array))]
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
          destFlag=(data_day_split[t].qa)*0
          cloud1=cgi_map_bitwise_flag(data_day_split[t].qa,1)
          idx=where(cloud1 eq 1, cnt)
          destFlag[idx]=1
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_cloud1_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          cloud2=cgi_map_bitwise_flag(data_day_split[t].qa,2)
          idx=where(cloud2 eq 1, cnt)
          destFlag[idx]=2
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_cloud2_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag, 2), red=r,gre=g,blu=b

          water=cgi_map_bitwise_flag(data_day_split[t].qa,3)
          idx=where(water eq 1, cnt)
          destFlag[idx]=3
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_water_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          invalid1=cgi_map_bitwise_flag(data_day_split[t].qa,8)
          idx=where(invalid1 eq 1, cnt)
          destFlag[idx]=4
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_invalid1_day'+strcompress(t+1, /remove)+'.tiff', reverse(destFlag,2), red=r,gre=g,blu=b

          invalid2=CGI_MAP_BITWISE_FLAG(data_day_split[t].qa,9)
          idx=where(invalid2 eq 1, cnt)
          destFlag[idx]=4
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_invalid2_day'+strcompress(t+1, /remove)+'.tiff', reverse(destflag,2), red=r,gre=g,blu=b

          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/LDTR_day'+strcompress(t+1, /remove)+'.tiff', reverse(bytscl(faparData.qa, min=0, max=25600),2), red=r,gre=g,blu=b
          red = [0,1,1,0,0.66,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
          gre = [0,0,0,0,0.66,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
          blu = [0,0,1,1,0.66,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
          TVLCT, red*255, gre*255, blu*255
          tvlct,r,g,b, /get
          write_tiff,'/space2/storage/projects/LAN/AVH/L3/PLC/1999/06/flag_pics/JRC_day'+strcompress(t+1, /remove)+'.tiff', reverse(faparData.flag,2), red=r,gre=g,blu=b
        endif
        ;tvscl, congrid(faparData.flag, 72, 360)
        data_day_split[t].valid=1
      endif
    endfor
    resFlags=resFlags[UNIQ(resFlags, SORT(resFlags))]
    print, resFlags
    ;save, data_day_split, filename='testData10.sav', /COMPRESS
    ;a=data_day_split
    vIdxs=where(data_day_split.valid eq 1, dNumber)
    daysNumber=dNumber
    data_day_split=data_day_split[vIdxs]
    tt=[0, daysNumber-1]
    waterMask=data_day_split[0].flag*0

    dayVeg=bytarr(xSplitDim,ySplitDim)
    dayVeg(*,*)=0
    ;indexBareSoil=indexVeg

    one=dayVeg
    one(*,*)=1

    ;window,1,xsize=360, ysize=360, title='flag 1'
    ;array=data_tc_split.flag
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    pixel_position=[460, 1680]
    for t=0, tt(1) do begin
      ;waterIdxs=where(data_day_split[t].flag eq 3,waterCnt)
      ;if waterCnt ne 0 then waterMask[waterIdxs]=waterMask[waterIdxs]+1
      ; cut off Nan
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] ge 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil

      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_maskVeg = where(validMaskVeg eq 1 and data_day_split(t).flag(*,*) eq 0, countVeg)
      ;final Soil Mask
      idx_maskSoil = where(validMaskSoil eq 1 and (data_day_split(t).flag(*,*) eq 4 or data_day_split(t).flag(*,*) eq 5), countSoil)
      ; previous version (without Nan)
      ;      idx_maskVeg = where(data_day_split(t).fapar(*,*) gt 0.0 and data_day_split(t).flag(*,*) eq 0 and  $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0, count1)
      ;      idx_maskBareSoil = where((data_day_split(t).flag(*,*) eq 4 or data_day_split(t).flag(*,*) eq 5) and $
      ;        data_day_split(t).fapar(*,*) ge 0.0 and $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0, count1)
      ;window,3
      ;tvscl, congrid(diff, 36,720)
      if countVeg gt 0 then dayVeg[idx_maskVeg]=dayVeg[idx_maskVeg]+one[idx_maskVeg]
      ;if countBSoil gt 0 then indexVeg[idx_maskVeg]=indexVeg[idx_maskVeg]+one[idx_maskVeg]
      ;if idx_maskBareSoil(0) ge 0 then indexBareSoil(idx_maskBareSoil)=indexBareSoil(idx_maskBareSoil)+one(idx_maskBareSoil)
    endfor
    ;==========================================================================================
    ;loadct,12
    ;window,0, xsize=720, ysize=360, title='Number of day over vegetation'
    ;tvscl, reverse(congrid(index,720,360),2)
    ;
    ;
    ; More than two dates
    ;
    ; associated values for the number of date is bigger or equal to 3
    ;
    idx_third = where(dayVeg ge 3, complement=flagNan)
    ;
    ;dims = SIZE(indexVeg, /DIMENSIONS)
    ;ind = ARRAY_INDICES(dims, idx_third, /DIMENSIONS)
    ;
    ;
    ;2580, 1720
    sm_make_tc_distance_eu_vegetation_m_w_unc, daysNumber, data_day_split, idx_third, day, meandat, std_mean, 3, index_2, [xSplitDim, ySplitDim], UNC=UNC

    ;window,2,xsize=360, ysize=360, title='flag 2'
    ;array=data_tc_split.flag
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    ;
    ;stop
    ;
    ;
    ; to check if follow is ok
    ;
    ;for t=0, tt(1)-1 do data_day_split(t).fapar=meandat(2,*,*)
    ;
    for t =0 , tt(1)  do begin      ; ng ++
      ;idx_t=where(day eq float(t) and index ge 3)
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
        wrongIndex=where(data_day_split(t).flag(idx_t) eq 21, countWrong)
        if countWrong ne 0 then stop
        data_tc_split.flag(idx_t)= data_day_split(t).flag(idx_t)
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

    ; all data
    ;    idx_check=where(data_tc_split.flag(*)eq 21.0, countWrong)
    ;    print, countWrong
    ;    ; indexes coming from for cycle (day=10/10)
    ;    idx_check=where(data_tc_split.flag(idx_t)eq 21.0, countWrong)
    ;    print, countWrong
    ;    help, idx_check
    ;    stop
    data_tc_split.dev_red_temp= std_mean.red
    data_tc_split.dev_nir_temp= std_mean.nir
    data_tc_split.dev_temp= std_mean.temp
    ;data_tc_split.dev_red_temp(idx_t)= std_mean(0,idx_t)
    ;data_tc_split.dev_nir_temp(idx_t)= std_mean(1,idx_t)
    ;data_tc_split.dev_temp(idx_t)= std_mean(2,idx_t)

    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->1',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,3,xsize=360, ysize=360, title='flag 3'
    ;array=data_tc_split.flag
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    ;window,1, xsize=72*3, ysize=360*3, title='-->1<--'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tvscl, congrid(, 72*3, 360*3)
    ;
    ;idx_third=where(index_2 ge 3)
    ;data_tc_split.nday(idx_third)=indexVeg(idx_third)
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)

    ;dayWrongIdx=where(data_tc_split.day gt 0 and data_tc_split.nday eq 0, dayWrongCnt)
    ;if dayWrongCnt ne 0 then stop
    ;

    ;window,1, xsize=720, ysize=360, title='FAPAR after more than 3 days'
    ;faparcolor
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720, 360),2)
    ;
    ;stop
    ;window,2, xsize=720, ysize=360, title='FLAG after more than 3 days'
    ;loadct,12
    ;tvscl, reverse(congrid(data_tc_split.flag, 720, 360),2)

    ;window,22, xsize=720, ysize=360, title='THE Day'
    ;tvscl, reverse(congrid(data_tc_split.day, 720, 360),2)
    ;stop
    ;==========================================================================================
    ; If only One date
    ;
    ; associated values for the only dates
    ;
    ;
    ;
    idx_one=where(dayVeg eq 1 or index_2 eq 1)
    ;totDay=data_day_split[0].flag*0
    ;
    for t=0, tt(1) do begin     ;   ng ++
      validMask=finite(data_day_split(t).fapar)
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_time = where(validMaskVeg eq 1 and (data_day_split(t).flag eq 0) and (dayVeg eq 1 or index_2 eq 1), countSingleDay)

;      idx_timeOld = where((data_day_split(t).flag eq 0) and (data_day_split(t).fapar gt 0.0) and (dayVeg eq 1 or index_2 eq 1), countSingleDayOld)
;      if countSingleDay ne countSingleDayOld then stop
      print, 'singleDay for day: ', t, countSingleDay
      if countSingleDay gt 0 then begin
        data_tc_split.nday(idx_time)=1
        data_tc_split.red(idx_time)=data_day_split(t).red(idx_time)
        data_tc_split.nir(idx_time)=data_day_split(t).nir(idx_time)
        data_tc_split.fapar(idx_time)=data_day_split(t).fapar(idx_time)
        wrongIndex=where(data_day_split(t).flag(idx_time) eq 21, countWrong)
        if countWrong ne 0 then stop
        data_tc_split.flag(idx_time)=data_day_split(t).flag(idx_time)
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

    ;data_tc_split.nday(idx_one)=1
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;data_tc_split.flag(idx_one)=0
    data_tc_split.dev_red_temp(idx_one)=0.
    data_tc_split.dev_nir_temp(idx_one)=0.
    data_tc_split.dev_temp(idx_one)=0.

    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->2',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,4,xsize=360, ysize=360, title='flag 4'
    ;array=data_tc_split.flag
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    ;window,2, xsize=72*3, ysize=360*3, title='-->2<--'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)

    ;dayWrongIdx=where(data_tc_split.day gt 0 and data_tc_split.nday  eq 0, dayWrongCnt)
    ;if dayWrongCnt ne 0 then stop
    ;
    ;faparcolor
    ;window,3, xsize=720, ysize=360, title='FAPAR after more than 3 days and 1 date'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720, 360),2)
    ;loadct,12
    ;window,23, xsize=720, ysize=360, title='THE Day 1'
    ;tvscl, reverse(congrid(data_tc_split.day, 720, 360),2)
    ;stop
    ;==========================================================================================
    ;
    ; If only two dates
    ;
    ; associated values for the only dates
    ;
    idx_two = where(dayVeg eq 2 or index_2 eq 2)
    ;
    fapar_two=fltarr(xSplitDim,3600)
    ;
    for t=0, tt(1)  do begin      ; ng ++
      buf=data_day_split(t).flag
      buf1=data_day_split(t).fapar

      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
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
          wrongIndex=where(data_day_split(t).flag(idx_time(idx_lp)) eq 21, countWrong)
          if countWrong ne 0 then stop
          data_tc_split.flag(idx_time(idx_lp))= data_day_split(t).flag(idx_time(idx_lp))
          ;overwriteCheck=where(data_tc_split.day(idx_time(idx_lp)) ne 255, overWriteCount)
          ;if overWriteCount then stop
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
        endif
      endif
    endfor
    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->3',  fpa_count, n_elements(data_tc_split.fapar)
    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,5,xsize=360, ysize=360, title='flag 5'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    ;
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    for t=0, tt(1)  do begin        ; NG ++
      ;idx_ok=where(data_day_split(t).flag eq 0 and data_day_split(t).fapar gt 0.0 and index eq 2 and data_tc_split.day ne t)
      ; MM & NG 22/9/2016
      validMask=finite(data_day_split(t).fapar(*,*))
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0)
      validMaskVeg=validMask*0
      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;create Veg mask

      validMaskVeg=validMask*validMaskVeg
      ;final Veg Mask
      idx_ok = where(validMaskVeg eq 1 and (dayVeg eq 2 or index_2 eq 2) and (data_tc_split.day ne t) and data_day_split(t).flag eq 0, countDay)

;      idx_okOld=where((data_day_split(t).flag eq 0) and (data_day_split(t).fapar gt 0.0) and (dayVeg eq 2 or index_2 eq 2) and (data_tc_split.day ne t), countDayOld)
;      if countDay ne countDayOld then stop
      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
      endif
    endfor
    ;
    ;
    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,6,xsize=360, ysize=360, title='flag 6'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    ;data_tc_split.nday(idx_two)=2
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;data_tc_split.flag(idx_two)=0
    ;dayWrongIdx=where(data_tc_split.day gt 0 and data_tc_split.nday  eq 0, dayWrongCnt)
    ;if dayWrongCnt ne 0 then stop

    ;
    ;faparcolor
    ;window,5, xsize=720, ysize=360, title='FAPAR after more than 3 days and 1 date and 2 dates'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720, 360),2)
    ;
    ;loadct,12
    ;window,25, xsize=720, ysize=360, title='THE Day 3'
    ;tvscl, reverse(congrid(data_tc_split.day, 720, 360),2)
    ;
    ;stop
    ;==========================================================================================
    ;
    ;
    ;window,7, xsize=72*3, ysize=360*3, title='FPA veg'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    ;

    ;window,8, xsize=72*3, ysize=360*3, title='RED veg'
    ;tv, reverse(congrid(data_tc_split.red*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.red*250.0, 72*3, 360*3)

    ;loadct,12
    ;window,7, xsize=720, ysize=360, title='Number of Days'
    ;tv, reverse(congrid(data_tc_split.nday*100.0, 720, 360),2)
    ;
    ;window,26, xsize=720, ysize=360, title='THE Day 4'
    ;tvscl, reverse(congrid(data_tc_split.day, 720, 360),2)

    ;plot, data_tc_split.fapar, meandat(2,*,*), psym = 1

    ;print,'Finish vegetation ....'
    ; stop
    ;==========================================================================================
    ; look for bare soil  pixels
    ;
    ; count the number of date where we have valid pixels over vegetation land
    ;
    ;
    indexBareSoil=bytarr(xSplitDim,3600)
    indexBareSoil(*,*)=0
    one=indexBareSoil
    one(*,*)=1
    for t=0, tt(1)  do begin        ;  ng ++
      validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
      idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] ge 0.0 and $
        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil

      ;      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0 and $
      ;        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
      ;        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      ;      validMaskVeg=validMask*0
      ;      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;      ;create Veg mask
      ;
      ;      validMaskVeg=validMask*validMaskVeg
      ;      ;final Veg Mask
      ;      idx_maskVeg = where(validMaskVeg eq 1 and data_day_split(t).flag(*,*) eq 0, countVeg)
      ;final Soil Mask
      idx_masks = where(validMaskSoil eq 1 and (data_day_split(t).flag(*,*) eq 4 or data_day_split(t).flag(*,*) eq 5), countSoil)
      ;;; fixed

      ;      idx_masksOld = where(data_day_split(t).fapar eq 0 and $
      ;        data_day_split(t).red(*,*) gt 0.0 and data_day_split(t).red(*,*) lt 1.0 and $
      ;        data_day_split(t).nir(*,*) gt 0.0 and data_day_split(t).nir(*,*) lt 1.0 and data_day_split(t).flag(*,*) eq 4 or $
      ;        data_day_split(t).flag(*,*) eq 5, countSoilOld)                                                                                       ; ng 2016
      ;      if countSoilOld ne countSoil then stop
      if idx_masks(0) ge 0 then indexBareSoil(idx_masks)=indexBareSoil(idx_masks)+one(idx_masks)
    endfor

    array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,7,xsize=360, ysize=360, title='flag 7'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    ;stop
    ;==========================================================================================
    ;window,9, xsize=72*3, ysize=360*3, title='dd bare soil'
    ;loadct,12
    ;tv, reverse(congrid(bytscl(indexs,min=0,max=10.),720*2,360*2),2)
    ;tv, congrid(bytscl(indexs,min=0,max=10.),72*3, 360*3)
    ;
    ;window,10, xsize=72*3, ysize=360*3, title='dd vegetation'
    ;tv, reverse(congrid(bytscl(index,min=0,max=10.),72*3, 360*3),2)
    ;tv, congrid(bytscl(index,min=0,max=10.),72*3,360*3)
    ;
    ;stop
    ;==========================================================================================
    ; More than two dates
    ;
    ; associated values for the number of dates is bigger than 3
    ;
    idx_thirds = where(indexBareSoil ge 3, complement=flagNan)
    print, '# soil pixels more than 3 times', N_elements(idx_thirds)
    ;stop

    ;==========================================================================================
    ;dims = SIZE(indexBareSoil, /DIMENSIONS)
    ;ind = ARRAY_INDICES(dims, idx_thirds, /DIMENSIONS)
    ;
    sm_make_tc_distance_eu_vegetation_m, daysNumber, data_day_split, idx_thirds, days, meandats, std_means, 2, index_2s, [xSplitDim, ySplitDim]
    ;
    ;
    ;mmask=fix(data_tc_split.fapar)
    ;mmask[*,*]=0
    array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,8,xsize=360, ysize=360, title='flag 8'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    ;    window, 1, xsize=360, ysize=360, title='day - 1'
    ;    tvscl, congrid(data_tc_split.day, 72, 360)
    ;    window, 2, xsize=360, ysize=360, title='fapar - 1'
    ;    tvscl, congrid(data_tc_split.fapar, 72, 360)
    ;    window, 3, xsize=360, ysize=360, title='nday - 1'
    ;    tvscl, congrid(data_tc_split.nday, 72, 360)
    bareSday=data_tc_split.red
    bareSday[*]=0

    for t=0 , tt(1) do begin        ; ng ++
      ;
      idx_t1=where(days eq t and index_2s ge 3, countThreeDays1)
      idx_t=where(days eq t and index_2s ge 3 and (data_day_split(t).flag eq 4 or $
        data_day_split(t).flag eq 5) and data_tc_split.flag ne 0, countThreeDays)
      ;idx_t1=where(days eq t, cnt1)
      ;idx_t2=where(index_2s ge 3, cnt2)
      ;mmask[idx_t2]=mmask[idx_t2]+1
      ;print, 'cnt', cnt, cnt1, cnt2
      ;
      print, 'countThreeDay (bare soil) for day: ', t, countThreeDays
      if countThreeDays ne 0 then begin
        data_tc_split.nday(idx_t)=index_2s[idx_t]
        data_tc_split.red(idx_t)= data_day_split(t).red(idx_t)
        data_tc_split.nir(idx_t)= data_day_split(t).nir(idx_t)
        data_tc_split.fapar(idx_t)= data_day_split(t).fapar(idx_t)
        ;window,13, xsize=72*3, ysize=360*3, title='V'+strcompress(t, /remove)
        ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
        ;window,14, xsize=72*3, ysize=360*3, title='V'+strcompress(t, /remove)
        ;tv, congrid((mmask/30)*250.0, 72*3, 360*3), title='V'+strcompress(t, /remove)
        wrongIndex=where(data_day_split(t).flag(idx_t) eq 21, countWrong)
        if countWrong ne 0 then stop
        data_tc_split.flag(idx_t)= data_day_split(t).flag(idx_t)
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
    ;idx_third=where(index_2s ge 3)
    ;data_tc_split.nday(idx_third)=indexBareSoil(idx_third)
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;    window, 4, xsize=360, ysize=360, title='day - 2'
    ;    tvscl, congrid(data_tc_split.day, 72, 360)
    ;    window, 5, xsize=360, ysize=360, title='fapar - 2'
    ;    tvscl, congrid(data_tc_split.fapar, 72, 360)
    ;    window, 6, xsize=360, ysize=360, title='nday - 2'
    ;    tvscl, congrid(data_tc_split.nday, 72, 360)
    ;dayWrongIdx=where(data_tc_split.day gt 0 and data_tc_split.nday eq 0, dayWrongCnt)
    ;if dayWrongCnt ne 0 then stop
    ;end

    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,9,xsize=360, ysize=360, title='flag 9'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->4',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,4, xsize=72*3, ysize=360*3, title='-->4<--'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    ;
    ;
    ;==========================================================================================
    ; If only One date
    ;
    ; associated values for the only dates
    ;
    ; MM & NG 23/9/2016
    idx_ones = where(((indexBareSoil eq 1) or (index_2s eq 1)) and data_tc_split.flag ne 0)
    ones=0b*data_tc_split.day+1
    ;
    for t=0, tt(1)  do begin          ;  ng ++
      ;idx_time = where(data_day_split(t).flag(idx_ones) eq 4)
      ; MM & NG 22/9/2016
      idx_time = where((data_day_split(t).flag(idx_ones) eq 4) or (data_day_split(t).flag(idx_ones) eq 5) , countSingleDay)
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
        wrongIndex=where(data_day_split(t).flag(idx_ones(idx_time)) eq 21, countWrong)
        if countWrong ne 0 then stop
        data_tc_split.flag(idx_ones(idx_time))=data_day_split(t).flag(idx_ones(idx_time))
        ;overwriteCheck=where(data_tc_split.day(idx_ones(idx_time)) ne 255, overWriteCount)
        ;if overWriteCount then stop
        data_tc_split.day(idx_ones(idx_time))=data_day_split(t).day(idx_ones(idx_time))
        bareSday(idx_ones(idx_time))=1.
      endif
      ;tvscl, congrid(reform(bareSday), 72, 360)
    endfor
    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->5',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,5, xsize=72*3, ysize=360*3, title='-->5<--'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    ;data_tc_split.nday(idx_ones)=1
    ;data_tc_split.flag(idx_ones)=4
    tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;    window, 7, xsize=360, ysize=360, title='day - 3'
    ;    tvscl, congrid(data_tc_split.day, 72, 360)
    ;    window, 8, xsize=360, ysize=360, title='fapar - 3'
    ;    tvscl, congrid(data_tc_split.fapar, 72, 360)
    ;    window, 9, xsize=360, ysize=360, title='nday - 3'
    ;    tvscl, congrid(data_tc_split.nday, 72, 360)

    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,10,xsize=360, ysize=360, title='flag 10'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]

    data_tc_split.dev_red_temp(idx_ones)=0.
    data_tc_split.dev_nir_temp(idx_ones)=0.
    data_tc_split.dev_temp(idx_ones)=0.
    ;
    ;window,2, xsize=720, ysize=360, title='RED after more than 3 days and 1 date'
    ;tv, reverse(congrid(data_tc_split.red*250.0, 720, 360),2)
    ;stop
    ;
    ;==========================================================================================
    ;
    ; If  two dates
    ;
    idx_two = where(indexBareSoil eq 2 or index_2s eq 2 and data_tc_split.flag ne 0)
    ;
    nir_two=fltarr(xSplitDim,3600)
    ;
    for t=0, tt(1)  do begin      ; ng ++
      buf=reform(data_day_split(t).flag)
      buf1=reform(data_day_split(t).nir)
      ;idx_time = where(buf eq 0 and buf1 gt 0.0 and indexs eq 2 or index_2s eq 2)
      ; MM & NG 22/9/2016

      validMask=finite(buf1)
      goodIndexes=where(validMask eq 1)
      ;indexPos=ARRAY_INDICES(data_day_split(t).fapar(*,*), goodIndexes)
      idxMaskSoil=where(buf1[goodIndexes] gt 0.0)
      validMaskSoil=validMask*0
      validMaskSoil[goodIndexes[idxMaskSoil]]=1
      ;create soil mask
      validMaskSoil=validMask*validMaskSoil

      ;      idxMaskVeg=where(data_day_split(t).fapar[goodIndexes] gt 0.0 and $
      ;        data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
      ;        data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
      ;      validMaskVeg=validMask*0
      ;      validMaskVeg[goodIndexes[idxMaskVeg]]=1
      ;      ;create Veg mask
      ;
      ;      validMaskVeg=validMask*validMaskVeg
      ;      ;final Veg Mask
      ;      idx_maskVeg = where(validMaskVeg eq 1 and data_day_split(t).flag(*,*) eq 0, countVeg)
      ;final Soil Mask
      idx_time = where(validMaskSoil eq 1 and (buf eq 4 or buf eq 5) and data_tc_split.flag ne 0 and (indexBareSoil eq 2 or index_2s eq 2), countTwoDays)
      ;;;;
      ;idx_timeOld = where((buf eq 4 or buf eq 5) and data_tc_split.flag ne 0 and (buf1 gt 0.0) and (indexBareSoil eq 2 or index_2s eq 2), countTwoDaysOld)
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
          wrongIndex=where(data_day_split(t).flag(idx_time(idx_lp)) eq 21, countWrong)
          if countWrong ne 0 then stop
          data_tc_split.flag(idx_time(idx_lp))= data_day_split(t).flag(idx_time(idx_lp))
          ;overwriteCheck=where(data_tc_split.day(idx_time(idx_lp)) ne 255, overWriteCount)
          ;if overWriteCount then stop
          data_tc_split.day(idx_time(idx_lp))=data_day_split(t).day
          bareSday(idx_time(idx_lp))=1.
        endif
      endif
      ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
      ;tvscl, congrid(reform(bareSday), 72, 360)
      DelIdlVar, buf
      DelIdlVar, buf1
    endfor
    ;    bareSday=data_tc_split.flag
    ;    aa=where(bareSday eq 4)
    ;    bareSday[*]=0
    ;    bareSday[aa]=1
    ;tvscl, congrid(reform(bareSday), 72, 360)
    ;tvscl, congrid(reform(bareSday), 360, 500)
    ;print, data_day_split[*].flag[Nodata[0]]
    ;buf=reform(data_tc_split.flag)
    ;buf=buf[355:385,2100:2200]
    ;Nodata=where(buf eq 11, ccc)
    ;print, ccc
    ;for i=0, tt(1)-1 do begin
    ;  testImage=(data_day_split[i].flag[355:385,2100:2200])[Nodata]
    ;  boh=where ((testImage eq 4) or (testImage eq 5) or (testImage eq 0), errorCount)
    ;  if errorCount ne 0 then stop
    ;endfor
    ;buf[*]=0
    ;buf[Nodata]=1
    ;tvscl, buf
    ;tvscl, congrid(reform(data_tc_split.nday), 72, 360)
    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,11,xsize=360, ysize=360, title='flag 11'
    ;tvscl, congrid(array, 72, 360)
    ;print, array[UNIQ(array, SORT(array))]
    ;aa=where(data_tc_split.fapar gt 0 and data_tc_split.fapar le 1, fpa_count)
    ;print, '-->6',  fpa_count, n_elements(data_tc_split.fapar)
    ;window,6, xsize=72*3, ysize=360*3, title='-->6<--'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc_split.fapar*250.0, 72*3, 360*3)
    ;
    ; compute the deviation ???? ---> do it after the third call ....
    ;
    tempFlag=data_tc_split.flag
    notAssigned1=where(data_tc_split.flag eq notAssignedFlag and data_tc_split.fapar gt 0., notAssignedCount)
    if notAssignedCount gt 0 then stop
    nCloudIceMx=0b*data_tc_split.flag
    nSeaMx=nCloudIceMx

    for t=0, tt(1)   do begin       ;  ng ++
      ;idx_ok=where(data_day_split(t).flag eq 4 and data_day_split(t).fapar eq 0.0 and indexs eq 2 and data_tc_split.day ne t)
      ; MM & NG 22/9/2016
      ;fill jrc_flag (composite) with water ONLY if fapar is lt 0 (invalid, never computed)
      idx_water=where((data_day_split(t).flag eq 3 and data_tc_split.fapar lt 0.0), checkWater)
      if checkWater ne 0 then data_tc_split.flag[idx_water]=3
      idx_ok=where((data_day_split(t).flag eq 4 or data_day_split(t).flag eq 5) and (data_day_split(t).fapar eq 0.0) and (indexBareSoil eq 2 or index_2s eq 2) and data_tc_split.day ne t)
      if idx_ok(0) ge 0 then begin
        data_tc_split.dev_red_temp(idx_ok)=abs(data_tc_split.red(idx_ok)-data_day_split(t).red(idx_ok))
        data_tc_split.dev_nir_temp(idx_ok)=abs(data_tc_split.nir(idx_ok)-data_day_split(t).nir(idx_ok))
        data_tc_split.dev_temp(idx_ok)=abs(data_tc_split.fapar(idx_ok)-data_day_split(t).fapar(idx_ok))
      endif
      ;only for test
      thisDayIndexes=where(data_tc_split.day eq data_day_split[t].day, count)
      if count ne 0 then begin
        data_tc_split.ts[thisDayIndexes]=data_day_split[t].ts[thisDayIndexes]
        data_tc_split.tv[thisDayIndexes]=data_day_split[t].tv[thisDayIndexes]
        data_tc_split.flag[thisDayIndexes]=data_day_split[t].flag[thisDayIndexes]
        data_tc_split.toc_red[thisDayIndexes]=data_day_split[t].toc_red[thisDayIndexes]
        data_tc_split.toc_nir[thisDayIndexes]=data_day_split[t].toc_nir[thisDayIndexes]
      endif

      ;tvscl, congrid(data_tc_split.flag, 72, 360)
      ;tvscl, congrid(data_day_split(t).flag, 72, 360)
      ;end test
      idxSea=where(data_day_split(t).flag eq 3, cntSea)
      idxCloudIce=where(data_day_split(t).flag eq 2, cntCloudIce)
      if cntCloudIce gt 0 then nSeaMx[idxSea]=nseaMx[idxSea]+ones[idxSea]
      if cntSea gt 0 then nCloudIceMx[idxCloudIce]=nCloudIceMx[idxCloudIce]+ones[idxCloudIce]
    endfor

    ; remap only not-yet-assigned pixels (when flag eq notAssignedFlag)... (?)
    ;    window, 0
    ;    tvscl, congrid(reform(data_tc_split.day), 72, 360)
    ;    dd=data_tc_split.day
    tempFlag=data_tc_split.flag
    notAssigned=where(data_tc_split.flag eq notAssignedFlag, notAssignedCount)
    if notAssignedCount gt 1 then begin
      print, '**Flag = ', notAssignedFlag, '!!! (not assigned value)***'
      ;at least one pixel classified as sea...
      idxSea=where(nSeaMx ne 0 and data_tc_split.flag eq notAssignedFlag, cntSea)
      ;at least one pixel classified as cloud...
      idxCloudIce=where(nCloudIceMx ne 0 and data_tc_split.flag eq notAssignedFlag, cntCloudIce)

      choosenDay=-1
      if cntSea gt 0 then begin
        for pix=0, cntSea-1 do begin
          ; lowest flag values... (?)
          flagList=data_day_split[*].flag[idxSea[pix]]
          unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
          if n_elements(unikFlags) ne 1 then stop
          ; work around to map best flag using C/Seawifs approach
          mapFlagList=mapFaparFlag(flagList)
          selectedFlag=min(mapFlagList)
          selectedDay=(where(selectedFlag eq mapFlagList))[0]
          selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
          ; come back to AVHRR standard
          data_tc_split.flag[idxSea[pix]]=selectedFlag
          ;data_tc_split.flag[idxSea[pix]]=data_day_split[selectedDay].flag[idxSea[pix]]
          data_tc_split.fapar[idxSea[pix]] = data_day_split[selectedDay].fapar[idxSea[pix]]
          data_tc_split.red[idxSea[pix]]=data_day_split[selectedDay].red[idxSea[pix]]
          data_tc_split.day[idxSea[pix]]=data_day_split[selectedDay].day
          data_tc_split.nir[idxSea[pix]]= data_day_split[selectedDay].nir[idxSea[pix]]
          data_tc_split.sigma_red[idxSea[pix]]= data_day_split[selectedDay].sigma_red[idxSea[pix]]
          data_tc_split.sigma_nir[idxSea[pix]]= data_day_split[selectedDay].sigma_nir[idxSea[pix]]
          data_tc_split.sigma[idxSea[pix]]= data_day_split[selectedDay].sigma[idxSea[pix]]
          data_tc_split.toc_red[idxSea[pix]]= data_day_split[selectedDay].toc_red[idxSea[pix]]
          data_tc_split.toc_nir[idxSea[pix]]= data_day_split[selectedDay].toc_nir[idxSea[pix]]
          data_tc_split.ts[idxSea[pix]]=data_day_split[selectedDay].ts[idxSea[pix]]
          data_tc_split.tv[idxSea[pix]]=data_day_split[selectedDay].tv[idxSea[pix]]
          data_tc_split.toc_red[idxSea[pix]]=data_day_split[selectedDay].toc_red[idxSea[pix]]
          data_tc_split.toc_nir[idxSea[pix]]=data_day_split[selectedDay].toc_nir[idxSea[pix]]
        endfor
      endif else begin
        nSeaMx[*]=0
        if cntCloudIce gt 0 then begin
          print, 'clouds:', cntCloudIce
          for pix=0, cntCloudIce-1 do begin
            ; just for "tv" test recycle nSeaMx variable
            nSeaMx[idxCloudIce[pix]]=1
            flagList=data_day_split[*].flag[idxCloudIce[pix]]
            unikFlags=flagList[UNIQ(flagList, SORT(flagList))]
            aa=where(unikFlags eq 21,c)
            ;if n_elements(unikFlags) ne 1 and c eq 0 then stop
            ; work around to map best flag using C/Seawifs approach
            mapFlagList=mapFaparFlag(flagList)
            notBad=where(mapFlagList ne 255, cnt)
            if cnt gt 0 then selectedFlag=max(mapFlagList[notBad]) else selectedFlag=255
            selectedDay=(where(selectedFlag eq mapFlagList))[0]
            selectedFlag=mapFaparFlag(selectedFlag,/REVERT)
            ; come back to AVHRR standard
            ;if selectedFlag ne 1 then begin
            data_tc_split.flag[idxCloudIce[pix]] = selectedFlag
            ;print, 'choose:', selectedFlag
            ;print, 'from:', unikFlags
            ; just for test
            ;data_tc_split.flag[idxCloudIce[pix]] = 7
            ;data_tc_split.flag[idxCloudIce[pix]]=data_day_split[selectedDay].flag[idxCloudIce[pix]]
            data_tc_split.fapar[idxCloudIce[pix]] = data_day_split[selectedDay].fapar[idxCloudIce[pix]]
            data_tc_split.day[idxCloudIce[pix]] = data_day_split[selectedDay].day
            data_tc_split.red[idxCloudIce[pix]]=data_day_split[selectedDay].red[idxCloudIce[pix]]
            data_tc_split.nir[idxCloudIce[pix]]= data_day_split[selectedDay].nir[idxCloudIce[pix]]
            data_tc_split.sigma_red[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_red[idxCloudIce[pix]]
            data_tc_split.sigma_nir[idxCloudIce[pix]]= data_day_split[selectedDay].sigma_nir[idxCloudIce[pix]]
            data_tc_split.sigma[idxCloudIce[pix]]= data_day_split[selectedDay].sigma[idxCloudIce[pix]]
            data_tc_split.toc_red[idxCloudIce[pix]]= data_day_split[selectedDay].toc_red[idxCloudIce[pix]]
            data_tc_split.toc_nir[idxCloudIce[pix]]= data_day_split[selectedDay].toc_nir[idxCloudIce[pix]]
            data_tc_split.ts[idxCloudIce[pix]]=data_day_split[selectedDay].ts[idxCloudIce[pix]]
            data_tc_split.tv[idxCloudIce[pix]]=data_day_split[selectedDay].tv[idxCloudIce[pix]]
            data_tc_split.toc_red[idxCloudIce[pix]]=data_day_split[selectedDay].toc_red[idxCloudIce[pix]]
            data_tc_split.toc_nir[idxCloudIce[pix]]=data_day_split[selectedDay].toc_nir[idxCloudIce[pix]]
            ;endif
          endfor
        endif else begin
          for jj=0, n_elements(notAssigned)-1 do begin
            fList=data_day_split[*].flag[notAssigned[jj]]
            unikFlags=fList[UNIQ(fList, SORT(fList))]
            if n_elements(unikFlags) ne 1 then stop
            if unikFlags ne 1 then stop
            data_tc_split.flag[notAssigned[jj]]=unikFlags
            print, 'undetermined flag-->', unikFlags
          endfor
        endelse
      endelse
    endif
    ;    diffDay=dd-data_tc_split.day
    ;    window, 1
    ;    tvscl, congrid(reform(data_tc_split.day), 72, 360)
    ;    window, 2
    ;    tvscl, congrid(reform(diffDay), 72, 360)
    ;      for jj=0, notAssignedCount-1 do begin
    ;        strangeReasonIdx=where((data_day_split[*].flag[notAssigned[jj]] ne 1) and (data_day_split[*].flag[notAssigned[jj]] ne 2), strangereasonCount)
    ;        if strangereasonCount gt 1 then begin
    ;          print, data_tc_split.fapar[notAssigned[jj]]
    ;          print, data_day_split[*].fapar[notAssigned[jj]]
    ;          print, data_day_split[*].flag[notAssigned[jj]]
    ;        endif
    ;      endfor
    ;      print, '**Flag = ',notAssignedFlag,'!!! (end)***'
    ;    endif


    ;array=data_tc_split.flag
    ;window,3, xsize=72*3, ysize=360*3, title='-->3<--'
    ;window,12,xsize=360, ys 360)
    ;print, array[UNIQ(array, SORT(array))];

    ;tvscl, congrid(data_tc_split.flag, 72, 360)
    ;data_tc_split.nday(idx_two)=2
    ;data_tc_split.flag(idx_two)=4
    ;    bareSday=data_tc_split.flag
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
    prevflag[subXStart:subXEnd, subYStart:subYEnd]=tempFlag
    data_tc.nday[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.nday[*,*]
    data_tc.day[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.day[*,*]

    data_tc.dev_red_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_red_temp[*,*]
    data_tc.dev_nir_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_nir_temp[*,*]
    data_tc.dev_temp[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.dev_temp[*,*]

    data_tc.sigma[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma[*,*]
    data_tc.sigma_red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma_red[*,*]
    data_tc.sigma_nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.sigma_nir[*,*]
    ; map mask water if jrc_flag says "pixel is water" at least 1 time on 2 (50%)....
    ;    if keyword_set(water_jrc) then begin
    ;      ;window,1, xsize=720*2, ysize=360*2, title='Water from JRC_FLAG!!!'
    ;      ;tvscl, congrid(data_tc.flag, 720*2, 360*2)
    ;      waterMask=1.*waterMask/tt[1]
    ;      waterIdxs=where(waterMask ge 0.5, waterCnt)
    ;      if waterCnt gt 0 then data_tc_split.flag[waterIdxs]=3
    ;    endif
    ;wrong=where(data_tc_split.flag[*,*] eq 3 and data_tc_split.fapar[*,*] gt 0.0, cntWrng)
    ;if cntWrng ne 0 then stop


    data_tc.flag[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.flag[*,*]
    data_tc.fapar[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.fapar[*,*]

    data_tc.nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.nir[*,*]
    data_tc.red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.red[*,*]
    ;only for test
    data_tc.ts[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.ts[*,*]
    data_tc.tv[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.tv[*,*]
    data_tc.toc_red[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.toc_red[*,*]
    data_tc.toc_nir[subXStart:subXEnd, subYStart:subYEnd]=data_tc_split.toc_nir[*,*]
    data_tc.faparMean[subXStart:subXEnd, subYStart:subYEnd]=faparMean
    ;end test

    ;window,11, xsize=720, ysize=360, title='FAPAR'
    ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc.fapar*250.0, 720, 360)
    ;
    ;window,12, xsize=720, ysize=360, title='RED'
    ;tv, reverse(congrid(data_tc_split.red*250.0, 720*2, 360*2),2)
    ;tv, congrid(data_tc.red*250.0, 720, 360)
    ;stop
  endfor
  ;
  ;===================================================================================================
  ;
  ;
  ; read from external file land use to mask water....
  ;  if ~keyword_set(water_jrc) then begin
  ;    land=readLandMask()
  ;    waterIdxs=where(land eq 0, waterCnt)
  ;    data_tc.flag[waterIdxs]=3
  ;  endif
  ;window,2, xsize=720*2, ysize=360*2, title='Water from land use!!!'
  ;tvscl, congrid(data_tc.flag, 720*2, 360*2)
  NanDay=where(data_tc.day ne 255, validCountDay)
  ; Add 1 to update 0-based day "index": more readable field
  if validCountDay gt 0 then data_tc.day=data_tc.day+1


  tNames=tag_names(data_tc)
;  prevExc=!EXCEPT
;  !EXCEPT=0
;  for i=0, n_elements(tNames)-1 do begin
;    dsType=size(data_tc.(i), /TYPE)
;    if dsType eq 4 then begin
;      testBand=data_tc.(i)[*,*]
;      goodIndexes=where(finite(testBand) eq 1)
;      setToNanIndexes=where(testBand[goodIndexes] eq 1 and testBand[goodIndexes] lt 0, nanCount)   
;      ;data_tc.(i)[*,*] lt 0, nanCount)
;      if nanCount gt 0 then data_tc.(i)[goodIndexes[setToNanIndexes]]=INT_NAN
;    endif
;  endfor
;  testMath=CHECK_MATH()
;  !EXCEPT=prevExc

  ; check Sahara mistery...


  ;data_tc.fapar[3500:3700, 2100]=0.5

  saharaIndex=where(data_tc.fapar eq 0 and data_tc.flag eq 6, countSahara)
  ;window,11, xsize=720, ysize=360, title='FAPAR'
  ;tv, reverse(congrid(data_tc_split.fapar*250.0, 720*2, 360*2),2)
  ;tv, congrid(data_tc.fapar*250.0, 720, 360)
  ;
  ;window,12, xsize=720, ysize=360, title='RED'
  ;tv, reverse(congrid(data_tc_split.red*250.0, 720*2, 360*2),2)
  ;tv, congrid(data_tc.red*250.0, 720, 360)
  ;stop
end
;========================================================================================================
