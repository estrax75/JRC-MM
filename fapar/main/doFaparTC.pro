FUNCTION doFaparTC, sensor, resolution, missionName, confDir, sourceDir, tempDir, outputDir, year, month, MONTHLY=MONTHLY, TENDAYS=TENDAYS
  ;
  ;
  COMMON bigData

  if ~obj_valid(operatorObj) then operatorObj=obj_new('GenericOperator')
  if ~obj_valid(fsObj) then fsObj=obj_new('FileSystem', /STAND)
  if ~obj_valid(utils) then utils=obj_new('Utility')
  ;
  ;
  ;dir_in='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR/'
  ;dirout='/net/netsea2/vol/vol06/data/projects/QA4ECV/WP4/'+sensor+'/FAPAR_TC/'
  outputDir=fsObj->adjustDirSep(outputDir, /ADD)
  sourceDir=fsObj->adjustDirSep(sourceDir, /ADD)
  tempDir=fsObj->adjustDirSep(tempDir, /ADD)
  print, 'working on', year, month, '...'
  dir_in=sourceDir
  dirout=outputDir
  IF KEYWORD_SET(TENDAYS) then begin
    first=[01,11,21]
    last=[10,20,utils->calcDayOfMonth([year,month,1,0])]
  endif
  IF KEYWORD_SET(MONTHLY) then begin
    first=[01]
    last=[utils->calcDayOfMonth([year,month,1,0])]
  endif
  
  ;for m=0, N_elements(month)-1 do begin
  ;
  ; look for files within the month
  ;
  ;


  ;file_name=sensor+'_'+platform+'_'+year+month(m)+'*'
  ;end_file='L2_MUL_000009_900S900N1800W1800E_PLC_0005D_PRO.HDF'
  ;expidfile = dir_in + file_name+end_file
  missionCode=getAVHRRNOAANumber(year, undef)
  ;
  ;monthDays=utility->calcDayOfMonth([y,m,1,0])
  ;monthDays=6
  d=1
  if sensor eq 'AVH09C1' then new_file=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, 1, missionName, missionCode, mainVarName)
  if sensor eq 'MODIS' then new_file=buildMODISFAPARFileName_D(sensor, resolution, year, month, 1, missionName, missionCode, mainVarName)

  ncfilename=fsObj->addFileExtension(new_file, 'NC'); nc
  hdffilename=fsObj->addFileExtension(new_file, 'HDF'); hdf

  ff1 = FILE_SEARCH(dir_in+ncfilename, COUNT=cnt1);, /FULL_QUALIFY)
  ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
  faparData=readFAPAR(dir_in, ncfilename, FOUND=FOUND)
  faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
  faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
  
  for type=0, n_elements(first)-1 do begin
    if sensor eq 'AVH09C1' then destTCFile=buildAVHRRFAPARFileName_TC(sensor, resolution, year, month, d, missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
    if sensor eq 'MODIS' then destTCFile=buildMODISFAPARFileName_TC(sensor, resolution, year, month, d, missionName, missionCode, mainVarName, startDay=first[type], endDay=last[type])
    expectedDays=last[type]-first[type]+1
    print, 'building file: ', destTCFile, '...'
    foundDays=0
    ;tcStruct=ptrarr(expectedDays)
    data_day=replicate(faparData, expectedDays)
    for d=first[type], last[type] do begin

      ncfilename=fsObj->addFileExtension(new_file, 'NC'); nc
      hdffilename=fsObj->addFileExtension(new_file, 'HDF'); hdf

      ff1 = FILE_SEARCH(dir_in+ncfilename, COUNT=cnt1);, /FULL_QUALIFY)
      ff2 = FILE_SEARCH(dir_in+hdffilename, COUNT=cnt2)
      
      ; read files requested in the period
      ;
      faparData=readFAPAR(dir_in, ncfilename, FOUND=FOUND)
      if keyword_set(FOUND) then begin
        foundDays++
        faparData.nir=1.*faparData.nir*faparData.slope_nir+faparData.offset_nir
        faparData.red=1.*faparData.red*faparData.slope_red+faparData.offset_red
        data_day[d-first[type]]=faparData
        faparData=0
      endif
      ;val_time=replicate(val_day, N_elements(ff_time))
      ;call_read_all, sensor, platform, year, month, sourceDir, ff1, val_time
      ;call_read_all, sensor, platform, year, month(m), dir_in, ff(id_file), val_time
      ;
      ; call time composite program
      ;stop
      ;endfor
      ;

    endfor
    daysInfo=[expectedDays,foundDays]
    call_composite, val_comp
    ;
    print, 'writing file: ', destTCFile, '...'

    new_file=sensor+'_'+platform+'_'+year+month(m)+first(t)+'000000_'+$
      year+month(m)+last(t)+'000000_'+$
      'L3_MUL_000009_900S900N1800W1800E_PLC_0005D_PRO.HDF'
    ; save the results
    ;
    ZEROISNAN=keyword_set(TYPE1) ;0
    ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
    ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
    ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
    remarkableFlags=[255,254,253]
    if keyword_set(ZEROISNAN) then begin
      DATA_NAN=255
      BYTE_NAN=0
      BYTE_RANGE=[1,255]
      remarkableFlags[*]=BYTE_NAN
    endif else begin
      DATA_NAN=255
      BYTE_NAN=255
      BYTE_RANGE=[0,250]
    endelse

    res=dataByteScaling(output.fpar, output.flag, FLAG_VALUES=[9,10], $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    output.fpar=res.resultData
    output.flag=res.resultFlag

    res=dataByteScaling(output.sigma, output.flag, $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    output.sigma=res.resultData
    output.flag=res.resultFlag

    res=dataByteScaling(output.red, output.flag, FLAG_VALUES=[9,10], $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    ;output.red=res.resultData
    output.flag=res.resultFlag

    res=dataByteScaling(OUTPUT.SIGMA_RED, output.flag, $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    ;output.sigma_red=res.resultData
    output.flag=res.resultFlag

    res=dataByteScaling(output.nir, output.flag, FLAG_VALUES=[13,14], $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    ;output.nir=res.resultData
    output.flag=res.resultFlag

    res=dataByteScaling(output.sigma_nir, output.flag, $
      DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
      DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE)
    ;output.sigma_nir=res.resultData
    output.flag=res.resultFlag

    flagTags=strupcase(['fpar', 'sigma'])
    tags=tag_names(output)

    for i=0, n_elements(flagTags)-1 do begin
      thisIdx=(where(flagTags[i] eq tags, count))[0]
      if count eq 1 then begin
        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_1, remarkableFlags[0])
        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_2, remarkableFlags[1])
        output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_3, remarkableFlags[2])
      endif
    endfor

    ;map -9999 on int data
    flagTags=strupcase(['red', 'nir', 'sigma_red', 'sigma_nir'])
    for i=0, n_elements(flagTags)-1 do begin
      thisIdx=(where(flagTags[i] eq tags, count))[0]
      nanIdxs=where(output.(thisIdx) gt 250, count)
      output.(thisIdx)=mapQualityFlags(output.(thisIdx), nanIdxs, INT_NAN)
    endfor

    ;
    ;if idx_4(0) ge 0 then img(idx_4)=-9996.0
    ;
    ;if idx_5(0) ge 0 then img(idx_5)=-9995.0
    ;
    ;
    ;

    print,'Write the results in ',outputDir+new_file
    ;
    ;
    ;
    ;
    ;idx_4 = where (flag_angles eq 1)
    ;idx_5 = where (flag_angles eq 2)
    ;
    ;
    ;if idx_3(0) ge 0 then img(idx_3)=0.0
    ;
    ;if idx_2(0) ge 0 then img(idx_2)=-9998.0
    ;
    ;if idx_1(0) ge 0 then img(idx_1)=-9997.0
    ;
    ;if idx_4(0) ge 0 then img(idx_4)=-9996.0
    ;
    ;if idx_5(0) ge 0 then img(idx_5)=-9995.0
    ;
    ;
    ; create output file
    ;
    ;
    sdid_day = HDF_SD_CREATE(sdid_outfile1, 'DOY', [a(1),a(2)], /byte)
    sdid_nday = HDF_SD_CREATE(sdid_outfile1, 'Number of Day', [a(1),a(2)], /byte)
    sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a(1),a(2)], /byte)
    sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a(1),a(2)], /float)
    sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a(1),a(2)], /float)
    sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a(1),a(2)], /byte)

    faparTCDSInfo=getStandardFaparTCDataSetInfo()
    bandNames=faparTCDSInfo.bandNames

    bandMeasaureUnits=faparTCDSInfo.bandMeasureunits
    dims=size(output.fpar, /DIMENSIONS)
    if keyword_set(FIRST_LOOK) then begin
      fLookDir='first_look'
      ;cd, dirout
      firstLookDir=outputDir+fLookDir
      fInfo=file_info(fLookDir)
      if ~(fInfo.exists) then file_mkdir, firstLookDir
      sampleImg=rebin(output.fpar, dims[0]/10,dims[1]/10)
      minvalue=min(output.fpar, max=maxvalue)
      sampleImg=bytscl(sampleImg)
      samplefilename='fl_'+new_file+'.gif'
      fullSampleFName=firstLookDir+path_sep()+samplefilename
      LOADCT, 14
      print, 'sampleImage-->', fullSampleFName
      write_gif, fullSampleFName, sampleImg
    endif

    ;  dataSets=[ptr_new(output.fpar, /NO_COPY), ptr_new(output.sigma, /NO_COPY), $
    ;    ptr_new(output.red, /NO_COPY), ptr_new(output.sigma_red, /NO_COPY), $
    ;    ptr_new(output.nir, /NO_COPY), ptr_new(output.sigma_nir, /NO_COPY), $
    ;    ptr_new(output.flag, /NO_COPY), $
    ;    ptr_new(reform(angles[*,*,0]), /NO_COPY), ptr_new(reform(angles[*,*,1]), /NO_COPY), ptr_new(reform(angles[*,*,2]), /NO_COPY), $
    ;    ptr_new(reform(reflectance(*,*,0)), /NO_COPY), ptr_new(reform(reflectance(*,*,1)), /NO_COPY), $
    ;    ptr_new(MASK_avhrr, /NO_COPY)]
    maskMin=min(MASK_avhrr, max=maskMax)
    dataSets=[ptr_new(output.fpar, /NO_COPY), ptr_new(output.sigma, /NO_COPY), $
      ptr_new(output.red, /NO_COPY), ptr_new(output.sigma_red, /NO_COPY), $
      ptr_new(output.nir, /NO_COPY), ptr_new(output.sigma_nir, /NO_COPY), $
      ptr_new(output.flag, /NO_COPY), $
      ptr_new(reform(angles[*,*,0]), /NO_COPY), ptr_new(reform(angles[*,*,1]), /NO_COPY), ptr_new(reform(angles[*,*,2]), /NO_COPY), $
      ptr_new(reform(reflectance(*,*,0)), /NO_COPY), ptr_new(reform(reflectance(*,*,1)), /NO_COPY), $
      ptr_new(MASK_avhrr, /NO_COPY)]

    bandSlopes=[1, 1, $
      10e-05, 10e-05, $
      10e-05, 10e-05, $
      1, $
      10e-03, 10e-03, 10e-03,$
      10e-05, 10e-05,$
      1]

    bandDataType=[1,1,$
      2,2,$
      2,2,$
      1,$
      2,2,2, $
      2,2,$
      1]

    bandIntercepts=lonarr(n_elements(bandNames))

    minMaxs=fltarr(n_elements(bandDataType), 2)
    nanList=fltarr(n_elements(bandDataType))

    minMaxs[*,*]=-1
    minMaxs[0,*]=DATA_RANGE;minMax[0,*]
    nanList[0]=BYTE_NAN

    minMaxs[1,*]=DATA_RANGE;minMax[0,*]
    nanList[1]=BYTE_NAN

    minMaxs[2,*]=DATA_RANGE;minMax[0,*]
    nanList[2]=INT_NAN

    minMaxs[3,*]=DATA_RANGE;minMax[0,*]
    nanList[3]=INT_NAN

    minMaxs[4,*]=DATA_RANGE;minMax[0,*]
    nanList[4]=INT_NAN

    minMaxs[5,*]=DATA_RANGE;minMax[0,*]
    nanList[5]=INT_NAN

    tempMin=min(output.flag, max=tempMax)
    minMaxs[6,*]=[tempMin, tempMax]
    nanList[6]=INT_NAN

    tempMin=min(angles(*,*,0), max=tempMax)
    minMaxs[7,*]=[0.,90.]
    nanList[7]=INT_NAN

    tempMin=min(angles(*,*,1), max=tempMax)
    minMaxs[8,*]=[0.,90.]
    nanList[8]=INT_NAN

    tempMin=min(angles(*,*,2), max=tempMax)
    minMaxs[9,*]=[-180,180]
    nanList[9]=INT_NAN

    tempMin=min(reflectance(*,*,0), max=tempMax)
    minMaxs[10,*]=[tempMin>0.,tempMax]
    nanList[10]=INT_NAN

    tempMin=min(reflectance(*,*,1), max=tempMax)
    minMaxs[11,*]=[tempMin>0.,tempMax]
    nanList[11]=INT_NAN


    minMaxs[12,*]=[maskMin, maskMax]
    nanList[12]=INT_NAN

    boundary=[-180.0, 180.0, -90, 90.]
    filePath=outputDir
    fName=new_file

    write_georef_ncdf, ncfilename, $
      bandNames, bandMeasaureUnits, $
      dataSets, bandDataType, bandIntercepts, bandSlopes, tempDir, boundary, $
      /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList

    ;  if keyword_set(ZEROISNAN) then begin
    ;    bandSlopes[0]=1./255
    ;    bandSlopes[1]=bandSlopes[0]
    ;    bandIntercepts[0]=0.
    ;    bandIntercepts[1]=bandIntercepts[0]
    ;    minMaxs[0,*]=[1,255]
    ;    minMaxs[1,*]=minMaxs[0,*]
    ;;    DATA_NAN=255
    ;;    BYTE_NAN=0
    ;  endif else begin
    ;    bandSlopes[0]=1./250
    ;    bandSlopes[1]=bandSlopes[0]
    ;    bandIntercepts[0]=0.
    ;    bandIntercepts[1]=bandIntercepts[0]
    ;    minMaxs[0,*]=[0,250]
    ;    minMaxs[1,*]=minMaxs[0,*]
    ;;    DATA_NAN=255
    ;;    BYTE_NAN=255
    ;;    BYTE_RANGE=[0,250]
    ;  endelse

    write_hdf, hdffilename, $
      bandNames, bandMeasaureUnits, $
      dataSets, bandDataType, bandIntercepts, bandSlopes, tempDir, boundary, $
      trueMinMaxs=minMaxs, nanList=nanList


    ;
    ;PPMSA_ALBEDOCOLOR
    ;window, 5, xsize=720*2, ysize=360*2, title='Rectified nir '+SENSOR
    ;tv, reverse(congrid(bytscl(output.nir,min=0.,max=0.8), 720*2,360*2),2)

    ;window, 6, xsize=720*2, ysize=360*2, title='Rectified Red '+SENSOR
    ;tv, reverse(congrid(bytscl(output.red, min=0.,max=0.8), 720*2,360*2),2)

    ;faparcolor
    ;window, 7, xsize=720*2, ysize=360*2, title='FAPAR '+SENSOR
    ;tv, reverse(congrid(output.fpar*250.0, 720*2,360*2),2)

    ;stop

    ;

    ;

    ;stop
    ;

    sdid_outfile1 = HDF_SD_START(dirout+new_file, /CREATE)
    a=size(val_comp.fapar)
    sdid_fpar = HDF_SD_CREATE(sdid_outfile1, 'FAPAR', [a(1),a(2)], /float)
    sdid_uncert = HDF_SD_CREATE(sdid_outfile1, 'Sigma FAPAR', [a(1),a(2)], /float)
    sdid_delta = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation FAPAR', [a(1),a(2)], /float)
    sdid_red = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED RED', [a(1),a(2)], /float)
    sdid_uncert_red = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED RED', [a(1),a(2)], /float)
    sdid_delta_red = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation Red', [a(1),a(2)], /float)
    sdid_nir = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED NIR', [a(1),a(2)], /float)
    sdid_uncert_nir = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED NIR', [a(1),a(2)], /float)
    sdid_delta_nir = HDF_SD_CREATE(sdid_outfile1, 'Temporal Deviation NIR', [a(1),a(2)], /float)
    sdid_day = HDF_SD_CREATE(sdid_outfile1, 'DOY', [a(1),a(2)], /byte)
    sdid_nday = HDF_SD_CREATE(sdid_outfile1, 'Number of Day', [a(1),a(2)], /byte)
    sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a(1),a(2)], /byte)
    sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a(1),a(2)], /float)
    sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a(1),a(2)], /float)
    sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a(1),a(2)], /byte)
    ;
    ; data_tc= {Composite, day: bytarr(7200,3600), $
    ;   nday: bytarr(7200,3600), $
    ;   fapar: fltarr(7200,3600), $
    ;   dev_temp: fltarr(7200,3600), $
    ;   sigma: fltarr(7200,3600), $
    ;   red: fltarr(7200,3600), $
    ;   dev_red_temp: fltarr(7200,3600), $
    ;   sigma_red:fltarr(7200,3600), $
    ;   nir: fltarr(7200,3600), $
    ;   dev_nir_temp: fltarr(7200,3600), $
    ;     sigma_nir: fltarr(7200,3600), $
    ;         flag: bytarr(7200,3600), $
    ;   toc_red: fltarr(7200,3600), $
    ;   toc_nir: fltarr(7200,3600), $
    ;   qa: bytarr(7200,3600)}
    ;
    HDF_SD_ADDDATA, sdid_fpar, val_comp.fapar
    HDF_SD_ADDDATA, sdid_uncert, val_comp.sigma
    HDF_SD_ADDDATA, sdid_delta, val_comp.dev_temp
    HDF_SD_ADDDATA, sdid_red, val_comp.red
    HDF_SD_ADDDATA, sdid_uncert_red, val_comp.sigma_red
    HDF_SD_ADDDATA, sdid_delta_red, val_comp.dev_red_temp
    HDF_SD_ADDDATA, sdid_nir, val_comp.nir
    HDF_SD_ADDDATA, sdid_uncert_nir, val_comp.sigma_nir
    HDF_SD_ADDDATA, sdid_delta_nir, val_comp.dev_nir_temp
    HDF_SD_ADDDATA, sdid_day, val_comp.day
    HDF_SD_ADDDATA, sdid_nday,val_comp.nday
    HDF_SD_ADDDATA, sdid_flag, val_comp.flag
    HDF_SD_ADDDATA, sdid_redtoc, val_comp.toc_red
    HDF_SD_ADDDATA, sdid_nirtoc, val_comp.toc_nir
    HDF_SD_ADDDATA, sdid_qa, val_comp.qa
    HDF_SD_END, sdid_outfile1
  endfor
  ;
  ;
end