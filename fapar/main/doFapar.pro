;function makeitglob_new, sensor, noaanumber, year, month, day
;@../../Library/library/jrc_core/mapQualityFlags
;@../../Library/library/jrc_core/mapQualityFlags
function doFapar, instrument, indicator, spatialResolution, level, missionName, mainVarName, missionCode, year, month, day, $
  sourceDir, outputDir, tempdir, $
  FIRST_LOOK=FIRST_LOOK, $
  TYPE1=TYPE1, TYPE2=TYPE2, NC=NC, HDF=HDF, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  OVERWRITE=OVERWRITE, TC_TYPE=TC_TYPE

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;NaN=-9999 ;!VALUES.F_NAN
  outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
  sourceDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')

  ;old version
  ;sourceFileName=buildBrfFileName_D(sensor, resolution, year, month, day, missionName, missionCode, mainVarName)
  ;if n_elements(sourceFileName) gt 1 then sourceFileName=sourceFileName[MISSIONOVERLAPINDEX]
  ;if sensor eq 'AVH09C1' then new_file=buildAVHRRFAPARFileName_D(sensor, resolution, year, month, day, missionName, missionCode, mainVarName, level, VERSION='01')
  ;if sensor eq 'MODIS' then new_file=buildMODISFAPARFileName_D(sensor, resolution, year, month, day, missionName, missionCode, mainVarName, level)
  ;end
  ; new filename convention
  ;instrument='AVH'
  version='N'+string(missionCode, format='(I02)');version='001'

  sourceFileInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
    product, version, 'NC',  indicator=indicator, level, projection=projection)

  ;if n_elements(sourceFileName) gt 1 then sourceFileName=sourceFileName[MISSIONOVERLAPINDEX]
  ;sourceFileName=buildBrfFileName_D(sensor, resolution, year, month, day, missionName, missionCode, mainVarName)
  if instrument eq 'AVH' then begin
    ncFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
      product, version, 'NC',  indicator=indicator, level, projection=projection)
    hdfFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
      product, version, 'HDF',  indicator=indicator, level, projection=projection)
  endif
  sourceDir=sourceDir+sourceFileInfo.filePath
  sourceDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  sourceFileName=sourceDir+sourceFileInfo.fileName

  destDir=outputDir+hdfFileInfo.filePath
  destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  resFileNC=destDir+ncFileInfo.fileName
  resFileHDF=destDir+hdfFileInfo.fileName

  sourceFullFileName = (file_search(sourceDir, sourceFileInfo.fileName, COUNT=count))[0];, /FULL_QUALIFY)
  ;  if count eq 0 then begin
  ;    sourceFullFileName = (file_search(sourceDir+TC_TYPE, sourceFileNC, COUNT=count))[0];, /FULL_QUALIFY)
  ;  endif
  ;  
  ;  sourceDir=filePath

  checkNC=file_info(resFileNC)
  checkHDF=file_info(resFileHDF)

  if count ne 1 then NOSOURCEAVAILABLE=1
  ;print, '******'
  ;print, 'write nc?', (checkOutput1[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(NC)
  ;print, 'write hdf?', (checkOutput2[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(HDF)
  ;print, '******'
  if keyword_set(NC) and ((checkNC[0].size eq 0) or keyword_set(OVERWRITE)) then NOWRITENC=0 else NOWRITENC=1
  if keyword_set(HDF) and ((checkHDF[0].size ne 0) or keyword_set(OVERWRITE)) then NOWRITEHDF=0 else NOWRITEHDF=1
  ;if (checkOutput1[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(NC) then NOWRITENC=1
  ;if (checkOutput2[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(HDF) then NOWRITEHDF=1
  ;if checkHDF[0].size ne 0 and ~keyword_set(OVERWRITE) then NONCWRITE=1

  if keyword_set(NOSOURCEAVAILABLE) or (keyword_set(NOHDFWRITE) and keyword_set(NOHDFWRITE)) then return, -1

  data=readBRF(sourceDir, sourceFileInfo.fileName, FOUND=FOUND)

  if ~keyword_set(FOUND) then return, -1

  INT_NAN=-9999
  DATA_RANGE=[0., 1.]
  DATA_NAN=255

  output={  ExpId_t, $
    fpar: fltarr(7200,3600), $
    sigma: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    sigma_red:fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    sigma_nir: fltarr(7200,3600), $
    flag: bytarr(7200,3600), $
    toc_red: fltarr(7200,3600), $
    toc_nir: fltarr(7200,3600), $
    qa: bytarr(7200,3600),$
    sza: bytarr(7200,3600)}
  ;
  ; initialization
  ;
  output.fpar(*,*)=255
  output.sigma(*,*)=255
  output.red(*,*)=255
  output.sigma_red(*,*)=255
  output.nir(*,*)=255
  output.sigma_nir(*,*)=255
  noaanumber=missionCode

  red_avhrr=data.red_avhrr & slope_red=data.slope_red & offset_red=data.offset_red
  nir_avhrr=data.nir_avhrr & slope_nir=data.slope_nir & offset_nir=data.offset_nir
  ts_avhrr=data.ts_avhrr & slope_ts=data.slope_ts & offset_ts=data.offset_ts
  tv_avhrr=data.tv_avhrr & slope_tv=data.slope_tv & offset_tv=data.offset_tv
  phi_avhrr=data.phi_avhrr & slope_phi=data.slope_phi & offset_phi=data.offset_phi
  qa_avhrr=data.brdf_qa_avhrr & slope_brdf_qa=data.slope_qa & offset_brdf_qa=data.offset_qa
  ;save memory
  data=0
  ;


  sizemat=size(red_avhrr)
  ;
  ; START
  ;
  ;
  reflectance=DBLARR(sizemat(1),sizemat(2),2)
  ;
  reflectance(*,*,0)=red_avhrr(*,*)*slope_red(0)+offset_red(0)
  reflectance(*,*,1)=nir_avhrr(*,*)*slope_nir(0)+offset_nir(0)
  ;
  angles=DBLARR(sizemat(1),sizemat(2),4)
  ;
  ;
  ; verify WITH eRIC AND mARTIN IF CORRECT
  ;
  angles(*,*,0)=ts_avhrr(*,*)*slope_ts(0)+offset_ts(0)
  angles(*,*,1)=tv_avhrr(*,*)*slope_tv(0)+offset_tv(0)
  ;
  output.sza=ts_avhrr(*,*)
  ; correction already applied
  ;RELAZ=phi_avhrr(*,*)*slope_phi(0)+offset_phi(0)
  ;SIN_REL_AZ = sin(RELAZ*!pi/180.)
  ;COS_REL_AZ = cos(RELAZ*!pi/180.)
  ;GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
  ;angles(*,*,2)=good_rel_az*180./!pi
  phi_avhrr=phi_avhrr*slope_phi(0)+offset_phi(0)
  angles(*,*,2)=phi_avhrr
  ;
  ;
  ;
  angles(*,*,3)=0.0
  ;
  ;
  flagstoc= bytarr(7200, 3600)

  red = [0,0,1,0,0.66,0.68,0.,0.55,0.00,0.00,0.85,0,00,0.]
  gre = [1,0,0,0,0.00,0.00,0.,0.55,0.77,0.66,0.00,0.00,0.]
  blu = [0,0,0,1,0.00,1.00,1.,1.00,1.00,0.55,0.80,0.77,0.]
  TVLCT, red*255, gre*255, blu*255

  coeffInfo=getSensorCoeffs(instrument, missionCode)

  flag2 = CheckDataTOC(red_avhrr*slope_red(0)+offset_red(0), nir_avhrr*slope_nir(0)+offset_nir(0), coeffInfo.soilCoeffs)
  flagstoc=flag2
  ;
  ;stop
  ;
  ;flag_angles = CheckAngle(angles(*,*,0), angles(*,*,1))
  ;window, 10, xsize=720, ysize=360, title='FPAR Angles'+SENSOR
  ;tv, reverse(congrid(flag_angles, 720,360),2)
  ;

  red_avhrr=0.
  nir_avhrr=0.
  ts_avhrr=0.
  tv_avhrr=0.
  phi_avhrr=0.
  ;
  ;=======================================================================
  ;
  ;	AOT
  ;
  ;=========================================================================
  ;read_data, dirroot, file_eric,'AOT', AOT, slope_aot, offset_aot

  ;window, 12, xsize=720, ysize=360, title='AAOT '+SENSOR
  ;tv, reverse(congrid(bytscl(aot*slope_aot(0)+offset_aot(0), min=0., max =0.8), 720,360),2)

  ;stop
  ;
  ;	0      Band 1 BRDF corrected;
  ;	       1 -- yes
  ;	       0 -- no
  ;	5      Band 2 BRDF corrected;
  ;	       1 -- yes
  ;	       0 -- no
  ;
  ;
  ;=======================================================================================

  rrq1=cgi_map_bitwise_flag(qa_avhrr,0)
  rrq2=cgi_map_bitwise_flag(qa_avhrr,5)
  ;
  idx_nocorr=where (rrq1 eq 0 or rrq2 eq 0)
  ;window,0, xsize=720*2, ysize=360*2
  ;tvscl, reverse(congrid(rrq1, 720*2, 360*2), 2)
  ;stop

  ;=====================================================================================
  ;
  ;
  ;1      Pixel is cloudy;
  ;   		1 -- yes
  ;	       0 -- no

  ;rr1=cgi_map_bitwise_flag(qc_avhrr,1)
  rr1=cgi_map_bitwise_flag(qa_avhrr,1)
  ;
  ;
  ; 2      Pixel contains cloud shadow;
  ;	       1 -- yes
  ;	       0 -- no
  ;
  ;rr2=cgi_map_bitwise_flag(qc_avhrr,2)
  rr2=cgi_map_bitwise_flag(qa_avhrr,2)
  ;
  ;  9 channel 2 value is invalid 1 = yes, 0 = no
  ;  8 Channel 1 value is invalid 1 = yes, 0 = no
  ;
  ;rr21=cgi_map_bitwise_flag(qc_avhrr,9)
  ;rr22=cgi_map_bitwise_flag(qc_avhrr,8)
  rr21=cgi_map_bitwise_flag(qa_avhrr,9)
  rr22=cgi_map_bitwise_flag(qa_avhrr,8)
  ;
  ;
  idxbad=where(rr21 eq 1 or rr22 eq 1)
  ;
  ; 3      Pixel is over water;
  ;	       1 -- yes
  ;	       0 -- no
  ;rr3=cgi_map_bitwise_flag(qc_avhrr,3)
  rr3=cgi_map_bitwise_flag(qa_avhrr,3)
  ;
  ;
  ;
  ;
  idx_mask = where(rr1 eq 1) ; or rr2 eq 1)		;----> cloud
  idx_mask2 = where(rr21 eq 1 or rr22 eq 1)		;----> invalid
  IDX_SEA=  where(rr3 eq 1)

  flag2(idxbad)=7.0		;	< bad


  S=SIZE(FLAG2)
  MASK_AVHRR=BYTARR(S(1),S(2))
  ;
  MASK_AVHRR(IDX_MASK)=2		;----> cloud
  ;MASK_AVHRR(IDX_MASK2)=2
  ;MASK_AVHRR(idx_nocorr)= 130
  ;flag2(idx_mask)=2.0

  MASK_AVHRR(IDX_SEA)= 3
  ;flag2[idx_mask]=2
  flag2[idx_sea]=3.0
  ;window, 11, xsize=720, ysize=360, title='QA '+SENSOR
  ;tv, reverse(congrid(MASK_avhrr, 720,360),2)

  ;MASK_AVHRR(idx_nocorr)= 0
  ;window, 11, xsize=720*2, ysize=360*2, title='QA '+SENSOR
  ;tv, reverse(congrid(MASK_avhrr, 720*2,360*2),2)
  ;stop
  ;
  ;window, 12, xsize=720*2, ysize=360*2, title='FLAG '+SENSOR
  ;tv, reverse(congrid(flag2, 720*2,360*2),2)
  ;
  ;stop
  ;
  ;

  if instrument eq 'AVH' then begin
    ;if sensor eq 'AVHRR' then begin
    sza=angles[*,*,0] ;/180.*!pi
    vza=angles[*,*,1] ;/180.*!pi
    saa=angles[*,*,2] ;/180.*!pi
    vaa=angles[*,*,3] ;/180.*!pi
    ;
    ;
    red=reflectance(*,*,0)
    nir=reflectance(*,*,1)

    ;
    ; use nasa formulae for toc band ... 0.05ρ+0.005
    ;
    D_BRF_ToA_RED=2.*(red*0.05+0.005)
    D_BRF_ToA_NIR=2.*(nir*0.05+0.005)
    ;
    ;
    ;
    ; make the process only over vegetated areas
    ;
    idx=where(flag2 eq 0)
    ;NOAString=strcompress(noaanumber, /REMOVE)
    NOAString=string(noaanumber, format='(I02)')
    ;window,4, title='sza '+extraName
    ;tv, rebin(bytscl(sza), 720, 360)
    ;window,5, title='vza '+extraName
    ;tv, rebin(bytscl(vza), 720, 360)
    if idx(0) ge 0 then begin

      FAPAR,'AVHRR', NOAString,sza(idx),vza(idx),saa(idx),vaa(idx),$
        red(idx),red(idx),nir(idx),$
        D_BRF_ToA_RED(idx),D_BRF_ToA_RED(idx),D_BRF_ToA_NIR(idx), $
        rhoRED, rhoNIR, D_rhoRED, D_rhoNIR, $
        D_rhotildeBLUE, D_rhotildeRED, D_rhotildeNIR, VI, D_VI, /TOC,$
        MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX
      ;
      ;window,6, title='fapar '+extraName
      output.fpar(idx)=vi
      ;tv, rebin(bytscl(output.fpar), 720, 360)
      ;if extraname eq '' then begin
      ;  fpr=output.fpar
      ;  save, fpr, filename='fpr'+extraName+'.sav'
      ;endif else begin
      ;  fpr_switch=output.fpar
      ;  save, fpr_switch, filename='fpr'+extraName+'.sav'
      ;endelse
      ;stop
      output.sigma(idx)=d_vi
      output.red(idx)=rhoRED
      output.nir(idx)=rhoNIR
      output.sigma_red(idx)=D_rhoRED
      output.sigma_nir(idx)=D_rhoNIR
      ;
      ;
      ;
      ;window,3
      ;plot, [0.,1.],[0.,1.], psym=1, yr=[0.,1.], xr=[0.,1.]
      ;for k=0, N_elements(idx)-1, 50 do plots, red(idx(k)), nir(idx(k)), psym=4, col=output.fpar(idx(k))*250.0
      ;stop

      idx_neg=where(output.fpar le 0.0)
      output.fpar(idx_neg)=0.0

      idx_big=where(output.fpar ge 1.0 and output.fpar le 10.0)
      output.fpar(idx_big)=1.0
      flag2(idx_neg)=4.0
      flag2(idx_big)=6.0
      ;FAPARCOLOR
      ;window, 0, xsize=720*2, ysize=360*2, title='FPAR '+SENSOR
      ;tv, reverse(congrid(output.fpar*250, 720*2,360*2),2)
      ;
      ;window, 1, xsize=720*2, ysize=360*2, title='New FLAG '+SENSOR
      ;tvscl, reverse(congrid(flag2, 720*2,360*2),2)
      ;stop
      ;
      ;	window, 1, xsize=720, ysize=360, title='Dev FPAR '+SENSOR
      ;    	tv, reverse(congrid(output.sigma*2.*250., 720,360),2)
      ;
      ;	window, 2, xsize=720, ysize=360, title='Rectified Red '+SENSOR
      ;    	tv, reverse(congrid(bytscl(output.sigma_red, min=0., max=0.1), 720,360),2)
      ;


      ;img=fltarr(7200,3600)
      ;img(idx)=rhoRED
      ;
      ;	window, 2, xsize=720, ysize=360, title='Rectified Red '+SENSOR
      ;    	tvscl, reverse(congrid(img, 720,360),2)
      ;
      ;
    endif
  endif
  ;
  ;
  ;stop
  ; make the process of rectifed only over bare soil
  ;
  idx_soil=where(flag2 eq 4.0)

  if idx_soil(0) ge 0 then begin
    ;
    ;
    ; BUG HERE AS DEPEND ON ANGLES ...
    ;
    sza=angles[*,*,0] ;/180.*!pi
    vza=angles[*,*,1] ;/180.*!pi
    saa=angles[*,*,2] ;/180.*!pi
    vaa=angles[*,*,3] ;/180.*!pi


    Rectified,'AVHRR', NOAString,sza(idx_soil),vza(idx_soil),saa(idx_soil),vaa(idx_soil),$
      red(idx_soil),red(idx_soil),nir(idx_soil),$
      D_BRF_ToA_RED(idx_soil),D_BRF_ToA_RED(idx_soil),D_BRF_ToA_NIR(idx_soil), $
      rhoRED_S, rhoNIR_S, D_rhoRED_S, D_rhoNIR_S, $
      D_rhotildeBLUE_S, D_rhotildeRED_S, D_rhotildeNIR_S, vi_s, d_vis, /TOC
    ;;
    ; flag = 3 overwrite
    output.fpar(idx_soil)=0.0
    output.sigma(idx_soil)=0.0
    output.red(idx_soil)=rhoRED_S
    output.nir(idx_soil)=rhoNIR_S
    output.sigma_red(idx_soil)=D_rhoRED_S
    output.sigma_nir(idx_soil)=D_rhoNIR_S
    ;
    ;img=fltarr(7200,3600)
    ;img(idx_soil)=rhoRED_S
    ;

    ;
    ;stop
  endif
  ;
  ;PPMSA_ALBEDOCOLOR
  ;	window, 4, xsize=720, ysize=360, title='Rectified Red '+SENSOR
  ;    	tv, reverse(congrid(bytscl(output.red, min=0., max=0.6), 720,360),2)
  ; overwrite with sea flag layer?!?
  flag2(idx_sea)=3
  output.flag=flag2
  ;
  idx_1 = where (flag2 eq 1)
  idx_2 = where (flag2 eq 2)
  idx_3 = where (flag2 eq 3)

  ;
  ;idx_4 = where (flag_angles eq 1)
  ;idx_5 = where (flag_angles eq 2)
  ;
  ;
  ;, ZEROISNAN=ZEROISNAN, VALUE_BYTES=VALUE_BYTES, DATA_MIN=DATA_MIN, DATA_MAX=DATA_MAX, DATA_NAN_RANGE=DATA_NAN_RANGE
  ZEROISNAN=keyword_set(TYPE1) ;0
  TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2) ;0
  bSInfo=getByteScalingSetting(ZEROISNAN=keyword_set(TYPE1), TWO51_TO_TWO55_ISNAN=keyword_set(TYPE2))
  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
  remarkableFlags=bSInfo.remarkableFlags
  DATA_NAN=bSInfo.DATA_NAN
  BYTE_NAN=bSInfo.BYTE_NAN
  BYTE_RANGE=bSInfo.BYTE_RANGE
  ;  if keyword_set(ZEROISNAN) then begin
  ;    DATA_NAN=255
  ;    BYTE_NAN=0
  ;    BYTE_RANGE=[1,255]
  ;    remarkableFlags[*]=BYTE_NAN
  ;  endif else begin
  ;    DATA_NAN=255
  ;    BYTE_NAN=255
  ;    BYTE_RANGE=[0,250]
  ;  endelse

  faparDSInfo=getStandardFaparDataSetInfo()

  bandNames=faparDSInfo.bandNames
  bandLongNames=faparDSInfo.bandLongNames
  bandStandardNames=faparDSInfo.bandStandardNames
  bandSlopes=faparDSInfo.bandSlopes
  bandMeasureUnits=faparDSInfo.bandMeasureUnits
  bandDataTypes=faparDSInfo.bandDataTypes
  bandIntercepts=faparDSInfo.bandIntercepts
  minMaxs=faparDSInfo.minMaxs
  nanList=faparDSInfo.nans

  trueSlopes=bandSlopes
  trueIntercepts=bandIntercepts
  header=faparDSInfo.header

  res=dataByteScaling(output.fpar, output.flag, FLAG_VALUES=[9,10], $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  output.fpar=res.resultData
  output.flag=res.resultFlag
  trueIntercepts[0]=outIntercept
  trueSlopes[0]=outSlope

  res=dataByteScaling(output.sigma, output.flag, $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  output.sigma=res.resultData
  output.flag=res.resultFlag
  trueIntercepts[1]=outIntercept
  trueSlopes[1]=outSlope

  res=dataByteScaling(output.red, output.flag, FLAG_VALUES=[9,10], $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;output.red=res.resultData
  output.flag=res.resultFlag
  ;trueIntercepts[2]=outIntercept
  ;trueSlopes[2]=outSlope

  res=dataByteScaling(OUTPUT.SIGMA_RED, output.flag, $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;output.sigma_red=res.resultData
  output.flag=res.resultFlag
  ;trueIntercepts[3]=outIntercept
  ;trueSlopes[3]=outSlope

  res=dataByteScaling(output.nir, output.flag, FLAG_VALUES=[13,14], $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;output.nir=res.resultData
  output.flag=res.resultFlag
  ;trueIntercepts[4]=outIntercept
  ;trueSlopes[4]=outSlope

  res=dataByteScaling(output.sigma_nir, output.flag, $
    DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, $
    DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, outSlope, outIntercept)
  ;output.sigma_nir=res.resultData
  output.flag=res.resultFlag
  ;trueIntercepts[5]=outIntercept
  ;trueSlopes[5]=outSlope
  output.flag[idx_sea]=3

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
  dims=size(output.fpar, /DIMENSIONS)
  sourceFileName=ST_fileSystem->getFileNameInfo(resFileNC, filePath=filePath, extension=extension)
  ;FIRST_LOOK=0
  if keyword_set(FIRST_LOOK) then begin
    fLookDir='first_look'
    ;cd, dirout
    firstLookDir=outputDir+fLookDir
    fInfo=file_info(fLookDir)
    if ~(fInfo.exists) then file_mkdir, firstLookDir
    sampleImg=rebin(output.fpar, dims[0]/10,dims[1]/10)
    minvalue=min(output.fpar, max=maxvalue)
    sampleImg=bytscl(sampleImg)
    samplefilename='fl_'+sourceFileName+'.gif'
    fullSampleFName=firstLookDir+path_sep()+samplefilename
    LOADCT, 14
    print, 'sampleImage-->', fullSampleFName
    write_gif, fullSampleFName, sampleImg
  endif

  dataSets=[ptr_new(output.fpar, /NO_COPY), ptr_new(output.sigma, /NO_COPY), $
    ptr_new(output.red, /NO_COPY), ptr_new(output.sigma_red, /NO_COPY), $
    ptr_new(output.nir, /NO_COPY), ptr_new(output.sigma_nir, /NO_COPY), $
    ptr_new(qa_avhrr, /NO_COPY), $
    ptr_new(reform(angles[*,*,0]), /NO_COPY), ptr_new(reform(angles[*,*,1]), /NO_COPY), ptr_new(reform(angles[*,*,2]), /NO_COPY), $
    ptr_new(reform(reflectance(*,*,0)), /NO_COPY), ptr_new(reform(reflectance(*,*,1)), /NO_COPY), $
    ptr_new(MASK_AVHRR, /NO_COPY)]
  ;ptr_new(output.flag, /NO_COPY)]

  ;  minMaxs[*,*]=-1
  ;  minMaxs[0,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[0]=BYTE_NAN
  ;
  ;  minMaxs[1,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[1]=BYTE_NAN
  ;
  ;  minMaxs[2,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[2]=INT_NAN
  ;
  ;  minMaxs[3,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[3]=INT_NAN
  ;
  ;  minMaxs[4,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[4]=INT_NAN
  ;
  ;  minMaxs[5,*]=DATA_RANGE;minMax[0,*]
  ;  nanList[5]=INT_NAN
  ;
  ;  tempMin=min(output.flag, max=tempMax)
  ;  minMaxs[6,*]=[tempMin, tempMax]
  ;  nanList[6]=INT_NAN
  ;
  ;  tempMin=min(angles(*,*,0), max=tempMax)
  ;  minMaxs[7,*]=[0.,90.]
  ;  nanList[7]=INT_NAN
  ;
  ;  tempMin=min(angles(*,*,1), max=tempMax)
  ;  minMaxs[8,*]=[0.,90.]
  ;  nanList[8]=INT_NAN
  ;
  ;  tempMin=min(angles(*,*,2), max=tempMax)
  ;  minMaxs[9,*]=[-180,180]
  ;  nanList[9]=INT_NAN
  ;
  ;  tempMin=min(reflectance(*,*,0), max=tempMax)
  ;  minMaxs[10,*]=[tempMin>0.,tempMax]
  ;  nanList[10]=INT_NAN
  ;
  ;  tempMin=min(reflectance(*,*,1), max=tempMax)
  ;  minMaxs[11,*]=[tempMin>0.,tempMax]
  ;  nanList[11]=INT_NAN
  ;
  ;
  ;  minMaxs[12,*]=[maskMin, maskMax]
  ;  nanList[12]=INT_NAN

  boundary=[-180.0, 180.0, -90, 90.]

  if n_elements(resFileNC) eq 1 then print,'Write the results in: ',resFileNC
  if n_elements(resFileHDF) eq 1 then print,'Write the results in: ',resFileHDF

  if keyword_set(NC) then write_georef_ncdf, resFileNC, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
    header=header

  if keyword_set(HDF) then write_hdf, resFileHDF, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, $
    trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
    header=header


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
end

