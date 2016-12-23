; ****************************************
; exit values of fapar
; true values from 1 to 255
; bad value 0
; *****************************************
;function makeitglob_new, sensor, noaanumber, year, month, day
;@../../Library/library/jrc_core/mapQualityFlags
;@../../Library/library/jrc_core/mapQualityFlags
function doFapar, instrument, indicator, spatialResolution, level, missionName, mainVarName, missionCode, year, month, day, $
  sourceDir, outputDir, tempdir, $
  FIRST_LOOK=FIRST_LOOK, $
  NC=NC, HDF=HDF, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX, $
  OVERWRITE=OVERWRITE, subfolder=subfolder

  !EXCEPT=2
  ; 2016, MM

  ; Purpose:

  ; Build Dailt Fapar values starting from BRF file
  ; The name of input & output files are build on-the-fly using various input parameter
  ; 
  ; NC : output format to geo NCDF
  ; HDF : output format to geo HDF
  ; MISSIONOVERLAPINDEX: set to 1 if you want to process the SECOND file (the one with NOAA higher), when more than one NOAA is available
  ;                       if omitted the code search only for the first one
  ; OVERWRITE: set to 1 if you want to cancel the previous result for this day.
  ; FIRST_LOOK: set to 1 to create a standard image of the result in addition to data file(s)
  ;  
  ;
  ; The inputs are
  ; flagList = list if original flags
  ; REVERT   = set if you want coding from SWF to AVHRR, otherwise AVHRR to SWF is performed

  ;Convert flag codes from AVHRR/IDL to SEAWIFS/C

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;NaN=-9999 ;!VALUES.F_NAN
  outputDir=ST_fileSystem->adjustDirSep(outputDir, /ADD)
  sourceDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  tempDir=ST_fileSystem->adjustDirSep(tempDir, /ADD)

  yearS=string(year, format='(I04)')
  monthS=string(month, format='(I02)')
  dayS=string(day, format='(I02)')

  version='N'+string(missionCode, format='(I02)');version='001'

  ; ToDo: remove "old" version (with wrong file name)...
  sourceFileInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
    product, version, 'NC',  indicator=indicator, level, projection=projection)
  ; ToDo: replace with this version when everything will work fine...
  ;sourceFileInfo=build_JRC_BRF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
  ;  product, version, 'NC',  indicator=indicator, level, projection=projection)

  ;if n_elements(sourceFileName) gt 1 then sourceFileName=sourceFileName[MISSIONOVERLAPINDEX]
  ;sourceFileName=buildBrfFileName_D(sensor, resolution, year, month, day, missionName, missionCode, mainVarName)

  ; build output filename (useful to check overwrite mode)
  if instrument eq 'AVH' then begin
    ncFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
      product, version, 'NC',  indicator=indicator, level, projection=projection)
    hdfFileInfo=build_JRC_FPA_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, $
      product, version, 'HDF',  indicator=indicator, level, projection=projection)
  endif
  testDir=sourceDir+sourceFileInfo.filePath
  ; useful for test (source dir not need to be organized in subfolder...
  if (file_info(testDir)).directory eq 1 then sourceDir=testDir   
  ; always add path_sep() as last character...
  sourceDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  sourceFileName=sourceDir+sourceFileInfo.fileName

  destDir=outputDir+hdfFileInfo.filePath
  destDir=ST_fileSystem->adjustDirSep(sourceDir, /ADD)
  ;insert or remove version here
  ;set intermediate folder for testing...
  ; ToDo: remove with final version!!!
  ;destDir=destDir+'v1.5'+path_sep()
  resFileNC=destDir+ncFileInfo.fileName
  resFileHDF=destDir+hdfFileInfo.fileName

  ;sourceFullFileName = (file_search(sourceDir, sourceFileInfo.fileName, COUNT=count))[0];, /FULL_QUALIFY)
  ; search file existence...
  fInfo=file_info(sourceDir+sourceFileInfo.fileName)
  count = 0
  sourceFullFileName = ''
  if fInfo.size gt 10 then begin
    count = 1
    sourceFullFileName = sourceDir+sourceFileInfo.fileName
  endif
  ;  if count eq 0 then begin
  ;    sourceFullFileName = (file_search(sourceDir+TC_TYPE, sourceFileNC, COUNT=count))[0];, /FULL_QUALIFY)
  ;  endif
  ;
  ;  sourceDir=filePath

  ; search file existence...
  checkNC=file_info(resFileNC)
  checkHDF=file_info(resFileHDF)
  ; search file existence (compress too)...
  checkNCzip=file_info(resFileNC+'.gz')
  checkHDFzip=file_info(resFileHDF+'.gz')

  if count ne 1 then NOSOURCEAVAILABLE=1
  ;print, '******'
  ;print, 'write nc?', (checkOutput1[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(NC)
  ;print, 'write hdf?', (checkOutput2[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(HDF)
  ;print, '******'
  ; set overwrite mode for each selected type (NC or HDF)
  if keyword_set(NC) and ((checkNC[0].size eq 0)  or keyword_set(OVERWRITE)) then NOWRITENC=0 else NOWRITENC=1
  if keyword_set(HDF) and (((checkHDF[0].size eq 0 ) AND (checkHDFzip[0].size eq 0)) or keyword_set(OVERWRITE)) then NOWRITEHDF=0 else NOWRITEHDF=1
  ;if (checkOutput1[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(NC) then NOWRITENC=1
  ;if (checkOutput2[0].size ne 0 and ~keyword_set(OVERWRITE)) and ~keyword_set(HDF) then NOWRITEHDF=1
  ;if checkHDF[0].size ne 0 and ~keyword_set(OVERWRITE) then NONCWRITE=1

  ; do the job ONLY if we need it!!! Otherwise, skip
  if keyword_set(NOSOURCEAVAILABLE) or (keyword_set(NOWRITEHDF) and keyword_set(NOWRITENC)) then return, -1

  print, 'start reading source file: ', sourceDir, sourceFileInfo.fileName
  ; reading input data
  data=readBRF(sourceDir, sourceFileInfo.fileName, FOUND=FOUND)
  print, 'brf ...done'

  if ~keyword_set(FOUND) then return, -1

  ; set some NaN
  INT_NAN=2^15 ;(for 16 bit int this is the first negative value) 
  DATA_RANGE=[0., 1.]
  DATA_NAN=-1

  ;main structure
  output={  fpar: fltarr(7200,3600), $f
    sigma: fltarr(7200,3600), $
    red: fltarr(7200,3600), $
    sigma_red:fltarr(7200,3600), $
    nir: fltarr(7200,3600), $
    sigma_nir: fltarr(7200,3600), $
    flag: bytarr(7200,3600), $
    toc_red: fltarr(7200,3600), $
    toc_nir: fltarr(7200,3600), $
    qa: bytarr(7200,3600)}
  ;
  ; initialization
  ;
  ; MM & NG 21/09/2016
  output.fpar(*,*)=!VALUES.F_NAN
  output.sigma(*,*)=!VALUES.F_NAN
  output.red(*,*)=!VALUES.F_NAN
  output.sigma_red(*,*)=!VALUES.F_NAN
  output.nir(*,*)=!VALUES.F_NAN
  output.sigma_nir(*,*)=!VALUES.F_NAN
  noaanumber=missionCode

  ; apply slope & offset
  red_avhrr=data.red_avhrr & slope_red=data.slope_red & offset_red=data.offset_red
  nir_avhrr=data.nir_avhrr & slope_nir=data.slope_nir & offset_nir=data.offset_nir
  ts_avhrr=data.ts_avhrr & slope_ts=data.slope_ts & offset_ts=data.offset_ts
  tv_avhrr=data.tv_avhrr & slope_tv=data.slope_tv & offset_tv=data.offset_tv
  phi_avhrr=data.phi_avhrr & slope_phi=data.slope_phi & offset_phi=data.offset_phi
  qa_avhrr=data.brf_qa_avhrr & slope_brf_qa=data.slope_qa & offset_brf_qa=data.offset_qa
  ; There is a bug on source file here, we cut also a valid -99.99 deg valid angle (Only for PHI)
  anglesNan=where(tv_avhrr eq -9999, cntAnglesNan)
  ;nan=where(ts_avhrr eq -9999, cnt2)
  ;nan=where(phi_avhrr eq -9999, cnt3)
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
  goodIdxs=where(finite(ts_avhrr), cnt)
  
  ; ts
  ;!EXCEPT=2
  tempAng=angles(*,*,0)
  tempAng[*]=!VALUES.F_NAN
  tempAng[goodIdxs]=ts_avhrr[goodIdxs]*slope_ts(0)+offset_ts(0)
  angles(*,*,0)=temporary(tempAng)
  
  ; tv
  tempAng=angles(*,*,1)
  tempAng[*]=!VALUES.F_NAN
  tempAng[goodIdxs]=tv_avhrr[goodIdxs]*slope_tv(0)+offset_tv(0)
  angles(*,*,1)=temporary(tempAng)
  
  
  tempAng=angles(*,*,2)
  tempAng[*]=!VALUES.F_NAN
  tempAng[goodIdxs]=phi_avhrr[goodIdxs]*slope_phi(0)+offset_phi(0)
  angles(*,*,2)=temporary(tempAng)
  ;
  ; correction already applied
  ;RELAZ=phi_avhrr(*,*)*slope_phi(0)+offset_phi(0)
  ;SIN_REL_AZ = sin(RELAZ*!pi/180.)
  ;COS_REL_AZ = cos(RELAZ*!pi/180.)
  ;GOOD_REL_AZ = atan(SIN_REL_AZ, COS_REL_AZ)
  ;angles(*,*,2)=good_rel_az*180./!pi
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
  ;PPMSA_ALBEDOCOLOR

  ; use this routine to get right coeffs for this instrument and this mission (NOAA)
  coeffInfo=getSensorCoeffs(instrument, missionCode)

  ; clouding and other flags mean is setting directly from band1 & band2 thresholds 
  flag2 = CheckDataTOC(red_avhrr*slope_red(0)+offset_red(0), nir_avhrr*slope_nir(0)+offset_nir(0), coeffInfo.soilCoeffs)
  ;flagstoc=flag2
  ;
  ;stop
  ;
  ;flag_angles = CheckAngle(angles(*,*,0), angles(*,*,1))
  ;colorset
  ;TVLCT, red*255, gre*255, blu*255
  ;window, 0, xsize=720, ysize=360, title='FLAG 0.0'
  ;tv, congrid(flag2, 720,360)
  ;
  ;  stop
  ; save memory
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
  ;	0      Band 1 BRF corrected;
  ;	       1 -- yes
  ;	       0 -- no
  ;	5      Band 2 BRF corrected;
  ;	       1 -- yes
  ;	       0 -- no
  ;
  ;
  ;=======================================================================================

  ; MM & NG
  ;rrq1=cgi_map_bitwise_flag(qa_avhrr,0)
  ;rrq2=cgi_map_bitwise_flag(qa_avhrr,5)
  ;
  ;idx_nocorr=where (rrq1 eq 0 or rrq2 eq 0)
  ; end
  ;
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
  ;rr1=cgi_map_bitwise_flag(qa_avhrr,1)
  ;
  ;
  ; 2      Pixel contains cloud shadow;
  ;	       1 -- yes
  ;	       0 -- no
  ;
  ;rr2=cgi_map_bitwise_flag(qc_avhrr,2)
  ;rr2=cgi_map_bitwise_flag(qa_avhrr,2)
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
  ;idx_mask = where(rr1 eq 1) ; or rr2 eq 1)		;----> cloud
  ;idx_mask2 = where(rr21 eq 1 or rr22 eq 1)		;----> invalid
  IDX_SEA=  where(rr3 eq 1)

  flag2(idxbad)=1.0		;	< bad

  ;window, 1, xsize=720, ysize=360, title='FLAG 1.0'+SENSOR
  ;tv, congrid(flag2, 720,360)
  ;S=SIZE(FLAG2)
  ;MASK_AVHRR=BYTARR(S(1),S(2))
  ;
  ;MASK_AVHRR(IDX_MASK)=2		;----> cloud
  ;MASK_AVHRR(IDX_MASK2)=2
  ;MASK_AVHRR(idx_nocorr)= 130
  ;flag2(idx_mask)=2.0

  ;MASK_AVHRR(IDX_SEA)= 3
  ;flag2[idx_mask]=2
  flag2[idx_sea]=3.0
  ;window, 2, xsize=720, ysize=360, title='FLAG 2.0'+SENSOR
  ;tv, congrid(flag2, 720,360)
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

      idx_neg=where(output.fpar le 0.0) ;and output.fpar gt INT_NAN )
      output.fpar(idx_neg)=0.0

      idx_big=where(output.fpar ge 1.0 and output.fpar le 10.0)
      output.fpar(idx_big)=1.0
      ; MM & NG
      ;flag2(idx_neg)=4.0
      flag2(idx_neg)=5.0
      flag2(idx_big)=6.0
      ;window, 3, xsize=720, ysize=360, title='FLAG 3.0'+SENSOR
      ;tv, congrid(flag2, 720,360)
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
  ; MM & NG
  mask=finite(sza)
  idx_soil=where(flag2 eq 4.0 or flag2 eq 5.0)

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
  ;window, 4, xsize=720, ysize=360, title='FLAG 4.0'+SENSOR
  ;tv, congrid(output.flag, 720,360)
  ;
  ;idx_1 = where(flag2 eq 1)
  ;idx_2 = where(flag2 eq 2)
  ;idx_3 = where(flag2 eq 3)

  ;
  ;idx_4 = where (flag_angles eq 1)
  ;idx_5 = where (flag_angles eq 2)
  ;
  ;
  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, NAN_BYTE_VALUE=0, VALUE_BYTES=[1,255])
  ;byteOutput=dataByteScaling(output.fpar, VALUE_BYTES=[0,250])
  ;remarkableFlags=bSInfo.remarkableFlags
  ; useful values coming from scaling routine
  ;DATA_NAN=bSInfo.DATA_NAN
  ;BYTE_NAN=bSInfo.BYTE_NAN
  ;BYTE_RANGE=bSInfo.BYTE_RANGE

  ; Info about which variables store and in which way is inside a product-dependended function
  faparDSInfo=getStandardFaparDataSetInfo()

  ; A lot of setting...
  bandNames=faparDSInfo.bandNames
  bandLongNames=faparDSInfo.bandLongNames
  bandStandardNames=faparDSInfo.bandStandardNames
  bandSlopes=faparDSInfo.bandSlopes
  bandMeasureUnits=faparDSInfo.bandMeasureUnits
  bandDataTypes=faparDSInfo.bandDataTypes
  bandIntercepts=faparDSInfo.bandIntercepts
  minMaxs=faparDSInfo.minMaxs
  scaledminmaxs=faparDSInfo.scaledminmaxs
  nanList=faparDSInfo.nans

  trueSlopes=bandSlopes
  trueIntercepts=bandIntercepts
  header=faparDSInfo.header

  ; 2003 -->
  ; scale float to a byte
  ; 1) [1..255] where [0] is bad
  ; 2) [0..250] where [251..255]
  ; 3) [0..254] where [255] is bad
  ; do it for fapar
  realFapar=output.fpar
  
  res=dataByteScaling(output.fpar, output.flag, $ ;FLAG_VALUES=[9,10], $
    DATA_NAN=!VALUES.F_NAN, BYTE_NAN=faparDSInfo.nans[0], $
    DATA_RANGE=faparDSInfo.minMaxs[0,*], BYTE_RANGE=faparDSInfo.scaledminmaxs[0,*], outSlope, outIntercept)
  output.fpar=res.resultData
  
  ; If VGT case (JRC_FLAG eq 0) set a value <> 0 (First available after byta_range[0])
  setToMinIdx=where(output.fpar eq reform(faparDSInfo.scaledminmaxs[0,0]) and output.flag eq 0, setToMinCount)
  output.fpar[setToMinIdx]=reform(faparDSInfo.scaledminmaxs[0,0])+1b
  ;idx=where(output.flag eq 0 or output.flag eq 4 or output.flag eq 5, num, compl=idxN)
  ;output.fpar[idx]=!VALUES.F_NAN
  
  trueIntercepts[0]=outIntercept
  trueSlopes[0]=outSlope

  ; do it for sigma-fapar
  res=dataByteScaling(output.sigma, output.flag, $
    DATA_NAN=!VALUES.F_NAN, BYTE_NAN=faparDSInfo.nans[1], $
    DATA_RANGE=faparDSInfo.minMaxs[1,*], BYTE_RANGE=reform(faparDSInfo.scaledminmaxs[1,*]), outSlope, outIntercept)
  output.sigma=res.resultData
  ;output.flag=res.resultFlag
  trueIntercepts[1]=outIntercept
  trueSlopes[1]=outSlope
  ;output.sigma[idx]=!VALUES.F_NAN

  flagTags=strupcase(['fpar', 'sigma'])
  tags=tag_names(output)

  ; check what here... MM
  ; useful later... maybe 
  ;for i=0, n_elements(flagTags)-1 do begin
  ;  thisIdx=(where(flagTags[i] eq tags, count))[0]
  ;  if count eq 1 then begin
  ;    output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_1, remarkableFlags[0])
  ;    output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_2, remarkableFlags[1])
  ;    output.(thisIdx)=mapQualityFlags(output.(thisIdx), idx_3, remarkableFlags[2])
  ;  endif
  ;endfor

  ;map -9999 on int data
  flagTags=strupcase(['red', 'nir', 'sigma_red', 'sigma_nir'])
  for i=0, n_elements(flagTags)-1 do begin
    thisIdx=(where(flagTags[i] eq tags, count))[0]
    nanIdxs=where(output.(thisIdx) gt 250, count)
    output.(thisIdx)=mapQualityFlags(output.(thisIdx), nanIdxs, INT_NAN)
  endfor

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

  ts=reform(angles[*,*,0])
  tv=reform(angles[*,*,1])
  phi=reform(angles[*,*,2])
  nanIdx=where(finite(output.red) eq 0, cnt)
  toc_red=reform(reflectance(*,*,0))
  toc_nir=reform(reflectance(*,*,1))
  delIdlvar, reflectance
  delIdlvar, angles
  if cnt ne 0 then begin
    ;anglesNan=where(tv_avhrr eq -9999, cntAnglesNan)
    ts[nanIdx]=INT_NAN
    tv[nanIdx]=INT_NAN
    phi[nanIdx]=INT_NAN
    output.sigma_red[nanIdx]=INT_NAN
    output.sigma_nir[nanIdx]=INT_NAN
    output.red[nanIdx]=INT_NAN
    output.nir[nanIdx]=INT_NAN
    toc_red[nanIdx]=INT_NAN
    toc_nir[nanIdx]=INT_NAN
  endif
  
  ;array of variable to put on file
  dataSets=[ptr_new(output.fpar, /NO_COPY), ptr_new(output.sigma, /NO_COPY), $
    ptr_new(output.red, /NO_COPY), ptr_new(output.sigma_red, /NO_COPY), $
    ptr_new(output.nir, /NO_COPY), ptr_new(output.sigma_nir, /NO_COPY), $
    ptr_new(qa_avhrr, /NO_COPY), $
    ptr_new(ts, /NO_COPY), ptr_new(tv, /NO_COPY), ptr_new(phi, /NO_COPY), $
    ptr_new(toc_red, /NO_COPY), ptr_new(toc_nir, /NO_COPY), $
    ptr_new(output.flag, /NO_COPY), ptr_new(realFapar, /NO_COPY)]

  boundary=[-180.0, 180.0, -90, 90.]

  if n_elements(resFileNC) eq 1 then print,'Write the results in: ',resFileNC
  if n_elements(resFileHDF) eq 1 then print,'Write the results in: ',resFileHDF
  ; product dependent header information
  date_created=ST_utils->getSysTime(/FILECOMPATIBILITY)
  satellite='NOAA '+strcompress(missionCode, /REMOVE)
  time_Coverage_Start=ST_utils->formatDate([year, month, day, 0, 0, 0], template='satellite')
  time_Coverage_End=ST_utils->formatDate([year, month, day, 23, 59, 59], template='satellite')
  header.cdr_variable=['cdr_variable', 'FAPAR']
  header.Process=['process', 'JRC FAPAR TOC algorithm - see QA4ECV ATBD']

  ;most of information coming from getStandardFaparDataSetInfo()
  ;writer did most of the dirty job (setting min, max, add_offset, scale_factor...)
  ;only peculiar variables (fapar & sigma fapar) are still ready and we DON'T need more computation 
  if keyword_set(NC) then write_georef_ncdf, resFileNC, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
    /NOREVERSE, trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
    id=ncFileInfo.filename, satellite=satellite, header=header, $
    date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End

  if keyword_set(HDF) then write_hdf, resFileHDF, $
    bandNames, bandStandardNames, bandLongNames, bandMeasureUnits, $
    dataSets, bandDataTypes, bandIntercepts, bandSlopes, tempDir, boundary, scaledminmaxs=scaledminmaxs, $
    trueMinMaxs=minMaxs, nanList=nanList, trueIntercepts=trueIntercepts, trueSlopes=trueSlopes, $
    id=hdfFileInfo.filename, satellite=satellite, header=header, $
    date_created=date_created, time_Coverage_Start=time_Coverage_Start, time_Coverage_End=time_Coverage_End, /POSTCOMPRESSION

  print, 'done'

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

