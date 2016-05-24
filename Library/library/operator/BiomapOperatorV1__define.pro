;@structure_definition.pro
;@/library/regionalgo/regionalgo
;@/library/system_io/buildBioMapVarFileName

FUNCTION BiomapOperatorV1::extractAlgoInfo, productName, prefix

  bandsString=strmid(productName, strlen(prefix), strlen(productName)-strlen(prefix))
  bandNames=strsplit(bandsString, '_', /EXTRACT)
  
  allAvWaves=self->getUsedWaveLength()
  lambdaIdx = intarr(n_elements(allAvWaves))
  
  rrsSet=''
  for i=0, n_elements(bandNames)-1 do begin
    idx=(where(fix(bandNames[i]) eq allAvWaves, count))[0]
    if count eq 1 then lambdaIdx[idx]=1
    rrsSet=rrsSet+'_'+bandNames[i]
  endfor
  rrsSet=strmid(rrsSet, 1, strlen(rrsSet)-1)
  name=strmid(prefix, 0, strlen(prefix)-1)
  return, {rrsSet:rrsSet, name:name, lambdaIdx:lambdaIdx}
  
END

FUNCTION BiomapOperatorV1::getAlgoInfo, rrsProductName, sensor, basin

  rrsId='rrs_single' & ratioId='rrs_ratio_'& ratioLogId='rrs_ratiolog10_'
  
  if strpos(rrsProductName, ratioId) ne -1 then return, self->extractAlgoInfo(rrsProductName, ratioId)
  if strpos(rrsProductName, ratioLogId) ne -1 then return, self->extractAlgoInfo(rrsProductName, ratioLogId)
  return, self->extractAlgoInfo(rrsProductName, rrsId)
  
END

; Main call
; mark with "_v1" to mantain neural net version

FUNCTION BiomapOperatorV1::doComputation, roiCode, productName, sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE

  common memtT, strc
  COMMON smurffCB, mainApp
  common refDate, myMonth, myYear, myDay
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;NAME
  ; regioanlgo
  ;
  ;OVERVIEW
  ;
  ; This IDL routine is provided for testing regional neural nets trained with data from the
  ; BiOMaP and CoASTS programs. Please refer to the companion pdf document for more details.
  ;
  ;INPUTS
  ;
  ; basin: Northern Adriatic Sea (code name: adrs), western Black Sea (blks), and
  ; Baltic Sea (blts).
  ;
  ; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
  ; yellow substance absorption at 412 nm (ay412).
  ;
  ; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf) and in-situ (rrs) radiometer.
  ; In fact, different algorithms are provided for specific set of center wavelengths.
  ; In all cases, input to the neural net to compute data products are remote sensing
  ; reflectance RRS values.
  ;
  ;EXECUTION
  ;
  ; 1) Extract the content of the zip file regionalgo.zip in a folder (e.g., 'c:\projects\idl\test').
  ;
  ; 2) Set the IDL working directory to that folder (enter cd, 'c:\projects\idl\test'
  ; in the IDL command line, note the comma after cd and that the path is within quotes).
  ;
  ; 3) Enter the following command in the IDL prompt: regioanlgo, 'blks', 'chlideachla', 'mer'.
  ; Note that the arguments basin, prod, sensor of the procedure regionalgo can be any combination
  ; from the set of options of here above (e.g., regioanlgo, 'blks', 'ay412', 'swf').
  ;
  ;CITATIONS FOR THE PUBLICATION OF RESULTS BASED ON REGIONALGO.PRO
  ;
  ; 1) D'Alimonte, G. Zibordi, J.-F. Berthon, E. Canuti, and T. Kajiyama,
  ; "Performance and applicability of bio-optical algorithms in different european seas,"
  ; Remote Sensing of Environment, vol. 124, no. 0, pp. 402-412, 2012.
  ;
  ; 2) T. Kajiyama, D. D'Alimonte, and G. Zibordi, "Regional algorithms for european seas:
  ; a case study based on meris data," IEEE Geosci. Remote Sens. Lett., 2013.
  ;
  ; 3) G. Zibordi, J.-F. Berthon, F. M�lin, and D. D'Alimonte, "Cross-site consistent in situ
  ; measurements for satellite ocean color applications: the biomap radiometric dataset,"
  ; Remote Sens. Environ., vol. 115, no. 8, pp. 2104-2115.
  ;
  ; 4) D. D'Alimonte, T. Kajiyama, G. Zibordi, and J.-F. Berthon, "Comparison between meris and
  ; regional products maps of dissolved and suspended matter," Remote Sensing of Environment, 2012,
  ; Submitted for publication.
  ;
  ;DISCLAIMER
  ;
  ; The regionalgo.pro routine and the coefficients of BiOMaP regional algorithms are provided
  ; ''AS IS'' and there is no any expressed or implied warranties.
  ;
  ;--------------------------------------------------------------------------------------------------
  
  NODATA=0
  doLog, '**', self.mainFileName, '**', level=0
  if n_elements(fileName) eq 1 then self->setMainFileName, fileName, OPEN=OPEN,COPY=COPY
  basin=self->mapRoiCode(roiCode, FOUND=FOUND)
  sensor=self->mapSensorCode(sensorCode, FOUND=FOUND)
  ;doLog, '***Memory trace starts here***', LEVEL=4
  ;doLog, /CHECKMEMORY
  self->buildInputFile, sensor, ignoreValue, pixelMask=pixelMask, textInputFile=outTextInputFile, pixelNumber=pixelNumber
  ;if n_elements(data) eq 0 then begin
  ;  pixelMaskTest=pixelMask
  ;endif
  ;doLog, 'buildInputFile memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ; Input data
  
  if ~(file_info(outTextInputFile)).read or (strlen(strcompress(outTextInputFile, /REMOVE_ALL)) eq 0) then begin
    doLog, 'No valid data for roi: ', roiCode, '; sensor: ', sensorCode, '; Ref file: ', self->getMainFileName(), '. Skip.', level=2
    NO_DATA=1
    return, 0
  endif
  
  readRes = dataread_v1(outTextInputFile, pixelNumber=pixelNumber)
  ;if n_elements(data) eq 0 then begin
  ;  data=dataNew
  ;endif
  ;data=0b
  ;help, /memory
  ;doLog, 'dataread memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  nData = n_elements(readRes[0, *])
  
  ; Input file with the coefficients of regional algorithms
  algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\regionalgo_v1.h5'
  if n_elements(strc) eq 0 then strc = self->getH5Struct()
  ;doLog, 'getH5Struct memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ;workaround???
  ;hid = H5F_OPEN(file)
  ;H5F_CLOSE, hid
  ; Correction for bi-directional effects (to activate, set the flag to 1)
  foqFlag = 0
  
  ; Rrs for the selected basin and sensor
  case basin of
    'adrs': begin
      case sensor of
        'swf': rrsSet = 'swf443_swf490_swf510_swf555'
        'mod': rrsSet = 'mod443_mod488_mod530_mod555'
        'mer': rrsSet = 'mer443_mer490_mer510_mer560'
        'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
      end
      lambdaIdx = [0, 1, 1, 1, 1, 0]
    endcase
    'blks': begin
      lambdaIdx = [0, 1, 1, 1, 1, 0]
      case sensor of
        ; start new version DDA + MM 2015 08 20
        ; adding variable centralBands
        ; overwrite variable lambdaIdx
        'swf': begin
          rrsSet = 'swf443_swf490_swf510_swf555'
          centralBands = [412, 443, 490, 510, 555, 670]
          lambdaIdx = [0, 1, 1, 1, 1, 0]
        end
        ; add for new version
        'mod': begin
          rrsSet = 'mod443_mod488_mod530_mod555'
          centralBands = [412, 443, 488, 530, 555, 667]
          lambdaIdx = [0, 1, 1, 1, 1, 0]
        end
        'mer': begin
          rrsSet = 'mer443_mer490_mer510_mer560'
          centralBands = [413, 443, 490, 510, 560, 665]
          lambdaIdx = [0, 1, 1, 1, 1, 0]
        end
        ; end new version DDA + MM 2015 08 20
        'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
      end
    endcase
    'blts': begin
      case sensor of
        'swf': rrsSet = 'swf490_swf510_swf555_swf670'
        'mod': rrsSet = 'mod488_mod530_mod555_mod667'
        'mer': rrsSet = 'mer490_mer510_mer560_mer665'
        'rrs': rrsSet = 'rrs491_rrs511_rrs555_rrs665'
      end
      lambdaIdx = [0, 0, 1, 1, 1, 1]
    endcase
  endcase
  
  ; f/Q correction
  foqflag=keyword_set(applyfoq)
  if foqFlag eq 1 then begin
  
    ; f/Q correction table
    fqfile=self->getFoqFile_v1()
    read_foq_table, fqfile, foq_table_common
    
    ; Algorithm for chl-a estimates
    algoName = strupcase(basin + '_' + rrsSet + '_to_chlideachla')
    algoArchive=tag_names(strc)
    algoIdx=where(strcmp(algoArchive, algoName) eq 1)
    model = strc.(algoIdx)._data
    
    ; Do three iterations
    for i = 0, 2 do begin
    
      ; Chla
      tmp = dataprod_v1(model, alog10(subSetData[where(lambdaIdx eq 1), *]))
      chla = tmp[0, *]
      
      ; Loop over data records
      for row = 0,  nData - 1 do begin
      
        ; f/Q
        get_foq_v1, foq_table_common, chla(row), data(6, row), data(7, row), data(8, row), foq_a
        
        ; f0/Q0
        get_foq_v1, foq_table_common, chla(row), 0, 0, 0, foq_0
        
        ; Loop over wavelengths
        for chn = 0, 5 do data[chn, row] = data(chn, row) * foq_0(chn) / foq_a(chn)
        
      end
      
    end
    
  endif
  
  ; Regional product
  algoName = strupcase(basin + '_' + rrsSet + '_to_' + productName)
  algoArchive=tag_names(strc)
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  algoArchive=0b
  model = strc.(algoIdx)._data
  dataRes=temporary(alog10(temporary(readRes[where(lambdaIdx eq 1), *])))
  ;data=0b
  ;if n_elements(res) eq 0 then res = dataprod_v1(temporary(model), temporary(dataRes))
  res = dataprod_v1(temporary(model), dataRes)
  ;res=0b & data=0b
  ;data=0b
  ;model=0b
  ;help, /memory
  ;doLog, 'dataprod memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ;delidlvar, strc
  ;delidlvar, model
  ;strc=0
  ;model=0b
  
  ;help, res
  ; first row product value, second row applicability value
  ;writeTestCase, readRes, dataRes, res, algoName, rrsSet, outTextInputFile
  self->updateFid
  if self->isEnviType() then begin
    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
    ;res=0b
    biomapThreshold=self.app->getKeyValue('BIOMAP_V1_APPLICABILITY_THRESHOLD')
    biomapThreshold=float(biomapThreshold)
    idx=where(~finite(appl) or appl gt biomapThreshold, count)
    if count gt 0 then values[idx]=ignoreValue
    ;idx=0b
    self->exportRrsBand, readRes, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=idx
    productValues=self->asciiToEnvi(values, pixelMask=pixelMask, ignoreValue=ignoreValue)
    ;wrongIdxs=where(values gt 500, wrongCount)
    ;if wrongCount ne 0 then begin
    ;  tempDir=mainApp->getKeyValue('TEMP_DIR')
    ;   doHistogram, productValues, tempDir, roiCode, day, month, year
    ;endif
    self->addBand, productValues, productName, /OVERWRITE
    idxs=where(~finite(appl) or appl gt 10., count)
    if count ne 0 then appl[idxs]=10
    idxs=0b
    ;productValues=self->asciiToEnvi(res[1,*], pixelMask=pixelMask, ignoreValue=ignoreValue)
    productValues=self->asciiToEnvi(appl, pixelMask=pixelMask, ignoreValue=ignoreValue)
    self->addBand, productValues, productName+self->getApplicabilityExtension(), /OVERWRITE
  ;doLog, 'store as envi memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  endif
  ;values=self->getBand(productName)
  ;idxs=where(res[0,*] ne 0 and res[0,*] -9999, count1)
  ;idxs=where(productValues ne 0 and productValues ne -9999, count2)
  ;idxs=where(values ne 0 and values ne -9999, count3)
  ;doLog, count1, count2, count3, level=0
  if ~(keyword_set(PRESERVE_DAT_FILE)) then self->removeLastDatFile
  return, 1
  
end

PRO BiomapOperatorV1::exportRrsBand, allRrs, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=hideIdxs

  COMMON smurffCB, mainApp
  
  if ~mainApp->isTestMode() then return
  rrsBands=mainApp->getKeyValue('TEST_RRS_CHECK_EXPORT')
  rrsId='rrs_' & ratioId='ratio_'& ratioLogId='ratiolog10_'
  allAvBands=strcompress(self->getUsedWaveLength(), /REMOVE)
  allAvWaves=strcompress(self->getUsedWaveLength(), /REMOVE)
  allAvBands=strupcase(rrsId+allAvBands)
  bandRatioTest=0
  utils=obj_new('Utility')
  rrsBands=utils->stringListToArray(rrsBands, separator=';', /STRING)
  obj_destroy, utils
  
  for i=0, n_elements(rrsBands)-1 do begin
    lambdaIdx=(where(strupcase(rrsBands[i]) eq allAvBands, count))[0]
    if count eq 1 then begin
      thisRRS=reform(allRrs[lambdaIdx,*])
      thisRRS=self->asciiToEnvi(thisRRS, pixelMask=pixelMask, ignoreValue=ignoreValue)
      if hideIdxs[0] ne -1 then thisRRS[hideIdxs]=ignoreValue
      self->addBand, thisRRS, rrsBands[i], /OVERWRITE
      continue
    endif
    ;ratioId='ratio_'
    ratiocheck=strpos(rrsBands[i], ratioId)
    ratiologcheck=strpos(rrsBands[i], ratioLogId)
    if ratiocheck ne -1 then begin
      resString=strmid(rrsBands[i],ratiocheck+strlen(ratioId), strlen(rrsBands[i])-strlen(ratioId) )
      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
      lambdaIdx1=(where(bandNames[0] eq allAvWaves, count1))[0]
      lambdaIdx2=(where(bandNames[1] eq allAvWaves, count2))[0]
      ratioValues=reform(allRrs[lambdaIdx1,*]/allRrs[lambdaIdx2,*])
      ratioValues=self->asciiToEnvi(ratioValues, pixelMask=pixelMask, ignoreValue=ignoreValue)
      if hideIdxs[0] ne -1 then ratioValues[hideIdxs]=ignoreValue
      self->addBand, ratioValues, rrsBands[i], /OVERWRITE
      continue
    endif
    if ratiologcheck ne -1 then begin
      resString=strmid(rrsBands[i],ratiologcheck+strlen(ratiologId), strlen(rrsBands[i])-strlen(ratiologcheck)+1 )
      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
      lambdaIdx1=(where(bandNames[0] eq allAvWaves, count1))[0]
      lambdaIdx2=(where(bandNames[1] eq allAvWaves, count2))[0]
      ratiologValues=alog(reform(allRrs[lambdaIdx1,*]/allRrs[lambdaIdx2,*]))
      ratiologValues=self->asciiToEnvi(ratiologValues, pixelMask=pixelMask, ignoreValue=ignoreValue)
      if hideIdxs[0] ne -1 then ratioValues[hideIdxs]=ignoreValue
      self->addBand, ratioValues, rrsBands[i], /OVERWRITE
      continue
    endif
  endfor
  
END

FUNCTION BiomapOperatorV1::getFilteredBand, productName, ignoreValue=ignorealue

  if n_elements(ignorealue) ne 1 then ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  data=self->getBand(productName, /SETNAN)
  dataApplicability=self->getBand(productName+self->getApplicabilityExtension())
  biomapThreshold=self.app->getKeyValue('BIOMAP_V1_APPLICABILITY_THRESHOLD')
  biomapThreshold=float(biomapThreshold)
  discardIdx=where(~finite(data) or (dataApplicability gt biomapThreshold) or (data eq ignoreValue), count)
  if count ne 0 then data[discardIdx]=!VALUES.F_NAN
  return, data
  
END

FUNCTION BiomapOperatorV1::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  return, call_function('buildBioMapVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
    
END

FUNCTION BiomapOperatorV1::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL

  COMMON smurffCB, mainApp
  
  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  
  ;rrsBands=mainApp->getKeyValue('TEST_RRS_CHECK_EXPORT')
  ;utils=obj_new('Utility')
  ;rrsBands=utils->stringListToArray(rrsBands, separator=';', /STRING)
  ;obj_destroy, utils
  ;extraParInfos=replicate(parInfos[0], n_elements(rrsBands))
  
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi, JULDAY=dType eq 'D')
  endfor
  ;  for i=0, n_elements(extraParInfos)-1 do begin
  ;    ncdfFilenames = [ncdfFilenames, self->buildOperatorResultFileName(dType, rrsBands[i], month, year, sensor, roi, JULDAY=dType eq 'D')]
  ;  endfor
  ;  if ~keyword_set(overwriteFlag) then begin
  ;    ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
  ;    if ~keyword_set(NONE) then parInfos=parInfos[idxs]
  ;  endif
  return, ncdfFilenames
  
END

FUNCTION BiomapOperatorV1::getBandToExportInfoList

  COMMON smurffCB, mainApp
  
  selectedCodes=self->getBandToExportList()
  
  nElem=n_elements(selectedCodes)
  parInfos=replicate(getParameterInfoStruct(), nElem*2)
  for i=0, nElem-1 do begin
    thisPar=self.app->getParameterByCode(selectedCodes[i])
    parInfos[i*2].bandName=thisPar.outputBandName
    parInfos[i*2].shortName=thisPar.code
    parInfos[i*2].longName=thisPar.outputBandName
    parInfos[i*2].displayName=thisPar.outputBandName
    parInfos[i*2].measureUnit=thisPar.measureUnit
    parInfos[i*2].description=thisPar.description
    
    parInfos[i*2+1].bandName=thisPar.outputBandName+self->getApplicabilityExtension()
    parInfos[i*2+1].shortName=thisPar.code+self->getApplicabilityExtension()
    parInfos[i*2+1].longName=thisPar.outputBandName+self->getApplicabilityExtension()
    parInfos[i*2+1].displayName=thisPar.outputBandName+self->getApplicabilityExtension()
    parInfos[i*2+1].measureUnit=''
    parInfos[i*2+1].description=thisPar.description+self->getApplicabilityExtension()
  endfor
  
  rrsBands=mainApp->getKeyValue('TEST_RRS_CHECK_EXPORT')
  utils=obj_new('Utility')
  rrsBands=utils->stringListToArray(rrsBands, separator=';', /STRING)
  obj_destroy, utils
  extraParInfos=replicate(parInfos[0], n_elements(rrsBands))
  
  for i=0, n_elements(extraParInfos)-1 do begin
    extraParInfos[i].bandName=rrsBands[i]
    extraParInfos[i].shortName=rrsBands[i]
    extraParInfos[i].longName=rrsBands[i]
    extraParInfos[i].displayName=rrsBands[i]
    extraParInfos[i].measureUnit='N/A'
    extraParInfos[i].description=rrsBands[i]
    checkRatio=strpos(rrsBands[i], 'ratio_')
    checkrrs= strpos(rrsBands[i], 'rrs_')
    checkRatioLog= strpos(rrsBands[i], 'ratiolog10_')
    if checkRatioLog ne -1 then extraParInfos[i].binsize=0.5
    if checkRatio ne -1 then extraParInfos[i].binsize=0.001
    if checkrrs ne -1 then extraParInfos[i].binsize=0.001
    extraParInfos[i].description=rrsBands[i]
  endfor
  
  if n_elements(extraParInfos) eq 0 then return, parInfos else return, [parInfos, extraParInfos]
  
END

FUNCTION BiomapOperatorV1::getBandList, sensorName, waveLength, FOUND=FOUND

  bandInfoMatrix = self->getBandListBySensorMatrix()
  
  operatorSensorNames=self->getSensorList()
  usedWaveLengthNames=self->getUsedWaveLength()
  
  sensorIdx=(where(sensorName eq operatorSensorNames))[0]
  waveIdx=(where(waveLength eq usedWaveLengthNames))[0]
  
  if sensorIdx[0] eq -1 then message, 'Sensor ', sensorName, ' not mapped. Exit.'
  if waveIdx[0] eq -1 then message, 'Wave Length ', waveLength, ' not mapped. Exit.'
  
  doLog, bandInfoMatrix[waveIdx, sensorIdx], level=0
  return, bandInfoMatrix[waveIdx, sensorIdx]
  
END

FUNCTION BiomapOperatorV1::mapSensorCode, sensorName, FOUND=FOUND

  FOUND=0
  applicationSensorNames = self->getApplicationSensorList()
  operatorSensorNames = self->getSensorList()
  idx=where(sensorName eq applicationSensorNames)
  
  if idx[0] ne -1 then begin
    FOUND=1
    return, operatorSensorNames[idx[0]]
  endif
  
  message, 'Sensor: ', sensorName, 'not available for Biomap Operator'
  return, ''
  
END

FUNCTION BiomapOperatorV1::enviToAscii, enviFileName, sensorCode, ignoreValue, textFileName=textFileName, pixelMask=pixelMask, pixelNumber=pixelNumber

  COMMON smurffCB, mainApp
  COMMON hdInfo, bandInfo
  
  tempDir=mainApp->getKeyValue('TEMP_DIR')
  fs=mainApp->getFileSystem()
  utility=mainApp->getUtility()
  fileName=fs->removeFileExtension(enviFileName)
  
  genOp=obj_new('GenericOperator', mainApp, tempDir, fileName=fileName, /OPEN)
  bNames=genOp->getBandNames()
  
  parameters=mainApp->getParameters()
  nElemPhysPar=n_elements(bNames)
  physParInfo=replicate(mainApp->getParameterByOutputBandName(bnames[0]), nElemPhysPar)
  waveLenghtList=strcompress(self->getUsedWaveLength(), /REMOVE)
  howMany=n_elements(waveLenghtList)
  
  for i=1, nElemPhysPar-1 do begin
    testPhys=mainApp->getParameterByOutputBandName(bnames[i], EXISTS=EXISTS)
    if keyword_set(EXISTS) then physParInfo[i]=testPhys else physParInfo[i].stdWaveLength='N/A'
  endfor
  stdWaveLengthList=physParInfo[*].stdWaveLength
  isWaveList=physParInfo[*].isRrs
  
  ;bandName=strarr(n_elements(waveLenghtList))
  datas=ptrarr(n_elements(waveLenghtList))
  mask=byte(genOp->getBand(physParInfo[0].outputBandName, /NOMASK))
  mask[*]=1b
  for i=0, n_elements(waveLenghtList)-1 do begin
    idx=(where(waveLenghtList[i] eq stdWaveLengthList))[0]
    bInfo=physParInfo[idx]
    doLog, waveLenghtList[i], ' --> ', bInfo.outputBandName, level=0
    thisData=genOp->getBand(bInfo.outputBandName, /NOMASK)
    mask=mask * (~(thisData le 0. or thisData eq ignoreValue))
    datas[i]=ptr_new(thisData, /NO_COPY)
  endfor
  ; MM 2013 04 22 new masking start
  masking=where(mask ne 1, count)
  if count gt 0 then begin
    for i=0, n_elements(waveLenghtList)-1 do begin
      (*datas[i])[masking]=ignoreValue
    endfor
  endif
  ; MM 2013 04 22 new masking end
  
  if n_elements(textFileName) ne 1 then textFileName=tempDir+path_sep()+utility->getSysTime(/FILECOMPATIBILITY)+'.dat'
  
  pixelNo=n_elements(*datas[0])
  openW, lun, textFileName, /GET_LUN
  headerInfo=''
  for i=0, n_elements(waveLenghtList)-1 do headerInfo=headerInfo+self->getBandList(sensorCode, stdWaveLengthList[i])+','
  bandInfo=strmid(headerInfo, 0, strlen(headerInfo)-1)
  extraBandNames=['sun_zenith','view_zenith','rel_azimuth']
  for i=0, n_elements(extraBandNames)-1 do headerInfo=headerInfo+extraBandNames[i]+','
  headerInfo=strmid(headerInfo, 0, strlen(headerInfo)-1)
  printf, lun, headerInfo
  pixelMask=1b-bytarr(pixelNo)
  for i=0l, pixelNo-1 do begin
    pixelLine=''
    for j=0l, n_elements(datas)-1 do begin
      value=(*datas[j])[i]
      if value eq ignoreValue then begin
        pixelMask[i]=0b
        break
      ;continue
      endif
      pixelLine=pixelLine+','+strcompress(value,/REMOVE)
    endfor
    ; fake sun angle related values
    if pixelMask[i] eq 0b then continue
    for j=0,n_elements(extraBandNames)-1 do pixelLine=pixelLine+','+'1'
    pixelLine=strmid(pixelLine, 1, strlen(pixelLine)-1)
    printF, lun, pixelLine
  endfor
  close, lun & free_lun, lun
  pixelNumber=pixelNo
  
  self.lastDatFile=textFileName
  obj_destroy, genOp
  for i=0, n_elements(datas)-1 do ptr_free, datas[i]
  if total(fix(pixelMask)) eq 0 then begin
    file_delete, textFileName
    return, ''
  endif
  return, textFileName
  
END

FUNCTION BiomapOperatorV1::mapRoiCode, roiName, FOUND=FOUND

  FOUND=0
  applicationROI = self->getApplicationRoiList()
  operatorROI = self->getRoiList()
  idx=(where(roiName eq applicationROI))[0]
  
  if idx[0] ne -1 then begin
    FOUND=1
    return, operatorROI[idx[0]]
  endif
  message, 'roi: '+roiName+' not available for Biomap Operator'
  return, 0
  
END

FUNCTION BiomapOperatorV1::asciiToEnvi, linearData, pixelMask=pixelMask, ignoreValue=ignoreValue

  idxs=where(pixelMask eq 1b, count)
  linearData=reform(linearData)
  pixelNo=n_elements(pixelMask)
  ;self->updateFid
  dims=self->getDataDimensions()
  xNb=dims[2]+1
  yNb=dims[4]+1
  dataValues=fltarr(xNb, yNb, /NO)
  dataValues[*]=ignoreValue
  next=0l
  doLog, '-->', count, n_elements(linearData), level=0
  if count ne n_elements(linearData) then stop
  
  for i=0l, pixelNo-1 do begin
    ;for j=0, n_elements(datas)-1 do begin
    if pixelMask[i] then begin
      value=float(linearData[next])
      dataValues[i]=value
      next++
    endif
  endfor
  
  return, dataValues
  
END

FUNCTION BiomapOperatorV1::getRoiList

  return, ['adrs', 'adrs', 'blks', 'blts']
  
END

FUNCTION BiomapOperatorV1::getApplicationRoiList

  ;return, ['ADRSEA', 'BLCK', 'BALT']
  ;return, ['MEDSEA', 'BLCK', 'BALT']
  return, ['NADR', 'ADRI', 'BLCK', 'BALT']
  
END

FUNCTION BiomapOperatorV1::getBandListBySensorMatrix

  bandBySensorList = [ $
    ['swf413','swf443', 'swf490', 'swf510', 'swf555', 'swf670'], $
    ['mod412','mod443', 'mod488', 'mod530', 'mod555', 'mod667'], $
    ['mer413', 'mer443', 'mer490', 'mer510', 'mer560', 'mer665'], $
    ['rrs412', 'rrs442', 'rrs491', 'rrs511', 'rrs555', 'rrs665'] ]
  return, bandBySensorList
  
END

FUNCTION BiomapOperatorV1::getUsedWaveLength

  return, [412, 440, 490, 510, 555, 670]
  
END


;FUNCTION BiomapOperatorV1::getParameterList
;
;  Utility=self.app->getUtility()
;  parCodeList=Utility->stringListToArray(self.app->getKeyValue('BIOMAP_PAR_LIST'), sep=';')
;  return, parCodeList
;
;END

FUNCTION BiomapOperatorV1::getApplicationSensorList

  sensorList = ['JRC', 'MODIS_EUR', 'N/A', 'N/A']
  return, sensorList
  
END

FUNCTION BiomapOperatorV1::getSensorList

  sensorList = ['swf', 'mod', 'mer', 'insitu']
  return, sensorList
  
END

PRO BiomapOperatorV1::setLastDatFile, fileName

  self.lastDatFile=fileName
  
END

FUNCTION BiomapOperatorV1::getLastDatFile

  return, self.lastDatFile
  
END

PRO BiomapOperatorV1::setConfFile, fileName

  self.confFile=fileName
  
END

FUNCTION BiomapOperatorV1::getConfFile

  return, self.confFile
  
END

FUNCTION BiomapOperatorV1::getMainFileName

  fName=self->GenericOperator::getMainFileName()
  if fName eq '' then return, './test.dat' else return, fName
  
END

PRO BiomapOperatorV1::setFoqFile, fileName

  self.foqFile=fileName
  
END

FUNCTION BiomapOperatorV1::getFoqFile

  ;if self.foqFile ne '' then return, self.foqFile else return, '.\fq.dat'
  return, self.app->getKeyValue('BIOMAP_FQ_FILE')
  
END

FUNCTION BiomapOperatorV1::isTest

  return, self.mainFileName eq ''
  
END

PRO BiomapOperatorV1::buildInputFile, sensor, ignoreValue, pixelMask=pixelMask, textInputfile=textInputfile, pixelNumber=pixelNumber

  if self->isTest() then begin
    isEnvi=0
    textInputfile=self->getMainFileName()
  endif
  if n_elements(textInputfile) eq 1 then return
  inputFileName=self->getMainFileName()
  if self->isEnviType() then asciiFile=self->enviToAscii(inputFileName, sensor, ignoreValue, pixelMask=pixelMask, pixelNumber=pixelNumber) else asciiFile=self.mainFileName
  textInputfile=asciiFile
  
END

FUNCTION BiomapOperatorV1::getApplicabilityExtension

  return, '_(applicability)'
  
END

PRO BiomapOperatorV1::cleanFileSystem

  self->removeMainFile
  self->removeLastDatFile
  
END

PRO BiomapOperatorV1::removeLastDatFile

  if self.lastDatFile ne '' then file_delete, self.lastDatFile, /ALLOW_NONEXISTENT, /QUIET
  
END

PRO BiomapOperatorV1::setH5Struct, strc

  ptr_free, self.h5Struct
  self.h5Struct = ptr_new(strc, /NO_COPY)
  
END

FUNCTION BiomapOperatorV1::getH5Struct

  ;doLog, /STACK, callingRoutine=callingRoutine
  ;doLog, 'before:', callingRoutine
  ;help, /memory
  if ptr_valid(self.h5Struct) then begin
    a=0
  ;strc=*self.h5Struct
  endif else begin
    a=0
    self->loadH5Struct, fileName
  ;strc=*self.h5Struct
  endelse
  ;wait, 3
  ;doLog, 'after:'
  ;help, /memory
  return, *self.h5Struct
  
END

PRO BiomapOperatorV1::loadH5Struct, fileName

  if self->getConfFile() eq '' then algoFile = '.\regionalgo.h5' else algoFile=self->getConfFile()
  strc = h5_parse(algoFile, /read_data)
  self->setH5Struct, strc
  
END

PRO BiomapOperatorV1::CleanUp

  self->cleanFileSystem
  self-> GenericOperator::Cleanup
  
END

FUNCTION BiomapOperatorV1::init, application, workingDir, periodType, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  if n_elements(confFile) eq 1 then self.confFile=confFile else message, 'Please, set the configuration File...'
  if n_elements(foqFile) eq 1 then self.foqFile=foqFile else doLog, 'Warning, foqFile not present...', level=1
  return, 1
  
END

PRO BiomapOperatorV1__Define

  Struct = { BiomapOperatorV1 , $
    confFile: '', $
    lastDatFile: '', $
    h5Struct: ptr_new(), $
    Inherits GenericOperator $
    }
    
END