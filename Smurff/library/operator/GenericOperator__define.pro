@..\library\ncdf_tools\ncdf_routines
FUNCTION GenericOperator::getBandsInfo, bandCodes

  parInfos=replicate(getParameterInfoStruct(), n_elements(bandCodes))
  for i=0, n_elements(bandCodes)-1 do begin
    thisPar=self.app->getParameterByCode(bandCodes[i])
    parInfos[i].bandName=thisPar.outputBandName
    parInfos[i].shortName=thisPar.code
    parInfos[i].longName=thisPar.outputBandName
    parInfos[i].displayName=thisPar.outputBandName
    parInfos[i].measureUnit=thisPar.measureUnit
    parInfos[i].description=thisPar.description
  endfor
  
  return, parInfos
  
END

FUNCTION GenericOperator::isBandToExport, bandCode

  if ptr_valid(self.bandToExportList) then expBands=*self.bandToExportList else return, 0
  idx=where(bandCode eq expBands, count)
  if count eq 1 then return, 1 else return, 0
  
END

FUNCTION GenericOperator::prepareBands, fileNames, parCode, NOTFOUND=NOTFOUND

  NOTFOUND=1
  bands=-1
  for i=0, n_elements(fileNames)-1 do begin
    fileName=fileNames[i]
    bandInfo=self->readNcdfVar(fileName, parCode, FOUND=FOUND1, /REVERSE, TRANSPOSE=TRANSPOSE)
    if ~keyword_set(FOUND1) then continue
    mapIn=readNcdfGeoInfo(fileName, /ADDPIXEL)
    ;ENVI_OPEN_DATA_FILE, fileName, r_fid=fid, /HDF_SD, HDFSD_DATASET=index
    ;ENVI_FILE_QUERY, Fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
    
    ;stepLon = (ABS(mapLimits[3]-mapLimits[1]))/(ns-1)
    ;stepLat = (ABS(mapLimits[2]-mapLimits[0]))/(nl-1)
    
    ; set the map info to the regular grid in ENVI file
    ;ps = [stepLon, stepLat]
    ;mc = [0.5D, 0.5D, mapLimits[1], mapLimits[2]]
    mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mapIn.mc, ps=mapIn.ps)
    
    ;NOTFOUND=1-keyword_set(FOUND)
    ;data=eRegStatOperator->readNcdfVar(fileName, parCode, FOUND=FOUND1, /REVERSE, TRANSPOSE=TRANSPOSE)
    mask=where(bandInfo.data ne -9999 and bandInfo.data ne 0 and finite(bandInfo.data), cnt)
    sizeInfo=size(bandInfo.data, /STRUCT)
    
    bands={data:bandInfo.data, mask:mask, mapInfo:mapInfo, nl:sizeInfo.dimensions[1], ns:sizeInfo.dimensions[0], dt:sizeInfo.type}
    NOTFOUND=0
  endfor
  return, bands
  
END

FUNCTION GenericOperator::readDataFromFile, periodType, month, year, dataDir, readFunction, parCode, $
    roiCode, roiArchiveCode, outputDir, $
    refRoi=refRoiCode, outMapInfo=outMapInfo, elabName=elabName, $
    FOUND=FOUND, ignoreValue=ignoreValue, fileName=fileName, $
    EXTRACT=EXTRACT;, prepareDataDir
    
  FOUND=0
  if keyword_set(EXTRACT) then begin
    band = call_function(readFunction+'_'+periodType, $
      periodType, month, year, roiCode, roiArchiveCode, $
      dataDir, outputDir, parCode, refRoi=refRoiCode, outMapInfo=outMapInfo, $
      NOTFOUND=NOTFOUND, GETCHLVAR='ALG_CHL' eq parCode, $
      /SETNAN, /GETMAPINFO, elabName=elabName)
    if ~NOTFOUND then begin
      FOUND=1
      dataInfo={name:parCode, idx:0, data:band}
      outMapInfo=outMapInfo
    endif
  endif else begin
    fileName=call_function(readFunction+'_'+periodType, $
      year, month, periodType, roiCode, sensor, parCode, dataDir, /FULLPATH)
      
    fInfo=file_info(fileName)
    if fInfo.exists then begin
      parInfo=self.app->getParameterByCode(parCode)
      scaleType=self->readNcdfAttr(fileName, parInfo.outputBandName, 'scaling', FOUND=FOUNDST);/GLOBAL
      dataInfo=self->readNcdfVar(fileName, parInfo.outputBandName, FOUND=FOUND, /REVERSE)
      if FOUNDST and strlowcase(scaleType) eq 'log10' then begin
        ignoreCount=0
        if n_elements(ignoreValue) eq 1 then ignoreIdxs=where(dataInfo.data eq ignoreValue, ignoreCount)
        dataInfo.data=10^dataInfo.data
        if ignoreCount ne 0 then dataInfo.data[ignoreIdxs]=ignoreValue
      endif
    endif
  endelse
  if keyword_set(FOUND) then begin
    dataInfo.data=self.app->applyRangeConditions(parCode, dataInfo.data, parCode, ignoreValue=ignoreValue)
    return, dataInfo
  endif
  
END

FUNCTION GenericOperator::getCompatibleData, dataPtrs, hideIdxs=hideIdxs, hideCount=hideCount, validIdxs=validIdxs, validCount=validCount, maskCondition=maskCondition, outPercentage=outPercentage

  execCommand=''
  for i=0, n_elements(dataPtrs)-1 do begin
    if ptr_valid(dataPtrs[i]) then begin
      thisBand='band'+strcompress(i+1, /REMOVE_ALL)
      res=execute(thisBand+'=*dataPtrs['+strcompress(i, /REMOVE_ALL)+']')
      execCommand=execCommand+'finite('+thisBand+') and '
    endif
  endfor
  execCommand=strmid(execCommand, 0, strlen(execCommand)-4)
  res=execute('validIdxs=where('+execCommand+', validCount, complement=hideIdxs, ncomplement=hideCount)')
  ;validIdxs=where(finite(*dataPtrs[0]) and finite(*dataPtrs[1]), validCount, complement=hideIdxs, ncomplement=hideCount)
  size1=size(band1)
  size2=size(band2)
  doLog, size1, LEVEL=4
  doLog, size2, LEVEL=4
  if n_elements(band1) ne 0 and validCount gt 0 then begin
    aa=where(band1[validIdxs] le 0, cnt)
    doLog, 'not zero?', cnt
  endif
  if n_elements(band2) ne 0 and validCount gt 0 then begin
    aa=where(band2[validIdxs] le 0, cnt)
    doLog, 'not zero?', cnt
  endif
  if n_elements(size1) eq n_elements(size2) then begin
    check=total(size1 ne size2 and validCount gt 0)
    if check gt 0 then doLog, 'No valid data to compare!', LEVEL=4
  endif
  outPercentage=1.*validCount / (n_elements(band1) > n_elements(band2)) * 100.
  return, validIdxs
  
END

FUNCTION GenericOperator::buildMaskConditions, parInfo, scallingType

  checkMaskConditions=''
  eps=self.app->getKeyValue('EPS')
  ignoreValue=self.app->getKeyValue('NAN_VALUE')
  ;maskExpr='(band eq '+ignoreValue+') or (band gt -'+eps+' and band lt '+eps+') or (finite(band) ne 1)'
  ;june 20th remove 0 as NaN
  maskExpr='(band eq '+ignoreValue+')'+' or (finite(band) ne 1)'
  
  checkMaskConditions[*]=maskExpr
  ;for i=0, length-1 do if strpos(outScallingTypeList[i], 'log') gt -1 then checkMaskConditions[i]='data eq -9999' else checkMaskConditions[i]='data le 0'
  if strpos(scallingType, 'log') gt -1 then checkMaskConditions=maskExpr else checkMaskConditions=maskExpr
  return, checkMaskConditions
  
END

FUNCTION GenericOperator::getMemoryBands

  idxs=where(self.memoryBandNames ne '', count)
  if count ne 0 then return, self.memoryBandNames[idxs] else return, ''
  
END

FUNCTION GenericOperator::isEnviType

  return, self.isEnviType
  
END

FUNCTION GenericOperator::hasGridReference

  existsLat=self->isABand('lon2d')
  existsLon=self->isABand('lat2d')
  return, existsLat and existsLon
  
END

FUNCTION GenericOperator::convertFileName, fileName, REMOVE_EXTENSION=REMOVE_EXTENSION

  testfileName=fileName
  isEnviType=self->isAnEnviFile(testfileName)
  if isEnviType or keyword_set(REMOVE_EXTENSION) then testFileName=self->removeFileExtension(testfileName) ;do it it always, just to be sure...
  return, testfileName
  
END

FUNCTION GenericOperator::isAnEnviFile, fileName

  fs=self.app->getFileSystem()
  sourceFileName=fs->getFileNameInfo(fileName, filePath=filePath, extension=extension)
  if strupcase(extension) eq 'ENVI' or strupcase(extension) eq 'HDR' or extension eq '' then return, 1
  return, 0
  
END

FUNCTION GenericOperator::isAnNcFile, fileName

  app=self->getApp()
  fs=app->getFileSystem()
  sourceFileName=fs->getFileNameInfo(fileName, filePath=filePath, extension=extension)
  if strupcase(extension) eq 'NC' then return, 1
  return, 0
  
END

FUNCTION GenericOperator::removeFileExtension, fileName

  fs=obj_new('FileSystem', /STAND_ALONE)
  fName=fs->removeFileExtension(fileName)
  obj_destroy, fs
  return, fName
  
END

FUNCTION GenericOperator::getFileToOverwriteList, month, year, outputDir, sensor, roi, overwriteFlag=overwriteFlag, parInfo=parInfo, NONE=NONE, fileToPreserveNo=fileToPreserveNo, fileToPreserveIdx=fileToPreserveIdx, allFiles=allFiles

  NONE=0
  resFileList=self->getFileNameToExport(month, year, sensor, roi, parInfo=parInfo, NONE=NONE, archiveDir=outputDir)
  allFiles=resFileList
  fileToPreserveNo=0
  fileToOverwriteList=['']
  if keyword_set(NONE) then count=0 else count=n_elements(resFileList)
  if (~keyword_set(overwriteFlag)) then begin
    fInfos=file_info(outputDir+path_sep()+resFileList)
    idx=where(fInfos.exists eq 0, count, complement=fileToPreserveIdx, ncomplement=fileToPreserveNo)
    if count ne 0 then fileToOverwriteList=resFileList[idx]
  endif else begin
    fileToOverwriteList=resFileList
  endelse
  if count eq 0 then NONE=1
  return, fileToOverwriteList
  
END

PRO GenericOperator::setPeriodType, value

  self.periodType=value
  
END

FUNCTION GenericOperator::getPeriodType

  return, self.periodType
  
END

FUNCTION GenericOperator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, archiveDir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  message, 'You must implement this one on your child class!!!'
  return, ''
  
END

PRO GenericOperator::writeAsNCDF, month, year, sensor, roi, $
    archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, refFileName=refFileName, $
    JULDAY=JULDAY, INTERVAL=INTERVAL, outScallingTypeList=outScallingTypeList, applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
  ;archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, refFileName=refFileName, $
  ;JULDAY=JULDAY, INTERVAL=INTERVAL, outScallingTypeList=outScallingTypeList, applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
    
  dType=self->getPeriodType()
  
  fileList=self->getFileNameToExport(month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, JULDAY=JULDAY, INTERVAL=INTERVAL);, overwriteFlag=overwriteFlag)
  if n_elements(fileToOverwriteList) eq 0 then fileToOverwriteList=fileList
  checkScaleType=n_elements(outScallingTypeList) gt 0
  for i=0, n_elements(fileList)-1 do begin
    ncdfFilename=fileList[i]
    idx=where(ncdfFilename eq fileToOverwriteList, count)
    if ~(self->isABand(parInfos[i].bandName)) then begin
      doLog, 'Band : '+parInfos[i].bandName+' doesn''t exist in '+self.mainFileName+'. Skip', level=0 ;4
      continue
    end
    if count eq 1 then begin
      if checkScaleType then scallingType=outScallingTypeList[i] else scallingType='linear'
      if n_elements(HISTOSTRUCT) eq 1 then begin
        HISTOSTRUCT.cutvalue=parinfos[i].cutvalue
        HISTOSTRUCT.binsize=parinfos[i].binsize
      endif
      self->exportBandAsGeoNc, parInfos[i].bandName, parInfos[i], ncdfFilename, year, month, archiveDir, $
        refFileName=refFileName, extraMaskConditions=extraMaskConditions, scallingType=scallingType, $
        applyFormula=applyFormula, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
    endif else begin
      doLog, ncdfFilename, ' still exists in the file system', level=3
    endelse
  endfor
;res={kvis:kvis, pp: PP_c2chl_inf_kvis , zeu: zeu_kvis }
  
END

FUNCTION GenericOperator::getFileNameToExport, month, year, sensor, roi, archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL

  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].bandName, month, year)
  endfor
  ;if ~keyword_set(overwriteFlag) then begin
  ;  ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
  ;  if ~keyword_set(NONE) then parInfos=parInfos[idxs]
  ;endif
  return, ncdfFilenames
  
END

FUNCTION GenericOperator::checkResultFileExistence, fileList, folder, idxs=idxs, NONE=NONE

  NONE=1
  listInfo=file_info(folder+path_sep()+fileList)
  notPresentIdx=where(listInfo.EXISTS ne 1, count)
  if count gt 0 then begin
    fileList=fileList[notPresentIdx]
    NONE=0
    idxs=notPresentIdx
  endif
  return, fileList
  
END

PRO GenericOperator::writeResult, month, year, sensor, roi, $
    archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, JULDAY=JULDAY, INTERVAL=INTERVAL, $
    outScallingTypeList=outScallingTypeList, applyFormula=applyFormula
  ;archiveDir=archiveDir, overwriteFlag=overwriteFlag, fileToOverwriteList=fileToOverwriteList, JULDAY=JULDAY, INTERVAL=INTERVAL, $
  ;outScallingTypeList=outScallingTypeList, applyFormula=applyFormula
    
  prevLog=self.app->getLogLevel()
  self.app->setLogLevel, 1
  self->writeAsNCDF, month, year, sensor, roi, archiveDir=archiveDir, overwriteFlag=overwriteFlag, $
    fileToOverwriteList=fileToOverwriteList, JULDAY=JULDAY, INTERVAL=INTERVAL, $
    outScallingTypeList=outScallingTypeList, applyFormula=applyFormula
  self.app->setLogLevel, prevLog
  
END

PRO GenericOperator::removeMainFile

  fs=self.app->getFileSystem()
  if self.mainFileName ne '' then fs->removeEnviFile, self.mainFileName
  
END

FUNCTION GenericOperator::getBandToExportInfoList

  selectedCodes=self->getBandToExportList()
  
  parInfos=replicate(getParameterInfoStruct(), n_elements(selectedCodes))
  for i=0, n_elements(selectedCodes)-1 do begin
    thisPar=self.app->getParameterByCode(selectedCodes[i])
    parInfos[i].bandName=thisPar.outputBandName
    parInfos[i].shortName=thisPar.code
    parInfos[i].longName=thisPar.outputBandName
    parInfos[i].displayName=thisPar.outputBandName
    parInfos[i].measureUnit=thisPar.measureUnit
    parInfos[i].description=thisPar.description
  endfor
  
  return, parInfos
  
END

PRO  GenericOperator::configureMaskFromMemory, dataValues, maskCondition

  self->setMaskBand, dataValues
  self->setMaskCondition, maskCondition
  
END

PRO  GenericOperator::copyMaskInfo, destinationOperator

  mBand=self->getMaskBand()
  destinationOperator->setMaskBand, mBand
  destinationOperator->setMaskCondition, self->getMaskCondition()
  
END

PRO  GenericOperator::configureMask, fileName, varName, maskCondition

  ;ignoreValue=0.
  doLog, 'Reading data for masking from: '+fileName, level=0
  euroMask=self->readNcdfVar(fileName, varName)
  euroMask.data=reverse(euroMask.data, 2)
  self->setMaskBand, euroMask.data
  ;util->strreplace(maskCondition, listOfBand[i], 'b'+strcompress(idx+1, /REMOVE_ALL))
  ;maskCondition=util->strreplace(maskCondition, 'ignoreValue', strcompress(ignoreValue, /REMOVE_ALL))
  self->setMaskCondition, maskCondition
;operator->setMaskCondition, 'finite(maskBand) eq 0 or maskBand eq '+strcompress(ignoreValue, /REMOVE)
  
END

;FUNCTION GenericOperator::ncdfVarCheck, fileName, GLOBAL=GLOBAL
;
; fileID = ncdf_open(fileName)
;
;
; return, 0
;
;END

FUNCTION GenericOperator::readNcdfAttr, fileName, varName, attrName, FOUND=FOUND, GLOBAL=GLOBAL

  ERROR=0
  catch, error_status
  FOUND=0
  res={name:'', idx:0, data:0}
  if error_status NE 0 THEN BEGIN
    catch, /CANCEL
    ncdf_close, fileID
    doLog, 'attrName: ', attrName, ' not found.', level=4
    FOUND=0
    return, -1
  endif
  
  fileID = ncdf_open(fileName)
  if ~keyword_set(GLOBAL) then begin
    varId=NCDF_VARID(fileID, varName)
    NCDF_ATTGET, fileID, varId, attrName, attrvalue
    FOUND=1
  endif else begin
    NCDF_ATTGET, fileID, varName, attrvalue, /GLOBAL
    FOUND=1
  endelse
  ncdf_close, fileID
  return, attrvalue
  
END

;FUNCTION GenericOperator::cropping, infilenames, inputVarList, outputVarList, conv_functions, $
;    tempDir, roiName, ignoreValue, NOTFOUND=NOTFOUND, MEMORY=MEMORY, targetCropInfo=targetCropInfo
FUNCTION GenericOperator::cropping, bandInfos, outputVarList, $
    tempDir, roiName, ignoreValue, NOTFOUND=NOTFOUND, MEMORY=MEMORY, targetCropInfo=targetCropInfo
    
  fileWriteFlag=(~keyword_set(MEMORY)) or (n_elements(targetCropInfo) eq 1)
  NOTFOUND=0
  varNo=n_elements(bandInfos)
  ;fileNo=n_elements(infilenames)
  var_file = intarr(varNo);
  varFileRes=intarr(varNo)
  
  utils=self.app->getUtility()
  fS=self.app->getFileSystem()
  pathSep=path_sep()
  
  tempfName=utils->getSysTime(/FILECOMPATIBILITY)
  enviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_temp_jrc.envi'
  intermediateEnviFileName=tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_int_out_jrc.envi'
  finalEnviFileName = tempDir+pathSep+tempfName+'_'+strcompress(roiName)+'_out_jrc.envi'
  if keyword_set(MEMORY) then memoryData=ptrarr(varNo) else memoryData=-1
  
  if fileWriteFlag then openw, lun, enviFileName, /GET_LUN
  ;fakeBand=readSingleBand('mld', infilenames, mask, NOTFOUND=NOTFOUND)
  ;print All
  for i=0, varNo-1 do begin
  
    ;band=readSingleBand(inputVarList[i], infilenames, mask, NOTFOUND=NOTFOUND)
    band=bandInfos[i]
    ;if ~keyword_set(NOTFOUND) then begin
    copyData=band.data
    dataSize=size(band.data, /STRUCT)
    if fileWriteFlag then writeu, lun, copyData
    if keyword_set(MEMORY) then begin
      memoryData[i]=ptr_new(copyData, /NO_COPY)
    endif
    validIdxs=where(band.data ne -9999 and band.data gt 0 and finite(band.data), cnt)
    if cnt ne 0 then doLog, '**'+outputVarList[i]+strcompress(max(band.data))+strcompress(min(band.data))+strcompress(mean(band.data))+'**' else doLog, '**'+'No data'+'**'
    mask=band.mask
  ;endif else begin
  ;  if fileWriteFlag then begin
  ;    close, lun
  ;    free_lun, lun
  ;  endif
  ;  return, -1
  ;endelse
    
  endfor
  if fileWriteFlag then begin
    close, lun
    free_lun, lun
  endif
  
  finalMask=mask
  mask=0b
  
  ; apply mask to each band
  if fileWriteFlag then begin
    applyMaskToEnviFile, enviFileName, intermediateEnviFileName, varNo, band.data, finalMask, ignoreValue
    ; setup the header in the ENVI file
    ;intermediateEnviFileName1=fs->removeFileExtension(intermediateEnviFileName)
    
    ;help, band, /STRUCT
    envi_setup_head_oxy,  $
      FNAME=fs->removeFileExtension(intermediateEnviFileName),$
      NS=band.ns,$
      NL=band.nl, $
      DATA_IGNORE_VALUE=ignoreValue, $
      NB=varNo, $
      DATA_TYPE=band.dt, $
      FILE_TYPE=0, $
      INTERLEAVE=0, $
      R_FID=stackFid, $
      MAP_INFO=band.mapinfo, $
      /WRITE,$
      /OPEN, $
      BNAMES=outputVarList
  endif
  ;doLog, band.ns, band.nl
  ;doLog, band.mapinfo.ps, band.mapinfo.mc
  
  if n_elements(targetCropInfo) eq 1 then begin
    PIXEL_SIZE=[abs(targetCropInfo.mapWindowBoundary[1]-targetCropInfo.mapWindowBoundary[0])/targetCropInfo.mapPixelExtension[0], abs(targetCropInfo.mapWindowBoundary[3]-targetCropInfo.mapWindowBoundary[2])/targetCropInfo.mapPixelExtension[1]]
    mosaicFile=doMosaic(intermediateEnviFileName, finalEnviFileName, ignoreValue, targetCropInfo, PIXEL_SIZE=PIXEL_SIZE);/PRESERVE_RESOLUTION)
    fs->correctEnviHeaderFileName, mosaicFile
    ENVI_OPEN_DATA_FILE, mosaicFile, r_fid=fid;, /HDF_SD, HDFSD_DATASET=sourceDataSetIndex
    ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
    doLog, 'GenericOperator::cropping-', ' dims,', 'ns,', 'nl: ', dims, ns, nl, LEVEL=4
    ;doLog, dataSize, LEVEL=4
    if keyword_set(MEMORY) then begin
      for i=0, varNo-1 do begin
        thisData = ENVI_GET_DATA(fid=fid, dims=[-1l, 0, ns-1, 0, nl-1], pos=i)
        ptr_free, memoryData[i]
        memoryData[i]=ptr_new(thisData, /NO_COPY)
      endfor
    endif
    doLog, '...crop from ' + roiName + ' found.' , LEVEL=3
    envi_file_mng, id=fid, /REMOVE
    ;remove envi intermediate file!!!
    removeEnviFiles, intermediateEnviFileName;, /ALL
  ;file_delete, intermediateEnviFileName, /ALLOW
  endif else begin
    finalEnviFileName=intermediateEnviFileName
  endelse
  if (keyword_set(MEMORY)) then begin
  ;here extract data
  endif
  
  ;fileName=fs->getFileNameInfo(infilenames[0], filePath=filePath, extension=extension)
  band=0b
  
  ;year = strmid(fileName, 1, 4);
  ;jday = strmid(fileName, 5, 3);
  
  ;caldat, julday(1,1,year) + jday, resMonth, resDay, resYear
  
  ;month = resMonth;
  
  return, {enviDataFile:finalEnviFileName, year:'N/A', month:'N/A', jday:'N/A', memoryData:memoryData}
END

FUNCTION GenericOperator::readNcdfVar, fileName, datasetName, FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE, targetCropInfo=targetCropInfo

  FOUND=0
  res={name:'', idx:0, data:0}
  if NCDF_IsValidFile(fileName) then begin
  
    fileID = ncdf_open(fileName)
    
    catch, error_status
    fileinq_struct=ncdf_inquire(fileID)
    ERROR=0
    
    if error_status NE 0 THEN BEGIN
      ERROR=1
      catch, /CANCEL
      ncdf_close, fileID
      doLog, 'dataSetName: ', datasetName, ' not found.', level=2
      return, res
    endif
    res={name:datasetName, idx:-1, data:0}
    
    for i=0, 100 do begin
      varinq_struct=ncdf_varinq(fileID,i)
      varname = varinq_struct.name
      dimensions = varinq_struct.dim
      numatts = varinq_struct.natts
      ;doLog, varname, LEVEL=4
      if strupcase(varinq_struct.name) eq strupcase(datasetName) then begin
        ;attname=ncdf_attname(fileID,i,attndx)
        ;ncdf_attget,fileID,varndx,attname,value
        varID=ncdf_varid(fileID,varname)
        ncdf_varget,fileID,varID,variable
        doLog, callingRoutine=callingRoutine, /STACK
        doLog, callingRoutine, fileName, LEVEL=4
        ndims=size(variable, /N_DIM)
        if keyword_set(REVERSE) then variable=reverse(temporary(variable), ndims)
        if keyword_set(TRANSPOSE) then variable=transpose(temporary(variable))
        res={name:datasetName, idx:i, data:variable}
        FOUND=1
        break
      endif
    endfor
    ncdf_close, fileID
  endif
  ; now crop!!!
  return, res
  
END

;FUNCTION GenericOperator::fileOpen, filename, _EXTRA = extra
;
;  ncdf_close, self.mainFid
;  if keyword_set(extra) then begin
;
;    fileId = ncdf_open(filename, _EXTRA = extra )
;
;  endif else begin
;
;    fileId = ncdf_open(filename)
;
;  endelse
;
;  self.mainFileName=filename
;  self.mainFid=fileId
;
;END

FUNCTION GenericOperator::getNcdfDataValues, fileName, varName, SET_NAN=SET_NAN, missingAttribName=missingAttribName, REVERSE=REVERSE, $
    FOUND=FOUND
    
  FOUND=0
  res=self->readNcdfVar(fileName, varName, REVERSE=REVERSE, FOUND=FOUND)
  if keyword_set(FOUND) then begin
    if keyword_set(SET_NAN) then begin
      missingAttr=self.app->getKeyValue('MISSING_VALUE_ATTRIB_IN')
      missingAttrs=strsplit(missingAttr, ';')
      for i=0, n_elements(missingAttrs)-1 do begin
        missing=self->readNcdfAttr(fileName, varName, missingAttr)
      endfor
      idxs=where(res.data eq missing, count)
      if count ne 0 then res.data[idxs] = !VALUES.F_NAN
    endif
  endif
  return, res
  
END

FUNCTION GenericOperator::getMaskBand

  if ptr_valid(self.maskBand) then maskBand=*self.maskBand
  return, maskBand
  
END

PRO GenericOperator::setMaskBand, maskData

  ptr_free, self.maskBand
  self.maskBand= ptr_new(maskData, /NO_COPY)
  
END

FUNCTION GenericOperator::getMaskCondition

  return, self.maskCondition
  
END

PRO GenericOperator::setMaskCondition, maskCondition

  self.maskCondition = maskCondition
  
END

FUNCTION GenericOperator::hasMask

  if ptr_valid(self.maskBand) then return, 1 else return, 0
  
END

FUNCTION GenericOperator::getMaskIndxsInfo

  maskBand=self->getMaskBand()
  executeConditionString='maskIdxs=where('
  executeConditionString=executeConditionString+self.maskCondition
  executeConditionString=executeConditionString+', maskCount, '
  executeConditionString=executeConditionString+'complement=nonMaskIdxs, ncomplement=nonMaskCount)'
  res=execute(executeConditionString)
  return, {maskIdxs:maskIdxs, maskCount:maskCount, nonMaskIdxs:nonMaskIdxs, nonMaskCount:nonMaskCount}
  
END

PRO GenericOperator::logStatsStuff, filename, data, info, year, month, nanValue

  fs=self.app->getFileSystem()
  doLog, '****', info.shortName, '****', level=0
  idx=where(data ne nanValue and finite(data) eq 1, nanCount, ncomplement=nanCount)
  idx2=where(data eq nanValue, maskedCount)
  tot=n_elements(data)
  newVar=data[idx]
  maxvar=max(newVar, min=minvar, /NAN)
  totcount=0
  ;if n_elements(year_str) eq 0 then year_str='2006'
  ;if n_elements(month) eq 0 then month='06'
  doLog, '**********', year, '****', month, '******', level=0
  doLog, 'nanCount: ', nanCount, level=0
  steps=findgen(10)/10.*(maxvar-minvar)+minvar
  counts=steps
  for k=0, 8 do begin
    aa=where(newVar ge steps[k] and newVar lt steps[k+1], count)
    totcount=totcount+count
    counts[k]=count
    doLog, steps[k], steps[k+1], count, float(count)/tot*100, '%'
  endfor
  aa=where(newVar ge steps[9], count)
  counts[k]=count
  window, 1
  wset,1
  plot, steps, counts, title=info.shortName+'*'+year+'*'+month+'*'+month+'(masked - included in NaN: '+strcompress(maskedCount, /REMOVE)+')', xrange=[min(steps), max(steps)], yrange=[0, max(counts)],color=255
  steps=[!values.F_NAN,steps]
  counts=[nanCount,counts]
  xyouts, 0.4, 0.9, '    class'+' - '+'    occurences', /NORM, color=255
  for k=0, 10 do begin
    doLog, 'unformatted: ', steps[k], counts[k], counts[k]/tot*100, level=0
    infoText=string(format='(F12.7,"  ",I7," (",F5.2, "%)")', steps[k], counts[k], counts[k]/tot*100)
    doLog, 'formatted: ', infoText, level=0
    xyouts, 0.4, 0.95-(1-float(k)/11)*.8, infoText, /NORM, color=180l+180l*255l+180l*255l*255l
  ;xyouts, 0.4, 0.95-(1-float(k)/10)*.8, strtrim(steps[k],1)+' - '+strtrim(counts[k],1)+' '+strtrim(float(counts[k])/tot,1)+'%', /NORM, color=180l+180l*255l+180l*255l*255l
  endfor
  ;infoText=string(format='(F11.8,"  ",I7.0," (",F5.2, "%)")', steps[k], counts[k], counts[k]/tot*100)
  ;xyouts, 0.4, 0.95-(1-float(k)/10)*.8, infoText, /NORM, color=180l+180l*255l+180l*255l*255l
  wshow, 1
  img=tvrd()
  newFileName=fs->addFileExtension(fileName, 'png', extensionSep=extensionSep, /REMOVEPREVIOUS)
  write_image, newFileName, 'PNG', img
  totcount=totcount+count
  doLog, 'over ', steps[9], count, level=0
  doLog, 'valid: ', totcount, level=0
  doLog, '**********', level=0
  wdelete,1
;tv, bytscl(var)
  
END

FUNCTION GenericOperator::buildCopyFileName, fileName

  fs=self.app->getFileSystem()
  util=self.app->getUtility()
  mark=util->getRandomNumber1to100(/STRING)
  sourceFileName=fs->getFileNameInfo(fileName, filePath=filePath, extension=extension)
  fName=fs->removeFileExtension(sourceFileName)
  ;fs->getFileNameInfo(
  destName=fName+'_'+mark
  ;destName=fs->getTempDir(/WITH)+fName+'_'+mark
  return, destName
  
END

PRO  GenericOperator::setMainFileName, fileName, COPY=COPY, OPEN=OPEN, REMOVE_EXTENSION=REMOVE_EXTENSION

  ;if keyword_set(REMOVE_EXTENSION) then testFileName=self->removeFileExtension(fileName) else testFileName=fileName
  testFileName=self->convertFileName(fileName, REMOVE_EXTENSION=REMOVE_EXTENSION)
  if keyword_set(COPY) then begin
    fs=self.app->getFileSystem()
    ;destName=self->buildCopyFileName(fileName)
    destName=self->buildCopyFileName(testFileName)
    fs->copyEnviFile, testFileName, destName, /TEMP, newfileName=newFileName
    self.mainFileName=newFileName
  ;fs->copyEnviFile, testFileName, fs->getTempDir(), newFileName=newFileName
  ;self.mainFileName=newFileName
  endif else begin
    self.mainFileName=testFileName
  endelse
  if keyword_set(OPEN) then self->updateFid
  
END

FUNCTION GenericOperator::getMainFileName

  return, self.mainFileName
  
END

PRO  GenericOperator::setWorkingDir, folder

  self.workingDir=folder
  
END

FUNCTION GenericOperator::getWorkingDir

  return, self.workingDir
  
END

FUNCTION GenericOperator::getMapInfo, enviFileName=enviFileName, enviFileId=enviFileId

  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  mapInfo = envi_get_map_info(FID=props.localFileId)
  return, mapInfo
  
END

PRO GenericOperator::buildGeoInfo, minLon, minLat, maxLon, maxLat, xNb, yNb, lat2d, lon2d, refFileName=refFileName, mapInfo=mapInfo

  if self->hasGridReference() then begin
    lat2d=self->getBand('lat2d', /NOMASK)
    lon2d=self->getBand('lon2d', /NOMASK)
  endif else begin
    if n_elements(refFileName) eq 1 then begin
      minLon=self->readNcdfAttr(refFileName, 'Min_Lon', /GLOBAL)
      maxLat=self->readNcdfAttr(refFileName, 'Max_Lat', /GLOBAL)
      minLat=self->readNcdfAttr(refFileName, 'Min_Lat', /GLOBAL)
      maxLon=self->readNcdfAttr(refFileName, 'Max_Lon', /GLOBAL)
      xNb=self->readNcdfAttr(refFileName, 'x_nb', /GLOBAL)
      yNb=self->readNcdfAttr(refFileName, 'y_nb', /GLOBAL)
    endif else begin
      if n_elements(mapInfo) ne 1 then begin
        dims=self->getDataDimensions()
        mapInfo=self->getMapInfo()
        
        xNb=dims[2]+1
        yNb=dims[4]+1
        tMinLon=mapInfo.mc[2]
        tMinLat=mapInfo.mc[3]
        tMaxLon=mapInfo.mc[2]+mapInfo.ps[0]*(xNb-1)
        tMaxLat=mapInfo.mc[3]-mapInfo.ps[1]*(yNb-1)
        minLon=1.*round(tMinLon*100)/100
        minLat=1.*round(tMinLat*100)/100
        maxLon=1.*round(tMaxLon*100)/100
        maxLat=1.*round(tMaxLat*100)/100
      endif else begin
        xNb=mapInfo.ns;+1
        yNb=mapInfo.nl;+1
        tMinLon=mapInfo.mc[2]
        tMinLat=mapInfo.mc[3]
        tMaxLon=mapInfo.mc[2]+mapInfo.ps[0]*(xNb-1)
        tMaxLat=mapInfo.mc[3]-mapInfo.ps[1]*(yNb-1)
        minLon=1.*round(tMinLon*100)/100
        minLat=1.*round(tMinLat*100)/100
        maxLon=1.*round(tMaxLon*100)/100
        maxLat=1.*round(tMaxLat*100)/100
      ;minLon=mapInfo.minLon
      ;minLat=mapInfo.minLat
      ;maxLon=mapInfo.maxLon
      ;maxLat=mapInfo.maxLat
      ;xNb=mapInfo.xNb
      ;yNb=mapInfo.xNb
      endelse
    endelse
  endelse
  
END

FUNCTION GenericOperator::importBandGeoNc, dType, bands, bandInfo, fileName, year, month, outputDir, dims=dims, $
    refFileName=refFileName, mapInfo=mapInfo, $
    title=title, scallingType=scallingType, applyFormula=applyFormula, extraMaskConditions=extraMaskConditions, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT, $
    NOTFOUND=NOTFOUND
    
  fileName=self->buildOperatorResultFileName(dType, displayName, month, year, sensor, roi, archiveDir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH)
  bandInfo=self->readNcdfVar(fileName, parCode, FOUND=FOUND1, /REVERSE, TRANSPOSE=TRANSPOSE)
  if ~keyword_set(FOUND1) then return, bandInfo else return ,-1 

END

PRO GenericOperator::exportBandAsGeoNc, bands, bandInfo, fileName, year, month, outputDir, dims=dims, $
    refFileName=refFileName, mapInfo=mapInfo, $
    title=title, scallingType=scallingType, applyFormula=applyFormula, extraMaskConditions=extraMaskConditions, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
  ;  title=title, scallingType=scallingType, applyFormula=applyFormula, extraMaskConditions=extraMaskConditions, DOHISTO=DOHISTO, HISTOSTRUCT=HISTOSTRUCT
    
  ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  
  att_struct = {name:"", ptr_value:ptr_new()} ; structure to store attributes
  
  if size(bands, /TYPE) eq 7 then begin
    bandToRead=1
    bandNo=n_elements(bands)
  endif else begin
    bandToRead=0
    bandNo=1
  endelse
  if ~keyword_set(TITLE) then title= bandInfo[0].longName+' '+strcompress(year, /REMOVE)+' '+strcompress(month, /REMOVE)
  
  if strmid(outputDir, strlen(outputDir), 1) ne path_sep() then newOutputDir=outputDir+path_sep() else newOutputDir=outputDir
  self->buildGeoInfo, minLon, minLat, maxLon, maxLat, xNb, yNb, lat2d, lon2d, refFileName=refFileName, mapInfo=mapInfo
  
  doLog, minLon, minLat, maxLon, maxLat, xNb, yNb, LEVEL=4
  if n_elements(extraMaskConditions) ne bandNo then begin
    maskConditions=strarr(bandNo)
    ;maskConditions[*]='data eq -9999'
    maskConditions[*]=self->buildMaskConditions(bandInfo, scallingType)
  endif else begin
    maskConditions=extraMaskConditions
  endelse
  
  if n_elements(applyFormula) ne bandNo then begin
    applyFuncts=strarr(bandNo)
    applyFuncts[*]=''
  endif else begin
    applyFuncts=applyFormula
  endelse
  
  for i=0,bandNo-1 do begin
  
    fullFileName=newOutputDir+fileName[0]
    
    if (file_info(fullFileName)).exists then $
      file_delete, fullFileName
      
    fileId = ncdf_file_create(fullFileName, /COL_MAJOR)
    
    ncdf_write_global_attribute, fileId, "Title", title
    ncdf_write_global_attribute, fileId, "Year", year
    ncdf_write_global_attribute, fileId, "Month", month
    ncdf_write_global_attribute, fileId, "history", 'Created by Joint Research Centre (IES) on '+systime()
    contact=self.app->getKeyValue('CONTACT_MAIL')
    ncdf_write_global_attribute, fileId, "contact", contact
    
    ncdf_write_global_attribute, fileId, "Min_Lat", fix(round(minLat<maxLat))
    ncdf_write_global_attribute, fileId, "Min_Lon", fix(round(minLon<maxLon))
    ncdf_write_global_attribute, fileId, "Max_Lat", fix(round(minLat>maxLat))
    ncdf_write_global_attribute, fileId, "Max_Lon", fix(round(maxLon>minLon))
    ncdf_write_global_attribute, fileId, "x_nb", xNb
    ncdf_write_global_attribute, fileId, "y_nb", yNb
    
    coordSysBuilder=self.app->getKeyValue('NCDF_OUT_COORDSYSBUILDER')
    ncdf_write_global_attribute, fileId, "_CoordSysBuilder", coordSysBuilder
    ncdfVersion=self.app->getKeyValue('NCDF_OUT_VERSION')
    ncdf_write_global_attribute, fileId, "Internal_Version_Date", ncdfVersion
    ;ncdf_write_global_attribute, fileId, "Conventions", 'CF-1.0'
    
    
    ; Write dimension datasets
    ; ------------------------
    att_nb = 4
    diminfo = {name:"",att:replicate(att_struct,att_nb)}
    diminfo.att[*].name = ['standard_name', 'long_name',  'units', 'axis']
    
    ;    timedim = 0
    ;    londim = findgen(xNb)/(xNb-1) * (abs(maxLon - minLon)) + min([minLon, maxLon])
    ;    latdim = findgen(yNb)/(yNb-1) * (abs(maxLat - minLat)) + min([minLat, maxLat])
    londim = (findgen(xNb)+1)/(xNb) * (abs(maxLon - minLon)) + min([minLon, maxLon])
    latdim = findgen(yNb)/(yNb) * (abs(maxLat - minLat)) + min([minLat, maxLat])
    diminfo.name = 'lat'
    diminfo.att[*].ptr_value = [ptr_new('Latitude'),ptr_new('latitude'),ptr_new('degrees_north'),ptr_new('Y')]
    
    dim_id_lat = ncdf_create_dimension(fileId, diminfo.name, yNb)
    
    var_id = ncdf_create_ds(fileId, float(latdim), dim_id_lat, diminfo)
    
    diminfo.name = 'lon'
    diminfo.att[*].ptr_value = [ptr_new('Longitude'),ptr_new('longitude'),ptr_new('degrees_east'),ptr_new('X')]
    
    dim_id_lon = ncdf_create_dimension(fileId, diminfo.name, xNb)
    var_id = ncdf_create_ds(fileId, float(londim), dim_id_lon, diminfo)
    
    ; Initialise structure for dataset
    att_nb = 5
    varinfo = {name:"",att:replicate(att_struct,att_nb)}
    ;if keyword_set(nanAttributeName) then missingAttribName=nanAttributeName else missingAttribName='missing_value'
    ;varinfo.att[*].name = ['standard_name', 'units', missingAttribName]
    missingAttr=self.app->getKeyValue('MISSING_VALUE_ATTRIB_OUT')
    varinfo.att[*].name = ['standard_name', 'long_name', 'units', missingAttr, 'scalling']
    
    ; dim_id_res = [dim_id_lon, dim_id_lat, dim_id_time]
    if bandToRead then thisData=self->getBand(bands[i]) else thisData=bands
    aa=where(finite(thisData), mcount)
    doLog, '**', mcount
    sizeInfo=size(thisData, /STRU)
    dim_id_res = [dim_id_lon, dim_id_lat]
    ;dim_id_res = size(thisData, /DIMENSIONS)
    
    band = make_array(xNb,yNb,TYPE=sizeInfo.type,VALUE=ignoreValue)
    varName=bandInfo[i].shortName
    if keyword_set(scallingType) then scaleType=scallingType else scaleType='linear'
    varinfo.name = varName
    varinfo.att[*].ptr_value = $
      [ptr_new(bandInfo[i].shortName),ptr_new(bandInfo[i].longName),ptr_new(bandInfo[i].measureUnit),ptr_new(ignoreValue),ptr_new(scaleType)]
    ;ignoreValue1=-9998.
    ;varinfo.att[*].ptr_value = $
    ;  [ptr_new(bandInfo[i].shortName),ptr_new(bandInfo[i].measureUnit),ptr_new(ignoreValue1)]
      
    ;apply mask (from memory i.e. another matrix)
    if self->hasMask() then begin
      ;maskIdxs:maskIdxs, maskCount:maskCount, nonMaskIdxs:nonMaskIdxs, nonMaskCount:nonMaskCount
      maskIdxsInfo=self->getMaskIndxsInfo()
      if maskIdxsInfo.nonMaskCount ne 0 then band[maskIdxsInfo.nonMaskIdxs]=thisData[maskIdxsInfo.nonMaskIdxs]
    endif else begin
      band=thisData
    endelse
    ;set Nan all others values
    idx=where(finite(band) eq 0, count)
    if count ne 0 then band[idx]=ignoreValue
    
    maskCondition=maskConditions[i]
    applyFunct=applyFuncts[i]
    maskCount=0
    
    ;apply mask (from data values self)...
    if maskCondition ne '' then begin
      res=execute('maskIdx=where('+maskCondition+', maskCount)')
    ;if maskCount ne 0 then data[maskIdx]=ignoreValue
    endif
    
    if (applyFunct ne '') and (applyFunct ne 'N/A') then begin
      res=execute('band='+applyFunct)
    endif
    
    ; Apply range conditions
    if strlowcase(scallingType) eq 'linear' then band=self.app->applyRangeConditions(bandInfo, band, 'band', ignoreValue=ignoreValue)
    
    if maskCount ne 0 then band[maskIdx]=ignoreValue
    if keyword_set(DOHISTO) then begin
      doHistogram, band, HISTOSTRUCT.folder, 'hist_'+fileName, HISTOSTRUCT.day, HISTOSTRUCT.month, HISTOSTRUCT.year, cutvalue=HISTOSTRUCT.cutvalue, binsize=HISTOSTRUCT.binsize
    endif
    aa=where(band ne ignoreValue, count)
    
    band=reverse(temporary(band),2)
    ;    doLog, '****************************'
    ;    help, data
    ;    doLog, xNb, yNb
    ;    doLog, '****************************'
    ;if self.app->IsTestMode() then self->logStatsStuff, fullFileName, band, bandInfo[i], year, month, ignoreValue
    var_id = ncdf_create_ds(fileId, band, dim_id_res, varinfo)
    ; Close monthly result file
    
    ncdf_file_close,fileId
    
    doLog, string(FORMAT='("Written:",A)',fullFileName), ' - Valid pixs:', strcompress(count, /REMOVE), LEVEL=4
    if count eq 0 then begin
      doLog, 'Warning!!! No valid pixels for the last output', LEVEL=4
    endif
  endfor
  
END

PRO GenericOperator::exportBand, bandNames, bandInfos, OUTPUTFORMAT=OUTPUTFORMAT

  if keyword_set(OUTPUTFORMAT) then begin
    if OUTPUTFORMAT eq 'GEONC' then begin
      self->exportBandAsGeoNc, bandNames, bandInfos
    endif else begin
      message, 'Output format not supported ('+OUTPUTFORMAT+')'
    endelse
  endif
  doLog, 'Output format not selected, using GEONC as default', level=1
  self->exportBandAsGeoNc, bandNames, bandInfos
  
END

PRO GenericOperator::updateFid, enviFileId=enviFileId

  ;help, /memory
  envi_file_mng, id=self.mainFid, /REMOVE
  envi_open_file, self.mainFileName+'.envi', r_fid=fid
  if n_elements(fid) gt 1 then for i=1, n_elements(fid)-1 do  envi_file_mng, id=fid[i], /remove
  self.mainFid=fid[0]
;help, /memory
  
END

FUNCTION GenericOperator::getLocalFileProperties, enviFileName=enviFileName, enviFileId=enviFileId

  localFileId=self.mainFid
  localFileName=self.mainFileName
  updateInternalFid=1
  if n_elements(enviFileName) eq 1 then begin
    localFileName=enviFileName
    envi_open_file, localFileName+'.envi', r_fid=localFId
    updateInternalFid=0
  endif
  if n_elements(enviFileId) eq 1 then begin
    localFId=enviFileId
    updateInternalFid=0
  endif
  return, {localFileName:localFileName, localFileId:localFileId, updateInternal:updateInternalFid}
  
END

FUNCTION GenericOperator::isAMemoryBand, bandName

  list=self->getMemoryBands()
  idx=where(strupcase(list) eq strupcase(bandName), count)
  return, idx 

END

FUNCTION GenericOperator::isABand, bandName, enviFileName=enviFileName, enviFileId=enviFileId

  memIdx=self->isAMemoryBand(bandName)
  if memIdx ne -1 then return, 1
  
  if size(bandName, /TYPE) ne 7 then return, 0
  
  if ~self.app->enviFileExists(self.mainFileName) then return, 0
  
  if self->getBandPos(bandName, enviFileName=enviFileName, enviFileId=enviFileId) ne -1 then begin
    return, 1
  endif
  return, 0
  
END

FUNCTION GenericOperator::getDataDimensions, enviFileName=enviFileName, enviFileId=enviFileId

  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  envi_file_query, props.localFileId, dims=c_dims
  return, c_dims
  
END

FUNCTION GenericOperator::getBandNames, enviFileName=enviFileName, enviFileId=enviFileId

  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  envi_file_query, props.localFileId, bnames=bnames
  return, bnames
  
END

FUNCTION GenericOperator::getBandPos, bandName, enviFileName=enviFileName, enviFileId=enviFileId, CASESENSITIVE=CASESENSITIVE

  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  envi_file_query, props.localFileId, bnames=bnames
  ;doLog, bnames
  testBnames=bnames
  testBandName=bandName
  if not(keyword_set(CASESENSITIVE)) then begin
    testBnames=strupcase(bnames)
    testBandName=strupcase(bandName)
  endif
  return, (where(testBandName eq testBnames))[0]
  
END

FUNCTION GenericOperator::getDataValues, dataSource, enviFileName=enviFileName, enviFileId=enviFileId

  dataValues=dataSource
  if size(dataSource, /TYPE) eq 7 then dataValues=self->getBand(dataSource, enviFileName=enviFileName, enviFileId=enviFileId)
  return, dataValues
  
END

FUNCTION GenericOperator::getBandFromMemory, bandName, FOUND=FOUND

  idx=(where(strupcase(self.memoryBandNames) eq strupcase(bandName), count))[0]
  FOUND=count eq 1
  if FOUND then return, *self.memoryBandValues[idx]
  
END

FUNCTION GenericOperator::getBand, bandName, enviFileName=enviFileName, enviFileId=enviFileId, NOMASK=NOMASK, $
  SETNAN=SETNAN, report=report

  if n_elements(report) eq 0 then report=getInvalidStruct()
  report.found=1
  if ~keyword_set(SETNAN) then begin
    doLog, bandName
    doLog, /STACK, callingRoutine=callingRoutine
    doLog, callingRoutine
  endif
  dV=self->getBandFromMemory(bandName, FOUND=FOUND)
  if keyword_set(FOUND) then begin
    band=dV
  endif else begin
    props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
    
    pos=self->getBandPos(bandName, enviFileName=props.localFileName, enviFileId=props.localFileId)
    dims=self->getDataDimensions(enviFileName=props.localFileName, enviFileId=props.localFileId)
    band=envi_get_data(fid=props.localFileId, dims=dims, pos=pos)
  endelse
  if ~keyword_set(NOMASK) or keyword_set(SETNAN) then begin
    maskCondition=self->buildMaskConditions(undef, '')
    res=execute('maskIdx=where('+maskCondition+', maskCount)')
    if keyword_set(SETNAN) then ignoreValue=!VALUES.D_NAN else ignoreValue=self.app->getKeyValue('NAN_VALUE')
    if maskCount ne 0 then band[maskIdx]=ignoreValue
    report.invalid_count=maskCount
    report.expected=n_elements(band)
  endif
  ;  doLog, 'read-from-envi'
  ;  help, datavalues
  return, band
  
END

;PRO GenericOperator::fillBand, bandDataSource, bandName, enviFileName=enviFileName, enviFileId=enviFileId
;
;  if not(self->isABand(bandDataSource, enviFileName=enviFileName, enviFileId=enviFileId)) then begin
;    pos=self->getBandPos(bandName, enviFileName=enviFileName, enviFileId=enviFileId)
;    if pos ne -1 then self->addBand, bandDataSource, bandName
;  endif
;
;END

PRO GenericOperator::addMemoryBand, bandDataSource, bandName, OVERWRITE=OVERWRITE, ignoreValue=ignoreValue, setIgnoreValue=setIgnoreValue

  memPosIdx=where(bandName eq self.memoryBandNames, countSearch)
  firstFreePosIdx=(where(self.memoryBandNames eq '', countFree))[0]
  if (countSearch eq 1 and keyword_set(OVERWRITE)) or memPosIdx eq -1 then begin
    if memPosIdx eq -1 then memPosIdx=firstFreePosIdx
    ptr_free, self.memoryBandValues[memPosIdx]
    if n_elements(setIgnoreValue) eq 1 then resValues=self->setIgnoreValue(bandDataSource, setIgnoreValue)
    resValues=self.app->applyRangeConditions(bandName, bandDataSource, bandName, ignoreValue=ignoreValue)
    self.memoryBandNames[memPosIdx]=bandName
    self.memoryBandValues[memPosIdx]=ptr_new(resValues)
  endif
  
END

PRO GenericOperator::addBand, bandDataSource, bandName, enviFileName=enviFileName, enviFileId=enviFileId, OVERWRITE=OVERWRITE, MEMORY=MEMORY, ignorevalue=ignorevalue, setIgnoreValue=setIgnoreValue

  ;ignoreValue=self.app->getKeyValue('NAN_VALUE')
  pos = self->getBandPos(bandName, enviFileName=enviFileName, enviFileId=enviFileId)
  if pos ne -1 and keyword_set(OVERWRITE) then begin
    doLog, 'overwriting...', bandname, level=0
    self->removeBand, bandName, enviFileName=enviFileName, enviFileId=enviFileId
    pos=-1
  endif
  if n_elements(ignoreValue) eq 0 then ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
  if keyword_set(MEMORY) then begin
    self->addMemoryBand, bandDataSource, bandName, OVERWRITE=OVERWRITE, ignoreValue=ignoreValue, setIgnoreValue=setIgnoreValue
    return
  endif
  ;  if keyword_set(MEMORY) then begin
  ;    ;dataValues=self.app->applyRangeConditions(bandName, dataValues, bandName, ignoreValue=ignoreValue)
  ;    self->addMemoryBand, bandDataSource, bandName, OVERWRITE=OVERWRITE
  ;  endif
  
  if pos eq -1 then begin
    props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
    envi_file_query, props.localFileId, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames
    ;band is coming from another file with data at pos 0 (a do_math result behaviour for example...)
    ;where bandDataSource is the FID
    if n_elements(bandDataSource) eq 1 then dataValues=envi_get_data(fid=bandDataSource, dims=c_dims, pos=0) else dataValues=bandDataSource
    if n_elements(setIgnoreValue) ne 0 then dataValues=self->setIgnoreValue(dataValues, setIgnoreValue)
    dataValues=self.app->applyRangeConditions(bandName, dataValues, bandName, ignoreValue=ignoreValue)
    mapInfo=self->getMapInfo()
    
    bnames=[bnames, bandName]
    bNumber=n_elements(bnames)
    
    ; add band as last
    doLog, 'adding... ', bandName, level=0
    ;doLog, 'Force Wait, write data', LEVEL=0
    ;wait,1
    ;envi_file_mng, id=props.localFileId, /REMOVE
    openw, lun, props.localFileName+'.envi', /GET_LUN, /APPEND
    writeu, lun, dataValues
    close, lun & free_lun, lun
    
    envi_setup_head_oxy, FNAME=props.localFileName, NS=ns, NL=nl, DATA_IGNORE_VALUE=ignoreValue, $
      NB=bNumber, DATA_TYPE=dt, FILE_TYPE=0, INTERLEAVE=0, MAP_INFO=mapinfo, /WRITE,$
      /OPEN, BNAMES=bNames
      
    self->updateFid, enviFileId=enviFileId
    ;doLog, 'Force Wait, update fid', LEVEL=0
    ;wait,1
    
    return
  endif
  ;error when band exists and no keyword_set (overwrite)
  message, 'band already exists!'
  
END

FUNCTION GenericOperator::setIgnoreValue, dataValues, ignoreValue

  nanIdxs=where(dataValues eq ignoreValue, count)
  if count ne 0 then dataValues[nanIdxs]=!VALUES.F_NAN
  return, dataValues
  
END

PRO GenericOperator::removeBand, bandName, enviFileName=enviFileName, enviFileId=enviFileId, CASESENSITIVE=CASESENSITIVE

  util=self.app->getUtility()
  fs=self.app->getFileSystem()
  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  
  envi_file_query, props.localFileId, ns=ns, nl=nl, nb=nb, data_type=dt, dims=c_dims, bnames=bnames
  
  howMany=n_elements(bnames)
  if not(keyword_set(CASESENSITIVE)) then begin
    testBnames=strupcase(bnames)
    testBandName=strupcase(bandName)
  endif
  removeBandIdx=where(testBandName eq testBnames, complement=leaveBandIdxs, count, ncomplement=countComplement)
  mapInfo=self->getMapInfo()
  
  if countComplement ne 0 then begin
    newBnames=bnames[leaveBandIdxs]
    bNumber=n_elements(newBnames)
  endif else begin
    newBnames=bandName
    bNumber=1
  endelse
  
  
  outFname = util->getSysTime(/FILECOMPATIBILITY)
  dir=fs->getTempDir(/WITH)
  outFname=dir+outFname
  
  ;envi_file_mng, id=props.localFileId, /REMOVE
  doLog, 'Force Wait, write data', LEVEL=0
  ;wait,1
  openw, lun, outFname, /GET_LUN
  doLog, 'removing...', bandname, level=0
  for i=0, howMany-1 do begin
    ;skip only selected band
    if i ne removeBandIdx[0] then begin
      dataValues = envi_get_data(fid=props.localFileId, dims=c_dims, pos=i)
      writeu, lun, dataValues
    endif
  endfor
  
  close, lun & free_lun, lun
  ;move temp file, overwriting original one
  file_move, outFname, props.localFileName+'.envi', /ALLOW, /OVERWRITE
  
  ;rebuild header
  envi_setup_head_oxy,  $
    FNAME=props.localFileName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=bNumber, $
    DATA_TYPE=dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=newBnames
    
  self->updateFid
  doLog, 'Force Wait, update fid', LEVEL=0
;wait,1
  
END

FUNCTION GenericOperator::buildExpression, originalExpression, listOfUsedBands, allBands, pos=pos

  replaceExpression=expression
  pos=lonarr(n_elements(listOfBand))
  
  for i=0, n_elements(listOfBand)-1 do begin
    thisBand=listOfBand[i]
    idx=(where(thisBand eq bnames, count))[0]
    pos[i]=idx
    replaceExpression=util->strreplace(replaceExpression, listOfBand[i], 'b'+strcompress(idx+1, /REMOVE_ALL))
  ;doLog, expression
  ;doLog, replaceExpression
  endfor
  return, replaceExpression
  
END

FUNCTION GenericOperator::doEnviBandMath, expression, listOfBand, resultBandName, enviFileName=enviFileName, enviFileId=enviFileId, MEMORY=MEMORY

  util=self.app->getUtility()
  props=self->getLocalFileProperties(enviFileName=enviFileName, enviFileId=enviFileId)
  
  envi_file_query, props.localFiD, dims=dims, bnames=allBands
  
  replaceExpression=self->buildExpression(originalExpression, listOfBand, allBands, posList=posList)
  
  fidlist = lonarr(n_elements(listOfBand))
  ; all the bands are in the same file
  fidList[*] = fid
  
  envi_doit, 'math_doit', $
    fid=fidList, pos=posList, dims=dims, $
    exp=replaceExpression, $
    r_fid=resultFid, /IN_MEMORY
    
  result=0
  
  bandDataValues = self->getDataValues(enviFileId=resultFid)
  if keyword_set(MEMORY) then begin
    envi_file_mng, id=resultFid, /REMOVE, /DELETE
  endif else begin
    self->addBand, bandDataValues, resultBandName, enviFileName=enviFileName, enviFileId=enviFileId
    self->updateFid, enviFileId=newFId
    if props.updateInternal then self.mainFid=newFId
  endelse
  return, result
  
END

PRO GenericOperator::testBandMath, fileName

  listOfBand=['Rrs_412-Mean', 'chl_a-Mean']
  expression="(Rrs_412-Mean + chl_a-Mean)/2"
  outbandName='MyMean'
  
  res = self->doEnviBandMath(fileName, expression, listOfBand, outbandName, ret_fid=ret_fid)
  
  listOfBand=['Rrs_412-Mean', 'chl_a-Mean', outbandName]
  expression="(Rrs_412-Mean + chl_a-Mean + " + outbandName + ")/3"
  outbandName='MyTestMean'
  res = self->doEnviBandMath(fileName, expression, listOfBand, /MEMORY)
  
  ;self->removeBand, fileName, 'MyTestMean', newFileFid=newFileFid
  self->removeBand, fileName, 'MyMean', newFileFid=newFileFid
  
  envi_file_query, newFileFid, dims=dims, bnames=bnames
  
END

PRO GenericOperator::addMask, maskValues

  ptr_free, self.mask
  self.mask=ptr_new(maskValues, /NO_COPY)
  
END

FUNCTION GenericOperator::getMask, EXIST=EXIST

  EXISTS=0
  if ptr_valid(self.mask) then begin
    EXISTS=1
    return, *self.mask
  endif
  return, 0
  
END

PRO GenericOperator::writeMask, filename=filename

END

PRO GenericOperator::setBandToExportList, list

  ptr_free, self.bandToExportList
  myList=list
  self.bandToExportList=ptr_new(myList, /NO_COPY)
  
END

PRO GenericOperator::createEnviFileFromBand, bandValues, bandName, fullFileName, ps=ps, mc=mc, MEMORY=MEMORY

  ;self.mainFileName=fullFileName
  ;self->removeMainFile
  self.mainFileName=fullFileName
  openw, lun, fullFileName+'.envi', /GET_LUN
  writeu, lun, bandValues
  close, lun & free_lun, lun
  
  sizeInfo=size(bandValues, /STRUCTURE)
  dt=sizeInfo.type
  NS=sizeInfo.dimensions[0]
  NL=sizeInfo.dimensions[1]
  NB=1
  
  ;stepLon = (ABS(sstMapWin[1]-sstMapWin[0]))/(ns-1)
  ;stepLat = (ABS(sstMapWin[2]-sstMapWin[3]))/(nl-1)
  
  ; set the map info to the regular grid in ENVI file
  ;ps = [stepLon, stepLat]
  ;mc = [0.5D, 0.5D, sstMapWin[0], sstMapWin[2]]
  mapInfo = ENVI_MAP_INFO_CREATE(/GEOGRAPHIC, mc=mc, ps=ps)
  bNumber=n_elements(bandName)
  bNames=bandName
  
  ; setup the header in the ENVI file
  envi_setup_head_oxy,  $
    FNAME=fullFileName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=ignoreValue, $
    NB=bNumber, $
    DATA_TYPE=dt, $
    FILE_TYPE=0, $
    INTERLEAVE=0, $
    R_FID=myId, $
    MAP_INFO=mapinfo, $
    /WRITE,$
    /OPEN, $
    BNAMES=bNames
    
  self.mainFid=myId
  if keyword_set(MEMORY) then self->addMemoryBand, bandValues, bandName
  
END

FUNCTION GenericOperator::getBandToExportList

  if ptr_valid(self.bandToExportList) then list=*self.bandToExportList else message, 'BandToExportList is empty!'
  return, list
  
END

PRO GenericOperator::CleanUp

  ;self.appFS=obj_new()
  self.app=obj_new()
  ptr_free, self.maskBand
  ptr_free, self.geoBand
  ptr_free, self.bandToExportList
  self-> Object::cleanup
  for i=0, n_elements(memoryBandValues)-1 do ptr_free, memoryBandValues[i]
  
END

FUNCTION GenericOperator::init, application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE
  ;REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE
    
  if not (self -> Object :: init()) then return, 0
  
  ;if n_elements( applicationFS ) eq 1 then self.appFS = applicationFS
  if n_elements(application) eq 1 then self.app = application
  if n_elements(workingDir) eq 1 then self.workingDir = workingDir
  if n_elements(periodType) eq 1 then self.periodType = periodType else self.periodType='M'
  if n_elements(fileName) eq 1 then self->setMainfileName, fileName, REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY
  if n_elements(maskBand) eq 1 then self.maskBand = ptr_new(maskBand, /NO_COPY)
  if n_elements(geoBand) eq 1 then self.geoBand = ptr_new(geoBand, /NO_COPY)
  if n_elements(bandToExportList) gt 0 then self->setBandToExportList, bandToExportList
  if keyword_set(ENVITYPE) then self.isEnviType = keyword_set(ENVITYPE)
  self.maskCondition='finite(maskBand) eq 0'
  return, 1
  
END

PRO GenericOperator__Define

  Struct = { GenericOperator , $
    workingDir: '', $
    isEnviType: 0b, $
    app : obj_new(), $
    mainFileName : '', $
    mainFid : 0l, $
    maskBand: ptr_new(), $
    maskCondition: '', $
    geoBand: ptr_new(), $
    bandToExportList: ptr_new(), $
    periodType: '', $
    memoryBandNames: strarr(20), $
    memoryBandValues: ptrarr(20), $
    Inherits Object $
    }
    
END