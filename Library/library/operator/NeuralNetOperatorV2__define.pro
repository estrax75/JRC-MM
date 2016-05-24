;@structure_definition.pro
;@/library/regionalgo/regionalgo
;@/library/system_io/buildBioMapVarFileName

PRO NeuralNetOperatorV2::setWaveLengths, wLens

  ptr_free, self.waveLengths
  self.waveLengths=ptr_new(wLens, /NO_COPY)
  
END

FUNCTION NeuralNetOperatorV2::getWaveLengths

  if ptr_valid(self.waveLengths) then wLens=*self.waveLengths
  return, wLens
  
END

PRO NeuralNetOperatorV2::exportSourceBand, allBands, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=hideIdxs

  self->exportRrsBand, allBands, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=hideIdxs
  
END

PRO NeuralNetOperatorV2::loadH5Struct, fileName

  if self->getConfFile() eq '' then algoFile = '.\mlp-rrs_v2.h5' else algoFile=self->getConfFile()
  strc = h5_parse(algoFile, /read_data)
  self->setH5Struct, strc
  
END

FUNCTION NeuralNetOperatorV2::getApplicationRoiList

  ;return, ['ADRSEA', 'BLCK', 'BALT']
  ;return, ['MEDSEA', 'BLCK', 'BALT']
  return, ['BLCK']
  
END

;FUNCTION NeuralNetOperatorV2::extractAlgoInfo, productName, prefix
;
;  bandsString=strmid(productName, strlen(prefix), strlen(productName)-strlen(prefix))
;  bandNames=strsplit(bandsString, '_', /EXTRACT)
;
;  allAvWaves=self->getUsedWaveLength()
;  lambdaIdx = intarr(n_elements(allAvWaves))
;
;  nlwSet=''
;  for i=0, n_elements(bandNames)-1 do begin
;    idx=(where(fix(bandNames[i]) eq allAvWaves, count))[0]
;    if count eq 1 then lambdaIdx[idx]=1
;    nlwSet=nlwSet+'_'+bandNames[i]
;  endfor
;  nlwSet=strmid(nlwSet, 1, strlen(nlwSet)-1)
;  name=strmid(prefix, 0, strlen(prefix)-1)
;  return, {nlwSet:nlwSet, name:name, lambdaIdx:lambdaIdx}
;
;END
;
;FUNCTION NeuralNetOperatorV2::getAlgoInfo, nlwProductName, sensor, basin
;
;  nlwId='nlw_single' & ratioId='nlw_ratio_'& ratioLogId='nlw_ratiolog10_'
;
;  if strpos(nlwProductName, ratioId) ne -1 then return, self->extractAlgoInfo(nlwProductName, ratioId)
;  if strpos(nlwProductName, ratioLogId) ne -1 then return, self->extractAlgoInfo(nlwProductName, ratioLogId)
;  return, self->extractAlgoInfo(nlwProductName, nlwId)
;
;END

; Main call
;FUNCTION NeuralNetOperatorV2::doComputation, roiCode, productName, sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE
;
;  common memtT, strc
;  COMMON smurffCB, mainApp
;  common refDate, myMonth, myYear, myDay
;
;;OVERVIEW
;;
;; This IDL procedure is provided in comply with requirements and specifications of the Service Contract N. CCR.IES.C390C391935.X0. Please refer to the companion pdf document for details.
;;
;;INPUTS
;;
;; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf).
;;
;; wavelengts: set of central wavebands to be used to derive data products
;;
;; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
;; yellow substance absorption at 412 nm (ay412).
;;
;;EXECUTION
;;
;;For testing and operational applications proceed as follows:
;; 1) Extract the content of the \texttt{blks.zip} file in a folder (e.g., c:/projects/idl/test). Set the IDL working directory to that folder  (enter cd, 'c:/projects/idl/test'
;; in the IDL command line, note the comma after cd and that the path is within quotes).
;; 2) Enter the following command in the IDL prompt:  blks, `mer', [413, 443, 490, 510, 560, 665], 'chlideachla'}. Note that the arguments sensor ( 'mer'}), wavelengths (e.g.,
;; [413, 443, 490, 510, 560, 665]}) and  product ('chlideachla'}) used  as input to the procedure blks can be any combination set. Input data for testing the blks.pro procedure
;; in the file sit-nlw.dat (the data header specifies column entries).
;;----------------------------------------------------------------------------
;
;  NODATA=0
;  doLog, '**', self.mainFileName, '**', level=0
;  if n_elements(fileName) eq 1 then self->setMainFileName, fileName, OPEN=OPEN,COPY=COPY
;  basin=self->mapRoiCode(roiCode, FOUND=FOUND)
;  sensor=self->mapSensorCode(sensorCode, FOUND=FOUND)
;  ;doLog, '***Memory trace starts here***', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  self->buildInputFile, sensor, ignoreValue, pixelMask=pixelMask, textInputFile=outTextInputFile, pixelNumber=pixelNumber
;  ;if n_elements(data) eq 0 then begin
;  ;  pixelMaskTest=pixelMask
;  ;endif
;  ;doLog, 'buildInputFile memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  ; Input data
;
;  if ~(file_info(outTextInputFile)).read or (strlen(strcompress(outTextInputFile, /REMOVE_ALL)) eq 0) then begin
;    doLog, 'No valid data for roi: ', roiCode, '; sensor: ', sensorCode, '; Ref file: ', self->getMainFileName(), '. Skip.', level=2
;    NO_DATA=1
;    return, 0
;  endif
;
;  readRes = dataread_v2(outTextInputFile);, pixelNumber=pixelNumber)
;  ;if n_elements(data) eq 0 then begin
;  ;  data=dataNew
;  ;endif
;  ;data=0b
;  ;help, /memory
;  ;doLog, 'dataread memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  nData = n_elements(readRes[0, *])
;
;  ; Input file with the coefficients of regional algorithms
;  algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\regionalgo.h5'
;  if n_elements(strc) eq 0 then strc = self->getH5Struct()
;  ;doLog, 'getH5Struct memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  ;workaround???
;  ;hid = H5F_OPEN(file)
;  ;H5F_CLOSE, hid
;  ; Correction for bi-directional effects (to activate, set the flag to 1)
;  foqFlag = 0
;
;  ; Nlw for the selected basin and sensor
;  case basin of
;    'adrs': begin
;      case sensor of
;        'swf': nlwSet = 'swf443_swf490_swf510_swf555'
;        'mod': nlwSet = 'mod443_mod488_mod530_mod555'
;        'mer': nlwSet = 'mer443_mer490_mer510_mer560'
;        'rrs': nlwSet = 'rrs442_rrs491_rrs511_rrs555'
;      end
;      lambdaIdx = [0, 1, 1, 1, 1, 0]
;    endcase
;    'blks': begin
;      lambdaIdx = [0, 1, 1, 1, 1, 0]
;      case sensor of
;        ; start new version DDA + MM 2015 08 20
;        ; adding variable centralBands
;        ; overwrite variable lambdaIdx
;        'swf': begin
;          nlwSet = 'swf443_swf490_swf510_swf555'
;          centralBands = [412, 443, 490, 510, 555, 670]
;          lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        ; add for new version
;        'mod': begin
;          nlwSet = 'mod443_mod488_mod530_mod555'
;          centralBands = [412, 443, 488, 530, 555, 667]
;          lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        'mer': begin
;          nlwSet = 'mer443_mer490_mer510_mer560'
;          centralBands = [413, 443, 490, 510, 560, 665]
;          lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        ; end new version DDA + MM 2015 08 20
;        'rrs': nlwSet = 'rrs442_rrs491_rrs511_rrs555'
;      end
;    endcase
;    'blts': begin
;      case sensor of
;        'swf': nlwSet = 'swf490_swf510_swf555_swf670'
;        'mod': nlwSet = 'mod488_mod530_mod555_mod667'
;        'mer': nlwSet = 'mer490_mer510_mer560_mer665'
;        'rrs': nlwSet = 'rrs491_rrs511_rrs555_rrs665'
;      end
;      lambdaIdx = [0, 0, 1, 1, 1, 1]
;    endcase
;  endcase
;
;  ; f/Q correction
;  foqflag=keyword_set(applyfoq)
;  if foqFlag eq 1 then begin
;
;    ; f/Q correction table
;    fqfile=self->getFoqFile()
;    read_foq_table, fqfile, foq_table_common
;
;    ; Algorithm for chl-a estimates
;    algoName = strupcase(basin + '_' + nlwSet + '_to_chlideachla')
;    algoArchive=tag_names(strc)
;    algoIdx=where(strcmp(algoArchive, algoName) eq 1)
;    model = strc.(algoIdx)._data
;
;    ; Do three iterations
;    for i = 0, 2 do begin
;
;      ; Chla
;      tmp = dataprod(model, alog10(subSetData[where(lambdaIdx eq 1), *]))
;      chla = tmp[0, *]
;
;      ; Loop over data records
;      for row = 0,  nData - 1 do begin
;
;        ; f/Q
;        get_foq, foq_table_common, chla(row), data(6, row), data(7, row), data(8, row), foq_a
;
;        ; f0/Q0
;        get_foq, foq_table_common, chla(row), 0, 0, 0, foq_0
;
;        ; Loop over wavelengths
;        for chn = 0, 5 do data[chn, row] = data(chn, row) * foq_0(chn) / foq_a(chn)
;
;      end
;
;    end
;
;  endif
;
;  ; Regional product
;  algoName = strupcase(basin + '_' + nlwSet + '_to_' + productName)
;  algoArchive=tag_names(strc)
;  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
;  algoArchive=0b
;  model = strc.(algoIdx)._data
;  dataRes=temporary(alog10(temporary(readRes[where(lambdaIdx eq 1), *])))
;  ;data=0b
;  ;if n_elements(res) eq 0 then res = dataprod(temporary(model), temporary(dataRes))
;  res = dataprod(temporary(model), dataRes)
;  ;res=0b & data=0b
;  ;data=0b
;  ;model=0b
;  ;help, /memory
;  ;doLog, 'dataprod memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  ;delidlvar, strc
;  ;delidlvar, model
;  ;strc=0
;  ;model=0b
;
;  ;help, res
;  ; first row product value, second row applicability value
;  ;writeTestCase, readRes, dataRes, res, algoName, nlwSet, outTextInputFile
;  self->updateFid
;  if self->isEnviType() then begin
;    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
;    ;res=0b
;    biomapThreshold=self.app->getKeyValue('BIOMAP_V1_APPLICABILITY_THRESHOLD')
;    biomapThreshold=float(biomapThreshold)
;    idx=where(~finite(appl) or appl gt biomapThreshold, count)
;    if count gt 0 then values[idx]=ignoreValue
;    ;idx=0b
;    self->exportNlwBand, readRes, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=idx
;    productValues=self->asciiToEnvi(values, pixelMask=pixelMask, ignoreValue=ignoreValue)
;    ;wrongIdxs=where(values gt 500, wrongCount)
;    ;if wrongCount ne 0 then begin
;    ;  tempDir=mainApp->getKeyValue('TEMP_DIR')
;    ;   doHistogram, productValues, tempDir, roiCode, day, month, year
;    ;endif
;    self->addBand, productValues, productName, /OVERWRITE
;    idxs=where(~finite(appl) or appl gt 10., count)
;    if count ne 0 then appl[idxs]=10
;    idxs=0b
;    ;productValues=self->asciiToEnvi(res[1,*], pixelMask=pixelMask, ignoreValue=ignoreValue)
;    productValues=self->asciiToEnvi(appl, pixelMask=pixelMask, ignoreValue=ignoreValue)
;    self->addBand, productValues, productName+self->getApplicabilityExtension(), /OVERWRITE
;  ;doLog, 'store as envi memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  endif
;  ;values=self->getBand(productName)
;  ;idxs=where(res[0,*] ne 0 and res[0,*] -9999, count1)
;  ;idxs=where(productValues ne 0 and productValues ne -9999, count2)
;  ;idxs=where(values ne 0 and values ne -9999, count3)
;  ;doLog, count1, count2, count3, level=0
;  if ~(keyword_set(PRESERVE_DAT_FILE)) then self->removeLastDatFile
;  return, 1
;
;end

FUNCTION NeuralNetOperatorV2::doComputation, roiCode, productName, sensorCode, waveLengths, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE

  common memtT, strc
  COMMON smurffCB, mainApp
  common refDate, myMonth, myYear, myDay
  common faker_nn, fakecount_nn
  
  ;OVERVIEW
  ;
  ; This IDL procedure is provided in comply with requirements and specifications of the Service Contract N. CCR.IES.C390C391935.X0. Please refer to the companion pdf document for details.
  ;
  ;INPUTS
  ;
  ; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf).
  ;
  ; wavelengts: set of central wavebands to be used to derive data products
  ;
  ; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
  ; yellow substance absorption at 412 nm (ay412).
  ;
  ;EXECUTION
  ;
  ;For testing and operational applications proceed as follows:
  ; 1) Extract the content of the \texttt{blks.zip} file in a folder (e.g., c:/projects/idl/test). Set the IDL working directory to that folder  (enter cd, 'c:/projects/idl/test'
  ; in the IDL command line, note the comma after cd and that the path is within quotes).
  ; 2) Enter the following command in the IDL prompt:  blks, `mer', [413, 443, 490, 510, 560, 665], 'chlideachla'}. Note that the arguments sensor ( 'mer'}), wavelengths (e.g.,
  ; [413, 443, 490, 510, 560, 665]}) and  product ('chlideachla'}) used  as input to the procedure blks can be any combination set. Input data for testing the blks.pro procedure
  ; in the file sit-nlw.dat (the data header specifies column entries).
  ;----------------------------------------------------------------------------
  
  NODATA=0
  doLog, '**', self.mainFileName, '**', level=0
  if n_elements(fileName) eq 1 then self->setMainFileName, fileName, OPEN=OPEN,COPY=COPY
  ;if n_elements(waveLengths) eq 0 then waveLengths=self->getWaveLengths()
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
    return, 1
  endif
  
  ;data = dataread_v2('./insituNlwMeasWaves.dat')
  readRes = dataread_v2(outTextInputFile, pixelNumber=pixelNumber)
  nData = n_elements(readRes[0, *])
  if nData lt 2 then begin
    NODATA=1
    if ~(keyword_set(PRESERVE_DAT_FILE)) then self->removeLastDatFile
    return, 1
  endif
  ;if n_elements(data) eq 0 then begin
  ;  data=dataNew
  ;endif
  ;data=0b
  ;help, /memory
  ;doLog, 'dataread memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ;nData = n_elements(readRes[0, *])
  
  ; Input file with the coefficients of regional algorithms
  algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\mlp-rrs_v2.h5'
  if n_elements(strc) eq 0 then strc = self->getH5Struct()
  ;doLog, 'getH5Struct memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ;workaround???
  ;hid = H5F_OPEN(file)
  ;H5F_CLOSE, hid
  ; Correction for bi-directional effects (to activate, set the flag to 1)
  foqFlag = 0
  ; channels & novelty (D'Alimonte suggestions, september 2015)
  ; 1) Novelty threshold for [mod412,mod443,mod488,mod530,mod555,mod667]: 0.88
  ; 2) Novelty threshold for [mod443,mod488,mod530,mod555,mod667]: 0.74
  ; 3) Novelty threshold for [mod443,mod488,mod530,mod555]: 0.90
  ; 4) Novelty threshold for [mod488,mod530,mod555]: 1.10
  ; end
  ;  
  ; Nlw for the selected basin and sensor
  case basin of
    'adrs': begin
      case sensor of
        'swf': bandSet = 'swf443_swf490_swf510_swf555'
        'mod': bandSet = 'mod443_mod488_mod530_mod555'
        'mer': bandSet = 'mer443_mer490_mer510_mer560'
        'rrs': bandSet = 'rrs442_rrs491_rrs511_rrs555'
      end
      lambdaIdx = [0, 1, 1, 1, 1, 0]
    endcase
    'blks': begin
      ;lambdaIdx = [0, 1, 1, 1, 1, 0]
      case sensor of
        ; start new version DDA + MM 2015 08 20
        ; adding variable centralBands
        ; overwrite variable lambdaIdx
        'swf': begin
          bandSet = 'swf443_swf490_swf510_swf555'
          centralBands = [412, 443, 490, 510, 555, 670]
          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
        end
        ; add for new version
        'mod': begin
          bandSetToUse=self.app->getKeyValue('NEURAL_NET_V2_BAND_SET')
          if bandSetToUse eq '' then  bandSetToUse='4BANDS'
          case bandSetToUse of
          '6BANDS': begin
          bandSet = 'mod412_mod443_mod488_mod530_mod555_mod667'
          centralBands = [412, 443, 488, 530, 555, 667]
          lambdaIdx = [1, 1, 1, 1, 1, 1]
          noveltyThr = 0.88
          end
          '5BANDS': begin
            bandSet = 'mod443_mod488_mod530_mod555_mod667'
            centralBands = [412, 443, 488, 530, 555, 667]
            lambdaIdx = [0, 1, 1, 1, 1, 1]
            noveltyThr = 0.74
          end
          '4BANDS': begin
            bandSet = 'mod443_mod488_mod530_mod555'
            centralBands = [ 443, 488, 530, 555]
            lambdaIdx = [0, 1, 1, 1, 1, 0]
            noveltyThr = 1.07
            ;noveltyThr = 0.90
          end
          '3BANDS': begin
            bandSet = 'mod488_mod530_mod555'
            centralBands = [488, 530, 555]
            lambdaIdx = [0, 0, 1, 1, 1, 0]
            noveltyThr = 1.10
          end
          endcase
          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
        end
        'mer': begin
          bandSet = 'mer443_mer490_mer510_mer560'
          centralBands = [413, 443, 490, 510, 560, 665]
          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
        end
        ; end new version DDA + MM 2015 08 20
        'rrs': bandSet = 'rrs442_rrs491_rrs511_rrs555'
      end
    endcase
    'blts': begin
      case sensor of
        'swf': bandSet = 'swf490_swf510_swf555_swf670'
        'mod': bandSet = 'mod488_mod530_mod555_mod667'
        'mer': bandSet = 'mer490_mer510_mer560_mer665'
        'rrs': bandSet = 'rrs491_rrs511_rrs555_rrs665'
      end
      ;lambdaIdx = [0, 0, 1, 1, 1, 1]
    endcase
  endcase
  
;  bandSet = sensor
;  lambdaIdx = [0, 0, 0, 0, 0, 0]
  ; smurff system build a full set band data, everytime
;  wavelengths=centralBands
;  for iCW = 0, n_elements(centralBands)-1 do begin
;    if min(abs(centralBands[iCW] - wavelengths)) eq 0 then begin
;      bandSet = bandSet + strcompress(string(centralBands[iCW]), /remove_all) + '_' + sensor
;      lambdaIdx[iCW] = 1
;    end
;  end
;  bandSet = strmid(bandSet, 0, strlen(bandSet) - 4)
  
  ; MLP products and novelty index
  ;algoName = strupcase('blks_' + nlwSet + '_to_' + prod)
  algoName = strupcase(basin + '_' + bandSet + '_to_' + productName)
  algoArchive=tag_names(strc)
  ;algoArchive=0b
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  dataRes=temporary(alog10(temporary(readRes[where(lambdaIdx eq 1), *])))
  model = strc.(algoIdx)._data
  res = dataprod_v2(temporary(model), dataRes)
  ; extract test data here START
;  outOfControlIdx=where((res[0,*] gt 45 or res[0,*] lt 0.1) and res[1,*] le 1, countOut)
;  test1=where(res[0,*] gt 45 and res[1,*] le 1, count1)
;  test2=where(res[0,*] lt 0.1 and res[1,*] le 1, count2)
;  if count1 gt 15 and count2 gt 15 then begin
;    toWrite=strarr(countOut)
;    for j=0,countOut-1 do begin
;      thisString=''
;      for i=0, 5 do thisString=thisString+','+strcompress(readRes[i, outOfControlIdx[j]], /REMOVE)
;      thisString=thisString+','+strcompress(res[0, outOfControlIdx[j]], /REMOVE)+','+strcompress(res[1, outOfControlIdx[j]], /REMOVE)
;      toWrite[j]=strmid(thisString, 1, strlen(thisString)-1)
;    endfor
;    openW, lun, outTextInputFile+'t', /GET_LUN
;    for j=0, countOut-1 do begin
;      printf, lun, toWrite[j]
;    endfor
;    close, lun & free_lun, lun
;  endif
; extract test data here END
  doLog, 'out values:', n_elements(res)/2, 'in values:', n_elements(dataRes)/6
  
  ;help, res
  ; first row product value, second row applicability value
  ;writeTestCase, readRes, dataRes, res, algoName, nlwSet, outTextInputFile
  self->updateFid
  if self->isEnviType() then begin
    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
    aa=where(pixelMask eq 1, count)
    doLog, 'values:', n_elements(values), 'mask values:', count
    ;res=0b
    neuralNetThreshold=self.app->getKeyValue('NEURAL_NET_V2_APPLICABILITY_THRESHOLD')
    if n_elements(fakecount_nn) ne 0 then fakecount_nn++ else fakecount_nn=1 
    save, dataRes, res, filename='E:\projects\idl\smurff\data\'+'nn_'+myMonth+'_'+myYear+'_'+strcompress(fakecount_nn, /REMOVE)+'.sav'
    if n_elements(noveltyThr) eq 1 then neuralNetThreshold=noveltyThr else neuralNetThreshold=float(neuralNetThreshold)  
    idx=where(~finite(appl) or appl gt neuralNetThreshold, count)
    if count gt 0 then values[idx]=ignoreValue
    ;if count eq n_elements(appl) then stop
    ;idx=0b
    ;self->exportSourceBand, readRes, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=idx
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

;PRO NeuralNetOperatorV2::exportNlwBand, allNlw, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=hideIdxs
;
;  COMMON smurffCB, mainApp
;
;  if ~mainApp->isTestMode() then return
;  nlwBands=mainApp->getKeyValue('TEST_NLW_CHECK_EXPORT')
;  nlwId='nlw_' & ratioId='ratio_'& ratioLogId='ratiolog10_'
;  allAvBands=strcompress(self->getUsedWaveLength(), /REMOVE)
;  allAvWaves=strcompress(self->getUsedWaveLength(), /REMOVE)
;  allAvBands=strupcase(nlwId+allAvBands)
;  bandRatioTest=0
;  utils=obj_new('Utility')
;  nlwBands=utils->stringListToArray(nlwBands, separator=';', /STRING)
;  obj_destroy, utils
;
;  for i=0, n_elements(nlwBands)-1 do begin
;    lambdaIdx=(where(strupcase(nlwBands[i]) eq allAvBands, count))[0]
;    if count eq 1 then begin
;      thisNLW=reform(allNlw[lambdaIdx,*])
;      thisNLW=self->asciiToEnvi(thisNLW, pixelMask=pixelMask, ignoreValue=ignoreValue)
;      if hideIdxs[0] ne -1 then thisNLW[hideIdxs]=ignoreValue
;      self->addBand, thisNLW, nlwBands[i], /OVERWRITE
;      continue
;    endif
;    ;ratioId='ratio_'
;    ratiocheck=strpos(nlwBands[i], ratioId)
;    ratiologcheck=strpos(nlwBands[i], ratioLogId)
;    if ratiocheck ne -1 then begin
;      resString=strmid(nlwBands[i],ratiocheck+strlen(ratioId), strlen(nlwBands[i])-strlen(ratioId) )
;      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
;      lambdaIdx1=(where(bandNames[0] eq allAvWaves, count1))[0]
;      lambdaIdx2=(where(bandNames[1] eq allAvWaves, count2))[0]
;      ratioValues=reform(allNlw[lambdaIdx1,*]/allNlw[lambdaIdx2,*])
;      ratioValues=self->asciiToEnvi(ratioValues, pixelMask=pixelMask, ignoreValue=ignoreValue)
;      if hideIdxs[0] ne -1 then ratioValues[hideIdxs]=ignoreValue
;      self->addBand, ratioValues, nlwBands[i], /OVERWRITE
;      continue
;    endif
;    if ratiologcheck ne -1 then begin
;      resString=strmid(nlwBands[i],ratiologcheck+strlen(ratiologId), strlen(nlwBands[i])-strlen(ratiologcheck)+1 )
;      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
;      lambdaIdx1=(where(bandNames[0] eq allAvWaves, count1))[0]
;      lambdaIdx2=(where(bandNames[1] eq allAvWaves, count2))[0]
;      ratiologValues=alog(reform(allNlw[lambdaIdx1,*]/allNlw[lambdaIdx2,*]))
;      ratiologValues=self->asciiToEnvi(ratiologValues, pixelMask=pixelMask, ignoreValue=ignoreValue)
;      if hideIdxs[0] ne -1 then ratioValues[hideIdxs]=ignoreValue
;      self->addBand, ratioValues, nlwBands[i], /OVERWRITE
;      continue
;    endif
;  endfor
;
;END
;
;FUNCTION NeuralNetOperatorV2::getBandToExportInfoList
;
;  COMMON smurffCB, mainApp
;
;  selectedCodes=self->getBandToExportList()
;
;  nElem=n_elements(selectedCodes)
;  parInfos=replicate(getParameterInfoStruct(), nElem*2)
;  for i=0, nElem-1 do begin
;    thisPar=self.app->getParameterByCode(selectedCodes[i])
;    parInfos[i*2].bandName=thisPar.outputBandName
;    parInfos[i*2].shortName=thisPar.code
;    parInfos[i*2].longName=thisPar.outputBandName
;    parInfos[i*2].displayName=thisPar.outputBandName
;    parInfos[i*2].measureUnit=thisPar.measureUnit
;    parInfos[i*2].description=thisPar.description
;
;    parInfos[i*2+1].bandName=thisPar.outputBandName+self->getApplicabilityExtension()
;    parInfos[i*2+1].shortName=thisPar.code+self->getApplicabilityExtension()
;    parInfos[i*2+1].longName=thisPar.outputBandName+self->getApplicabilityExtension()
;    parInfos[i*2+1].displayName=thisPar.outputBandName+self->getApplicabilityExtension()
;    parInfos[i*2+1].measureUnit=''
;    parInfos[i*2+1].description=thisPar.description+self->getApplicabilityExtension()
;  endfor
;
;  nlwBands=mainApp->getKeyValue('TEST_NLW_CHECK_EXPORT')
;  utils=obj_new('Utility')
;  nlwBands=utils->stringListToArray(nlwBands, separator=';', /STRING)
;  obj_destroy, utils
;  extraParInfos=replicate(parInfos[0], n_elements(nlwBands))
;
;  for i=0, n_elements(extraParInfos)-1 do begin
;    extraParInfos[i].bandName=nlwBands[i]
;    extraParInfos[i].shortName=nlwBands[i]
;    extraParInfos[i].longName=nlwBands[i]
;    extraParInfos[i].displayName=nlwBands[i]
;    extraParInfos[i].measureUnit='N/A'
;    extraParInfos[i].description=nlwBands[i]
;    checkRatio=strpos(nlwBands[i], 'ratio_')
;    checknlw= strpos(nlwBands[i], 'nlw_')
;    checkRatioLog= strpos(nlwBands[i], 'ratiolog10_')
;    if checkRatioLog ne -1 then extraParInfos[i].binsize=0.5
;    if checkRatio ne -1 then extraParInfos[i].binsize=0.001
;    if checknlw ne -1 then extraParInfos[i].binsize=0.001
;    extraParInfos[i].description=nlwBands[i]
;  endfor
;
;  if n_elements(extraParInfos) eq 0 then return, parInfos else return, [parInfos, extraParInfos]
;
;END

FUNCTION NeuralNetOperatorV2::getRoiList

  return, ['blks']
  
END

;PRO NeuralNetOperatorV2::cleanFileSystem
;
;  self-> BiomapOperatorV1::cleanFileSystem
;
;END

;PRO NeuralNetOperatorV2::CleanUp
;
;  self->cleanFileSystem
;  self-> BiomapOperatorV1::Cleanup
;
;END

FUNCTION NeuralNetOperatorV2::init, application, workingDir, periodType, waveLengths, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
  
  if not (self -> BiomapOperatorV1 :: init(application, workingDir, periodType, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY)) then return, 0
  if n_elements(waveLengths) ne 0 then wLengs=waveLengths else wLengs=[413, 443, 490, 510, 560, 665]
  self.waveLengths=ptr_new(wLengs, /NO_COPY)
  return, 1
  
END

PRO NeuralNetOperatorV2__Define

  Struct = { NeuralNetOperatorV2 , $
    waveLengths: ptr_new(), $
    Inherits BiomapOperatorV1 $
  }
  
END