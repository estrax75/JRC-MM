PRO NeuralNetOperatorV3::loadH5Struct, fileName

  if self->getConfFile() eq '' then algoFile = '.\mlp-rrs_v3.h5' else algoFile=self->getConfFile()
  strc = h5_parse(algoFile, /read_data)
  self->setH5Struct, strc
  
END

;FUNCTION NeuralNetOperatorV3::doComputation, roiCode, productName, sensorCode, waveLengths, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE
;
;  common memtT, strc
;  COMMON smurffCB, mainApp
;  common refDate, myMonth, myYear, myDay
;  common faker_nn, fakecount_nn
;
;  ;OVERVIEW
;  ;
;  ; This IDL procedure is provided in comply with requirements and specifications of the Service Contract N. CCR.IES.C390C391935.X0. Please refer to the companion pdf document for details.
;  ;
;  ;INPUTS
;  ;
;  ; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf).
;  ;
;  ; wavelengts: set of central wavebands to be used to derive data products
;  ;
;  ; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
;  ; yellow substance absorption at 412 nm (ay412).
;  ;
;  ;EXECUTION
;  ;
;  ;For testing and operational applications proceed as follows:
;  ; 1) Extract the content of the \texttt{blks.zip} file in a folder (e.g., c:/projects/idl/test). Set the IDL working directory to that folder  (enter cd, 'c:/projects/idl/test'
;  ; in the IDL command line, note the comma after cd and that the path is within quotes).
;  ; 2) Enter the following command in the IDL prompt:  blks, `mer', [413, 443, 490, 510, 560, 665], 'chlideachla'}. Note that the arguments sensor ( 'mer'}), wavelengths (e.g.,
;  ; [413, 443, 490, 510, 560, 665]}) and  product ('chlideachla'}) used  as input to the procedure blks can be any combination set. Input data for testing the blks.pro procedure
;  ; in the file sit-nlw.dat (the data header specifies column entries).
;  ;----------------------------------------------------------------------------
;
;  NODATA=0
;  doLog, '**', self.mainFileName, '**', level=0
;  if n_elements(fileName) eq 1 then self->setMainFileName, fileName, OPEN=OPEN,COPY=COPY
;  ;if n_elements(waveLengths) eq 0 then waveLengths=self->getWaveLengths()
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
;    return, 1
;  endif
;
;  ;data = dataread_v3('./insituNlwMeasWaves.dat')
;  readRes = dataread_v3(outTextInputFile, pixelNumber=pixelNumber)
;  nData = n_elements(readRes[0, *])
;  if nData lt 2 then begin
;    NODATA=1
;    if ~(keyword_set(PRESERVE_DAT_FILE)) then self->removeLastDatFile
;    return, 1
;  endif
;  ;if n_elements(data) eq 0 then begin
;  ;  data=dataNew
;  ;endif
;  ;data=0b
;  ;help, /memory
;  ;doLog, 'dataread memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  ;nData = n_elements(readRes[0, *])
;
;  ; Input file with the coefficients of regional algorithms
;  algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\mlp-rrs_v3.h5'
;  if n_elements(strc) eq 0 then strc = self->getH5Struct()
;  ;doLog, 'getH5Struct memory', LEVEL=4
;  ;doLog, /CHECKMEMORY
;  ;workaround???
;  ;hid = H5F_OPEN(file)
;  ;H5F_CLOSE, hid
;  ; Correction for bi-directional effects (to activate, set the flag to 1)
;  foqFlag = 0
;  ; channels & novelty (D'Alimonte suggestions, september 2015)
;  ; 1) Novelty threshold for [mod412,mod443,mod488,mod530,mod555,mod667]: 0.88
;  ; 2) Novelty threshold for [mod443,mod488,mod530,mod555,mod667]: 0.74
;  ; 3) Novelty threshold for [mod443,mod488,mod530,mod555]: 0.90
;  ; 4) Novelty threshold for [mod488,mod530,mod555]: 1.10
;  ; end
;  ;
;  ; Nlw for the selected basin and sensor
;  case basin of
;    'adrs': begin
;      case sensor of
;        'swf': bandSet = 'swf443_swf490_swf510_swf555'
;        'mod': bandSet = 'mod443_mod488_mod530_mod555'
;        'mer': bandSet = 'mer443_mer490_mer510_mer560'
;        'rrs': bandSet = 'rrs442_rrs491_rrs511_rrs555'
;      end
;      lambdaIdx = [0, 1, 1, 1, 1, 0]
;    endcase
;    'blks': begin
;      ;lambdaIdx = [0, 1, 1, 1, 1, 0]
;      case sensor of
;        ; start new version DDA + MM 2015 08 20
;        ; adding variable centralBands
;        ; overwrite variable lambdaIdx
;        'swf': begin
;          bandSet = 'swf443_swf490_swf510_swf555'
;          centralBands = [412, 443, 490, 510, 555, 670]
;          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        ; add for new version
;        'mod': begin
;          bandSetToUse=self.app->getKeyValue('NEURAL_NET_BAND_SET')
;          if bandSetToUse eq '' then  bandSetToUse='4BANDS'
;          case bandSetToUse of
;          '6BANDS': begin
;          bandSet = 'mod412_mod443_mod488_mod530_mod555_mod667'
;          centralBands = [412, 443, 488, 530, 555, 667]
;          lambdaIdx = [1, 1, 1, 1, 1, 1]
;          noveltyThr = 0.88
;          end
;          '5BANDS': begin
;            bandSet = 'mod443_mod488_mod530_mod555_mod667'
;            centralBands = [412, 443, 488, 530, 555, 667]
;            lambdaIdx = [0, 1, 1, 1, 1, 1]
;            noveltyThr = 0.74
;          end
;          '4BANDS': begin
;            bandSet = 'mod443_mod488_mod530_mod555'
;            centralBands = [ 443, 488, 530, 555]
;            lambdaIdx = [0, 1, 1, 1, 1, 0]
;            noveltyThr = 1.07
;            ;noveltyThr = 0.90
;          end
;          '3BANDS': begin
;            bandSet = 'mod488_mod530_mod555'
;            centralBands = [488, 530, 555]
;            lambdaIdx = [0, 0, 1, 1, 1, 0]
;            noveltyThr = 1.10
;          end
;          endcase
;          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        'mer': begin
;          bandSet = 'mer443_mer490_mer510_mer560'
;          centralBands = [413, 443, 490, 510, 560, 665]
;          ;lambdaIdx = [0, 0, 0, 0, 0, 0]
;        end
;        ; end new version DDA + MM 2015 08 20
;        'rrs': bandSet = 'rrs442_rrs491_rrs511_rrs555'
;      end
;    endcase
;    'blts': begin
;      case sensor of
;        'swf': bandSet = 'swf490_swf510_swf555_swf670'
;        'mod': bandSet = 'mod488_mod530_mod555_mod667'
;        'mer': bandSet = 'mer490_mer510_mer560_mer665'
;        'rrs': bandSet = 'rrs491_rrs511_rrs555_rrs665'
;      end
;      ;lambdaIdx = [0, 0, 1, 1, 1, 1]
;    endcase
;  endcase
;
;;  bandSet = sensor
;;  lambdaIdx = [0, 0, 0, 0, 0, 0]
;  ; smurff system build a full set band data, everytime
;;  wavelengths=centralBands
;;  for iCW = 0, n_elements(centralBands)-1 do begin
;;    if min(abs(centralBands[iCW] - wavelengths)) eq 0 then begin
;;      bandSet = bandSet + strcompress(string(centralBands[iCW]), /remove_all) + '_' + sensor
;;      lambdaIdx[iCW] = 1
;;    end
;;  end
;;  bandSet = strmid(bandSet, 0, strlen(bandSet) - 4)
;
;  ; MLP products and novelty index
;  ;algoName = strupcase('blks_' + nlwSet + '_to_' + prod)
;  algoName = strupcase(basin + '_' + bandSet + '_to_' + productName)
;  algoArchive=tag_names(strc)
;  ;algoArchive=0b
;  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
;  dataRes=temporary(alog10(temporary(readRes[where(lambdaIdx eq 1), *])))
;  model = strc.(algoIdx)._data
;  res = dataprod(temporary(model), dataRes)
;  ; extract test data here START
;;  outOfControlIdx=where((res[0,*] gt 45 or res[0,*] lt 0.1) and res[1,*] le 1, countOut)
;;  test1=where(res[0,*] gt 45 and res[1,*] le 1, count1)
;;  test2=where(res[0,*] lt 0.1 and res[1,*] le 1, count2)
;;  if count1 gt 15 and count2 gt 15 then begin
;;    toWrite=strarr(countOut)
;;    for j=0,countOut-1 do begin
;;      thisString=''
;;      for i=0, 5 do thisString=thisString+','+strcompress(readRes[i, outOfControlIdx[j]], /REMOVE)
;;      thisString=thisString+','+strcompress(res[0, outOfControlIdx[j]], /REMOVE)+','+strcompress(res[1, outOfControlIdx[j]], /REMOVE)
;;      toWrite[j]=strmid(thisString, 1, strlen(thisString)-1)
;;    endfor
;;    openW, lun, outTextInputFile+'t', /GET_LUN
;;    for j=0, countOut-1 do begin
;;      printf, lun, toWrite[j]
;;    endfor
;;    close, lun & free_lun, lun
;;  endif
;; extract test data here END
;  doLog, 'out values:', n_elements(res)/2, 'in values:', n_elements(dataRes)/6
;
;  ;help, res
;  ; first row product value, second row applicability value
;  ;writeTestCase, readRes, dataRes, res, algoName, nlwSet, outTextInputFile
;  self->updateFid
;  if self->isEnviType() then begin
;    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
;    aa=where(pixelMask eq 1, count)
;    doLog, 'values:', n_elements(values), 'mask values:', count
;    ;res=0b
;    neuralNetThreshold=self.app->getKeyValue('NEURAL_NET_APPLICABILITY_THRESHOLD')
;    if n_elements(fakecount_nn) ne 0 then fakecount_nn++ else fakecount_nn=1
;    save, dataRes, res, filename='E:\projects\idl\smurff\data\'+'nn_'+myMonth+'_'+myYear+'_'+strcompress(fakecount_nn, /REMOVE)+'.sav'
;    if n_elements(noveltyThr) eq 1 then neuralNetThreshold=noveltyThr else neuralNetThreshold=float(neuralNetThreshold)
;    idx=where(~finite(appl) or appl gt neuralNetThreshold, count)
;    if count gt 0 then values[idx]=ignoreValue
;    ;if count eq n_elements(appl) then stop
;    ;idx=0b
;    ;self->exportSourceBand, readRes, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=idx
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
;    ;doLog, 'store as envi memory', LEVEL=4
;    ;doLog, /CHECKMEMORY
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

FUNCTION NeuralNetOperatorV3::doBlackSeaAlg, strc, basin, aopSet, data, prod

  ; Tag names of the MLP in the archive file
  algoArchive=tag_names(strc)
  
  ; MLP_BLKS products and novelty index
  algoName = strupcase(basin + '_' + aopSet + '_to_' + prod)
  print, algoName 
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  mlpBlks = strc.(algoIdx)._data
  ;res = dataprod_v3(mlpBlks, alog10(data(where(lambdaIdx eq 1), *)))
  res = dataprod_v3(mlpBlks, data)
  
  ; MLP_BLK1 products and novelty index
  algoName = strupcase(strmid(basin, 0, 3) + '1_' + aopSet + '_to_' + prod)
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  mlpBlk1 = strc.(algoIdx)._data
  ;res1 = dataprod_v3(mlpBlk1, alog10(data(where(lambdaIdx eq 1), *)))
  res1 = dataprod_v3(mlpBlk1, data)
  
  ; MLP_BLK2 products and novelty index
  algoName = strupcase(strmid(basin, 0, 3) + '2_' + aopSet + '_to_' + prod)
  algoIdx=where(strcmp(algoArchive, algoName) eq 1)
  mlpBlk2 = strc.(algoIdx)._data
  ;res2 = dataprod_v3(mlpBlk2, alog10(data(where(lambdaIdx eq 1), *)))
  res2 = dataprod_v3(mlpBlk2, data)
  
  aa=where(finite(res),count)
  aa=where(finite(res1),count1)
  aa=where(finite(res2),count2)
  
  ; MLP_BLK1 and MLP_BLK2 combined results
  ;0.85 1.08 1.14 0.78 1.25 1.45
  ;ref threshold
  thresh1 = 1.08
  ;max threshold
  ;thresh1 = 1.14

  ;ref threshold
  thresh2 =  1.25
  ;max threshold
  ;thresh2 = 1.45

  ;resC = fltarr(1, n_elements(res(0,*)))
  resC = fltarr(2, n_elements(res(0,*)))
  idx1 =  where((res1[1, *] le thresh1))
  idx2 =  where((res2[1, *] le thresh2))
  idxC =  where((res1[1, *] le thresh1) and (res2[1, *] le thresh2))
  resC[0, idx1] = res1[0, idx1]
  resC[0, idx2] = res2[0, idx2]
  w1 = 1 / (10 ^ res1[1, idxC])
  w2 = 1 / (10 ^ res2[1, idxC])
  resC[0, idxC] = (w1 * res1[0, idxC] + w2 * res2[0, idxC]) / (w1 + w2)
  ; output compatibility, add novelty
  resC[1, *]=10.
  resC[1, idx1] = res1[1, idx1]
  resC[1, idx2] = res2[1, idx2]
  return, resC 
  
END

FUNCTION NeuralNetOperatorV3::doComputation, roiCode, productName, sensorCode, waveLengths, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE

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
  
  ;data = dataread('./insituNlwMeasWaves.dat')
  readRes = dataread_v3(outTextInputFile, pixelNumber=pixelNumber)
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
  algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\mlp-rrs_v3.h5'
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
          bandSetToUse=self.app->getKeyValue('NEURAL_NET_V3_BAND_SET')
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
  print, '*****'
  print, 'v3', algoFile, basin, bandSet, productName
  print, '*****'

  model = strc.(algoIdx)._data
  print, '**model=***'

  if basin eq 'blks' then res=self->doBlackSeaAlg(strc, basin, bandSet, dataRes, productName) else res = dataprod_v3(temporary(model), dataRes) 
  doLog, 'out values:', n_elements(res)/2, 'in values:', n_elements(dataRes)/6
  
  self->updateFid
  if self->isEnviType() then begin
    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
    aa=where(pixelMask eq 1, count)
    doLog, 'values:', n_elements(values), 'mask values:', count
    ;res=0b
    neuralNetThreshold=self.app->getKeyValue('NEURAL_NET_V3_APPLICABILITY_THRESHOLD')
    if n_elements(fakecount_nn) ne 0 then fakecount_nn++ else fakecount_nn=1
    save, dataRes, res, filename=mainApp->getKeyValue('TEMP_DIR')+'nn_'+myMonth+'_'+myYear+'_'+strcompress(fakecount_nn, /REMOVE)+'.sav'
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

FUNCTION NeuralNetOperatorV3::init, application, workingDir, periodType, waveLengths, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
  REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
  
  if not (self -> NeuralNetOperatorV2 :: init(application, workingDir, periodType, waveLengths, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY)) then return, 0
  return, 1
  
END

PRO NeuralNetOperatorV3__Define

  Struct = { NeuralNetOperatorV3 , $
    Inherits NeuralNetOperatorV2 $
  }
  
END