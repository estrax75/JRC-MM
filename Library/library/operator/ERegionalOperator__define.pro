;@structure_definition.pro
;@/library/regionalgo/regionalgo
;@/library/system_io/buildBioMapVarFileName

; Main call
FUNCTION ERegionalOperator::doComputation, roiCode, productName, sensorCode, applyfoq=applyfoq, ignoreValue=ignoreValue, textInputFile=textInputFile, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA, PRESERVE_DAT_FILE=PRESERVE_DAT_FILE

  common memtT, strc
  COMMON smurffCB, mainApp
  common refDate, myMonth, myYear, myDay
  common faker_emp, fakecount_emp
  
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
  ;algoFile=self->getConfFile() & if algoFile eq '' then algoFile = '.\regionalgo.h5'
  ;if n_elements(strc) eq 0 then strc = self->getH5Struct()
  ;doLog, 'getH5Struct memory', LEVEL=4
  ;doLog, /CHECKMEMORY
  ;workaround???
  ;hid = H5F_OPEN(file)
  ;H5F_CLOSE, hid
  ; Correction for bi-directional effects (to activate, set the flag to 1)
  foqFlag = 0
  
  ; Rrs for the selected basin and sensor
  rrsProductName=strlowcase(productName)
  if strpos(rrsProductName, 'rrs') ne -1 then begin
    algoInfo=self->getAlgoInfo(rrsProductName, sensor, basin)
    lambdaIdx=algoInfo.lambdaIdx & rrsSet=algoInfo.rrsSet & algoName=algoInfo.name
  endif else begin
    case basin of
      ;    'adrs': begin
      ;      case sensor of
      ;        'swf': rrsSet = 'swf443_swf490_swf510_swf555'
      ;        'mod': rrsSet = 'mod443_mod488_mod530_mod555'
      ;        'mer': rrsSet = 'mer443_mer490_mer510_mer560'
      ;        'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
      ;      end
      ;      lambdaIdx = [0, 1, 1, 1, 1, 0]
      ;    endcase
      'blks': begin
        case sensor of
          'swf': rrsSet = 'swf490_swf555'
          'mod': rrsSet = 'mod488_mod555'
          ;'mer': rrsSet = 'mer443_mer490_mer510_mer560'
          ;'rrs': rrsSet = 'rrs442_rrs491_rrs511_rrs555'
          else:message, 'sensor: ' + sensor + ' not allowed'
        end
        lambdaIdx = [0, 0, 1, 0, 1, 0]
      endcase
      'blts': begin
        case sensor of
          'swf': rrsSet = 'swf555_swf670'
          'mod': rrsSet = 'mod555_mod667'
          ;'mer': rrsSet = 'mer490_mer510_mer560_mer665'
          ;'rrs': rrsSet = 'rrs491_rrs511_rrs555_rrs665'
          else:message, 'sensor: ' + sensor + ' not allowed'
        end
        lambdaIdx = [0, 0, 0, 0, 1, 1]
      endcase
    endcase
  endelse
  
  ; Regional product
  if rrsSet eq '' then begin
    aa=dialog_message('roi/sensor: '+roiName+'/'+sensor+' not available for Regional Operator')
    return, 0
  endif
  if n_elements(algoName) eq 0 then algoName = self.algoPrefix+strlowcase(basin + '_' + rrsSet + '_to_' + productName)
  dataRes=temporary(readRes[where(lambdaIdx eq 1), *])
  ;waves=strcompress(self->getUsedWaveLength(), /REMOVE)
  ;for i=0, n_elements(lambdaIdx)-1 do begin
  ;  t=strcompress(fix(randomu(seed)*100), /REMOVE)
  ;  doHistogram, reform(dataRes[i,*]), self.app->getKeyValue('TEMP_DIR'), 'hist_'+waves[i]+t, '001', '06', '2006', binsize=0.001
  ;endfor
  
  ;data=0b
  ;if n_elements(res) eq 0 then res = dataprod(temporary(model), temporary(dataRes))
  res = call_function(algoName, dataRes)
  
  ; first row product value, second row applicability value
  ;writeTestCase, readRes, dataRes, res, algoName, rrsSet, outTextInputFile
  self->updateFid
  if self->isEnviType() then begin
    values=reform(temporary(res[0,*])) & appl=reform(temporary(res[1,*]))
    ;res=0b
    biomapThreshold=self.app->getKeyValue('BIOMAP_V1_APPLICABILITY_THRESHOLD')
    ;ignoreValue=float(self.app->getKeyValue('NAN_VALUE'))
    biomapThreshold=float(biomapThreshold)
    if n_elements(fakecount_emp) ne 0 then fakecount_emp++ else fakecount_emp=1 
    ;save, dataRes, res, filename='E:\projects\idl\smurff\data\'+'emp_'+myMonth+'_'+myYear+'_'+strcompress(fakecount_emp, /REMOVE)+'.sav'
    idx=where(~finite(appl) or appl gt biomapThreshold, count)
    if count gt 0 then values[idx]=ignoreValue
    idx=0b
    ;productValues=self->asciiToEnvi(res[0,*], pixelMask=pixelMask, ignoreValue=ignoreValue)
    productValues=self->asciiToEnvi(values, pixelMask=pixelMask, ignoreValue=ignoreValue)
    ;productValues=self.app->applyRangeConditions(productName, productValues, productName, ignoreValue=ignoreValue)
    ;wrongIdxs=where(values gt 500, wrongCount)
    ;if wrongCount ne 0 then begin
    ;  tempDir=mainApp->getKeyValue('TEMP_DIR')
    ;   doHistogram, productValues, tempDir, roiCode, day, month, year
    ;endif
    self->addBand, productValues, productName, /OVERWRITE, ignoreValue=ignoreValue
    idxs=where(~finite(appl) or appl gt 10., count)
    if count ne 0 then appl[idxs]=10
    ;productValues=self->asciiToEnvi(res[1,*], pixelMask=pixelMask, ignoreValue=ignoreValue)
    productValues=self->asciiToEnvi(appl, pixelMask=pixelMask, ignoreValue=ignoreValue)
    self->addBand, productValues, productName+self->getApplicabilityExtension(), /OVERWRITE, ignoreValue=ignoreValue
    
    ;if self.app->isTestMode() then begin
    ;  self->exportRrsBand, readRes, pixelMask=pixelMask, ignoreValue=ignoreValue, hideIdxs=idxs
    ;endif
    idxs=0b
    ;values=self->getBand(productName)
    ;idxs=where(res[0,*] ne 0 and res[0,*] -9999, count1)
    ;idxs=where(productValues ne 0 and productValues ne -9999, count2)
    ;idxs=where(values ne 0 and values ne -9999, count3)
    ;doLog, count1, count2, count3, level=0
    if ~(keyword_set(PRESERVE_DAT_FILE)) then self->removeLastDatFile
    return, 1
  endif
  
END

FUNCTION ERegionalOperator::buildOperatorResultFileName, dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL

  return, call_function('buildERegionalVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
;  if dType eq 'M' then return, buildBioMapVarFileName_M(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
;  if dType eq 'D' then return, buildBioMapVarFileName_D(dType, displayName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
    
END

FUNCTION ERegionalOperator::mapRoiCode, roiName, FOUND=FOUND

  FOUND=0
  applicationROI = self->getApplicationRoiList()
  operatorROI = self->getRoiList()
  idx=(where(roiName eq applicationROI))[0]
  
  if idx[0] ne -1 then begin
    FOUND=1
    return, operatorROI[idx[0]]
  endif
  message, 'roi: '+roiName+' not available for Regional Operator'
  return, 0
  
END

FUNCTION ERegionalOperator::getRoiList

  ;return, ['adrs', 'adrs', 'blks', 'blts']
  return, ['blks', 'blts']
  
END

FUNCTION ERegionalOperator::getApplicationRoiList

  ;return, ['NADR', 'ADRI', 'BLCK', 'BALT']
  return, ['BLCK', 'BALT']
  
END

PRO ERegionalOperator::CleanUp

  self->cleanFileSystem
  self-> BiomapOperatorV1::Cleanup
  
END

FUNCTION ERegionalOperator::init, application, workingDir, periodType, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  confFile=''
  if not (self -> BiomapOperatorV1 :: init(application, workingDir, periodType, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY)) then return, 0
  self.algoPrefix='e_regionalgo_'
  return, 1
  
END

PRO ERegionalOperator__Define

  Struct = { ERegionalOperator , $
    algoPrefix: '', $
    Inherits BiomapOperatorV1 $
    }
    
END