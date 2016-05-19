FUNCTION Parameter::applyRangeConditions, parCode, bandValues, bandVarName, ignoreValue=ignoreValue, SETNAN=SETNAN, NORANGE=NORANGE

  return, self->checkValueRange(parCode, bandValues, ignoreValue=ignoreValue, CASESENSITIVE=CASESENSITIVE, SETNAN=SETNAN, NORANGE=NORANGE)
  
END

FUNCTION Parameter::checkValueRange, code, data, ignoreValue=ignoreValue, CASESENSITIVE=CASESENSITIVE, SETNAN=SETNAN, NORANGE=NORANGE

  par=self->getElementByCode(code, CASESENSITIVE=CASESENSITIVE)
  ignoreCount=0
  validCount=0
  if n_elements(ignoreValue) eq 1 then ignoreIdxs=where(data eq ignoreValue, ignoreCount, complement=validIdxs, ncomplement=validCount)
  res=data
  if ~keyword_set(NORANGE) then begin
    minVal=min(data, max=maxVal, /NAN)
    if validCount gt 0 then minVal=min(data[validIdxs], max=maxVal, /NAN)
    if strupcase(par.minValueAccepted) ne 'N/A' then lower=float(par.minValueAccepted) else lower= minVal-float(minVal)/10
    if strupcase(par.maxValueAccepted) ne 'N/A' then upper=float(par.maxValueAccepted) else upper= maxVal+float(maxVal)/10
    if ~n_elements(ignoreValue) eq 1 then dataIgnoreValue=maxVal else dataIgnoreValue=ignoreValue
    validIdxs=where(data gt lower and data lt upper and data ne dataIgnoreValue and finite(data), validCount)
  endif
  if (n_elements(ignoreValue) eq 1) and ~keyword_set(SETNAN) then res[*]=ignoreValue else res[*]=!VALUES.F_NAN
  if validCount gt 0 then res[validIdxs]=data[validIdxs]
  if ignoreCount gt 0 and ~keyword_set(SETNAN) then res[ignoreIdxs]=ignoreValue
  return, res
  
END

FUNCTION Parameter::getElementByCode, code, CASESENSITIVE=CASESENSITIVE, EXISTS=EXISTS

  allCodes=self->getCodes()
  checkCode=code
  if ~keyword_set(CASESENSITIVE) then begin
    allCodes=strupcase(allCodes)
    checkCode=strupcase(code)
  endif
  idx=(where(checkCode eq allCodes, count))[0]
  list=self->getList()
  if count eq 1 then begin
    EXISTS=1
    element=list[idx]
  endif else begin
    EXISTS=0
    element=-1
  endelse
  return, element
  
END

FUNCTION Parameter::getElementByOutputBandName, code, CASESENSITIVE=CASESENSITIVE, EXISTS=EXISTS

  EXISTS=0
  allCodes=self->getOutputBandNames()
  if ~keyword_set(CASESENSITIVE) then begin
    allCodes=strupcase(allCodes)
    code=strupcase(code)
  endif
  idx=(where(code eq allCodes, count))[0]
  list=self->getList()
  if count eq 1 then begin
    EXISTS=1
    return, list[idx]
  endif
  return, -1
  
END

FUNCTION Parameter::getElementByInputBandName, code, CASESENSITIVE=CASESENSITIVE, EXISTS=EXISTS

  EXISTS=0
  allCodes=self->getInputBandNames()
  if ~keyword_set(CASESENSITIVE) then begin
    allCodes=strupcase(allCodes)
    code=strupcase(code)
  endif
  idx=(where(code eq allCodes, count))[0]
  list=self->getList()
  if count eq 1 then begin
    EXISTS=1
    return, list[idx]
  endif
  return, -1
  
END

FUNCTION Parameter::getFullElement, code, INDEX=INDEX

  element=self->getListStructDef()
  if n_elements(code) ne 1 then elementIndex=INDEX else elementIndex=self->getIndexByCode(code)
  thisList=self->getList()
  element.code=thisList.code[elementIndex]
  element.name=thisList.name[elementIndex]
  element.displayname=thisList.displayname[elementIndex]
  element.graphicDisplayname=thisList.graphicDisplayname[elementIndex]
  element.measureUnit=thisList.measureUnit[elementIndex]
  element.graphicMeasureUnit=thisList.graphicMeasureUnit[elementIndex]
  element.minValueAccepted=thisList.minValueAccepted[elementIndex]
  element.maxValueAccepted=thisList.maxValueAccepted[elementIndex]
  element.waveLength=thisList.waveLength[elementIndex]
  element.stdWaveLength=thisList.stdWaveLength[elementIndex]
  element.conversionFunction=thisList.isRrs[elementIndex]
  element.conversionFunction=thisList.conversionFunction[elementIndex]
  element.description=thisList.description[elementIndex]
  
END

FUNCTION Parameter::getIndexByCode, code

  codes=self->getCodes()
  idx=(where(code eq codes))[0]
  return, idx
  
END

FUNCTION Parameter::buildFullFieldList, simpleList

  baseStruct=self->getListStructDef()
  nElem=n_elements(simpleList)
  list=replicate(baseStruct, nElem)
  for i=0, nElem-1 do begin
    list[i].code=simpleList[i]
    list[i].inputBandName=simpleList[i]
    list[i].outputBandName=simpleList[i]
    list[i].name=simpleList[i]
    list[i].displayname=simpleList[i]
    list[i].graphicDisplayname=simpleList[i]
    list[i].measureUnit=simpleList[i]
    list[i].graphicMeasureUnit=simpleList[i]
    list[i].minValueAccepted=simpleList[i]
    list[i].maxValueAccepted=simpleList[i]
    list[i].waveLength=simpleList[i]
    list[i].stdWaveLength=simpleList[i]
    list[i].isRrs=simpleList[i]
    list[i].conversionFunction=simpleList[i]
    list[i].description='Descr of: '+simpleList[i]
  endfor
  return, list
  
END

FUNCTION  Parameter::getVersion

  return, '1.0'
  
END

FUNCTION Parameter::buildRecordFromElement, index, structElement

  code=structElement
  name=structElement
  inputBandName=structElement
  outputBandName=structElement
  displayName=structElement
  graphicDisplayName=structElement
  measureUnit='Measure unit of: '+structElement
  graphicMeasureUnit='Graphic Measure unit of: '+structElement
  minValueAccepted=structElement
  maxValueAccepted=structElement
  waveLength=structElement
  stdWaveLength=structElement
  description='Descr of: '+structElement
  isRrs=0
  conversionFunction=''
  record=[code, inputBandName, outputBandName, name, displayName, graphicDisplayName, measureUnit, graphicMeasureUnit, $
    minValueAccepted, maxValueAccepted, waveLength, stdWaveLength, isRrs, conversionFunction, description]
  return, record
  
END

FUNCTION Parameter::getisRrss

  thisList=self->getList()
  return, thisList[*].isRrs
  
END

FUNCTION Parameter::getDisplayNames

  thisList=self->getList()
  return, thisList[*].displayName
  
END

FUNCTION Parameter::getGraphicDisplayNames

  thisList=self->getList()
  return, thisList[*].graphicDisplayName
  
END

FUNCTION Parameter::getMeasurUnits

  thisList=self->getList()
  return, thisList[*].measureUnit
  
END

FUNCTION Parameter::getGraphicMeasurUnits

  thisList=self->getList()
  return, thisList[*].graphicMeasureUnit
  
END

FUNCTION Parameter::getMinValueAccepteds

  thisList=self->getList()
  return, thisList[*].minValueAccepted
  
END

FUNCTION Parameter::getMaxValueAccepteds

  thisList=self->getList()
  return, thisList[*].maxValueAccepted
  
END

FUNCTION Parameter::getWaveLengths

  thisList=self->getList()
  return, thisList[*].waveLength
  
END

FUNCTION Parameter::getStdWaveLengths

  thisList=self->getList()
  return, thisList[*].stdWaveLength
  
END

FUNCTION Parameter::getCodes

  thisList=self->getList()
  return, thisList[*].code
  
END

FUNCTION Parameter::getInputBandNames

  thisList=self->getList()
  return, thisList[*].inputBandName
  
END

FUNCTION Parameter::getOutputBandNames

  thisList=self->getList()
  return, thisList[*].outputBandName
  
END

FUNCTION Parameter::getDescriptions

  thisList=self->getList()
  return, thisList[*].description
  
END

FUNCTION Parameter::getConversionFunctions

  thisList=self->getList()
  return, thisList[*].conversionFunction
  
END

PRO Parameter::streamPrint

  print, '***********************'
  print, '**Start of<',OBJ_CLASS(self),'>**'
  
  print, '**** fileName:<', self.fileName,'>'
  thisList=self->getList()
  for i=0, n_elements(thisList)-1 do begin
    print, '**element n.<', strcompress(i, /REMOVE),'>'
    print, '**** code:<', thisList[i].code,'>'
    print, '**** inputBandName:<', thisList[i].inputBandName,'>'
    print, '**** outputBandName:<', thisList[i].outputBandName,'>'
    print, '**** name:<', thisList[i].displayName,'>'
    print, '**** displayName:<', thisList[i].displayName,'>'
    print, '**** graphicDisplayName:<', thisList[i].graphicDisplayName,'>'
    print, '**** measureunit:<', thisList[i].measureUnit,'>'
    print, '**** graphicMeasureunit:<', thisList[i].graphicMeasureUnit,'>'
    print, '**** minValueAccepted:<', thisList[i].minValueAccepted,'>'
    print, '**** maxValueAccepted:<', thisList[i].maxValueAccepted,'>'
    print, '**** waveLength:<', thisList[i].waveLength,'>'
    print, '**** stdWaveLength:<', thisList[i].stdWaveLength,'>'
    print, '**** isRrs:<', thisList[i].isRrs,'>'
    print, '**** conversionFunction:<', thisList[i].description,'>'
    print, '**** description:<', thisList[i].description,'>'
    print, '**'
  endfor
  
  print, '***********************'
  print, '**End of:<',OBJ_CLASS(self),'>**'
  
END

FUNCTION Parameter::getListStructDef

  struct = { code:'',$
    inputBandName:'', $
    outputBandName:'', $
    name:'', $
    displayName:'', $
    graphicDisplayName:'', $
    measureUnit:'', $
    graphicMeasureUnit:'', $
    minValueAccepted:'', $
    maxValueAccepted:'', $
    waveLength:'', $
    stdWaveLength:'', $
    isRrs: 0, $
    conversionFunction: '', $
    description: '' $
    }
    
  return, struct
  
END

FUNCTION Parameter::buildListStruct, fieldsInfo, NOFILL=NOFILL

  thisStruct=self->getListStructDef()
  if ~keyword_set(NOFILL) gt 0 then begin
    thisStruct.code=fieldsInfo[0]
    thisStruct.inputBandName=fieldsInfo[1]
    thisStruct.outputBandName=fieldsInfo[2]
    thisStruct.name=fieldsInfo[3]
    thisStruct.displayName=fieldsInfo[4]
    thisStruct.graphicDisplayName=fieldsInfo[5]
    thisStruct.measureUnit=fieldsInfo[6]
    thisStruct.graphicMeasureUnit=fieldsInfo[7]
    thisStruct.minValueAccepted=fieldsInfo[8]
    thisStruct.maxValueAccepted=fieldsInfo[9]
    thisStruct.waveLength=fieldsInfo[10]
    thisStruct.stdWaveLength=fieldsInfo[11]
    thisStruct.isRrs=fix(fieldsInfo[12])
    thisStruct.conversionFunction=fieldsInfo[13]
    thisStruct.description=fieldsInfo[14]
  endif
  return, thisStruct
  
END

FUNCTION Parameter::listFieldsExpectedNumber

  return, n_tags(self->buildListStruct(/NOFILL))
  
END

FUNCTION Parameter::init, application, filename, mode=mode

  if not(self -> ConfigurableData::init(application, filename, mode=mode)) then return, 0
  return, 1
  
END

PRO Parameter::cleanUp

  self -> ConfigurableData::cleanUp
  
END

PRO Parameter__Define

  Struct = { Parameter , $
    Inherits ConfigurableData $
    }
    
END