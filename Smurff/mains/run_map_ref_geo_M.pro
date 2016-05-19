PRO run_map_ref_geo_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  buildRoiFileNameFunction='buildGlobMapVarFileName'
  readVarDataFunction='readStandardNcContents'
  buildMosaicFileName='buildGlobMapVarFileName'
  refOperatorName='GLOBOPERATOR'
  ;mosaicRoiMapCode='MAP_EUR_2KM'
  destRoiCode=request->getOutputRoi()
  roiList=request->getRoiList()
  roiArchiveList=request->getRoiArchiveList()
  
  roiNo=n_elements(roiList)
  destRoiCodes=strarr(roiNo)
  for i=0, roiNo-1 do begin
    roiInfo=mainApp->getROIInfoByCode(roiList[i])
    destRoiCodes[i]=roiInfo.refRoiCode
  endfor
  ;newRoiListIndex=sort(destRoiCode)
  uniqDestRoiCode=destRoiCodes[uniq(destRoiCodes)]
  destRoiNo=n_elements(uniqDestRoiCode)
  
  for i=0, destRoiNo-1 do begin
    indexes=where(uniqDestRoiCode[i] eq destRoiCodes, count)
    ;for j=0, count do thisRoiList=thisRoiList, roiList[indexes[]] 
    request->setRoiList,roiList[indexes]
    request->setRoiArchiveList,roiArchiveList[indexes]
    ;roiInfo=mainApp->getROIInfoByCode(uniqDestRoiCode[i])
    request->setOutputRoi, uniqDestRoiCode[i]
    destRoiCode=uniqDestRoiCode[i]
    run_mosaic_emis_M, request, destRoiCode, NODISPLAY=NODISPLAY, $
      buildRoiFileNameFunction=buildRoiFileNameFunction, $
      readVarDataFunction=readVarDataFunction, $
      buildMosaicFileName=buildMosaicFileName, $
      refOperatorName=refOperatorName
  endfor
  
END