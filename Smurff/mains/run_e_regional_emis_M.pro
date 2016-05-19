PRO run_e_regional_emis_M, request, NODISPLAY=NODISPLAY

  buildRoiFileNameFunction='buildERegionalResultFileName'
  readVarDataFunction='readStandardNcContents'
  buildMosaicFileName='buildEmisERegionalFileName'
  refOperatorName='EREGIONALEMIS'
  ;mosaicRoiMapCode='MAP_EUR_2KM'
  destRoiCode=request->getOutputRoi()
  run_mosaic_emis_M, request, destRoiCode, NODISPLAY=NODISPLAY, $
    buildRoiFileNameFunction=buildRoiFileNameFunction, $
    readVarDataFunction=readVarDataFunction, $
    buildMosaicFileName=buildMosaicFileName, $
    refOperatorName=refOperatorName
   
END