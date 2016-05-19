PRO run_compare_emis_M, request, NODISPLAY=NODISPLAY

  buildRoiFileNameFunction='buildCompareResultFileName'
  readVarDataFunction='readStandardNcContents'
  buildMosaicFileName='buildEmisCompareFileName'
  refOperatorName='COMPAREEMIS'
  ;mosaicRoiMapCode='MAP_EUR_2KM'
  destRoiCode=request->getOutputRoi()
  run_mosaic_emis_M, request, destRoiCode, NODISPLAY=NODISPLAY, $
    buildRoiFileNameFunction=buildRoiFileNameFunction, $
    readVarDataFunction=readVarDataFunction, $
    buildMosaicFileName=buildMosaicFileName, $
    refOperatorName=refOperatorName
    
END
