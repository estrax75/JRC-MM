PRO run_biomap_emis_M, request, NODISPLAY=NODISPLAY

  buildRoiFileNameFunction='buildBiomapResultFileName'
  readVarDataFunction='readStandardNcContents'
  buildMosaicFileName='buildEmisBioMapFileName'
  refOperatorName='BIOMAPEMIS'
  ;mosaicRoiMapCode='MAP_EUR_2KM'
  destRoiCode=request->getOutputRoi()
  run_mosaic_emis_M, request, destRoiCode, NODISPLAY=NODISPLAY, $
    buildRoiFileNameFunction=buildRoiFileNameFunction, $
    readVarDataFunction=readVarDataFunction, $
    buildMosaicFileName=buildMosaicFileName, $
    refOperatorName=refOperatorName
   
END