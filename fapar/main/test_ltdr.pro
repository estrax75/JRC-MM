pro test_ltdr, pixelToTest

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  sourceDir='/space3/storage/products/LAN/AVH/L1/PLC/2003/07/'
  dataSetFPA=ST_operator->readNcdfVar(sourcedir+'AVH_20030725_001D_900S900N1800W1800E_0005D_FPA_N16.NC', 'LDTR_FLAG', FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  dataSetBRDF=ST_operator->readNcdfVar(sourcedir+'AVH_20030725_001D_900S900N1800W1800E_0005D_BRDF_N16.NC', 'LDTR_FLAG', FOUND=FOUND, REVERSE=REVERSE, TRANSPOSE=TRANSPOSE)
  
  aa=cgi_map_bitwise_flag(dataSetFPA.data, pixelToTest)
  bb=cgi_map_bitwise_flag(fix(dataSetFPA.data*2), pixelToTest)
  cc=cgi_map_bitwise_flag(dataSetBRDF.data, pixelToTest)
  
  idxaa=where(aa eq 1, cntaa)
  idxbb=where(bb eq 1, cntbb)
  idxcc=where(cc eq 1, cntcc)
  print, total(dataSetFPA.data-(dataSetBRDF.data))
  print, total(dataSetFPA.data-(dataSetBRDF.data/2))
  print, total(cc-aa)
  print, total(cc-bb)
  print, cntaa
  print, cntbb
  print, cntcc
  stop

end

