function filterBadValues, dataSet, METHODNAME=METHODNAME, REPORT=REPORT

  COMMON smurffCB, mainApp

  if n_elements(METHODNAME) ne 1 then METHODNAME='STDDEV'
  if obj_valid(mainApp) then sigmaFactor=float(mainApp->getKeyValue('FILTER_SIGMA_FACTOR')) else sigmaFactor=3. 
  
  res=moment(dataSet, MEAN=filterMean, SDEV=filterStdDev, MAXMOMENT=2, /NAN)
  
  idx=where(dataSet ge filterMean-sigmaFactor*filterStdDev and dataSet le filterMean+sigmaFactor*filterStdDev, nCount, complement=discardIndexes, NCOMPLEMENT=discardCount)
  idx2=where(finite(dataSet), validCount, NCOMPLEMENT=NanCount)
  report.expected=n_elements(dataSet)
  
  if n_elements(REPORT) eq 0 then REPORT=getInvalidStruct()
  report.valid_count=validCount
  report.invalid_count=NanCount
  report.sigma_filter=discardCount-NanCount
  if discardCount ne NanCount then begin
    if discardCount ne 0 then begin
      ;report.valid_count=validCount
      ;report.invalid_count=NanCount
      ;report.sigma_filter=discardCount-NanCount
      ; Now (January, 11th 2016): take no Action
      ;dataSet[discardIndexes]=!VALUES.F_NAN
    endif
  endif
  return, dataSet
  
end
