function doStat, dataSet, statName, validCondition=validCondition, NAN=NAN, FOUND=FOUND

  ;validIdxs=where(finite(thisDataSet) eq 1 and thisDataSet ne 0 and thisDataSet lt 10000, count)
  FOUND=0
  if n_elements(validCondition) ne 1 then valCondition='finite(dataSet)' else valCondition=validCondition
  execRes=execute('validIdxs=where('+valCondition+', count)')
  statValue=!VALUES.F_NAN
  if count eq 0 then return, {statValue:!VALUES.F_NAN, count:count}
  myData=dataSet[validIdxs]
  doLog, count
  if count gt 0 then begin
    switch strlowcase(statName) of
      'stddev': begin
        ;statValue=moment(myData, /NAN)
        statValue=stddev(myData, /NAN, /DOUBLE)
        FOUND=1
        break
      end
      'mean': begin
        ;statValue=moment(myData, /NAN)
        statValue=total(myData, /NAN)/count
        FOUND=1
        break
      end
      'max': begin
        ;statValue=moment(myData, /NAN)
        statValue=max(myData, /NAN)
        FOUND=1
        break
      end
      'min': begin
        ;statValue=moment(myData, /NAN)
        statValue=min(myData, /NAN)
        FOUND=1
        break
      end
      'coeffvar': begin
        ;statValue=moment(myData, /NAN)
        temp=moment(myData, VARIANCE=mVariance, MEAN=mMean, /NAN, /DOUBLE)
        stdDev=sqrt(mVariance)
        statValue=stdDev/mMean
        doLog, 'mVariance:', mVariance, LEVEL=4
        doLog, 'stdDev:', stdDev, LEVEL=4
        doLog, 'mMean:', mMean, LEVEL=4
        doLog, 'statValue (stddev/mean):', statValue, LEVEL=4
        FOUND=1
        break
      end
    endswitch
  endif
  return, {statValue:statValue[0], count:count}
  
end
