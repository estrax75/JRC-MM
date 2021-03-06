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
      'specialflag': begin
        ; exclusive cut off flags list
        exclusiveIndex=[0,1,3,4,5,8,9,12,14,15,16,19,21,22,25,26]
        ;revised by FM and GZ 9th march 2016
        ;flagv[0] GT 0 OR flagv[1] GT 0 OR flagv[3] GT 0 OR $
        ;  flagv[4] GT 0 OR flagv[5] GT 0 OR flagv[8] GT 0 OR $
        ;  flagv[9] GT 0 OR flagv[12] GT 0 OR flagv[14] GT 0 OR flagv[15] GT 0 OR flagv[16] GT 0 OR $
        ;  flagv[19] GT 0 OR flagv[21] GT 0 OR flagv[22] GT 0 OR flagv[25] GT 0 OR flagv[26] GT 0 ) THEN BEGIN
        ;  HIGLINT             8    3
        ;  CLDICE            512    9
        glintIndex=3
        hiSatZenIndex=5
        IceCloudindex=9
        checkFlag=0
        flagToBit=flagIntToBin(dataSet, NOFLAG=NOFLAG)
        statValue=intarr(4)
        if ~keyword_set(NOFLAG) then begin
          flagsTotal=intarr(n_elements(flagToBit))
          for m=0, n_elements(flagToBit)-1 do flagsTotal[m]=total(*(flagToBit[m]))
          glintIdxs=where(*flagToBit[glintIndex] ne 0, countGlintFlag)
          hiSatZenidxs=where(*flagToBit[hiSatZenIndex] ne 0, counthiSatZen)
          IceCloudidxs=where(*flagToBit[IceCloudindex] ne 0, countIceCloudFlag)
          ;        excludeIdxs=where(*flagToBit[exclusiveIndex[0]] ne 0 or *flagToBit[exclusiveIndex[1]] ne 0 or *flagToBit[exclusiveIndex[2]] ne 0 or $
          ;          *flagToBit[exclusiveIndex[3]] ne 0 or *flagToBit[exclusiveIndex[4]] ne 0 or *flagToBit[exclusiveIndex[5]] ne 0 or $
          ;          *flagToBit[exclusiveIndex[6]] ne 0 or *flagToBit[exclusiveIndex[7]] ne 0 or *flagToBit[exclusiveIndex[8]] ne 0 or $
          ;          *flagToBit[exclusiveIndex[9]] ne 0 or *flagflagToBitData[exclusiveIndex[10]] ne 0 or *flagToBit[exclusiveIndex[11]] ne 0, countExclude)
          fTBTotal=*flagToBit[0]
          fTBTotal[*]=0
          fTBGlint=fTBTotal & fTBHiSatZen=fTBTotal & fTBIceCloud=fTBTotal
          for i=0, n_elements(exclusiveIndex)-1 do begin
            thisIndex=exclusiveIndex[i]
            thisfTB=*flagToBit[thisIndex]
            excludeIdxs=where(thisfTB, partialCount)
            if partialCount gt 0 then begin
              fTBTotal[excludeIdxs]=fTBTotal[excludeIdxs]+1
              if glintIndex eq thisIndex then fTBGlint[*]=fTBGlint[*]+1
              if hiSatZenIndex eq thisIndex then fTBHiSatZen[*]=fTBHiSatZen[*]+1
              if IceCloudindex eq thisIndex then fTBIceCloud[*]=fTBIceCloud[*]+1
            endif
          endfor
          statValue[0]=total(fTBGlint)
          statValue[1]=total(fTBHiSatZen)
          statValue[2]=total(fTBIceCloud)
          statValue[3]=total(fTBTotal)
          ;for i=0, n_elements(exclusiveIndex)-1 do checkFlag+=total(*flagData[exclusiveIndex[i]])
        endif
        statValue=ptr_new(statValue, /NO_COPY)
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
