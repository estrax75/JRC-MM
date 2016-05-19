FUNCTION computematchup_v3, flagData, angData, aotData, chlaData, rrs443Data,rrs490Data, rrs555Data, flagDataOriginal

  refMatchUp=n_elements(rrs443Data)
  
  ; y title plot settings

  cvInParCodes=['Rrs_443', 'Rrs_490', 'Rrs_555']
  trueFlagInNames=['Glint', 'HiSatZen', 'CloudIce', 'Glint_HiSatZen_CloudIce']
  ;trueFlagInCodes=['HIGLINT [3]', 'HISATZEN [5]', 'CLDICE [9]', 'Exclusive list']
  trueFlagInCodes=['G', 'H', 'C', 'GHC']
  trueFlagNo=n_elements(trueFlagInCodes)
  rrsCompositeNames='R!Drs!N!X(443),R!Drs!N!X(490),R!Drs!N!X(555)'
  
  leqSymbol='$\leq$ '
  geqSymbol='$\geq$ '
  neqSymbol='$\neq$ '
  andSymbol='&'
  
  flagPixelLimit=0
  if flagPixelLimit eq 0 then symbol='= ' else symbol=leqSymbol
  cvThresholds=[0.1, 0.2]
  thrInParNames=['Chla', '!9t!X!D!3a (865) !E!X!N', '!9a!X!N']
  thrInParCodes=['chlor_a', 'aot_865', 'angstrom']
  meanThresholds=[0.1,0.1,1.]
  thrInParNum=n_elements(thrInParCodes)

  title='Homogeneus Cases ('+rrsCompositeNames+':'
  title=title+' F!DE!N!X '+symbol+strcompress(flagPixelLimit, /REMOVE)+' '+andSymbol

  ;flag coding
  ;  0:ATMFAIL, 1:LAND, 2:BADANC, 3:HIGLINT, 4:HILT, 5:HISATZEN, 7:NEGLW
  ;  8:STRAYLIGHT, 9:CLDICE, 10: COCCOLITH, 12:HISOLZEN, 14:LOWLW, 15: CHLFAIL,
  ;  16: NAVWARN, 19: MAXAERITER, 21: CHLWARN, 22: ATMWARN, 25:NAVFAIL
  ;    IF ( flagv[0] GT 0 OR flagv[1] GT 0 OR flagv[3] GT 0 OR $
  ;         flagv[4] GT 0 OR flagv[5] GT 0 OR flagv[8] GT 0 OR $
  ;         flagv[9] GT 0 OR flagv[12] GT 0 OR flagv[16] GT 0 OR $
  ;         flagv[19] GT 0 OR flagv[22] GT 0 OR flagv[25] GT 0 ) THEN fstatus=-1
  ;
  
  flagPixelLimitText=strcompress(flagPixelLimit, /REMOVE)

  ;**1**
  matchUpDesc1='1) Only cut off flags condition'
  matchUpPlotTitle1=rrsCompositeNames+': '+' F!DE!N!X '+symbol+flagPixelLimitText
  matchUp1=0
  nonFlaggedCount=refMatchUp
  checkFlag=0
  if ptr_valid(flagData[0]) then begin
    exclusiveIndex=[0,1,3,4,5,8,9,12,16,19,22,25]
    checkFlag=0
    idx=where(*flagData[exclusiveIndex[0]] ne 0 or *flagData[exclusiveIndex[1]] ne 0 or *flagData[exclusiveIndex[2]] ne 0 or $
      *flagData[exclusiveIndex[3]] ne 0 or *flagData[exclusiveIndex[4]] ne 0 or *flagData[exclusiveIndex[5]] ne 0 or $
      *flagData[exclusiveIndex[6]] ne 0 or *flagData[exclusiveIndex[7]] ne 0 or *flagData[exclusiveIndex[8]] ne 0 or $
      *flagData[exclusiveIndex[9]] ne 0 or *flagData[exclusiveIndex[10]] ne 0 or *flagData[exclusiveIndex[11]] ne 0, countFlag)
    for i=0, n_elements(exclusiveIndex)-1 do checkFlag+=total(*flagData[exclusiveIndex[i]])
    nonFlaggedCount=refMatchUp-countFlag
  endif
  
  if ptr_valid(flagData[0]) then begin
    if checkFlag ne 0 then matchUp1=1
  endif
  
  a=moment(rrs443Data, mean=m443, sdev=stdev443)
  b=moment(rrs490Data, mean=m490, sdev=stdev490)
  c=moment(rrs555Data, mean=m555, sdev=stdev555)
  a=moment(angData, mean=mAng, sdev=stdevAng)
  b=moment(aotData, mean=mAot, sdev=stdevAot)
  c=moment(chlaData, mean=mChla, sdev=stdevChla)

  ;**2**
  thisCv=cvThresholds[0]
  thisCvText=string(format='(f3.1)', thisCv)
  
  matchUpDesc2a='2a)  *1* AND Cv of Rrs 443, 490, 555 < '+thisCvText
  matchUpPlotTitle2a='Homogeneus Cases ('+rrsCompositeNames+':'+' F!DE!N!X '+symbol+flagPixelLimitText+' '+andSymbol+' '+'C!Dv!N!X '+'< '+thisCvText+')'
  matchUp2a=matchUp1
  if matchUp2a eq 0 then begin
    cv443=stdev443/m443
    cv490=stdev490/m490
    cv555=stdev555/m555
    if cv443 le thisCv and cv490 le thisCv and cv555 le thisCv then matchUp2a=0 else matchUp2a=1 
  endif

  ;**3**
  thisCv=cvThresholds[1]
  thisCvText=string(format='(f3.1)', thisCv)

  matchUpDesc2b='2b)  *1* AND Cv of Rrs 443, 490, 555 < '+thisCvText
  matchUpPlotTitle2b='Homogeneus Cases ('+rrsCompositeNames+':'+' F!DE!N!X '+symbol+flagPixelLimitText+' '+andSymbol+' '+'C!Dv!N!X '+'< '+thisCvText+')'
  matchUp2b=matchUp1
  if matchUp2b eq 0 then begin
    cv443=stdev443/m443
    cv490=stdev490/m490
    cv555=stdev555/m555
    if cv443 le thisCv and cv490 le thisCv and cv555 le thisCv then matchUp2b=0 else matchUp2b=1
  endif
  
  ;**4**
  thisCv=cvThresholds[0]
  thisCvText=string(format='(f3.1)', thisCv)

  title3a=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title3a=title3a+' '+'QC!Ds!N!X '+')'
  ;for i=1, 3 do title3a=title3a+' '+thrInParNames[i]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[i])+')'

  matchUpDesc3a='3a)  *2a* and simultaneous thresholds checking of chla (<=0.1), angstrom(<=1.0) and aot(<=0.1)'
  matchUpPlotTitle3a=title3a
  matchUp3a=matchup2a
  if matchup3a eq 0 then begin
    if mAng le 1.0 and mAot le 0.1 and mChla le 0.1 then matchUp3a=0 else matchUp3a=1
  endif

  ;**5**
  thisCv=cvThresholds[1]
  thisCvText=string(format='(f3.1)', thisCv)
  
  title3b=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title3b=title3b+' '+'QC!Ds!N!X '+')'
  ;for i=1, 3 do title3b=title3b+' '+thrInParNames[i]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[i])+')'
  
  matchUpDesc3b='3b)  *2b* and simultaneous thresholds checking of chla (<=0.1), angstrom(<=1.0) and aot(<=0.1)'
  matchUpPlotTitle3b=title3b
  matchUp3b=matchup2b
  if matchup3b eq 0 then begin
    if mAng le 1.0 and mAot le 0.1 and mChla le 0.1 then matchUp3b=0 else matchUp3b=1
  endif

  thisCv=cvThresholds[0]
  thisCvText=string(format='(f3.1)', thisCv)

  ;**6**

  title4a=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title4a=title4a+' '+thrInParNames[0]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[0])+')'
  matchUpDesc4a='4a)  *2a* and threshold checking of chla (<=0.1)'
  matchUpPlotTitle4a=title4a
  matchUp4a=matchup2a

  if matchup4a eq 0 then begin
    if mChla le meanThresholds[0] then matchUp4a=0 else matchUp4a=1
  endif

  ;**7**

  thisCv=cvThresholds[1]
  thisCvText=string(format='(f3.1)', thisCv)

  title4b=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title4b=title4b+' '+thrInParNames[0]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[0])+')'

  matchUpDesc4b='4b)  *2b* and threshold checking of chla (<=0.1)'
  matchUpPlotTitle4b=title4b
  matchUp4b=matchup2b
  if matchup4b eq 0 then begin
    if mChla le meanThresholds[0] then matchUp4b=0 else matchUp4b=1
  endif

  ;**8**

  thisCv=cvThresholds[0]
  thisCvText=string(format='(f3.1)', thisCv)

  title5a=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title5a=title5a+' '+thrInParNames[1]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[1])+')'

  matchUpDesc5a='5a)  *2a* and threshold checking of angstrom (<=1.0)'
  matchUpPlotTitle5a=title5a
  matchUp5a=matchup2a
  if matchup5a eq 0 then begin
    if mAng le meanThresholds[2] then matchUp5a=0 else matchUp5a=1
  endif

  ;**9**

  thisCv=cvThresholds[1]
  thisCvText=string(format='(f3.1)', thisCv)

  title5b=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title5b=title5b+' '+thrInParNames[1]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[1])+')'

  matchUpDesc5b='5b)  *2b* and threshold checking of angstrom (<=1.0)'
  matchUpPlotTitle5b=title5b
  matchUp5b=matchup2b
  if matchup5b eq 0 then begin
    if mAng le meanThresholds[2] then matchUp5b=0 else matchUp5b=1
  endif

  ;**10**

  thisCv=cvThresholds[0]
  thisCvText=string(format='(f3.1)', thisCv)

  title6a=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title6a=title6a+' '+thrInParNames[2]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[2])+')'

  matchUpDesc6a='6a)  *2a* and threshold checking of aot(<=0.1)'
  matchUpPlotTitle6a=title6a
  matchUp6a=matchup2a
  if matchup6a eq 0 then begin
    if mAot le meanThresholds[1] then matchUp6a=0 else matchUp6a=1
  endif

  ;**11**

  thisCv=cvThresholds[1]
  thisCvText=string(format='(f3.1)', thisCv)

  title6b=title+' C!Dv!N!X '+'< '+thisCvText+' '+andSymbol
  title6b=title6b+' '+thrInParNames[2]+' '+leqSymbol+string(format='(f3.1)', meanThresholds[2])+')'

  matchUpDesc6b='6b)  *2b* and threshold checking of aot(<=0.1)'
  matchUpPlotTitle6b=title6b
  matchUp6b=matchup2b
  if matchup6b eq 0 then begin
    if mAot le meanThresholds[1] then matchUp6b=0 else matchUp6b=1
  endif

  ;flatStats=*(flagData[0])
  ;                    flagTimeSeriesData[k, 0, totalDayCount+duplicate]=flatStats[0]
  ;                    flagTimeSeriesData[k, 1, totalDayCount+duplicate]=flatStats[1]
  ;                    flagTimeSeriesData[k, 2, totalDayCount+duplicate]=flatStats[2]
  ;                    flagTimeSeriesData[k, 3, totalDayCount+duplicate]=flatStats[3]
  ;flagTimeSeriesData[k, 0, subscribeTotalDayCount]=flatStats[0] ; Glint
  ;flagTimeSeriesData[k, 1, subscribeTotalDayCount]=flatStats[1] ; HiSatZen
  ;flagTimeSeriesData[k, 2, subscribeTotalDayCount]=flatStats[2] ; IceCloud
  ;flagTimeSeriesData[k, 3, subscribeTotalDayCount]=flatStats[3] ; Total

  title7s=strarr(4)
  for i=0, n_elements(title7s)-1 do title7s[i]='Flagged Cases ('+trueFlagInNames[i]+': F!D'+trueFlagInCodes[i]+'!N!X '+neqSymbol+strcompress(flagPixelLimit, /REMOVE)+')'
  
  statRes=doStat(flagDataOriginal, 'specialFlag')
  flagStats=*(statRes.statValue)
  
  ;**12**

  matchUpDesc7a='7) Flag '+trueFlagInNames[0]
  matchUpPlotTitle7a=title7s[0]
  matchUp7a=flagStats[0] gt 0 ? 1 : 0

  ;**13**

  matchUpDesc7b='7) Flag '+trueFlagInNames[1]
  matchUpPlotTitle7b=title7s[1]
  matchUp7b=flagStats[1] gt 0 ? 1 : 0

  ;**14**

  matchUpDesc7c='7) Flag '+trueFlagInNames[2]
  matchUpPlotTitle7c=title7s[2]
  matchUp7c=flagStats[2] gt 0 ? 1 : 0

  ;**15**

  matchUpDesc7d='7) Flag '+trueFlagInNames[3]
  matchUpPlotTitle7d=title7s[3]
  matchUp7d=total([matchUp7a, matchUp7b, matchUp7c]) gt 0 ? 1 : 0

  return, {desc:[matchUpDesc1,matchUpDesc2a,matchUpDesc2b,matchUpDesc3a,matchUpDesc3b,matchUpDesc4a,matchUpDesc4b,$
      matchUpDesc5a,matchUpDesc5b,matchUpDesc6a,matchUpDesc6b,matchUpDesc7a,matchUpDesc7b,matchUpDesc7c,matchUpDesc7d], $
    plotTitles:[matchUpPlotTitle1,matchUpPlotTitle2a,matchUpPlotTitle2b,matchUpPlotTitle3a,matchUpPlotTitle3b,matchUpPlotTitle4a,matchUpPlotTitle4b,$
      matchUpPlotTitle5a,matchUpPlotTitle5b,matchUpPlotTitle6a,matchUpPlotTitle6b,matchUpPlotTitle7a,matchUpPlotTitle7b,matchUpPlotTitle7c,matchUpPlotTitle7d], $
    values:[matchUp1,matchUp2a,matchUp2b,matchUp3a,matchUp3b,matchUp4a,matchUp4b,matchUp5a,matchUp5b,matchUp6a,matchUp6b,$
      matchUp7a,matchUp7b,matchUp7c,matchUp7d], $
    nonFlaggedCount:nonFlaggedCount, $
    statDesc:['mean', 'stddev'], $
    angStat:[mAng, stdevAng], $
    aotStat:[mAot, stdevAot], $
    chlaStat:[mChla, stdevChla] $
    }
  
END