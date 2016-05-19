FUNCTION computematchup_v2, flagData, angData, aotData, chlaData, rrs443Data,rrs490Data, rrs555Data

  refMatchUp=n_elements(rrs443Data)
  
  ;flag coding
  ;  0:ATMFAIL, 1:LAND, 2:BADANC, 3:HIGLINT, 4:HILT, 5:HISATZEN, 7:NEGLW
  ;  8:STRAYLIGHT, 9:CLDICE, 10: COCCOLITH, 12:HISOLZEN, 14:LOWLW, 15: CHLFAIL,
  ;  16: NAVWARN, 19: MAXAERITER, 21: CHLWARN, 22: ATMWARN, 25:NAVFAIL
  ;    IF ( flagv[0] GT 0 OR flagv[1] GT 0 OR flagv[3] GT 0 OR $
  ;         flagv[4] GT 0 OR flagv[5] GT 0 OR flagv[8] GT 0 OR $
  ;         flagv[9] GT 0 OR flagv[12] GT 0 OR flagv[16] GT 0 OR $
  ;         flagv[19] GT 0 OR flagv[22] GT 0 OR flagv[25] GT 0 ) THEN fstatus=-1
  ;
  
  matchUpDesc1='1) Only cut off flags condition'
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
  
  matchUpDesc2='2)  *1* AND Cv of Rrs 443, 490, 555 < 0.2)'
  matchUp2=matchUp1
  if matchUp2 eq 0 then begin
    a=moment(rrs443Data, mean=m443, sdev=stdev443)
    b=moment(rrs490Data, mean=m490, sdev=stdev490)
    c=moment(rrs555Data, mean=m555, sdev=stdev555)
    cv443=stdev443/m443
    cv490=stdev490/m490
    cv555=stdev555/m555
    if cv443 le 0.2 and cv490 le 0.2 and cv555 le 0.2 then matchUp2=0 else matchUp2=1 
  endif
  
  matchUpDesc3='3)  *2* and simultaneous thresholds checking of chla (<=0.1), angstrom(<=1.0) and aot(<=0.1)'
  matchUp3=matchup2
  if matchup3 eq 0 then begin
    a=moment(angData, mean=mAng, sdev=stdevAng)
    b=moment(aotData, mean=mAot, sdev=stdevAot)
    c=moment(chlaData, mean=mChla, sdev=stdevChla)
    if mAng le 1.0 and mAot le 0.1 and mChla le 0.1 then matchUp3=0 else matchUp3=1
  endif

  matchUpDesc4='4)  *2* and threshold checking of chla (<=0.1)'
  matchUp4=matchup2
  if matchup4 eq 0 then begin
    a=moment(angData, mean=mAng, sdev=stdevAng)
    b=moment(aotData, mean=mAot, sdev=stdevAot)
    c=moment(chlaData, mean=mChla, sdev=stdevChla)
    if mChla le 0.1 then matchUp4=0 else matchUp4=1
  endif

  matchUpDesc5='5)  *2* and threshold checking of angstrom (<=1.0)'
  matchUp5=matchup2
  if matchup5 eq 0 then begin
    a=moment(angData, mean=mAng, sdev=stdevAng)
    b=moment(aotData, mean=mAot, sdev=stdevAot)
    c=moment(chlaData, mean=mChla, sdev=stdevChla)
    if mAng le 1.0 then matchUp5=0 else matchUp5=1
  endif

  matchUpDesc6='6)  *2* and threshold checking of aot(<=0.1)'
  matchUp6=matchup2
  if matchup6 eq 0 then begin
    a=moment(angData, mean=mAng, sdev=stdevAng)
    b=moment(aotData, mean=mAot, sdev=stdevAot)
    c=moment(chlaData, mean=mChla, sdev=stdevChla)
    if mAot le 0.1 then matchUp6=0 else matchUp6=1
  endif

  return, {desc:[matchUpDesc1,matchUpDesc2,matchUpDesc3,matchUpDesc4,matchUpDesc5,matchUpDesc6], values:[matchUp1,matchUp2,matchUp3,matchUp4,matchUp5,matchUp6], nonFlaggedCount:nonFlaggedCount}
  
END