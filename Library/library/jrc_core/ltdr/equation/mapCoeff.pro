function mapCoeff, inputFile, NOAACode, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;NoaaCodes=['07', '09', '11', '14', '16']
  
  ;; bs wrong
  ;bareSoilsAs=[0.58294, 0.58374, 0.73712, 0.75176, 0.58489]
  ;bareSoilsBs=[0.84146, 0.84175, 0.84904, 0.85528, 0.84228]
  fileCoeff=ST_fileSystem->buildSimpleMapEntryFromFile(inputFile)

;  noaaCodeType=size(NOAACode, /TYPE)
;  if noaaCodeType ge 2 and noaaCodeType le 5 then testNoaa=string(NOAACode[0], format='(I02)')
;  if noaaCodeType eq 7 then testNoaa=NOAACode
;
;  check=where(testNoaa eq NoaaCodes, count)
;  if count lt 1 then message, 'wrong noaa code: '+testNoaa
;  if count gt 1 then check=check[MISSIONOVERLAPINDEX]

  coeff={bareSoilsAs:!VALUES.D_NAN, bareSoilsBs:!VALUES.D_NAN, $
    baresoilRhoicBand1:!VALUES.D_NAN, baresoilRhoicBand2:!VALUES.D_NAN, $
    baresoilKiBand1:!VALUES.D_NAN, baresoilKiBand2:!VALUES.D_NAN, $
    baresoilThetahgBand1:!VALUES.D_NAN, baresoilThetahgBand2:!VALUES.D_NAN, $
    RhoicBand1:!VALUES.D_NAN, RhoicBand2:!VALUES.D_NAN, $
    KiBand1:!VALUES.D_NAN, KiBand2:!VALUES.D_NAN, $
    ThetahgBand1:!VALUES.D_NAN, ThetahgBand2:!VALUES.D_NAN, $
    g0Coeff1:!VALUES.D_NAN, g0Coeff2:!VALUES.D_NAN, g0Coeff3:!VALUES.D_NAN, g0Coeff4:!VALUES.D_NAN, g0Coeff5:!VALUES.D_NAN, g0Coeff6:!VALUES.D_NAN $
  }
  tags=tag_names(coeff)
  tagsNo=n_elements(tags)
  for i=0, tagsNo-1 do begin
    idx=where(tags[i] eq strupcase(fileCoeff.id), count)
    if count eq 1 then coeff.(i)=double(fileCoeff[idx].value)
  endfor

  return, coeff

end