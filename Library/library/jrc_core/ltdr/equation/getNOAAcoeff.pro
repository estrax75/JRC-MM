function getNOAAcoeff, NOAACode

  NoaaCodes=['07', '09', '11', '14', '16']

  ;; bs wrong
  ;bareSoilsAs=[0.58294, 0.58374, 0.73712, 0.75176, 0.58489]
  ;bareSoilsBs=[0.84146, 0.84175, 0.84904, 0.85528, 0.84228]
  bareSoilsAs=[1.32632, 1.31409, 1.34178, 1.34250, 1.31520]
  bareSoilsBs=[0.0141544, 0.0140947, 0.008644, 0.0100730, 0.0156363]

  baresoilRhoicBand1=[0.58294, 0.58374, 0.73712, 0.75176, 0.58489]
  baresoilRhoicBand2=[0.60515, 0.60543, 0.76222, 0.76353, 0.60614]

  baresoilKiBand1=[0.84146, 0.84175, 0.84904, 0.85528, 0.84228]
  baresoilKiBand2=[0.84459, 0.84450, 0.85771, 0.85630, 0.84467]

  baresoilThetahgBand1=[-0.06983, -0.06956, -0.05469, -0.05599, -0.06947]
  baresoilThetahgBand2=[-0.06227, -0.06233, -0.05058, -0.05020, -0.06209]
  ;;
  ;rho
  RhoicBand1=[0.60006, 0.59477, 0.59595, 0.58979, 0.61133]
  RhoicBand2=[0.72635, 0.72805, 0.73884, 0.75070, 0.73745]

  ;k
  KiBand1=[0.96317, 0.95708, 0.95879, 0.95283, 0.97763]
  KiBand2=[0.84130, 0.84128, 0.84220, 0.84381, 0.84211]

  ;theta
  ThetahgBand1=[-0.06168, -0.06026, -0.06053, -0.05880, -0.06397]
  ThetahgBand2=[-0.02754, -0.02742, -0.02655, -0.02551, -0.02665]

  g0Coeff1=[0.22737, 0.23521, 0.23492, 0.25088, 0.26676]
  g0Coeff2=[0.28904,0.29624, 0.29298, 0.31514, 0.33483]
  g0Coeff3=[0.0022195,0.0022730, 0.0025478, 0.0032777, 0.002950]
  g0Coeff4=[-0.22274,-0.22633, -0.24116, -0.29402, -0.29184]
  g0Coeff5=[0.26722,0.26999, 0.27107, 0.28744, 0.27581]
  g0Coeff6=[0.0066666, 0.0070926, -0.00030251, -0.022702, -0.011802]

  noaaCodeType=size(NOAACode, /TYPE)
  if noaaCodeType ge 2 and noaaCodeType le 5 then testNoaa=string(NOAACode[0], format='(I02)')
  if noaaCodeType eq 7 then testNoaa=NOAACode

  check=where(testNoaa eq NoaaCodes, count)
  if count ne 1 then message, 'wrong noaa code: '+testNoaa

  res={bareSoilsAs:bareSoilsAs[check], bareSoilsBs:bareSoilsBs[check], $
    baresoilRhoicBand1:baresoilRhoicBand1[check], baresoilRhoicBand2:baresoilRhoicBand2[check], $
    baresoilKiBand1:baresoilKiBand1[check], baresoilKiBand2:baresoilKiBand2[check], $
    baresoilThetahgBand1:baresoilThetahgBand1[check], baresoilThetahgBand2:baresoilThetahgBand2[check], $
    RhoicBand1:RhoicBand1[check], RhoicBand2:RhoicBand2[check], $
    KiBand1:KiBand1[check], KiBand2:KiBand2[check], $
    ThetahgBand1:ThetahgBand1[check], ThetahgBand2:ThetahgBand2[check], $
    g0Coeff1:g0Coeff1[check], g0Coeff2:g0Coeff2[check], g0Coeff3:g0Coeff3[check], g0Coeff4:g0Coeff4[check], g0Coeff5:g0Coeff5[check], g0Coeff6:g0Coeff6[check] $
  }

  return, res

end