; Main call
FUNCTION PPOperator::buildMaskConditions, parInfo, scallingType

  checkMaskConditions=''
  eps=self.app->getKeyValue('EPS')
  ignoreValue=self.app->getKeyValue('NAN_VALUE')
  maskExpr='(band eq '+ignoreValue+') or (band gt -'+eps+' and band lt '+eps+') or (finite(band) ne 1)'
  ;june 20th remove 0 as NaN
  ;maskExpr='(band eq '+ignoreValue+')'+' or (finite(band) ne 1)'
  
  checkMaskConditions[*]=maskExpr
  ;for i=0, length-1 do if strpos(outScallingTypeList[i], 'log') gt -1 then checkMaskConditions[i]='data eq -9999' else checkMaskConditions[i]='data le 0'
  if strpos(scallingType, 'log') gt -1 then checkMaskConditions=maskExpr else checkMaskConditions=maskExpr
  return, checkMaskConditions
  
END

FUNCTION PPOperator::buildRrsName, name

  ;splitting=strsplit(name, '_' , /EXTRACT)
  name='Rrs_'+name+'-Mean'
  return, name
  
END

;PRO PPOperator::doRatioStuff
;
;  COMMON rrsStuff, names
;  
;  rrsbandnames=''
;  rrsId='rrs_' & ratioId='ratio_'& ratioLogId='ratiolog10_'
;  rrsBands=self.app->getKeyValue('TEST_RRS_CHECK_EXPORT')
;  utils=obj_new('Utility')
;  rrsBands=utils->stringListToArray(rrsBands, separator=';', /STRING)
;  obj_destroy, utils
;  
;  for i=0, n_elements(rrsBands)-1 do begin
;    ratiocheck=strpos(rrsBands[i], ratioId)
;    ratiologcheck=strpos(rrsBands[i], ratioLogId)
;    if ratiocheck ne -1 then begin
;      resString=strmid(rrsBands[i],ratiocheck+strlen(ratioId), strlen(rrsBands[i])-strlen(ratioId) )
;      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
;      bandName=self->buildRrsName(bandNames[0])
;      band1=self->getBand(bandName, /SETNAN)
;      bandName=self->buildRrsName(bandNames[1])
;      band2=self->getBand(bandName, /SETNAN)
;      validIdxs=where(finite(band1) and finite(band2), count)
;      if count gt 0 then begin
;        ratiologValues=band1
;        ratiologValues[*,*]=!VALUES.F_NAN
;        ratiologValues[validIdxs]=alog(band1[validIdxs]/band2[validIdxs])
;        self->addBand, ratiologValues, resString, /OVERWRITE
;        rrsbandnames=[rrsbandnames, resString]
;      endif
;      continue
;    endif
;    if ratiologcheck ne -1 then begin
;      resString=strmid(rrsBands[i],ratiologcheck+strlen(ratiologId), strlen(rrsBands[i])-strlen(ratiologcheck)+1 )
;      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
;      bandNames=strsplit(resString, '_', /EXTRACT, /PRESERVE)
;      bandName=self->buildRrsName(bandNames[0])
;      band1=self->getBand(bandName, /SETNAN)
;      bandName=self->buildRrsName(bandNames[1])
;      band2=self->getBand(bandName, /SETNAN)
;      validIdxs=where(finite(band1) and finite(band2), count)
;      if count gt 0 then begin
;        ratiologValues=band1
;        ratiologValues[*,*]=ignoreValue
;        ratiologValues[validIdxs]=alog(band1[validIdxs]/band2[validIdxs])
;        self->addBand, ratiologValues, resString, /OVERWRITE
;        rrsbandnames=[rrsbandnames, resString]
;      endif
;    endif
;    continue
;  endfor
;  
;END

FUNCTION PPOperator::doComputation, archiveDir, year, month, COPY=COPY, fileName=fileName, OPEN=OPEN

  ;'chl', 'sst', 'dl', 'par', 'z', 'kpar'
  if n_elements(fileName) then self->setMainFileName, filename, COPY=COPY, OPEN=OPEN
  self->updateFid
  envi_file_query, self.mainFid, bnames=bnames
  
  ;parInfo=self.app->getParameterByOutputBandName('chl_a-Mean')
  
  physicals=self.app->getPhysicalFromYear(year)
  physical=physicals[0]
  
  chlCode=physical->getParameterCodeChl()
  
  parInfo=self.app->getParameterByCode(chlCode)
  chl=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('sst')
  parInfo=self.app->getParameterByCode('sst')
  sst=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByCode('dl')
  parInfo=self.app->getParameterByOutputBandName('dl')
  dl=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('sza')
  parInfo=self.app->getParameterByCode('sza')
  sza=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('daacPar')
  parInfo=self.app->getParameterByCode('daacPar')
  par=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('a_490_qaa-Mean')
  parInfo=self.app->getParameterByCode('a_490_qaa-Mean')
  a=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('bbp_490_qaa-Mean')
  parInfo=self.app->getParameterByCode('bbp_490_qaa-Mean')
  bbp=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;parInfo=self.app->getParameterByOutputBandName('Zeu_lee-Mean')
  parInfo=self.app->getParameterByCode('Zeu_lee-Mean')
  kpar=self->getBand(parInfo.outputBandName, /SETNAN)
  
  ;doLeeKVis method creates bands {'kvis', 'zeu', 'kvis_mean'}
  res=self->doLeeKVis(a, bbp, sza)
  
  parInfo=self.app->getParameterByCode('kvis')
  z=self->getBand(parInfo.outputBandName, /SETNAN)
  ;doA_c2chl method creates band {'A_c2chl'}
  
  res=self->doAc2chl(chl, sst, dl, par, z, kpar)
  ;doA_c2chl method creates band {'A_c2chl', 'Omega'}
  omegaIstar = self->doOmega(par,1);
  parInfo=self.app->getParameterByCode('AC2chl')
  aC2chl=self->getBand(parInfo.outputBandName, /SETNAN)
  
  parInfo=self.app->getParameterByCode('omega')
  omega=self->getBand(parInfo.outputBandName, /SETNAN)
  
  PPc2chlInfKvis=self->doPPInfinity(aC2chl, omega)
  conv_factor = 0.001;
  PPc2chlInfKvis = PPc2chlInfKvis * conv_factor;
  
  parInfo=self.app->getParameterByCode('PP')
  self->addBand, PPc2chlInfKvis, parInfo.outputBandName, /OVERWRITE
  
END

; 1.
FUNCTION PPOperator::doLeeKvis, a, bbp, sza
  ;% Calculate kvis at euphotic depth from a & bbp
  ;%
  ;% Input:
  ;%  a        - double(samples,wavelengths)   - absorption
  ;%  bbp      - double(samples,wavelengths)   - backscattering
  ;%  i490     - double                        - index of 490nm wavelength (2nd dimension of a & bbp)
  ;%  sza      - double(samples)               - sun zenith angle
  ;%
  ;% Returns:
  ;%  kvis     - double(samples)           - kvis at euphotic depth
  ;%  zeu      - double(samples)           - Euphotic depth
  ;%  kvis_mean- double(samples)           - Mean of kvis from surface to euphotic depth

  K1 = self->doLeeK1Sza(a, bbp, sza);
  K2 = self->doLeeK2Sza(a, bbp, sza);
  
  ;[kvis, zeu, kvis_mean] = lee_Kvis_zeu(K1, K2);
  return, self->doLeeKvisZeu(K1, K2)
  
END

; 2.
FUNCTION PPOperator::doLeeK1Sza, a490, bb490, sza
  ;% Calculate K1 using sun zenith angle
  ;%
  ;% Input:
  ;%   a490    - double(samples) - absorption at 490nm
  ;%   bb490   - double(samples) - backscattering at 490nm
  ;%   sza     - double (samples) - Sun Zenith Angle in radians
  ;%
  ;% Output:
  ;%   leeK1sza - double(samples) - K1

  a0 = 0.090;
  
  return, self->doLeeK1(a490, bb490) * (1 + a0 * sin(sza));
  
END

; 3.
FUNCTION PPOperator::doLeeK1, a490, bb490
  ;% Calculate K1
  ;% Input:
  ;%   a490    - double(samples) - absorption at 490nm
  ;%   bb490   - double(samples) - backscattering at 490nm
  ;%
  ;% Output:
  ;%   leeK1   - double(samples) - K1

  x = [-0.057, 0.482, 4.221];
  
  return, x[0] + x[1] * a490^0.5 + x[2] * bb490;
  
END

; 4.
FUNCTION PPOperator::doLeeK2Sza, a490, bb490, sza
  ;% Calculate K1 using sun zenith angle
  ;%
  ;% Input:
  ;%   a490    - double(samples) - absorption at 490nm
  ;%   bb490   - double(samples) - backscattering at 490nm
  ;%   sza     - double (samples) - Sun Zenith Angle in radians
  ;%
  ;% Output:
  ;%   leeK2sza - double(samples) - K2


  a1 = 1.465;
  a2 = -0.667;
  
  return, self->doLeeK2(a490, bb490) * (a1 + a2 * cos(sza));
  
END

; 5.
FUNCTION PPOperator::doLeeK2, a490, bb490
  ;% Calculate K1 using sun zenith angle
  ;%
  ;% Input:
  ;%   a490    - double(samples) - absorption at 490nm
  ;%   bb490   - double(samples) - backscattering at 490nm
  ;%   sza     - double (samples) - Sun Zenith Angle in radians
  ;%
  ;% Output:
  ;%   leeK2sza - double(samples) - K1

  s = [0.183, 0.702, -2.567];
  
  return, s[0]  + s[1] * a490 + s[2] * bb490;
  
END

; 6.
FUNCTION PPOperator::doLeeKvisZeu, K1, K2, dz=dz, z_min=z_min, z_max=z_max
  ;% Calculate kvis at intervals of 0.2 metres from 0 to 150m,
  ;% looking for euphotic depth (where kvis .* depth is closest to 4.60517)
  ;%
  ;% Input:
  ;%   K1 - double(samples) - K1
  ;%   K2 - double(samples) - K2
  ;%
  ;% Output:
  ;%   kvis        - double(samples)   - Kvis at euphotic depth
  ;%   zeu         - double(samples)   - euphotic depth
  ;%   kvis_mean   - double(samples)   - mean of kvis from surface to euphotic depth

  ;% Find kvis at 20cm intervals till 150m
  if n_elements(dz) eq 0 then dz = 0.2;    % depth step
  if n_elements(z_min) eq 0 then z_min = 0;   % min depth
  if n_elements(z_max) eq 0 then z_max = 150; % max depth
  z_nb = 1 + (z_max-z_min) / dz; % Number of depth steps
  
  ;% Initialise results
  ;self->getMask()
  dims=self->getDataDimensions()
  
  dims=[dims[2]+1, dims[4]+1]
  zeu = fltarr(dims[0], dims[1]) * !VALUES.F_NAN;
  kvis = fltarr(dims[0], dims[1]) * !VALUES.F_NAN;
  kvis_mean = fltarr(dims[0], dims[1]);
  
  ;% Find euphotic depth
  ;% by finding where ratio of irradiance at surface to irradiance at depth z is 0.01 or
  ;% in other words, kvis_z * z = 4.6
  
  ;% Initialise current minimum of kvix * z to maximum
  curr_min = fltarr(dims[0], dims[1]) + 99999999;
  ;%h = waitbar(0,sprintf('Calculating kvis from %0u to %u at steps of %0.1f',z_min,z_max,dz));
  ;zList=findgen(((float(z_max)-z_min)/dz)+1)*.2+z_min
  zList=findgen(z_nb)*dz+z_min
  ;  for z = z_min, z_max, dz do begin
  for zIdx = 0, z_nb-1 do begin
    ;%  waitbar(z/z_max);
    ;kvis_z = lee_Kvis_z(K1,K2,z);
    kvis_z = self->doLeeKvisZ(K1,K2,zList[zIdx]);
    kvis_mean = kvis_mean + kvis_z;
    ;new_min = abs((kvis_z * z - 4.60517));
    new_min = abs((kvis_z * zList[zIdx] - 4.60517));
    ;inew_min = new_min < curr_min;
    inew_min = where(new_min lt curr_min, count);
    ;kvis(inew_min) = kvis_z(inew_min);
    if count ne 0 then begin
      kvis[inew_min] = kvis_z[inew_min];
      ;zeu[inew_min] = z;
      zeu[inew_min] = zList[zIdx];
      curr_min[inew_min] = new_min[inew_min];
    endif
  endfor
  
  kvis_mean = kvis_mean / z_nb;
  ;only for test
  ;kvis_mean[*]=.2
  
  parInfo=self.app->getParameterByCode('kvis')
  self->addBand, kvis, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('zeu')
  self->addBand, zeu, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('kvis_mean')
  self->addBand, kvis_mean, parInfo.outputBandName, /OVERWRITE
  ;%close(h);
  return, 1
  
END

; Order of calling: 7.
FUNCTION PPOperator::doLeeKvisZ, K1, K2, z
  ;% Calculate kvis at depth z
  ;%
  ;% Input:
  ;%   K1 - double(samples) - K1
  ;%   K2 - double(samples) - K2
  ;%   z   - double(samples) - Depth to which to calculate PP
  ;%
  ;% Output:
  ;%   kvis - double(samples) - Kvis

  kvis = K1 + K2 / (1 + z)^0.5;
  return, kvis
  
END

; 8.
FUNCTION PPOperator::doAc2chl, chl, sst, dl, par, z, kpar
  ;  % Calculations for pp for given depth (z)
  ;  %
  ;  % Input:
  ;  %   chl - double(samples) - Chlorophyll  (mg m^-3)
  ;  %   sst - double(samples) - Sea Suface Temperature (degC)
  ;  %   dl  - double(samples) - Day Length (hours)
  ;  %   par - double(samples) - PAR (Einstein m^-2 Day)
  ;  %   z   - double(samples) - Depth to which to calculate PP (m)
  ;  %   kpar- double(samples) - Kpar (m-1)
  ;  %
  ;  % Output:
  ;  %   A_c2chl  - double(samples) -
  ;  %   omega    - double(samples) -
  ;  %   pmax_c2chl - double(samples) -
  ;  %   c2chl - double(samples) -
  ;  no3 = no3_global(sst,chl);
  ;
  ;  kH = kpar * z;
  ;
  ;  c2chl = c2chl_ratio(no3, sst, par, kH, month=month, year=year, outputdir=outputdir, euro_map=euro_map, ix_good_euro=ix_good_euro);
  ;
  ;  pmax_c2chl = pmax_c2chl_func(sst, c2chl);
  ;
  ;  A_c2chl = chl * pmax_c2chl * dl / kpar;
  ;
  ;  omega = omega_func(par,exp(-kH));
  ;
  ;  return, {A_c2chl:A_c2chl, omega:omega, pmax_c2chl:pmax_c2chl, c2chl:c2chl}
  no3 = self->doNo3Global(sst,chl);
  
  kH = kpar * z;
  
  c2chl = self->doC2chlRatio(no3, sst, par, kH);
  
  pmax_c2chl = self->doPmaxC2chl(sst, c2chl);
  
  AC2chl = chl * pmax_c2chl * dl / kpar;
  
  omega = self->doOmega(par,exp(-kH));
  
  parInfo=self.app->getParameterByCode('AC2chl')
  self->addBand, AC2chl, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('omega')
  self->addBand, omega, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('pmax_c2chl')
  self->addBand, pmax_c2chl, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('c2chl')
  self->addBand, c2chl, parInfo.outputBandName, /OVERWRITE
  
END

; 9
FUNCTION PPOperator::doNo3Global, sst, chl
  ;% Calculate Nitrate
  ;%
  ;% Input:
  ;%   sst - double(samples) - Sea Suface Temperature
  ;%   chl - double(samples) - Chlorophyll
  ;%
  ;% Output:
  ;%   no3 - double(samples) - Nitrate

  ;% NO3 all latitudes
  no3 = 25.22 - (1.96 * sst) + (0.04 * sst^2) - (1.21 * chl) + (0.05 * chl^2);
  no3 = self->doNo3Limit(no3);
  return, no3
  
END

; 10
FUNCTION PPOperator::doNo3Limit, no3
  ;% function to limit no3
  ;%
  ;% Input:
  ;%   no3 - double(samples) - Nitrate
  ;%
  ;% Output:
  ;%   no3 - double(samples) - Nitrate limited to .01

  ;no3(no3 < .01 & no3 ~= 0.0) = .01;
  idxs=where(no3 lt .01 and no3 ne 0.0, count)
  if count ne 0 then no3[idxs]=.01
  return, no3
  
end

; 11
FUNCTION PPOperator::doC2ChlRatio, no3, sst, par, kH

  ;% Calculations for pp for given depth (z)
  ;%
  ;% Input:
  ;%   no3 - double(samples) - Nitrate limited to .01
  ;%   sst - double(samples) - Sea Suface Temperature (degC)
  ;%   par - double(samples) - PAR (Einstein m^-2 Day)
  ;%   kH   - double(samples) -
  ;%
  ;% Output:
  ;%   c2chl - double(samples) -
  ;%
  ;% Modifications:
  ;%
  ;% Mod1 24/10/2008 J.Challis
  ;% Kn calculation should be no3 limited to 0.01 <Kn<1.0
  ;% Mod1
  ;%Kn = 1.0 < no3 > 0.01;

  Kn = no3;
  idxs=where(no3 lt 0.1 and no3 ne 0., count)
  if count ne 0 then Kn[idxs]=0.1
  
  idxs=where(no3 gt 1, count)
  if count ne 0 then Kn[idxs]=1
  
  C1=exp(0.05 * sst);
  
  idxs = where(kH gt 0, count, ncomplement=complcount);
  if complcount gt 0 then doLog, systime()+' _c2chl_ratio : pixels have kH <= 0', level=1
  
  C2 = no3*!VALUES.F_NAN;
  
  C2[idxs]=exp(-0.059 * (par[idxs] / kH[idxs]) * (1-exp(-kH[idxs])));
  
  c2chl=1. / (0.003 + 0.0154 * C1 * C2 * no3 / (Kn + no3));
  
  parInfo=self.app->getParameterByCode('c2')
  self->addBand, C2, parInfo.outputBandName, /OVERWRITE
  parInfo=self.app->getParameterByCode('kH')
  self->addBand, kh, parInfo.outputBandName, / OVERWRITE
  
  return, c2chl
  
END

;12
FUNCTION PPOperator::doPmaxC2chl, sst, c2chl
  ;  %
  ;  % Input:
  ;  %   sst - double(samples) - Sea Suface Temperature (degC)
  ;  %   c2chl - double(samples) -
  ;  %
  ;  % Output:
  ;  %   pmax_c2chl - double(samples) -

  mumax = 0.97 * exp(0.0633 * sst);
  
  return, mumax * c2chl / 24.0;
  
END

; 13 & 16
FUNCTION PPOperator::doOmega, par, coeff
  ;  %
  ;  % Input:
  ;  %   par - double(samples) - PAR
  ;  %   coeff - double - Coefficient 1: calculate to infinity else exp(-kH)
  ;  %
  ;  % Output:
  ;  %   omega - double(samples) -

  polyc = self->doPolyCalc(par,coeff);
  ;omega = sum(polyc,2);
  ;check this...
  omega = total(polyc,3);
  return, omega
  
end

;14
FUNCTION PPOperator::doPolyCalc, par, coeff
  ;          %
  ;          % Input:
  ;          %   par - double(samples) - PAR
  ;          %   coeff - double - Coefficient 1: calculate to infinity else exp(-kH)
  ;          %
  ;          % Output:
  ;          %   omega - double(samples) -

  omega=[6.1035E-1, -8.9251E-2, 8.1477E-3, -3.7427E-4, 6.6103E-6];
  ;check now array is 2-D!!
  omegaNo = n_elements(omega);
  ;sample_nb = n_elements(par);
  dims=self->getDataDimensions()
  
  dims=[dims[2]+1, dims[4]+1]
  
  ;% Initialise array for results
  ;polyn = zeros(sample_nb,omega_nb);
  polyn= make_array(dims[0], dims[1], omegaNo, /FLOAT)
  ;polyn = fltarr(sample_nb,omega_nb);
  parCoeff = self->doIstar(par) * coeff;
  
  for i = 0, omegaNo-1 do polyn[*,*,i] = omega[i] * parCoeff ^ i;
  return, polyn
  
END

;15
FUNCTION PPOperator::doIstar, par
  ;     %
  ;     % Input:
  ;     %   par - double(samples) - PAR (Einstein m^-2 Day)
  ;     %
  ;     % Output:
  ;     %   istar - double(samples) -

  return, 0.2 * par;
  
END

;17
FUNCTION PPOperator::doPPInfinity, aC2chlZeu, omegaIstar
  ;  % Calculate Primary Production to infinity
  ;  %
  ;  % Input:
  ;  %   A_c2chl_zeu double(samples) - A_c2chl calculated at euphotic depth
  ;  %   omega_istar - double(samples) -
  ;  %
  ;  % Output:
  ;  %   PP_c2chl_inf double(samples) - Primary Production to Infinity

  return, aC2chlZeu * omegaIstar
  
END

;PRO PPOperator::writeResult, month, year, archiveDir, overwriteFlag=overwriteFlag
;
;  prevLog=self.log
;  self.log=1
;  self->writeAsNCDF, month, year, archiveDir, overwriteFlag=overwriteFlag
;  self.log=prevLog
;
;END

PRO PPOperator::cleanMainFile

  parInfo=self.app->getParameterByCode('c2')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('kH')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('kvis')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('zeu')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('kvis_mean')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('AC2chl')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('omega')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('pmax_c2chl')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('c2chl')
  self->removeBand, parInfo.outputBandName
  parInfo=self.app->getParameterByCode('PP')
  self->removeBand, parInfo.outputBandName
  
END

FUNCTION PPOperator::buildOperatorResultFileName, dType, displayName, month, year

  ;return, buildPPVarFileName(dType, displayName, month, year)
  fileName=call_function('buildPPVarFileName'+'_'+dType, $
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
  return, fileName
;return, buildPPVarFileName(dType, displayName, month, year)
  
END

;FUNCTION PPOperator::getFileNameToExport, month, year, archiveDir, parInfo=parInfo, NONE=NONE
;
;  dType='M'
;  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
;  NONE=0
;
;  parInfos=self->getBandToExportInfoList()
;  ncdfFilenames=strarr(n_elements(parInfos))
;  for i=0, n_elements(parInfos)-1 do begin
;    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year)
;  endfor
;  if ~keyword_set(overwriteFlag) then begin
;    ncdfFilenames=self->checkResultFileExistence(ncdfFilenames, archiveDir, idxs=idxs, NONE=NONE)
;    if ~keyword_set(NONE) then parInfos=parInfos[idxs]
;  endif
;  return, ncdfFilenames
;
;END

PRO PPOperator::CleanUp

  self-> GenericOperator::Cleanup
  
END

FUNCTION PPOperator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, $
    OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList)) then return, 0
  return, 1
  
END

PRO PPOperator__Define

  Struct = { PPOperator , $
    Inherits GenericOperator $
    }
    
END