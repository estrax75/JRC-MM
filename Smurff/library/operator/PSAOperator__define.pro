PRO PSAOperator::prepareToExport

  bandToExports=self->getBandToExportInfoList()
  excludList=strupcase(['oxysat','psa','oxyrisk_adv','oxyrisk_no_adv'])
  for i=0, n_elements(bandToExports)-1 do begin
    bName=bandToExports[i].bandname
    idx=where(strupcase(bName) eq excludList, count)
    if count eq 0 then self->addBand, self->getBand(bName, /SETNAN), bName, /OVERWRITE
  endfor 

END

FUNCTION PSAOperator::doComputation, archiveDir, year, month, COPY=COPY, OPEN=OPEN, filename=filename


  ;'chl', 'sst', 'dl', 'par', 'z', 'kpar'
  maxDepth=float(self.app->getKeyValue('MAX_DEPTH'))
  if n_elements(filename) eq 1 then self->setMainFileName, filename, COPY=COPY, OPEN=OPEN
  ;self->setMainFileName, filename, COPY=COPY, OPEN=OPEN
  ;self->updateFid
  
  parInfo=self.app->getParameterByOutputBandName('umx')
  ;umx=self->getBand(parInfo.outputBandName)
  umxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vmx')
  ;vmx=self->getBand(parInfo.outputBandName)
  vmxCode=parInfo.outputBandName

  parInfo=self.app->getParameterByOutputBandName('smx')
  ;vmx=self->getBand(parInfo.outputBandName)
  smxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('ubot')
  ;ubot=self->getBand(parInfo.outputBandName)
  ubotCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vbot')
  ;vbot=self->getBand(parInfo.outputBandName)
  vbotCode=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('sigm')
  parInfo=self.app->getParameterByCode('msigm')
  ;sigm=self->getBand(parInfo.outputBandName)
  sigmCode=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('depth')
  parInfo=self.app->getParameterByOutputBandName('dep2d')
  depthCode=parInfo.outputBandName
  depth=self->getBand(depthCode, /NOMASK)
  
  parInfo=self.app->getParameterByOutputBandName('depmx')
  ;depmx=self->getBand(parInfo.outputBandName)
  depmxCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cstrat')
  ;Cstrat=self->getBand(parInfo.outputBandName)
  CstratCode=parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('bfri')
  parInfo=self.app->getParameterByOutputBandName('ustar')
  ;bfri=self->getBand(parInfo.outputBandName)
  bfriCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('tbot')
  ;tbot=self->getBand(parInfo.outputBandName)
  tbotCode=parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('sbot')
  ;sbot=self->getBand(parInfo.outputBandName)
  sbotCode=parInfo.outputBandName
  
  ; Cadvmx - Mixed Layer Advection Index
  ; ------
  
  ;res.Cadvmx = Cadv(sea.umx, sea.vmx)
  ;cadvMix = self->doCadv('umx', 'vmx')
  cAdvMix = self->doCadv(umxCode, vmxCode)
  
  ; Cadvbl - Mixed Layer Advection Index
  ; ------
  
  ;res.Cadvbl = Cadv(sea.ubot, sea.vbot)
  ;Cadvbl = self->doCadv('ubot', 'vbot')
  Cadvbl = self->doCadv(ubotCode, vbotCode)
  
  ; Cstrat - Stratification Index
  ; ------
  
  ;res.Cstrat = Cstrat(sea.sigm)
  ;Cstrat = self->doCstrat('sigm')
  Cstrat = self->doCstrat(sigmCode)
  
  
  ; Cblt
  ; ----
  
  ;res.Cblt = Cblt(sea.depth, sea.depmx, res.Cstrat)
  ;Cblt = self->doCblt('depth', 'depmx', cstrat)
  Cblt = self->doCblt(depthCode, depmxCode, cstrat)
  
  ; Cbfri - Friction Velocity Index
  ; -----
  
  ;Cbfri = Cbfri(sea.bfri)
  ;Cbfri = self->doCbfri('bfri')
  Cbfri = self->doCbfri(bfriCode)
  
  ; Coxy_sat - Oxygen saturation
  ; --------
  
  ;Coxy_sat = Coxy_sat(sea.tbot, sea.sbot)
  ;CoxySat = self->doCoxySat('tbot', 'sbot')
  CoxySat = self->doCoxySat(tbotCode, sbotCode)
  
  ; Tx_deg - bottom degradation velocity
  ; ------
  
  ;Tx_deg = Tx_deg(sea.tbot)
  ;TxDeg = self->doTxDeg('tbot')
  TxDeg = self->doTxDeg(self->getBand(tbotCode, /SETNAN))
  
  ; CTx_deg - Index of bottom degradation velocity
  ; -------
  
  ;res.CTx_deg = CTx_deg(Tx_deg)
  CTxDeg = self->doCTxDeg(TxDeg)
  
  self->addBand, coxySat, 'OxySat', /OVERWRITE, setIgnoreValue=float(self.app->getKeyValue('EPS'))
  self->addBand, cAdvMix, 'cAdvMix', /OVERWRITE
  self->addBand, cAdvbl, 'cAdvbl', /OVERWRITE
  self->addBand, Cstrat, 'Cstrat', /OVERWRITE
  self->addBand, Cblt, 'Cblt', /OVERWRITE
  self->addBand, Cbfri, 'Cbfri', /OVERWRITE
  self->addBand, TxDeg, 'TxDeg', /OVERWRITE
  self->addBand, CTxDeg, 'CTxDeg', /OVERWRITE

  ; Cphys_bott - Bottom Layer Physics Index (for PSA)
  ; ----------
  
  ;res.Cphys_bott_PSA = Cphys_bott(res.Cbfri, res.Coxy_sat, res.Cstrat, res.CTx_deg, res.Cadvmx, res.Cadvbl, res.Cblt, sea.depth, max_depth, WEIGHT=2)
  CphysBottPSA = self->doCphysBott(cbfri, coxySat, cStrat, CTxDeg, cAdvMix, cAdvbl, cblt, depth, maxDepth, WEIGHT=2)
  self->addBand, CphysBottPSA, 'Psa', /OVERWRITE, /MEMORY, setIgnoreValue=float(self.app->getKeyValue('EPS'))
  
  ; Cphys_bott - Bottom Layer Physics Index (for OXYRISK)
  ; ----------
  
  ;res.Cphys_bott_OXYRISK = Cphys_bott(res.Cbfri, res.Coxy_sat, res.Cstrat, res.CTx_deg, res.Cadvmx, res.Cadvbl, res.Cblt, sea.depth, max_depth, WEIGHT=1)
  CphysBottOXYRISK = self->doCphysBott(Cbfri, coxySat, cstrat, CTxDeg, cAdvMix, Cadvbl, cblt, depth, maxDepth, WEIGHT=1)
  self->addBand, CphysBottOXYRISK, 'OxyRisk_No_Adv', /OVERWRITE, /MEMORY, setIgnoreValue=float(self.app->getKeyValue('EPS'))
  self->prepareToExport
  
  maskCondition='maskBand eq 0'; and finite(maskBand) eq 1
  self->configureMaskFromMemory, depth, maskCondition
  return, 1
  
END

; 1.
FUNCTION PSAOperator::doCadv, uCode, vCode

  ; --- Cadv - Advection Index

  thresh_adv = 0.09 ;m/s
  Cadv = self->doCIndex(self->doUvVelocity(uCode, vCode), thresh_adv)
  
  return,Cadv
  
END

;2.
FUNCTION PSAOperator::doUvVelocity, uCode, vCode

  ;------------------------  Advection  -------------------

  ;u=self->readNcdfVar(self.mainFileName, uCode, REVERSE=REVERSE)
  ;v=self->readNcdfVar(self.mainFileName, vCode, REVERSE=REVERSE)
  u=self->getBand(uCode, /SETNAN)
  v=self->getBand(vCode, /SETNAN)
  
  return, sqrt(u^2 + v^2)
  
end

;3.
FUNCTION PSAOperator::doCIndex, value, thresh

  eps=float(self.app->getKeyValue('EPS'))
  
  ; Very low value for initialisations of minima
  ;eps=0.00001
  
  Cindex = (1.0 - value / thresh) > eps < 1.0
  
  return, Cindex
  
end


FUNCTION PSAOperator::doCstrat, sigmCode

  ;sign=self->readNcdfVar(self.mainFileName, sigmCode, REVERSE=REVERSE)
  eps=float(self.app->getKeyValue('EPS'))
  
  sigm=self->getBand(sigmCode, /SETNAN)
  thresh_sigm = 0.07    ;kg/m4
  
  Cstrat = (sigm / thresh_sigm) > eps < 1.
  
  return, Cstrat
  
END

FUNCTION PSAOperator::doCblt, depthCode, depmxCode, cstratValues

  eps=float(self.app->getKeyValue('EPS'))
  
  ;depth=self->readNcdfVar(self.mainFileName, depthCode, REVERSE=REVERSE)
  ;depmx=self->readNcdfVar(self.mainFileName, depmxCode, REVERSE=REVERSE)
  depth=self->getBand(depthCode, /NOMASK)
  depmx=self->getBand(depmxCode, /SETNAN)
  
  blt_min = 0.0
  blt_max = 40.0
  Cstrat_min = 0.2
  Cstrat_max = 1.0
  
  blt = depth - depmx
  
  CbltValues = 0.00005 * blt^3 - 0.0032 * blt^2 + 0.0199 * blt + $
    0.9901
    
  ix = where(blt ge blt_max or blt le blt_min or cstratValues lt Cstrat_min or cstratValues gt Cstrat_max,ix_nb)
  
  if ix_nb gt 0 then CbltValues[ix] = 0.0
  
  return, CbltValues > eps < 1.0
  
END

FUNCTION PSAOperator::doCbfri, bfriCode

  ;bfri=self->readNcdfVar(self.mainFileName, bfriCode, REVERSE=REVERSE)
  bfri=self->getBand(bfriCode, /SETNAN)
  
  ; --- Cbfri - Friction Velocity Index
  thresh_bfri = 0.0170
  Cbfri = self->doCIndex(bfri,thresh_bfri)
  
  return,Cbfri
  
  
END

FUNCTION PSAOperator::doCoxySat, tbotCode, sbotCode

  eps=float(self.app->getKeyValue('EPS'))
  ;  tbot=self->readNcdfVar(self.mainFileName, tbotCode, REVERSE=REVERSE)
  ;  sbot=self->readNcdfVar(self.mainFileName, sbotCode, REVERSE=REVERSE)
  tbot=self->getBand(tbotCode, /SETNAN)
  sbot=self->getBand(sbotCode, /SETNAN)
  
  temp_min = 8.0
  sal_min = 5.0
  temp_max = 22.0
  sal_max = 38.0
  
  ;Weiss 1970 Deep-Sea Research
  oxy_sat_min = self->doOxySat(temp_max, sal_max)
  
  ;Calculate Oxygen Saturation Index
  Coxy_sat = (1. - ((self->doOxySat(tbot, sbot) - oxy_sat_min) / (self->doOxySat(temp_min, sal_min) - oxy_sat_min)))
  
  return, Coxy_sat > eps < 1.0
  
  
END

FUNCTION PSAOperator::doOxySat, teta, sal

  ;Weiss 1970 Deep-Sea Research
  tabs = 273.15 + teta  ; absolute temperature in Kelvin
  oxy_sat = exp(-173.4292 + 249.6339 * (100. / tabs) + 143.3483 * alog(tabs / 100.) $
    -21.8492 * tabs / 100. + sal * (-0.033096 + 0.014259 * tabs / 100. $
    -0.0017 * (tabs / 100.)^2))
    
  return, oxy_sat
END

;=========================================================================
;Function to calculate the bottom degradation velocity given the temperature input variable
;=========================================================================
FUNCTION PSAOperator::doTxDeg, values

  ;degradation rate = f(temperature bottom)
  ;Q10=2 [Eppley 1972]: f(T) = exp(0.07 * T)
  ;Low T values of deg. rate in Heiskanen (PhD) and high T in Avnimelech (1995)

  Tx_deg = 0.0264 * exp(0.07 * values)
  
  return, Tx_deg
  
END

FUNCTION PSAOperator::doCTxDeg, values

  eps=float(self.app->getKeyValue('EPS'))
  Tmin = 2.0
  Tmax = 24.0
  
  Tx_deg_min = self->doTxDeg(Tmin)
  
  ;Calculate the Index
  ;  CTx_deg = ((Tx_deg - Tx_deg_min) / $
  ;    (self->doTxDeg(Tmax) - Tx_deg_min))
  CTx_deg = ((values - Tx_deg_min) / $
    (self->doTxDeg(Tmax) - Tx_deg_min))
    
  return, CTx_deg  > eps  < 1.0
  
END

FUNCTION PSAOperator::doCphysBott, Cbfri, CoxySat, Cstrat, CTxDeg, Cadvmx, Cadvbl, Cblt, depth, maxDepth, WEIGHT=weight
  ; --- Cphys_bott - Bottom Layer Physics Index (for PSA)

  ; weight should be 2 for Cphys_bott used in PSA calculation and
  ;                  1 for                    OXYRISK calculation

  if n_elements(weight) eq 0 then MESSAGE,"ERROR - must set weight for Cbfri"
  
  Cstrat_thresh = 0.2
  
  ; Initialise return array
  
  cphysBott = make_array(SIZE=size(Cstrat),VALUE=!VALUES.F_NAN)
  
  ; Only calculate where depth less than max depth
  
  ix_depth = where(depth le maxDepth,ix_depth_nb)
  
  if ix_depth_nb gt 0 then begin
  
    cphysBott[ix_depth] = (weight * Cbfri[ix_depth]) + CoxySat[ix_depth] + Cstrat[ix_depth] + CTxDeg[ix_depth]
    
    ix_depth_strat = where(Cstrat[ix_depth] lt Cstrat_thresh,ix_depth_strat_nb $
      ,complement=ix_depth_nostrat,ncomplement=ix_depth_nostrat_nb)
      
    if ix_depth_strat_nb gt 0 then begin
      ix = ix_depth[ix_depth_strat]
      cphysBott[ix] = (cphysBott[ix] + Cadvmx[ix]) / (4.0 + weight)
    endif
    
    if ix_depth_nostrat_nb gt 0 then begin
      ix = ix_depth[ix_depth_nostrat]
      cphysBott[ix] = (cphysBott[ix] + Cadvbl[ix] + Cblt[ix]) / (5.0 + weight)
    endif
    
  endif
  
  return,cphysBott
end

FUNCTION PSAOperator::doPSA, CphysBott, CphysSurf

  ; PSA
  ; ---

  PSA = (CphysBott + CphysSurf) / 2.0
  
  return,PSA
  
end

PRO PSAOperator::cleanMainFile

  parInfo=self.app->getParameterByOutputBandName('umx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vmx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('ubot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('vbot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('rhomax')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('dep2d')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('depmx')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('Cstrat')
  self->removeBand, parInfo.outputBandName
  
  ;parInfo=self.app->getParameterByOutputBandName('bfri')
  parInfo=self.app->getParameterByOutputBandName('ustar')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('tbot')
  self->removeBand, parInfo.outputBandName
  
  parInfo=self.app->getParameterByOutputBandName('sbot')
  self->removeBand, parInfo.outputBandName
  
END


;FUNCTION PSAOperator::buildPSAFileName, archiveDir, year, date, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL
;
;  ;if keyword_set(JULDAY) then fileName=string(format='("PSA_", I4, I03, "_", I4, I03, "_MO")', year, sjday, year, ejday) else fileName=string(format='("PSA_", I4, "_", I02, "_MO")', year, month)
;  prefix="EMIS_" ; "PSA_"
;  
;  fileName=string(format='(A, I4, "_", I02, "_MO")', prefix, year, date)
;  if keyword_set(JULDAY) then fileName=string(format='(A, I4, I03, "_MO")', prefix, year, date)
;  if keyword_set(INTERVAL) then begin
;    sjday = julday(date,1,year) - julday(1,1,year) + 1;
;    ejday = julday(date+1,1,year) - julday(1,1,year);
;    fileName=string(format='(A, I4, I03, "_", I4, I03, "_MO")', prefix, year, sjday, year, ejday)
;  endif
;  
;  if keyword_set(FULLPATH) then return, archiveDir+path_sep()+fileName else return, fileName
;  
;END

FUNCTION PSAOperator::buildOperatorResultFileName, dType, displayName, month, year

  ;return, buildPSAVarFileName(dType, displayName, month, year)
  return, call_function('buildPSAVarFileName'+'_'+dType, $ 
    year, month, dType, roi, sensor, displayName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
  
END

PRO PSAOperator::CleanUp

  self-> GenericOperator::Cleanup
  
END

FUNCTION PSAOperator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, OPEN=OPEN, COPY=COPY, $
  ENVITYPE=ENVITYPE, bandToExportList=bandToExportList

  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList)) then return, 0
  return, 1
  
END

PRO PSAOperator__Define

  Struct = { PSAOperator , $
    Inherits GenericOperator $
    }
    
END