; Main call
FUNCTION MultiROISplitOperator::buildOperatorResultFileName, dType, resultBandName, month, year, sensor, roi, archivedir, JULDAY=JULDAY, INTERVAL=INTERVAL, FULLPATH=FULLPATH

  ;pars=*self.parameters
  ;if dType eq 'M' then return, buildCompareFileName_M(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'D' then return, buildCompareFileName_D(pars[0], pars[1], displayName, month, year, sensor, roi)
  ;if dType eq 'M' then return, buildCompareFileName_M(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  ;if dType eq 'D' then return, buildCompareFileName_D(resultBandName, year, month, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL)
  return, call_function('buildMultiROISplitFileName'+'_'+dType, $
    year, month, dType, roi, sensor, resultBandName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

PRO MultiROISplitOperator::CleanUp

  self-> MultiROIOperator::Cleanup
  
END

FUNCTION MultiROISplitOperator::init, application, workingDir, periodType, resultBandName, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  if not (self -> MultiROIOperator :: init(application, workingDir, periodType, resultBandName, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY)) then return, 0
  return, 1
  
END

PRO MultiROISplitOperator__Define

  Struct = { MultiROISplitOperator , $
    Inherits MultiROIOperator $
    }
    
END