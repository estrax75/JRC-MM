; Main call
FUNCTION ERegionalEmisOperator::buildOperatorResultFileName, dType, resultBandName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL

  return, call_function('buildEmisERegionalFileName'+'_'+dType, $
    year, month, dType, roi, sensor, resultBandName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

PRO ERegionalEmisOperator::CleanUp

  self->cleanFileSystem
  self->BioMapOperatorV1::Cleanup
  
END

FUNCTION ERegionalEmisOperator::init, application, workingDir, periodType, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  confFile=''
  if not (self -> BioMapOperatorV1 :: init(application, workingDir, periodType, mask=mask, geoData=geoData, confFile=confFile, foqFile=foqFile, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY)) then return, 0
  return, 1
  
END

PRO ERegionalEmisOperator__Define

  Struct = { ERegionalEmisOperator , $
    Inherits BioMapOperatorV1 $
    }
    
END