; Main call
FUNCTION BiomapEmisOperator::doComputation, dataPtrs, ignoreValue=ignoreValue, pixelMask=pixelMask, fileName=fileName, NO_DATA=NO_DATA

  return, 1
  
end

FUNCTION BiomapEmisOperator::buildOperatorResultFileName, dType, resultBandName, month, year, sensor, roi, JULDAY=JULDAY, INTERVAL=INTERVAL

  return, call_function('buildEmisBioMapFileName'+'_'+dType, $
    year, month, dType, roi, sensor, resultBandName, archivedir, FULLPATH=FULLPATH, JULDAY=JULDAY, INTERVAL=INTERVAL, TEMPDIR=TEMPDIR, NOTFOUND=NOTFOUND)
    
END

FUNCTION BiomapEmisOperator::getFileNameToExport, month, year, sensor, roi, $
    archiveDir=archiveDir, parInfos=parInfos, NONE=NONE, JULDAY=JULDAY, INTERVAL=INTERVAL
    
  dType=self->getPeriodType()
  ;bandToExport=self.app->getKeyValue('NAN_VALUE')
  NONE=0
  
  parInfos=self->getBandToExportInfoList()
  ncdfFilenames=strarr(n_elements(parInfos))
  for i=0, n_elements(parInfos)-1 do begin
    ncdfFilenames[i] = self->buildOperatorResultFileName(dType, parInfos[i].displayName, month, year, sensor, roi, JULDAY=dType eq 'D', INTERVAL=INTERVAL)
  endfor
  return, ncdfFilenames
  
END

PRO BiomapEmisOperator::cleanMainFile

  self->removeBand, 'resBand'
;parInfo=self.app->getParameterByCode('kH')
;self->removeBand, parInfo.outputBandName
  
END

FUNCTION BiomapEmisOperator::isTest

  return, self.mainFileName eq ''
  
END

PRO BiomapEmisOperator::cleanFileSystem

  self->removeMainFile
  
END

PRO BiomapEmisOperator::CleanUp

  self->cleanFileSystem
  self->GenericOperator::Cleanup
  
END

FUNCTION BiomapEmisOperator::init, application, workingDir, periodType, $
    mask=mask, geoData=geoData, fileName=fileName, bandToExportList=bandToExportList, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, ENVITYPE=ENVITYPE, OPEN=OPEN, COPY=COPY
    
  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, bandToExportList=bandToExportList, fileName=fileName, $
    REMOVE_EXTENSION=REMOVE_EXTENSION, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE)) then return, 0
  return, 1
  
END

PRO BiomapEmisOperator__Define

  Struct = { BiomapEmisOperator , $
    Inherits GenericOperator $
    }
    
END