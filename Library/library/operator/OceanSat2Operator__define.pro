;@structure_definition.pro

; Main call
FUNCTION OceanSat2Operator::doComputation
  
  return, 1
  
END


PRO OceanSat2Operator::cleanMainFile

  ;parInfo=self.app->getParameterByCode('PP')
  ;self->removeBand, parInfo.outputBandName
  
END

PRO OceanSat2Operator::CleanUp

  self-> GenericOperator::Cleanup
  
END

FUNCTION OceanSat2Operator::init, application, workingDir, periodType, mask=mask, geoData=geoData, fileName=fileName, $
  OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList 

  if not (self -> GenericOperator :: init(application, workingDir, periodType, maskBand=maskBand, geoBand=geoBand, fileName=fileName, OPEN=OPEN, COPY=COPY, ENVITYPE=ENVITYPE, bandToExportList=bandToExportList)) then return, 0
  return, 1
  
END

PRO OceanSat2Operator__Define

  Struct = { OceanSat2Operator , $
    Inherits GenericOperator $
    }
    
END