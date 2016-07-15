function searchProductFile, productName, instrument, spatialResolution, indicator, year, month, startDay, endDay, noaanumber, level, version, formatType, inputDir

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  res={fullFileName:'', fileName:'', filePath:'', FOUND:0}
  case productName of
    ;    'BRF':begin
    ;      baseFile=getBRFFile(year, month, startDay, endDay, noaanumber)
    ;      basefileName=ST_fileSystem->addFileExtension(baseFile ,formatType)
    ;
    ;      resFile=file_search(inputDir,basefileName, count=count, /FULLY_QUALIFY_PATH)
    ;      if count ne 1 then begin
    ;        tryDir=inputDir+extraPath
    ;        resFile=file_search(tryDir,basefileName, count=count, /FULLY_QUALIFY_PATH)
    ;        if count ne 1 then return, {fullFileName:'', fileName:'', filePath:'', FOUND:0}
    ;      endif
    ;      onlyFileName=ST_fileSystem->getFileNameInfo(resFile, filePath=filePath, extension=extension)
    ;      res={fullFileName:resFile, fileName:onlyFileName, filePath:filePath, FOUND:1}
    ;    end
    'BRDF':begin
      fileInfo=build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, startDay, timestamp, temporalResolution, location, spatialResolution, $
        product, version, formatType,  indicator=indicator, level, projection=projection)
      ;fileToSearch=inputDir+fileInfo.fileName
      searchDir=inputDir
      searchDir=searchDir+fileInfo.filePath

      resFile=file_search(searchDir,fileInfo.fileName, count=count, /FULLY_QUALIFY_PATH)
      if count eq 1 then begin
        onlyFileName=ST_fileSystem->getFileNameInfo(resFile, filePath=filePath, extension=extension)
        res={fullFileName:resFile[0], fileName:onlyFileName[0], filePath:filePath[0], FOUND:1}
      endif
    end

    else: begin
      message, 'productName: '+productName+' not available'
      return, res
    end

  end
  return, res
end