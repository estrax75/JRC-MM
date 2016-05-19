function computeCropInfo, cropRoiInfo, masterCropInfo

  COMMON smurffCB, mainApp

  utility=mainApp->getUtility()
  cropCoord=float([cropRoiInfo.lonMin, cropRoiInfo.lonMax, cropRoiInfo.latMax, cropRoiInfo.latMin])
  masterCoord=float([masterCropInfo.lonMin, masterCropInfo.lonMax, masterCropInfo.latMax, masterCropInfo.latMin])
  if utility->IsNumber(cropRoiInfo.nbRows) then begin
    ; resolution definition from slave ROI
    maxY=max([cropCoord[2], cropCoord[3]], min=minY) & maxX=max([cropCoord[0], cropCoord[1]], min=minX)
    extY=abs(maxY-minY) & extX=abs(maxX-minX)
    cropExtension=[cropRoiInfo.nbRows, cropRoiInfo.nbLines]
    cropPixelResolution=[extX/cropExtension[0], extY/cropExtension[1]]
  endif else begin
    ; resolution definition from master ROI
    maxY=max([masterCoord[2], masterCoord[3]], min=minY) & maxX=max([masterCoord[0], masterCoord[1]], min=minX)
    extY=abs(maxY-minY) & extX=abs(maxX-minX)
    masterExtension=[masterCropInfo.nbRows, masterCropInfo.nbLines]
    cropPixelResolution=[extX/masterExtension[0], extY/masterExtension[1]]
    cropExtension=[abs(cropCoord[1]-cropCoord[0])/cropPixelResolution[0], abs(cropCoord[3]-cropCoord[2])/cropPixelResolution[1]] 
  endelse
  preserveResolution=1
  cropRoiInfo={mapWindowBoundary:cropCoord, mapPixelExtension:cropExtension, mapPixelResolution:cropPixelResolution, preserveResolution:1}
  return, cropRoiInfo
  
end