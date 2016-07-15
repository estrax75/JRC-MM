function build_JRC_Generic_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  if n_elements(timestamp) eq 1 then timeInfo=timestamp+'_'+temporalResolution else timeInfo=temporalResolution 
  if n_elements(location) eq 0 then location1='900S900N1800W1800E' else location1=location
  if n_elements(projection) eq 0 then projection1='LAEA' else projection1=projection

  fileName=string(format='(A, "_", I4, I02, I02, "_", A, "_", A, "_", A, "_", A, "_", A, ".", A)', $
    instrument, year, month, day, timeInfo, location1, spatialResolution, product, version, fileType)

  filePath=string(format='(A, A, A, A, A, A, A, A, I4, A, I02)', $
    indicator, path_sep(), instrument, path_sep(), level, path_sep(), projection1, path_sep(), year, path_sep(), month)

  return, {fileName:fileName, filePath:filePath}

end

function build_JRC_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  temporalResolution='001D'

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  temporalResolution1='010D'

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution1, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  temporalResolution1='001M'
  day1=1

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day1, timestamp, temporalResolution1, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_MeanAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(version) eq 0 then version1='ALGMEAN' else version1=version+'ALGMEAN'

  return, build_JRC_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version1, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_TCAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(version) eq 0 then version1='ALGTC' else version1=version+'ALGTC'

  return, build_JRC_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version1, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(version) eq 0 then version1='ALGMEAN' else version1=version+'ALGMEAN'

  return, build_JRC_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version1, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(version) eq 0 then version1='ALGTC' else version1=version+'ALGTC'

  return, build_JRC_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version1, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_AVH_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_Daily_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_AVH_TCAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_TCAlg_TenDays_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_AVH_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_TCAlg_Monthly_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_AVH_MeanAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_MeanAlg_TenDays_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_BRDF_AVH_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_BRDF_AVH_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_BRDF_AVH_MeanAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_MeanAlg_Monthly_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_BRDF_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_BRDF_AVH_TCAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

;**
function build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRDF'

  return, build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_MeanAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_TCAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

pro nametest

  instrument='AVH1'
  year=2000
  month=8
  day=5
  timestamp='000000'
  temporalResolution='001Y'
  location='900S900N1800W1800E'
  product='MUL'
  version='001'
  indicator='LAN'
  level='L2'
  projection='LAEA'
  filetype='hdf'

  print, 'build_JRC_Generic_Product_FileName', build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_Monthly_Product_FileName', build_JRC_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_TenDays_Product_FileName', build_JRC_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_Daily_Product_FileName', build_JRC_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_Daily_Product_FileName', build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_FPA_AVH_Daily_Product_FileName', build_JRC_FPA_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_FPA_AVH_TCAlg_TenDays_Product_FileName', build_JRC_FPA_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_FPA_AVH_MeanAlg_TenDays_Product_FileName', build_JRC_FPA_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName', build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName', build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_TCAlg_TenDays_Product_FileName', build_JRC_BRDF_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_MeanAlg_Monthly_Product_FileName', build_JRC_BRDF_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_MeanAlg_Monthly_Product_FileName', build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_MeanAlg_TenDays_Product_FileName', build_JRC_BRDF_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_TCAlg_Monthly_Product_FileName', build_JRC_BRDF_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_Daily_Product_FileName', build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_MeanAlg_TenDays_Product_FileName', build_JRC_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_TCAlg_Monthly_Product_FileName', build_JRC_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_TCAlg_TenDays_Product_FileName', build_JRC_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_Daily_Product_FileName', build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_MeanAlg_Monthly_Product_FileName', build_JRC_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_TCAlg_TenDays_Product_FileName', build_JRC_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_Daily_Product_FileName', build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_TCAlg_Monthly_Product_FileName', build_JRC_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_MeanAlg_Monthly_Product_FileName', build_JRC_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_TCAlg_TenDays_Product_FileName', build_JRC_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_MeanAlg_TenDays_Product_FileName', build_JRC_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_Monthly_Product_FileName', build_JRC_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_TenDays_Product_FileName', build_JRC_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_Daily_Product_FileName', build_JRC_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_Generic_Product_FileName', build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)

end