function build_JRC_Generic_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  if n_elements(timestamp) eq 1 then timeInfo=timestamp+'_'+temporalResolution else timeInfo=temporalResolution 
  if n_elements(location) eq 0 then location1='900S900N1800W1800E' else location1=location
  if n_elements(projection) eq 0 then projection1='PLC' else projection1=projection
  ; search mode...
  if n_elements(version) eq 0 then thisVersion='???' else thisVersion=version

  fileName=string(format='(A, "_", I4, I02, I02, "_", A, "_", A, "_", A, "_", A, "_", A, ".", A)', $
    instrument, year, month, day, timeInfo, location1, spatialResolution, product, thisVersion, fileType)

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
  level1='L3'

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day1, timestamp, temporalResolution1, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level1, projection=projection)

end

function build_JRC_DailyInterval_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  level1='L3'

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution1, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level1, projection=projection)

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

function build_JRC_TCAlg_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(version) eq 0 then version1='ALGTC' else version1=version+'ALGTC'

  return, build_JRC_Generic_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version1, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_TCAlg_DailyInterval_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
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

function build_JRC_AVH_TCAlg_DailyInterval_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  instrument1='AVH'

  return, build_JRC_TCAlg_Daily_Product_FileName(instrument1, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
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

function build_JRC_BRF_AVH_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRF'

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

function build_JRC_BRF_AVH_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRF'

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

function build_JRC_BRF_AVH_MeanAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRF'

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

function build_JRC_BRF_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRF'

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

function build_JRC_BRF_AVH_TCAlg_TenDays_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='BRF'

  return, build_JRC_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

;**
function build_JRC_FPA_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_Diff_AVH_MeanAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPADFF'

  return, build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_TCAlg_DailyInterval_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  nOfDays=strsplit(temporalResolution, 'D', /EXTRACT, /PRESERVE_NULL)
  nOfDays=STRING(nOfDays[0], FORMAT='(i03)')
  temporalResolution1=nOfDays+'D'
  level1='L3'

  return, build_JRC_AVH_TCAlg_DailyInterval_Product_FileName(instrument, year, month, day, timestamp, temporalResolution1, location, spatialResolution, product1, version1, fileType,$
    indicator=indicator, level1, projection=projection)
  ;return, build_JRC_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
  ;  indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_AVH_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPA'

  return, build_JRC_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_Diff_TCAlg_Monthly_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPADFF'

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

  print, 'year, month, day', year, month, day
  return, build_JRC_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product1, version, fileType,$
    indicator=indicator, level, projection=projection)

end

function build_JRC_FPA_DIFF_Daily_Product_FileName, instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
  indicator=indicator, level, projection=projection

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  product1='FPADFF'

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
  level='L1'
  projection='PLC'
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
  print, 'build_JRC_BRF_AVH_TCAlg_TenDays_Product_FileName', build_JRC_BRF_AVH_TCAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRF_AVH_MeanAlg_Monthly_Product_FileName', build_JRC_BRF_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_AVH_MeanAlg_Monthly_Product_FileName', build_JRC_AVH_MeanAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_MeanAlg_TenDays_Product_FileName', build_JRC_BRDF_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_TCAlg_Monthly_Product_FileName', build_JRC_BRDF_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRF_AVH_MeanAlg_TenDays_Product_FileName', build_JRC_BRF_AVH_MeanAlg_TenDays_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRF_AVH_TCAlg_Monthly_Product_FileName', build_JRC_BRF_AVH_TCAlg_Monthly_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRDF_AVH_Daily_Product_FileName', build_JRC_BRDF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
    indicator=indicator, level, projection=projection)
  print, 'build_JRC_BRF_AVH_Daily_Product_FileName', build_JRC_BRF_AVH_Daily_Product_FileName(instrument, year, month, day, timestamp, temporalResolution, location, spatialResolution, product, version, fileType,$
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