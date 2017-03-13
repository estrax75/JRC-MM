;+
; :Author: mariomi
;-
;@.\Library\library\objects\GenericOperator.pro
;@../Library/library/objects/FileSystem.pro
function read_BRF, folder, fileName, bandName, FOUND=FOUND, FULL=FULL, USENAN=USENAN, $
  PIXELS_PROCESS=PIXELS_PROCESS, CSV=CSV, NC=NC, ORIGDIMS=ORIGDIMS, extraData=extraData

  ;  avBandNames=['BRF_BAND_1', 'BRF_BAND_2', 'SIGMA_BRF_BAND_1', 'SIGMA_BRF_BAND_2', $
  ;    'TS', 'TV', 'PHI', 'QA', 'Q1', 'Q2']
  if keyword_set(PIXELS_PROCESS) then begin
    if keyword_set(CSV) then allData=readSPP_BRF_FromCSV(folder, fileName, FOUND=FOUND, ORIGDIMS=ORIGDIMS, extraData=extraData)
    if keyword_set(NC) then allData=readSPP_BRF_FromNC(folder, fileName, FOUND=FOUND, ORIGDIMS=ORIGDIMS, extraData=extraData)
  endif else begin
    allData=readMatrix_Brf_FromNC(folder, fileName, bandName, FOUND=FOUND, FULL=FULL, USENAN=USENAN)
  endelse

  return, allData

end