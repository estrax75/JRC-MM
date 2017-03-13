function getDataTCStruct, xSplitDim,ySplitDim, INIT=INIT

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  struct={  day: bytarr(xSplitDim,ySplitDim), $
    nday: bytarr(xSplitDim,ySplitDim), $
    fapar: fltarr(xSplitDim,ySplitDim), $
    dev_temp: fltarr(xSplitDim,ySplitDim), $
    sigma: fltarr(xSplitDim,ySplitDim), $
    red: fltarr(xSplitDim,ySplitDim), $
    dev_red_temp: fltarr(xSplitDim,ySplitDim), $
    sigma_red:fltarr(xSplitDim,ySplitDim), $
    nir: fltarr(xSplitDim,ySplitDim), $
    dev_nir_temp: fltarr(xSplitDim,ySplitDim), $
    sigma_nir: fltarr(xSplitDim,ySplitDim), $
    jrc_flag: bytarr(xSplitDim,ySplitDim), $
    ltdr_flag: intarr(xSplitDim,ySplitDim), $
    ts: fltarr(xSplitDim,ySplitDim), $
    tv: fltarr(xSplitDim,ySplitDim), $
    phi: fltarr(xSplitDim,ySplitDim), $
    toc_red: fltarr(xSplitDim,ySplitDim), $
    toc_nir: fltarr(xSplitDim,ySplitDim), $
    valid:0}
  if keyword_set(INIT) then begin
    DATA_NAN=ST_utils->getNan(/DATA)
    INT_NAN=ST_utils->getNan(/INT)
    BYTE_NAN=ST_utils->getNan(/BYTE)
    notAssignedFlag=15

    struct.jrc_flag=notAssignedFlag ; init to not-assigned flag coding....
    struct.ltdr_flag=INT_NAN ; init to not-assigned flag coding....
    struct.fapar=DATA_NAN
    struct.red=DATA_NAN
    struct.nir=DATA_NAN

    struct.dev_red_temp=DATA_NAN
    struct.dev_nir_temp=DATA_NAN
    struct.dev_temp=DATA_NAN

    struct.sigma=DATA_NAN
    struct.sigma_red=DATA_NAN
    struct.sigma_nir=DATA_NAN

    struct.day=BYTE_NAN
    struct.nday=BYTE_NAN

    struct.ts=INT_NAN
    struct.tv=INT_NAN
    struct.phi=INT_NAN
    struct.toc_red=DATA_NAN
    struct.toc_nir=DATA_NAN

  endif
  return, struct

end