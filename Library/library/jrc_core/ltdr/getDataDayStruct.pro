function getDataDayStruct, xSplitDim,ySplitDim

  return, {  day: 0b, $
    fapar: fltarr(xSplitDim,ySplitDim), $
    sigma: fltarr(xSplitDim,ySplitDim), $
    red: fltarr(xSplitDim,ySplitDim), $
    sigma_red:fltarr(xSplitDim,ySplitDim), $
    nir: fltarr(xSplitDim,ySplitDim), $
    sigma_nir: fltarr(xSplitDim,ySplitDim), $
    jrc_flag: bytarr(xSplitDim,ySplitDim), $
    ltdr_flag: intarr(xSplitDim,ySplitDim), $
    ts: fltarr(xSplitDim,ySplitDim), $
    tv: fltarr(xSplitDim,ySplitDim), $
    phi: fltarr(xSplitDim,ySplitDim), $
    toc_red: fltarr(xSplitDim,ySplitDim), $
    toc_nir: fltarr(xSplitDim,ySplitDim), $
    valid:0}

end