function getDataTCStruct1, xDim,yDim, INIT=INIT

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  if n_elements(xDim) eq 0 then begin
    xDim=7200
    yDim=3600
  endif

  struct={day: bytarr(xDim,yDim), $
    nday: bytarr(xDim,yDim), $
    fapar: fltarr(xDim,yDim), $
    dev_temp: fltarr(xDim,yDim), $
    sigma: fltarr(xDim,yDim), $
    red: fltarr(xDim,yDim), $
    dev_red_temp: fltarr(xDim,yDim), $
    sigma_red:fltarr(xDim,yDim), $
    nir: fltarr(xDim,yDim), $
    dev_nir_temp: fltarr(xDim,yDim), $
    sigma_nir: fltarr(xDim,yDim), $
    ltdr_flag: intarr(xDim,yDim), $
    jrc_flag: bytarr(xDim,yDim), $
    ts: fltarr(xDim,yDim), $
    tv: fltarr(xDim,yDim), $
    phi: fltarr(xDim,yDim), $
    toc_red: fltarr(xDim,yDim), $
    toc_nir: fltarr(xDim,yDim), $
    faparMean: fltarr(xDim,yDim)}

  if keyword_set(INIT) then begin
    DATA_NAN=ST_utils->getNan(/DATA)
    INT_NAN=ST_utils->getNan(/INT)
    BYTE_NAN=ST_utils->getNan(/BYTE)
    
    struct.fapar=DATA_NAN
    struct.red=DATA_NAN
    struct.nir=DATA_NAN

    struct.dev_red_temp=DATA_NAN
    struct.dev_nir_temp=DATA_NAN
    struct.dev_temp[*,*]=DATA_NAN

    struct.sigma=DATA_NAN
    struct.sigma_red=DATA_NAN
    struct.sigma_nir=DATA_NAN

    struct.tv=INT_NAN
    struct.phi=INT_NAN
    struct.ts=INT_NAN
    struct.toc_red=DATA_NAN
    struct.toc_nir=DATA_NAN
    ;
    ; initialize to invalid
    struct.jrc_flag=1
    ; day range [1..31]; 255 means no data
    struct.day=BYTE_NAN
    ; nday range [1..31]; 255 means no data
    struct.nday=BYTE_NAN

  endif
  return, struct

end