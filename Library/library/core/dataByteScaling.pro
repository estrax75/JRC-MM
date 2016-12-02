; phi brf <> phi fapar
FUNCTION dataByteScaling, data, flagMatrix, $;, FLAG_VALUES=FLAG_VALUES, $
  DATA_RANGE=DATA_RANGE, BYTE_RANGE=BYTE_RANGE, $
  DATA_NAN=DATA_NAN, BYTE_NAN=BYTE_NAN, outSlope, outIntercept

  resData=data

  outInterval=(BYTE_RANGE[1]-BYTE_RANGE[0])
  stepRange=(1d*DATA_RANGE[1]-1d*DATA_RANGE[0])/(outInterval)
  if finite(DATA_NAN) eq 0 then nanIdxs=where(finite(data) eq 0, preNanCount, complement=validIdxs) else nanIdxs=where(data eq DATA_NAN, preNanCount, complement=validIdxs) 
  
  resData=bytscl(resData, MIN=DATA_RANGE[0], MAX=DATA_RANGE[1], TOP=outInterval, /NAN);+VALUE_BYTES[0]
  resData=resData+BYTE_RANGE[0]

  idxneg=where(data lt DATA_RANGE[0])
  if idxneg[0] ge 0 then begin
    resData[idxneg]=0.0;DATA_RANGE[0]
    ;if keyword_set(FLAG_VALUES) then flagMatrix[idxneg]= FLAG_VALUES[0]
  endif

  ;idxbig=where(output.fpar gt DATA_MAX and output.fpar le 250.0)
  idxbig=where(data gt DATA_RANGE[1])
  if idxbig[0] ge 0 then begin
    resData[idxbig]=1.0;DATA_RANGE[1]
    ;if keyword_set(FLAG_VALUES) then flagMatrix[idxbig]= FLAG_VALUES[1]
  endif
  if preNanCount ne 0 then resData[nanIdxs]=BYTE_NAN
  outSlope=stepRange
  outIntercept=-BYTE_RANGE[0]*stepRange
  ; check 0..250 case:  
  ;aa=where(resdata eq 200)
  ;idx=aa[0]
  ;print, resdata[idx], resdata[idx]*outSlope+outIntercept, data[idx]
  if n_elements(flagMatrix) eq 0 then flagMatrix=resdata
  return, {resultData:resdata, resultFlag:flagMatrix}


END
