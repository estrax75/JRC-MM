FUNCTION mapFaparFlag, flagList, REVERT=REVERT

  ;
  ;
  ; Map flags coding (FAPAR Daily) from SWF to AVHRR and viceversa
  ;
  ; 2016, MM
  ;
  ; The inputs are
  ; flagList = list if original flags 
  ; REVERT   = set if you want coding from SWF to AVHRR, otherwise AVHRR to SWF is performed

  ;Convert flag codes from AVHRR/IDL to SEAWIFS/C

  label=['Land','Bad Value','CloudIce','Water','Bare Soil','Negative Rectified','Saturation Rectified']
  AVHRR=[0,1,2,3,4,5,6]
  SWF=[100,255,200,0,105,104,103]
  destFlags=0*flagList
  destFlags[*]=-1

  if ~(keyword_set(REVERT)) then begin
    cutInvalid=where(flagList gt max(AVHRR), notValid)
    if notValid gt 0 then flagList[cutInvalid]=1
    for i=0, n_elements(AVHRR)-1 do begin
      posIdxs=where(AVHRR[i] eq flagList, cnt)
      if cnt gt 0 then destFlags[posIdxs]=SWF[i]
    endfor
  endif else begin
    for i=0, n_elements(SWF)-1 do begin
      posIdxs=where(SWF[i] eq flagList, cnt)
      if cnt gt 0 then destFlags[posIdxs]=AVHRR[i]
    endfor
  endelse
  
  return, destFlags

END
