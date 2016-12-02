function getByteScalingSetting, ZEROISNAN=ZEROISNAN, TWO51_TO_TWO55_ISNAN=TWO51_TO_TWO55_ISNAN

  
  catch, error_status
  if error_status NE 0 THEN BEGIN
    ERROR=1
    catch, /CANCEL
    msg='CRITICAL: choose a bytescaling standard!! (/ZEROISNAN or //TWO51 available)'
    errMsg=dialog_message(msg, /ERROR)
    message, msg
    exit
  endif

  remarkableFlags=[255,254,253]

  if keyword_set(ZEROISNAN) then begin
    ; MM check 
    DATA_NAN=-1
    BYTE_NAN=0
    BYTE_RANGE=[1,255]
    remarkableFlags[*]=BYTE_NAN
  endif else begin
    if keyword_set(TWO51_TO_TWO55_ISNAN) then begin
      DATA_NAN=255
      BYTE_NAN=255
      BYTE_RANGE=[0,250]
    endif
  endelse
  return, { $
    DATA_NAN: DATA_NAN, $
    BYTE_NAN:BYTE_NAN, $
    BYTE_RANGE: BYTE_RANGE, $
    remarkableFlags: remarkableFlags $
  }

end