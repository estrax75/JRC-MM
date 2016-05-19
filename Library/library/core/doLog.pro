PRO doLog, string1, string2, string3, string4, string5, string6, string7, string8, string9, level=level, CHECKMEMORY=CHECKMEMORY, STACK=STACK, callingRoutine=callingRoutine

  COMMON smurffCB, mainApp
  COMMON checkMem, lastMemory
  
  if obj_valid(mainApp) then begin
    mainApp->doLog, string1, string2, string3, string4, string5, string6, string7, string8, string9, level=level
  endif
  if keyword_set(CHECKMEMORY) then begin
    ;help, /memory
    thisMemory = long(MEMORY(/CURRENT))
    if n_elements(lastMemory) ne 0 then begin
      print, 'memory allocated since last call:', thisMemory-lastMemory
    endif else begin
      print, 'memory mark:', thisMemory
    endelse
    lastMemory=thisMemory
  endif
  if keyword_set(STACK) then begin
    Help, Calls=callStack
    
    ; Parse the name of this program module, and the module that called this one.
    
    IF Float(!Version.Release) GE 5.2 THEN BEGIN
      thisRoutine = (StrSplit(StrCompress(callStack[0])," ", /Extract))[0]
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
    ENDIF ELSE BEGIN
      thisRoutine = (Str_Sep(StrCompress(callStack[0])," "))[0]
      callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]
    ENDELSE
    
  endif
END