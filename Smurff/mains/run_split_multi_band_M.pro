PRO run_split_multi_band_M, request, NODISPLAY=NODISPLAY

  COMMON smurffCB, mainApp
  
  inputParameterList=request->getInputParameterList(NOTFOUND=NOTFOUND)
  outputParameterList=request->getOutputParameterList(NOTFOUND=NOTFOUND)
  
  totPar=n_elements(inputParameterList)
  for i=0, totPar-2 do begin
    singleParameter=inputParameterList[i]
    parInfo=extractParameterStruct(inputParameterList[i])
    outputParameterList[1]=strcompress(parInfo.label, /REMOVE)
    ;tags=tag_names(parInfo)
    ;for j=0, n_elements(tags)-1 do parString=parInfo.(j)+'$'
    ;singleParameter=singleParameter+';'+inputParameterList[totPar-1]
    request->setInputParameterList, [singleParameter, inputParameterList[totPar-1]]
    passedVar=outputParameterList
    request->setOutputParameterList, passedVar
    run_multi_band_M, request, NODISPLAY=NODISPLAY    
  endfor
  
END