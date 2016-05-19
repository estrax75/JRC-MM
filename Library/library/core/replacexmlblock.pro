PRO replaceXMLBlock, sourceUnit, destUnit, attributeName, attributeValue, attribType

  startTag='<'+attributeName+'>'
  bufferString=''
  printf, destUnit, startTag
  if strlen(attributeValue) ne 0 then begin
    attribValues=(strsplit(attributeValue, ';', /EXTRACT))
    endTag='</'+attributeName+'>'
    ;typeCodeTag='<typeCode>'+strcompress(size(attribValues, /TYPE), /REMOVE_ALL)+'</typeCode>'
    typeCodeTag='<typeCode>'+attribType+'</typeCode>'
    numberOfElementsTag='<numberOfElements>'+strcompress(n_elements(attribValues), /REMOVE_ALL)+'</numberOfElements>'
    attribOpenTag='<value>'
    attribCloseTag='</value>'
    readf, sourceUnit, bufferString
    ; discard all rows until reach attrib tag end
    while not(eof(sourceUnit)) do begin
      if (strupcase(bufferString) eq strupcase(endTag)) then break else readf, sourceUnit, bufferString
      doLog, bufferString
    endwhile
    printf, destUnit, typeCodeTag
    if attribType ne '2' then printf, destUnit, numberOfElementsTag
    for i=0, n_elements(attribValues)-1 do printf, destUnit, attribOpenTag+attribValues[i]+attribCloseTag
    printf, destUnit, endTag
  endif
  
END