function fillWithSpace, stringArray, maxLength

 strResult=stringArray
 for i=0, n_elements(stringArray)-1 do begin
  thisL=strlen(stringArray[i])
  if thisL lt maxLength then begin
    spaces=''
    for j=0, maxLength-thisL do spaces=spaces+' '
    strResult[i]=strResult[i]+spaces
  endif
 endfor
 return, strResult

end