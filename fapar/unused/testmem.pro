pro testmem

  dims=[7200,3600]
  dimMod1=dims[0] mod 3 & dim0=dims[0] / 3
  dimMod2=dims[1] mod 3 & dim1=dims[1] / 3


  index_to_subcribe=lonarr(3,3)
  for i=0, 2 do begin
    for j=0, 2 do begin
      index_to_subcribeX=1.*i*dim0
      index_to_subcribeY=1.*j*dim1
      print, 'x from:', index_to_subcribeX, 'y from:', index_to_subcribeY
      print, 'x to:', index_to_subcribeX+dim0-1, 'y to:', index_to_subcribeY+dim1-1
    endfor
  endfor
  
;  for i=0, 2 do begin
;    for j=0, 2 do begin
;      index_to_subcribeX=1.*i*dim0
;      index_to_subcribeY=1.*i*dim1
;      print, 'x:', index_to_subcribeX, 'y:', index_to_subcribeY
;    endfor
;  endfor

end