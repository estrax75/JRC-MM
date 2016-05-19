pro tvMatrix, data, subscribe, mapDataInfo

 COMMON mapDataInfo, lastMapDataInfo
 
 if n_elements(mapDataInfo) eq 0 then mapDataInfo=lastMapDataInfo else lastMapDataInfo=mapDataInfo
 mapData=make_array(SIZE=mapDataInfo)
 mapData[subscribe]=data
 mapData[where(mapData eq 0 or ~finite(mapData) or mapData eq -9999)]=!values.F_NAN
 tv, bytscl(reverse(mapData,2), /NAN)

end