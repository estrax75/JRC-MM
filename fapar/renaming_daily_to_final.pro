PRO renaming_daily_to_final

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  home1='/space3/storage/products/LAN/AVH/L1/PLC'
  home2='/space3/storage/products/LAN/AVH/L1/PLC'
  home=home1
  startYear=1982
  endYear=2006
  NOACodeList=intarr(2,7)
  NOACodeList[0,0:3]=[7,9,11,14]
  NOACodeList[1,0:3]=2
  NOACodeList[0,4:6]=[16,18,19]
  NOACodeList[1,4:6]=3

  for year=startYear, endYear do begin
    
    sYear=string(year, '(I4)')
    for month=1, 12 do begin
      sMonth=string(month, '(I02)')
      files=file_search(home+path_sep()+sYear+path_sep()+sMonth, '*FPA*.NC', COUNT=count)
      for f=0, count do begin
        sourceFileName=ST_fileSystem->getFileNameInfo(files[f], filePath=sfilePath, extension=extension)
;        AVH_19990629_001D_900S900N1800W1800E_0005D_FPA_N14.nc
;        to
;        1)    AVH2_NOA14_19990629_19990629_L2_MUL_900S900N1800W1800E_PLC_0005D_V01.nc (N07,N09,N11,N14)
;        2)    AVH3_NOA16_19990629_19990629_L2_MUL_900S900N1800W1800E_PLC_0005D_V01.nc (N16,N18,N19)
        fields=strsplit(sourceFileName, '_', /EXTRACT, /PRESERVE)
        NOAACode=fix(strmid(fields[6], 1, 2))
        idx=(where(NOACodeList[0,*] eq NOAACode, count))[0]
        if count ne 1 then stop
        outAVH=NOACodeList[1,idx]
        outName='AVH'+string(outAVH, '(I1)')+'_'+'NOA'+string(NOAACode, '(I02)')+'_'+fields[1]+'_'+fields[1]+'_'+'L2'+'_'+'MUL'+'_'+fields[3]+'_'+'PLC'+'_'+fields[4]+'_'+'V01'+'.nc'
        print, sourceFileName
        print, outName
        ;file_move, files[f],  sfilePath+path_sep()+outName, /ALLOW, /QUIET
      endfor
    endfor
  endfor

END