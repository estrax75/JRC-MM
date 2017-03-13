pro paint_a_map

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  ;filename1='AVH09_NOA14_19990601_19990630_L3_FPA_900S900N1800W1800E_0005D_FULL_ALGTC_CLOUDTYPE_1_REMOVECLOUD_YES_APPLYHIGHSIGMA_NO_SIGMAWEIGHTED_NO.NC'
  ;filename2='CASEC_AVH09_NOA14_19990601_19990630_L3_FPA_900S900N1800W1800E_0005D_FULL_ALGTC_CLOUDTYPE_1_REMOVECLOUD_NO_APPLYHIGHSIGMA_YES_SIGMAWEIGHTED_YES.NC'
  ;filedir='E:\mariomi\Desktop\tc_versioning'
  filedir='E:\mariomi\Desktop\input_for_gif'
  fileList=file_search(filedir+'\*.NC')
  for i=0, n_elements(fileList)-1 do begin
    faparFile=ST_fileSystem->getFileNameInfo(fileList[i], filePath=filePath, extension=extension)
    jrcflag1=read_AVHRR(filePath[0], faparFile[0], FOUND=FOUND, $
      FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName='JRC_FLAG')
    jrcf=jrcflag1.data
    fapar1=read_AVHRR(filePath[0], faparFile[0], FOUND=FOUND, $
      FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName='FAPAR')
    ;  fapar2=read_AVHRR(filedir, fileName2, FOUND=FOUND, $
    ;    FULL=FULL, APPLY_CONVERSION=APPLY_CONVERSION, varName='FAPAR', offset=offset, count=count)
    values1=fapar1.data
    ;  values2=fapar2.data
    setTo255=where(finite(values1) eq 0, count)
    if keyword_set(count) then values1[setTo255]=255b
    values1=byte(fix(values1))
    ;  setTo255=where(finite(values2) eq 0, count)
    ;  if keyword_set(count) then values2[setTo255]=255b
    ;  values2=byte(fix(values2))
    ;  diff=fix(values1)-fix(values2)
    ;  diffImg=bytscl(diff)
    ;desert=where(jrcf eq 4 or jrcf eq 5, cnt)
    ;if cnt gt 0 then diffImg[desert]=254

    device, decomposed=0
    faparcolor, r,g,b
    r=reform(r)
    g=reform(g)
    b=reform(b)
    ;
    ;  colorMapImg=bytarr(765, 360)
    ;  width=floor(765./250)
    ;  shiftPx=0
    ;  ;
    ;  for i=0, 250 do begin
    ;    colorMapImg[shiftPx:(shiftPx+width)-1, 0:359]=i
    ;    shiftPx+=width
    ;    print, shiftPx
    ;  endfor
    ;  colorMapImg=colorMapImg[0:shiftPx-width, *]
    ;
    ;  write_gif, filedir+'\fapar_colorbar.gif', colorMapImg, r,g,b

    write_gif, filedir+path_sep()+faparFile+'cloud_removed.gif', values1, r,g,b
    values1=congrid(values1, 720, 360)
    write_gif, filedir+path_sep()+faparFile+'cloud_removed_pauper.gif', values1, r,g,b
  endfor

;  write_gif, filedir+'\fapar_cloud_with_high_sigma.gif', values2, r,g,b
;  values2=congrid(values2, 720, 360)
;  write_gif, filedir+'\fapar_cloud_with_high_sigma_pauper.gif', values2, r,g,b
;  ;a=histogram(diffImg)
;  ;plot, a
;  ;device, decomposed=0
;  BlueRedColor, r,g,b
;
;  ;for i=110, 169 do pcolor(*,i)=[0, green[i-120], blue[i-120]]
;  ;diffImg[3300:3500,7000]=110
;
;  colorMapImg=bytarr(720, 360)
;  width=floor(720./50)
;  shiftPx=0
;  for i=128, 128+49 do begin
;    colorMapImg[shiftPx:(shiftPx+width)-1, 0:359]=i
;    shiftPx+=width
;    print, shiftPx
;  endfor
;  colorMapImg=colorMapImg[0:shiftPx-width, *]
;
;  r=reform(r)
;  g=reform(g)
;  b=reform(b)
;
;  write_gif, filedir+'\diff_colorbar.gif', colorMapImg, r,g,b
;
;  diffTmbImg=congrid(diffImg, 720, 360)
;  write_gif, filedir+'\fapar_diff_pauper.gif', diffTmbImg, r,g,b
;  write_gif, filedir+'\fapar_diff.gif', diffImg, r,g,b
;  ;tv, congrid(values, 720, 360);, r=r, g=g, b=b

end

