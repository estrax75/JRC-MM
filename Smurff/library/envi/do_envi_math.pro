function do_envi_math, fileName, expression, listOfBand, outbandName, MEMORY=MEMORY, ret_fid=ret_fid

  COMMON smurffCB, mainApp
  
  util=mainApp->getUtility()
  operator=mainApp->getEnviOperator()
  ;
  ; First restore all the base save files.
  ;
  ;envi, /restore_base_save_files
  ;
  ; Initialize ENVI and send all errors
  ; and warnings to the file batch.txt
  ;
  ;envi_batch_init, log_file='batch.txt'
  ;
  ; Open the input file
  ;
  envi_open_file, fileName+'.envi', r_fid=fid
  if (fid eq -1) then begin
    envi_batch_exit
    return, 0
  endif
  ;
  ; Set the keywords. We will perform the
  ; band math on all samples in the file.
  ;
  envi_file_query, fid, dims=dims, bnames=bnames
  
  replaceExpression=expression
  pos=lonarr(n_elements(listOfBand))
  
  
  for i=0, n_elements(listOfBand)-1 do begin
    thisBand=listOfBand[i]
    idx=(where(thisBand eq bnames, count))[0]
    pos[i]=idx
    replaceExpression=util->strreplace(replaceExpression, listOfBand[i], 'b'+strcompress(idx+1, /REMOVE_ALL))
    ;doLog, expression
    ;doLog, replaceExpression
  endfor
  
  t_fid = lonarr(n_elements(listOfBand))
  t_fid[*] = fid 
  pos  = pos
  ;exp = 'b1 + b2'
  ;out_name = 'testimg'
  ;
  ; Perform the band math processing
  ;
  envi_doit, 'math_doit', $
    fid=t_fid, pos=pos, dims=dims, $
    exp=replaceExpression, $
    r_fid=result_fid, /IN_MEMORY
  
  result=0
  ret_fid=fid

  if ~keyword_set(MEMORY) then operator->addBand, fileName, ret_fid, result_fid, outbandName else result=outbandData
  envi_file_mng, id=ret_fid, /REMOVE 
  envi_file_mng, id=result_fid, /REMOVE
  envi_open_file, fileName+'.envi', r_fid=fid
  ret_fid=fid
  
  ;if ~keyword_set(MEMORY) then operator->addBand, fileName+'.envi', result_fid, outbandName else result=outbandData
  
  return, result
;
; Exit ENVI
;
;envi_batch_exit
end
