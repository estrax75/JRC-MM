pro resizeApply, fullFileName, $
    OUT_NAME=outfname, $
    PIXEL_SIZE=pixel_size, $
    INTERP=interp, $
    GEO_BOUND=GEO_BOUND
    
  compile_opt strictarr, hidden
  
  ENVI_OPEN_DATA_FILE, fullFileName, r_fid=fid;, /HDF_SD, HDFSD_DATASET=index
  ENVI_FILE_QUERY, fid, DATA_TYPE=dt, DIMS=dims, NS=ns, NL=nl, NB=nb
  
  map_info=ENVI_GET_MAP_INFO(fid)
  
  if n_elements(pixel_size) eq 0 then begin
    rfactor = [1d,1d]
  endif else begin
    rfactor=fltarr(2)
    rfactor[0]=map_info.ps[0]/pixel_size[0]
    rfactor[1]=map_info.ps[1]/pixel_size[1]
  endelse
  if n_elements(interp) then interpolation = 0 else interpolation=interp 
  
;  m_res = MAGIC_MEM_CHECK(fid=fid, dims=m_dims, out_dt=out_dt, nb=out_nb, $
;    out_name=outfname, $
;    in_memory=0)
    
  xmap = geo_Bound[0:1]
  ymap = geo_Bound[2:3]
  p_map = PTR_NEW(map_info)
  ENVI_MAP_CONVERT, xfloc, yfloc, xmap, ymap, p_map=p_map
  xfloc = FIX(xfloc)
  yfloc = FIX(yfloc)
  resize_flag = (xfloc[0] ge 0) and (xfloc[1] lt o_ns) and $
    (yfloc[0] ge 0) and (yfloc[1] lt o_nl)
    
  resize_dims = [-1,xfloc,yfloc]
  
  ENVI_DOIT, 'resize_doit', $
    dims=resize_dims,$
    fid=fid,$
    out_name=outfname, $
    pos=INDGEN(out_nb),$
    r_fid=fid, rfact=rfactor, $
    out_bname=bnames, $
    interp=interp
    
end
