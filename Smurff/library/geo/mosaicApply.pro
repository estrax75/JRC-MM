;pro mosaicApply, p_data, $
;                  OUT_NAME=outfname, $
;                  BACKGROUND=bval

pro mosaicApply, p_data, $
    OUT_NAME=outfname, $
    BACKGROUND=bval, $
    PIXEL_SIZE=pixel_size, $
    INTERP=interp, $
    ORDER=order, $
    IGNORE_VALUE=ignore_value, $
    LIMITS=limits, $
    bnames=bnames
    
  compile_opt strictarr, hidden
  
  ; query for and setup the necessary info to apply the current mosaic.
  if (N_ELEMENTS(order) ne 0) then begin
    (*p_data).p_order = PTR_NEW(order)
  endif
  
  info = (*(*p_data).p_info)[*(*p_data).p_order]
  num_of_images = N_ELEMENTS(info)
  
  if (N_ELEMENTS(bval) ne 0) then bval = 0
  
  if (N_ELEMENTS(ignore_value) lt num_of_images) then $
    info.ignore_value = FLTARR(num_of_images)
    
  ; MM check multiple/single ignore_value setting
  if n_elements(IGNORE_VALUE) eq 1 then info.ignore_value=FLTARR(num_of_images)+IGNORE_VALUE
  if N_elements(IGNORE_VALUE) eq num_of_images then info.ignore_value=IGNORE_VALUE
  
  out_dt = ENVI_DATA_TYPE_MAX(info.dt)
  out_nb = 0L
  single_band = 0
  for i=0L, N_ELEMENTS(info)-1 do begin
    info[i].o_file->GETPROPERTY, p_pos=p_pos
    out_nb = out_nb > N_ELEMENTS(*p_pos)
    single_band = (single_band or N_ELEMENTS(*p_pos) eq 1)
  endfor
  ;
  color_balance = (TOTAL(info.adjust gt 0) gt 0)
  
  if ((*p_data).georef) then begin
    if (N_ELEMENTS(pixel_size) ne 0) then pixel_size = pixel_size $
    else pixel_size = (*p_data).ps
    interp = 0
    ; recalculate the info x0,y0 with the output pixel size
    MOSAIC_WIDGET_MAP_LOCATIONS, p_data, pixel_size=pixel_size, x0=map_x0, $
      y0=map_y0, map_ptr=map_ptr, map_info=map_info
  endif else begin
    pixel_size = [1d,1d]
    interp = 0
  endelse
  
  if (N_ELEMENTS(interp) ne 0) then interp = interp
  
  o_ns = CEIL((*p_data).mosaic_size[0] * ((*p_data).ps[0] / pixel_size[0]))
  o_nl = CEIL((*p_data).mosaic_size[1] * ((*p_data).ps[1] / pixel_size[1]))
  m_dims = [-1L, 0, o_ns-1, 0, o_nl-1]
  info[0].o_file->GETPROPERTY, fid=fid
  m_res = MAGIC_MEM_CHECK(fid=fid, dims=m_dims, out_dt=out_dt, nb=out_nb, $
    out_name=outfname, $
    in_memory=0)
  if (m_res.cancel) then RETURN
  
  MOSAIC_WIDGET_GET_PROPERTIES, p_data, same_size=same_size, $
    has_feathering=has_feathering, has_reprojection=has_reprojection, $
    has_color_balancing=has_color_balancing
    
  input_special = 1
  if (out_nb eq 3 and single_band) then special = input_special
  
  ; new, super cool method
  mosaic_info = MOSAIC_WIDGET_SETUP_STRUCT(p_data, info, map_x0=map_x0, $
    map_y0=map_y0, map_ptr=map_ptr, pixel_size=pixel_size, $
    special=special)
    
  resize_flag=0b
  
  if (N_ELEMENTS(limits) ne 0) then begin
  
    xmap = limits[0:1]
    ymap = limits[2:3]
    doLog, 'map_info-->', map_info
    p_map = PTR_NEW(map_info)
    ENVI_MAP_CONVERT, xfloc, yfloc, xmap, ymap, p_map=p_map
    xfloc = FIX(xfloc)
    yfloc = FIX(yfloc)
    resize_flag = (xfloc[0] ge 0) and (xfloc[1] lt o_ns) and $
      (yfloc[0] ge 0) and (yfloc[1] lt o_nl)
      
    resize_dims = [-1,xfloc,yfloc]
    
  endif
  
  doLog, '------->', xfloc, yfloc, xmap, ymap
  if resize_flag then  begin
  
    out_mosaic_fname = ENVI_GET_TMP()
    
    ;doLog, bnames
    ENVI_PREP, 'mosaic_new_doit', 'Mosaicking', 1, $
      info=mosaic_info, o_ns=o_ns, o_nl=o_nl, out_name=out_mosaic_fname, $
      out_bname=bnames, in_memory=0, out_nb=out_nb, $
      out_dt=out_dt, background=bval, interp=interp, $
      map_info=map_info, out_ps=pixel_size, /free_pointers, $
      r_fid=mos_fid
      
      
    ENVI_DOIT, 'resize_doit', $
      dims=resize_dims,$
      fid=mos_fid,$
      out_name=outfname, $
      pos=INDGEN(out_nb),$
      r_fid=r_fid, rfact=[1,1], $
      out_bname=bnames
      
    envi_file_mng, id=mos_fid, /remove, /delete, /no_warning
    
  endif else $
  
    ENVI_PREP, 'mosaic_new_doit', 'Mosaicking', 1, $
    info=mosaic_info, o_ns=o_ns, o_nl=o_nl, out_name=outfname, $
    out_bname=bnames, in_memory=0, out_nb=out_nb, $
    out_dt=out_dt, background=bval, interp=interp, $
    map_info=map_info, out_ps=pixel_size, /free_pointers, $
    r_fid=mos_fid
    
end
