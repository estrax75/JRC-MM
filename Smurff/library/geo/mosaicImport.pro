pro mosaicImport, p_data, o_files, ignoreValue, info=restore_info

  compile_opt strictarr, hidden
  ;
  ; main routine to add selected files into the current mosaic. this routine
  ; is called from both "Import Files..." and "Restore Template..."
  ;
  if (N_ELEMENTS(restore_info) gt 0) then o_files = restore_info.o_file
  p_info = (*p_data).p_info
  previous = N_ELEMENTS(*p_info)
  count = 0L
  for i=0L, N_ELEMENTS(o_files)-1 do begin
    o_files[i]->GETPROPERTY, fid=fid, p_pos=p_pos, dims=dims
    ENVI_FILE_QUERY, fid, sname=sname, data_type=dt, nb=nb, p_map=p_map, $
      xstart=xstart, ystart=ystart
    name = (N_ELEMENTS(*p_pos) eq 1) ? $
      'Band ' + STRTRIM((*p_pos)[0]+1L,2) + ':' + sname : sname
    has_map_info = ENVI_HAS_MAP_INFO(p_map, non_regular=non_regular)
    proj = ENVI_GET_PROJECTION(p_map=p_map, pixel_size=pixel_size, $
      rotation=rotation)
    if (N_ELEMENTS(*(*p_data).p_proj) gt 0 && $
      ((proj.type eq 0 && (*(*p_data).p_proj).type ne 0) || $
      (proj.type ne 0 && (*(*p_data).p_proj).type eq 0))) then $
      has_map_info = 0
    ;
    if ((*p_data).georef && has_map_info && $
      N_ELEMENTS(*(*p_data).p_proj) eq 0) then begin
      ; set the default projection and pixel size to this georeferenced entry.
      base_projection_set = 1
      if (non_regular) then ENVI_PROJ_FIX_NAME, proj
      *(*p_data).p_proj = proj
      (*p_data).ps = pixel_size
    endif
    reproject = 0
    bounds = DBLARR(4)
    if ((*p_data).georef and has_map_info) then begin
      if (ENVI_PROJ_COMPARE(*(*p_data).p_proj,proj) eq 0 || $
        rotation ne 0 || non_regular) then begin
        t_ns = dims[2] - dims[1] + 1L
        t_nl = dims[4] - dims[3] + 1L
        reproject = 1
        grid = LONG([t_ns/200d, t_nl/200d]) > 50
        ; reproject_info will contain:
        ;   {out_ns, out_nl, p_map}
        ENVI_CONVERT_FILE_MAP_PROJECTION, fid=fid, pos=*p_pos, dims=dims, $
          o_proj=*(*p_data).p_proj, grid=grid, out_name='', $
          just_compute=reproject_info
        dims = [-1L,0,reproject_info.out_ns-1, $
          0,reproject_info.out_nl-1]
        reproject_info.bounds = MOSAIC_BOUNDING_BOX(dims, reproject_info.p_map)
      endif else $
        bounds = MOSAIC_BOUNDING_BOX(dims, p_map)
    endif
    ns = dims[2] - dims[1] + 1L
    nl = dims[4] - dims[3] + 1L
    if (reproject eq 0) then $
      reproject_info = {out_ns:ns, out_nl:nl, p_map:p_map, bounds:bounds}
    ;
    info = (N_ELEMENTS(restore_info) eq 0) ? {mosaic_info_struct} : restore_info[i]
    info.id = ENVI_UNIQUEID()
    info.o_file = o_files[i]
    info.name = name
    info.dt = dt
    info.ns = ns
    info.nl = nl
    info.nb = nb
    info.use_offset = 1
    info.auto_place = (has_map_info and (*p_data).georef)
    info.georef = has_map_info
    info.p_map = p_map
    info.reproject = reproject
    info.background = 1e-34
    info.p_image = PTRARR(5, /allocate)
    ; p_image:
    ;   [0] hold resampled data with larger dimension = 512
    ;   [1] bytscaled resampled data to match current mosaic widget size
    ;   [2:4] result of where() for ignore and background value see through.
    ;         for gray scale or rgb bands.
    info.p_cutline = PTR_NEW(/allocate)
    info.p_reproject = PTR_NEW(reproject_info, /no_copy)
    if ((*p_data).georef eq 0 or info.georef eq 0) then info.ps = 1d
    ;
    if (N_ELEMENTS(restore_info) eq 0) then begin
      t_previous = N_ELEMENTS(*p_info)
      bands = MOSAIC_WIDGET_DEFAULT_RGB_BANDS(fid, *p_pos)
      ;info.ignore_value = 1e-34
      if n_elements(ignoreValue) ne 0 then info.ignore_value = ignoreValue else info.ignore_value = 1e-34
      info.rgb = (ENVI_24BIT_COLOR() and bands[0] ge 0)
      info.bands = (bands[0] ge 0) ? [bands,(*p_pos)[0]] : $
        REPLICATE((*p_pos)[0],4)
      info.stretch = 2.
      
      ;
      (*p_data).mosaic_size = (*p_data).mosaic_size > [ns, nl]
      if ((*p_data).georef eq 0) then begin
        info.offset = [dims[1]+xstart, dims[3]+ystart]
        x_max = info.x0 + info.ns
        y_max = info.y0 + info.nl
        if (info.USE_OFFSET && info.x0 eq 0 && info.y0 eq 0) then begin
          tmin = (t_previous eq 0) ? info.offset : $
            [MIN((*p_info).offset[0]), MIN((*p_info).offset[1])]
          info.x0 = info.offset[0] - (tmin[0]<info.offset[0])
          info.y0 = info.offset[1] - (tmin[1]<info.offset[1])
          add = (tmin - info.offset) > 0L
          x_max = info.x0 + info.ns
          y_max = info.y0 + info.nl
          if (t_previous gt 0) then begin
            (*p_info).x0 = (*p_info).x0 + add[0]
            (*p_info).y0 = (*p_info).y0 + add[1]
            x_max = x_max > MAX((*p_info).x0 + (*p_info).ns)
            y_max = y_max > MAX((*p_info).y0 + (*p_info).nl)
          endif
        endif
        (*p_data).mosaic_size = (*p_data).mosaic_size > [x_max, y_max]
      endif
    endif
    ;
    MOSAIC_WIDGET_GET_IMAGE, p_data, info
    MOSAIC_WIDGET_SETUP_CUTLINE, info, *(*p_data).p_proj
    ;
    *p_info = (N_ELEMENTS(*p_info) eq 0) ? TEMPORARY(info) : [*p_info, info]
    count += 1
  endfor
  if (count lt N_ELEMENTS(o_files)) then $
    ; need to free up objects which were not imported cause of "cancel"
    ; button being pressed.
    OBJ_DESTROY, o_files[count:*]
  if (count eq 0) then begin
    if (KEYWORD_SET(base_projection_set)) then $
      temp = TEMPORARY(*(*p_data).p_proj)
    RETURN
  endif
  ;
  order = LINDGEN(count) + previous
  *(*p_data).p_order = (N_ELEMENTS(*(*p_data).p_order) eq 0) ? $
    TEMPORARY(order) : [*(*p_data).p_order, order]
  ;
  if ((*p_data).georef eq 0 && previous eq 0 && $
    N_ELEMENTS(restore_info) eq 0) then $
    MOSAIC_WIDGET_QUERY_MOSAIC_SIZE, p_data, group=group $
  else if ((*p_data).georef) then $
  MOSAIC_WIDGET_MAP_LOCATIONS, p_data, /assign
  
MOSAIC_WIDGET_ASSIGN_MOSAIC_SIZE, p_data

end
