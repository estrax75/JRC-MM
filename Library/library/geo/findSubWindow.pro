;function [start, edge, lat, lon, iflipdim] = find_subwindow(in_map_win,in_map_size,out_map_win)
function findSubwindow, in_map_win, in_map_size, out_map_win
  ;% Find start, edge, latitude and longitude of window within larger window
  ;%
  ;% Input:
  ;%   in_map_win  - double(lat0,lon0,latn,lonn) - Lat/lons of window
  ;%   in_map_size - double(ysize,xsize) - size of window in pixels
  ;%   out_map_win - double(lat0,lon0,latn,lonn) - Lat/lons of window
  ;%
  ;% Output:
  ;%   start   - double(2) - y,x start point within in_map_win
  ;%   edge    - double(2) - ysize, xsize size of map
  ;%   lat     - lat(ysize) - latitudes of map
  ;%   lon     - lon(xsize) - longitudes of map
  ;%   iflipdim- double(array) - dimensions to flip
  ;%
  ;% Mods:
  ;% 17/1/08 J.CHallis didn't work when reading whole sst window (edge was 1 to many)
  ;% so removed 1 +
  ;% 20/5/08 J.Challis didnt work when reading sst for Europe (edge was 1 too small)
  ;% so divided by 1 less than map size

  out_map_range = out_map_win([2,3]) - out_map_win([0,1]);
  
  ;%in_map_range = [in_map_win(3) - in_map_win(1), in_map_win(4) - in_map_win(2)];
  in_map_range = in_map_win([2,3]) - in_map_win([0,1]);
  
  ;% See if in and out maps are the same way up
  ;ix_flip = (in_map_range < 0) ~= (out_map_range < 0);
  ix_flip = (in_map_range < 0) ne (out_map_range < 0);
  ;iflipdim = find(ix_flip);
  iflipdim = where(ix_flip ne 0);
  if ix_flip[0] then begin
    tmp = in_map_win[0];
    in_map_win[0] = in_map_win[2];
    in_map_win[2] = tmp;
    in_map_range[0] = -in_map_range[0];
  endif
  if ix_flip[1] then begin
    tmp = in_map_win[1];
    in_map_win[1] = in_map_win[3];
    in_map_win[3] = tmp;
    in_map_range[2] = -in_map_range[1];
  endif
  
  ;% 20/5/08 J.Challis:
  ;in_deg41pix_2 = in_map_range ./ (in_map_size-1) ./ 2;
  in_deg41pix_2 = float(in_map_range) / (in_map_size-[1,1]) / [2,2];
  
  ;start = (out_map_win([0,1]) - in_map_win([0,1])) .* in_map_size ./ in_map_range;
  start = (out_map_win([0,1]) - in_map_win([0,1])) * in_map_size / in_map_range;
  ;% 17/1/08 Was:
  ;%edge = 1 + out_map_range .* in_map_size ./ in_map_range;
  ;% 17/1/08 Now:
  ;%edge = out_map_range .* (in_map_size-1) ./ in_map_range;
  ;% 20/5/08 J.Challis:
  ;edge = 1 + out_map_range .* (in_map_size-1) ./ in_map_range;
  edge = round(1. + float(out_map_range) * (in_map_size-1) / in_map_range);
  ;% 20/5/08 J.Challis:
  ;lat = out_map_win[0] + (0.5:edge[0]-1) .* in_map_range[0] ./ (in_map_size[0]);
  ;lon = out_map_win[1] + (0.5:edge[1]-1) .* in_map_range[1] ./ (in_map_size[1]);
  lat = float(out_map_win[0]) + (0.5+indgen(edge[0]-1)) * in_map_range[0] / (in_map_size[0]);
  lon = float(out_map_win[1]) + (0.5+indgen(edge[1]-1)) * in_map_range[1] / (in_map_size[1]);
  ;lat = lat';
  lat = transpose(temporary(lat));
  return, {start:start, edge:edge, lat:lat, lon:lon, iflipdim:iflipdim}
end