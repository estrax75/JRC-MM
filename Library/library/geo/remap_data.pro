;;function [res ix_good lat lon] = remap_data(data, lat, lon, map_win, dlon, dlat)
;function remap_data(data, lat, lon, map_win, dlon, dlat)
;;% Remap data to map window using lat/lons
;;% 
;;% Initialise European window grid
;
;lat0 = map_win[0];
;lon0 = map_win[1];
;;****
;ref = makerefmat(lon0,lat0, dlon, dlat);
;
;map_range = [map_win([3,4]) - map_win([1,2])];
;map_size = round(map_range ./ [dlat dlon]) + 1;
;
;;% Find location of pixels using the aggregated lat/lons from all the
;;% SeaWiFS rois
;[row, col] = latlon2pix(ref,lat,lon);
;row = round(row);
;col = round(col);
;ix_eur = sub2ind(map_size,row,col);
;
;
;;% For each unique location, take average of all data values with same location
;;% Sorry - can't do this it takes FOREVER!!
;;%res = nan(numel(uloc),size(data,2));
;
;;%for i = 1:numel(uloc)
;;%    if mod(i,1000) == 0
;;%        disp(i)
;;%    end
;;%    res(i,:) = mean(data(ix_eur == uloc(i),:));
;;%end
;
;;% create logical index to good values
;ix_good = zeros(map_size) ~= 0;
;ix_good(ix_eur) = 1;
;
;res = nan(map_size(1) .* map_size(2), size(data,2));
;res(ix_eur,:) = data;
;res = res(ix_good,:); 
;
;;% Compute pixel centers of spatial coordinates for georeferenced image or data grid
;if nargout > 2
;  [lon,lat] = pixcenters(ref, size(ix_good),'makegrid');
;end
;
;end