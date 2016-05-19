@/fapar/compile
@../Library/library/ncdf_tools/compile
@/reader/compile
@/colors/compile
@/avhrr/compile
pro merge_BRF

  ;day=['01','02','03','04','05','06','07','08','09','10','11','12',$
  ;     '13','14','15','16','17','18','19','20','21','22','23','24','25',$
  ;     '26','27','28','29','30','31']
  day=string(indgen(31)+1, format='(I02)')
  month=['01','03','05','07','08','10','12']

  sensorName='AVHRR'
  sensorType='16' ;'14'
  twoDigitYear='03'

  for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, sensorName, sensorType, twoDigityear, month[m], day[k]

  ;day=['01','02','03','04','05','06','07','08','09','10','11','12',$
  ;     '13','14','15','16','17','18','19','20','21','22','23','24','25',$
  ;     '26','27','28','29','30']
  day=string(indgen(30)+1, format='(I02)')
  month=['04','06','09','11']

  for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, sensorName, sensorType, twoDigityear, month[m], day[k]

  day=string(indgen(28)+1, format='(I02)')
  month=['02']

  for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, sensorName, sensorType, twoDigityear, month[m], day[k]

END