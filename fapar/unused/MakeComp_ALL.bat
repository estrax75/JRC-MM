.rnew read_seasonal_avhrr_surface.pro
.rnew FaparColor.pro
.rnew PPMSA_AlbedoColor.pro
.rnew fapar_uncertainties.pro

.rnew makfaparwithavhrrV2016.pro
.rnew rectified_uncertainties.pro

day=['01','02','03','04','05','06','07','08','09','10','11','12',$
     '13','14','15','16','17','18','19','20','21','22','23','24','25',$
     '26','27','28','29','30','31']
month=['01','03','05','07','08','10','12']

for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, 'AVHRR', '16', '03', month(m), day(k)

day=['01','02','03','04','05','06','07','08','09','10','11','12',$
     '13','14','15','16','17','18','19','20','21','22','23','24','25',$
     '26','27','28','29','30']
month=['04','06','09','11']

for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, 'AVHRR', '16', '03', month(m), day(k)

day=['01','02','03','04','05','06','07','08','09','10','11','12',$
     '13','14','15','16','17','18','19','20','21','22','23','24','25',$
     '26','27','28']
month=['02']

for m=0, 11 do  for k=0, N_elements(day)-1  do makeitglob, 'AVHRR', '16', '03', month(m), day(k)

