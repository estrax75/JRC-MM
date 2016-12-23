;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='TC'
;launch_fapar_tc, 2003, 2003, 7, 7, 'L2', 1, TC_TYPE='MONTHLY', TA_TYPE='MEAN'
;launch_fapar_tc, 1999, 1999, 6, 6, 1, TC_TYPE='MONTHLY', TA_TYPE='TC', data_dir='C:\data\AVHRR\FP'
pro launch_fapar_tc_2003_all

  
  launch_fapar_tc, 2003, 2003, 6, 6, TC_TYPE='MONTHLY', TA_TYPE='TC', cloudtype=0
  ;launch_fapar_tc, 2003, 2003, 6, 6, TC_TYPE='MONTHLY', TA_TYPE='TC', cloudtype=1
  launch_fapar_tc, 2003, 2003, 6, 6, TC_TYPE='MONTHLY', TA_TYPE='TC', cloudtype=2
  launch_fapar_tc, 2003, 2003, 6, 6, TC_TYPE='MONTHLY', TA_TYPE='TC', cloudtype=3

end