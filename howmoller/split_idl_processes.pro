;do_faparComp_Howmuller, 1, 1999, 14, 'FAPAR_DIFF', 'FAPAR_DIFF', 'HDF', '/space3/storage/products/AVHRR_LDTR', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='NONE', TC_TYPE='DAILY'
;do_faparComp_Howmuller, 7, 2003, 16, 'FAPAR_DIFF', 'FAPAR_DIFF', 'NC', '/space3/storage/products/results/FAPAR/COMPARISONS', '/space3/storage/products/results/FAPAR/COMPARISONS', eps=EPS, TA_TYPE='MEAN', TC_TYPE='MONTHLY'
;pro SPLIT_IDL_PROCESSES

 !quiet=1
 argc = COMMAND_LINE_ARGS( COUNT=count )
 cd, '/home/mariomi/IDLWorkspace85/howmoller'
 restore, 'launch_howmoller.sav'
 launch_BRF_AVHRR_Howmuller, fix(argc[0]), fix(argc[1]), argc[2]
 exit

;end