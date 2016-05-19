
; @/home/melinfr/OC/MER/extract/ExtractDataAEROC.bat

; Procedures needed:
.comp /home/melinfr/OC/CAL/MER/read_hdf_data
.comp /home/melinfr/OC/CAL/MER/read_hdf_dataset

.comp /home/melinfr/OC/CAL/MER/GetMERName
.comp /home/melinfr/OC/CAL/MER/GetMERInfo

.comp /home/melinfr/OC/CAL/MER/FindPixel

.comp /home/melinfr/OC/CAL/MER/ExtractSubset

.comp /home/melinfr/OC/CAL/MER/SelectData

.comp /home/melinfr/OC/CAL/MER/ExtractData

; ----------------------------------------------------------------------

option = 1 ; 1: basic set of variables; 2: all QAA IOPs; 3: all terms for RT equation

; Site chosen for extraction of data.

k_site = 1 ; 

; directory where the data files are. Must finish with / .
in_dir = "/net/netsea1/vol/vol21_h01/data/MER_CAL/"

; directory where the non selected files will be transferred .
out_dir = "TBD"

; Size of extraction (extraction of a square of n_side x n_side pixels) with n_side = 2n_e +1
n_side = 3
 n_side = 5

; type of the files to be read
root = "M20*.L2_*C*"

ExtractData,k_site,option,n_side,in_dir,out_dir,root

; End of Program