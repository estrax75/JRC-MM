function preproc_v3, x, mu, s, ndata

; Data pre-processing
return, (x-replicate(1, ndata)##mu)/(replicate(1, ndata)##s)

end
;_____________________________________________________________________________

function postproc_v3, x, mu, s, ndata

; Data post-processing
return, x*(replicate(1, ndata)##s)+replicate(1, ndata)##mu

end
;_____________________________________________________________________________

function dataprod_v3, model, x

; Variables definition
ndata = n_elements(x[0, *])
ncol = n_elements(x[*, 0])
ones = replicate(1., ndata)

; Novelty detection
scored = (x - ones##model.muIn)##transpose(model.pcvecs)
scaled = scored / (ones ## sqrt(model.pcvals))
; novelty = sqrt(scaled(0, *)^2 + scaled(1, *)^2 + scaled(2, *)^2)
novelty = transpose(sqrt(total(scaled^2, 1))/ncol)

; Data product
values = 10^postproc_v3(tanh(preproc_v3(x, model.muIn, model.stdIn, ndata)##transpose(model.par.w1)+ones##model.par.b1)##transpose(model.par.w2)+ones##model.par.b2, model.muOut, model.stdOut, ndata)

; Return results
return, [values, novelty]

end
;_____________________________________________________________________________

function dataread_v3, filename, pixelnumber=pixelnumber

; Read data from file
row = ''
openr, lun, filename, /get_lun
readf, lun, row
row = strcompress(row)
tmp = str_sep(row, ',')
nCol = n_elements(tmp)
if n_elements(pixelNumber) eq 0 then pixs=200000L else pixs=long(pixelNumber)
readData = dblarr(nCol, pixs, /NOZERO)

iRow = 0l
while not eof(lun) do begin
   readf, lun, row
   row = strcompress(row)
   tmp = str_sep(row, ',')
   readData[0:nCol - 1, iRow] = tmp[0:nCol - 1]
   iRow = iRow + 1l
endwhile
free_lun, lun
return, readData[*, 0:iRow - 1]

end
;_____________________________________________________________________________

pro blks_v3, sensor, wavelengths, prod

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;NAME
; blks
;
;OVERVIEW
;
; This IDL procedure is provided in comply with requirements and specifications of the Service Contract N. CCR.IES.C390C391935.X0. Please refer to the companion pdf document for details.
;
;INPUTS
;
; sensor: MERIS (code name: mer), MODIS (mod), SeaWiFS (swf).
;
; wavelengts: set of central wavebands to be used to derive data products
;
; prod (data product name): chla (code name: chlideachla), total suspended matter (tsm) and
; yellow substance absorption at 412 nm (ay412).
;
;EXECUTION
;
;For testing and operational applications proceed as follows:
; 1) Extract the content of the \texttt{blks.zip} file in a folder (e.g., c:/projects/idl/test). Set the IDL working directory to that folder  (enter cd, 'c:/projects/idl/test'
; in the IDL command line, note the comma after cd and that the path is within quotes).
; 2) Enter the following command in the IDL prompt:  blks, `mer', [413, 443, 490, 510, 560, 665], 'chlideachla'}. Note that the arguments sensor ( 'mer'}), wavelengths (e.g.,
; [413, 443, 490, 510, 560, 665]}) and  product ('chlideachla'}) used  as input to the procedure blks can be any combination set. Input data for testing the blks.pro procedure
; in the file sit-nlw.dat (the data header specifies column entries).
;----------------------------------------------------------------------------

; Input data
data = dataread_v3('./insituRrsModWaves.dat', pixelnumber=pixelnumber)
nData = n_elements(data[0, *])

; Input file with the coefficients of the Black Sea regional algorithms
algoFile = './mlp-rrs.h5'
strc = h5_parse(algoFile, /read_data)

; Central bands for the selected sensor
case sensor of
   'swf': begin
      centralBands = [412, 443, 490, 510, 555, 670]
   end
   'mod': begin
      centralBands = [412, 443, 488, 530, 555, 667]
   end
   'mer': begin
      centralBands = [413, 443, 490, 510, 560, 665]
   end
endcase

; Wavelength subscription
lambdaIdx = [0, 0, 0, 0, 0, 0]
aopSet = sensor
for iCW = 0, n_elements(centralBands)-1 do begin
   if min(abs(centralBands[iCW] - wavelengths)) eq 0 then begin
      aopSet = aopSet + strcompress(string(centralBands[iCW]), /remove_all) + '_' + sensor
      lambdaIdx[iCW] = 1
   end
end
aopSet = strmid(aopSet, 0, strlen(aopSet) - 4)

; Tag names of the MLP in the archive file
algoArchive=tag_names(strc)

; MLP_BLKS products and novelty index
algoName = strupcase('blks_' + aopSet + '_to_' + prod)
algoIdx=where(strcmp(algoArchive, algoName) eq 1)
mlpBlks = strc.(algoIdx)._data
res = dataprod_v3(mlpBlks, alog10(data(where(lambdaIdx eq 1), *)))

; MLP_BLK1 products and novelty index
algoName = strupcase('blk1_' + aopSet + '_to_' + prod)
algoIdx=where(strcmp(algoArchive, algoName) eq 1)
mlpBlk1 = strc.(algoIdx)._data
res1 = dataprod_v3(mlpBlk1, alog10(data(where(lambdaIdx eq 1), *)))

; MLP_BLK2 products and novelty index
algoName = strupcase('blk2_' + aopSet + '_to_' + prod)
algoIdx=where(strcmp(algoArchive, algoName) eq 1)
mlpBlk2 = strc.(algoIdx)._data
res2 = dataprod_v3(mlpBlk2, alog10(data(where(lambdaIdx eq 1), *)))

; MLP_BLK1 and MLP_BLK2 combined results
thresh1 = 1.08
thresh2 = 1.25
resC = fltarr(1, n_elements(res(0,*)))
idx1 =  where((res1[1, *] le thresh1))
idx2 =  where((res2[1, *] le thresh2))
idxC =  where((res1[1, *] le thresh1) and (res2[1, *] le thresh2))
resC(idx1) = res1(0, idx1)
resC(idx2) = res2(0, idx2)
w1 = 1 / (10 ^ res1(1, idxC))
w2 = 1 / (10 ^ res2(1, idxC))
resC(idxC) = (w1 * res1(0, idxC) + w2 * res2(0, idxC)) / (w1 + w2)

; Display results
print, [res, res1, res2, resC]

end
;_____________________________________________________________________________
