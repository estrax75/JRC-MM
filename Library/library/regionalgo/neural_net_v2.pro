;call personal phone
function preproc_v2, x, mu, s, ndata

; Data pre-processing
return, (x-replicate(1, ndata)##mu)/(replicate(1, ndata)##s)

end
;_____________________________________________________________________________

function postproc_v2, x, mu, s, ndata

; Data post-processing
return, x*(replicate(1, ndata)##s)+replicate(1, ndata)##mu

end
;_____________________________________________________________________________

function dataprod_v2, model, x

; Variables definition
ndata = n_elements(x[0, *])
ncol = n_elements(x[*, 0])
ones = replicate(1., ndata)

; Novelty detection
scored = (x - ones##model.muIn)##transpose(model.pcvecs)
scaled = scored / (ones ## sqrt(model.pcvals))
; novelty = sqrt(scaled(0, *)^2 + scaled(1, *)^2 + scaled(2, *)^2)
if ncol eq n_elements(scaled) then begin
    scaled=reform(scaled, ncol, 1)
    ;stop
endif
novelty = transpose(sqrt(total(scaled^2, 1))/ncol)

; Data product
values = 10^postproc_v2(tanh(preproc_v2(x, model.muIn, model.stdIn, ndata)##transpose(model.par.w1)+ones##model.par.b1)##transpose(model.par.w2)+ones##model.par.b2, model.muOut, model.stdOut, ndata)

; Return results
return, [values, novelty]

end
;_____________________________________________________________________________

;function dataread, filename
function dataread_v2, filename, pixelNumber=pixelNumber

; Read data from file
row = ''
openr, lun, filename, /get_lun
readf, lun, row
row = strcompress(row)
tmp = str_sep(row, ',')
nCol = n_elements(tmp)
;data = dblarr(nCol, 10000)
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

pro blks, sensor, wavelengths, prod

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
data = dataread_v2('./insituNlwMeasWaves.dat')
nData = n_elements(data[0, *])

; Input file with the coefficients of regional algorithms
algoFile = './mlp-nlw.h5'
strc = h5_parse(algoFile, /read_data)

; Lwn central bands for the selected sensor
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
nlwSet = sensor
for iCW = 0, n_elements(centralBands)-1 do begin
   if min(abs(centralBands[iCW] - wavelengths)) eq 0 then begin
      nlwSet = nlwSet + strcompress(string(centralBands[iCW]), /remove_all) + '_' + sensor
      lambdaIdx[iCW] = 1
   end
end
nlwSet = strmid(nlwSet, 0, strlen(nlwSet) - 4)

; MLP products and novelty index
algoName = strupcase('blks_' + nlwSet + '_to_' + prod)
algoArchive=tag_names(strc)
algoIdx=where(strcmp(algoArchive, algoName) eq 1)
model = strc.(algoIdx)._data
res = dataprod_v2(model, alog10(data(where(lambdaIdx eq 1), *)))
print, res

end
;_____________________________________________________________________________
