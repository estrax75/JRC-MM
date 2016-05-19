function str, stringV

 return, strcompress(stringV, /REMOVE_ALL)

end

function strn, number, LENGTH = length, PADTYPE = padtype, PADCHAR = padchar, $
                       FORMAT = Format
;+
; NAME:
; STRN
; PURPOSE:
; Convert a number to a string and remove padded blanks.
; EXPLANATION:
; The main and original purpose of this procedure is to convert a number
; to an unpadded string (i.e. with no blanks around it.)  However, it 
; has been expanded to be a multi-purpose formatting tool.  You may 
; specify a length for the output string; the returned string is either 
; set to that length or padded to be that length.  You may specify 
; characters to be used in padding and which side to be padded.  Finally,
; you may also specify a format for the number.  NOTE that the input 
; "number" need not be a number; it may be a string, or anything.  It is
; converted to string.
;
; CALLING SEQEUNCE:
; tmp = STRN( number, [ LENGTH=, PADTYPE=, PADCHAR=, FORMAT = ] )
;
; INPUT:
; NUMBER    This is the input variable to be operated on.  Traditionally,
;    it was a number, but it may be any scalar type.
;
; OPTIONAL INPUT:
; LENGTH    This KEYWORD specifies the length of the returned string.  
;   If the output would have been longer, it is truncated.  If 
;   the output would have been shorter, it is padded to the right 
;   length.
; PADTYPE   This KEYWORD specifies the type of padding to be used, if any.
;   0=Padded at End, 1=Padded at front, 2=Centered (pad front/end)
;   IF not specified, PADTYPE=1
; PADCHAR   This KEYWORD specifies the character to be used when padding.
;   The default is a space (' ').
; FORMAT    This keyword allows the FORTRAN type formatting of the input
;   number (e.g. '(f6.2)')
;
; OUTPUT:
; tmp       The formatted string
;
; USEFUL EXAMPLES:
; print,'Used ',strn(stars),' stars.'  ==> 'Used 22 stars.'
; print,'Attempted ',strn(ret,leng=6,padt=1,padch='0'),' retries.'
;   ==> 'Attempted 000043 retries.'
; print,strn('M81 Star List',length=80,padtype=2)
;   ==> an 80 character line with 'M81 Star List' centered.
; print,'Error: ',strn(err,format='(f15.2)')
;   ==> 'Error: 3.24'     or ==> 'Error: 323535.22'
;
; HISTORY:
; 03-JUL-90 Version 1 written by Eric W. Deutsch
; 10-JUL-90 Trimming and padding options added         (E. Deutsch)
; 29-JUL-91 Changed to keywords and header spiffed up     (E. Deutsch)
; Ma7 92 Work correctly for byte values (W. Landsman)
; 19-NOV-92 Added Patch to work around IDL 2.4.0 bug which caused an
; error when STRN('(123)') was encountered.            (E. Deutsch)
; Converted to IDL V5.0   W. Landsman   September 1997
;       Handles array input, M. Sullivan March 2014
;       Use V6.0 notation W. Landsman April 2014
;-
 On_error,2
  if ( N_params() LT 1 ) then begin
    print,'Call: IDL> tmp=STRN(number,[length=,padtype=,padchar=,format=])'
    print,"e.g.: IDL> print,'Executed ',strn(ret,leng=6,padt=1,padch='0'),' retries.'"
    return,''
    endif
  if (N_elements(padtype) eq 0) then padtype=1
  if (N_elements(padchar) eq 0) then padchar=' '
  if (N_elements(Format) eq 0) then Format=''

  padc = byte(padchar)
  pad = string(replicate(padc[0],200))

  tmp=STRARR(N_ELEMENTS(number))
  FOR i=0L,N_ELEMENTS(number)-1 DO BEGIN
     ss=size(number[i]) & PRN=1 & if (ss[1] eq 7) then PRN=0
     if ( Format EQ '') then tmp[i] = strtrim( string(number[i], PRINT=PRN),2) $
     else tmp[i] = strtrim( string( number[i], FORMAT=Format, PRINT=PRN),2)
     
     if (N_elements(length) eq 0) then length=strlen(tmp[i])
     
     if (strlen(tmp[i]) gt length) then tmp[i]=strmid(tmp[i],0,length)
     
     if (strlen(tmp[i]) lt length) && (padtype eq 0) then begin
        tmp[i] += strmid(pad,0,length-strlen(tmp[i]))
     endif
     
     if (strlen(tmp[i]) lt length) && (padtype eq 1) then begin
        tmp[i] = strmid(pad,0,length-strlen(tmp[i]))+tmp[i]
     endif
     
     if (strlen(tmp[i]) lt length) and (padtype eq 2) then begin
        padln=length-strlen(tmp[i]) & padfr=padln/2 & padend=padln-padfr
        tmp[i]=strmid(pad,0,padfr)+tmp[i]+strmid(pad,0,padend)
     endif
  endfor
;;Return an array if passed an array, or not if not
  IF ( SIZE(number,/DIMENSION) EQ 0 ) THEN RETURN,tmp[0] ELSE RETURN,tmp
end

;
;+
; NAME:
;        SIGFIG
;
;
; PURPOSE:
;        Accept a scalar numerical value or an array of numbers and
;        return the numbers as strings with the specified number of
;        significant figures.
;
; CALLING SEQUENCE:
;        RESULT = SigFig(Number, Nfig [, /SCIENTIFIC, /PLUSSES, /NUMERICAL)
;
; INPUTS:
;        Number - Scalar or array of numerical values (float, double, int)
;        Nfig   - Number of signficant figures desired in the output
;
; OUTPUTS:
;        String representation of the input with the specified number
;        of signficant figures.
;
; KEYWORD PARAMTERS:
;        /SCIENTIFIC - return the numbers in scientific notation
;        /PLUSSES    - Include plus signs for positive numbers
;        /NUMERICAL  - Return numerical, rather than string, values
;
; RESTRICTIONS:
;        This only works with IDL 6.0 or later. Use Erik Rosolosky's
;        SIGFIG for earlier versions of IDL.
;
; EXAMPLE:
;        IDL> print, sigfig(-0.0001234, 2)
;        -0.00012
;        IDL> print, sigfig(1.234, 1)
;        1.
;        IDL> print, sigfig(1234, 1)
;        1000
;        IDL> print, sigfig(-0.0001234, 2, /sci)
;        -1.2e-4
;        IDL> print, sigfig(1234, 2, /plus)
;        +1200
;        IDL> print, sigfig(1234, 2, /plus, /sci)
;        +1.2e+3
;
; MODIFICATION HISTORY:
; Inspired long ago by Erik Rosolowsky's SIGFIG:
;     http://www.cfa.harvard.edu/~erosolow/idl/lib/lib.html#SIGFIG
;
; This version written by JohnJohn Sept 29, 2005
;-

;;; SF_STR - The way STRING() should behave by default
function sf_str, stringin, format=format
  return, strcompress(string(stringin, format=format), /rem)
end

;;; SF_TRANS_DEC - TRANSlate the DECimal point in a number of order
;;;                unity, round it, and translate back.
function sf_trans_dec, numin, nsigin, order_inc=order_inc
  nel = n_elements(numin)
  
  ;;; Double precision can't handle more than 19 sig figs
  nsig = nsigin < 19
  
  ;;; Gonna have to move the decimal nsig-1 places to the right before rounding
  move = nsig-1
  len = max(strlen(numin))
  
  ;;; Pad with zeros if the move is too large
  move = move < (len-1)
  
  ;;; Create a string with just the digits, no decimal
  ;nodec = strjoin(strsplit(num,'.',/ext), '')
  nodec = strmid(numin, 0, 1)+strmid(numin, 2, len)
  
  ;;; Move the decimal, so nsig digits are to the left of the new
  ;;; decimal position
  num0 = strmid(nodec,0,1+move)+'.'+strmid(nodec,1+move,len)
  
  ;;; Round the new number
  num1 = strcompress(round(double(num0),/l64), /rem)
  len1 = strlen(num1)
  
  ;;; If the number needs to be rounded up to 10., then set the
  ;;; order_inc keyword so the calling routine knows to add one to the
  ;;; order of magnitude
  test1 = str(round(double(num0)))
  test2 = str(fix(double(num0)))
  cond = strmid(test1, 0, 1) eq '1' and strmid(test2, 0, 1) eq '9'
  order_inc = fltarr(nel)
  w = where(cond, nc)
  if nc gt 0 then order_inc[w] = 1
  ;;; Move the decimal back and return to sender
  num  = strmid(num1, 0, 1)+'.'+strmid(num1, 1, nsig-1)
  return, num
end

function sigfig, NumIn, Nfig $
    , string_return=string_return $
    , scientific=scientific $
    , numerical=numerical $
    , plusses=plusses
    
  Num = double(NumIn)
  Nel = n_elements(Num)
  
  ;;; Convert the input number to scientific notation
  TestString = sf_str(abs(double(Num)), format='(e)')
  Epos = strpos(TestString[0], 'e')
  
  ;;; Test sign of the order
  Osign = intarr(Nel)+1
  StrOsign = strmid(TestString, Epos+1, 1)
  Wneg = where(strosign eq '-', Nneg)
  if Nneg gt 0 then Osign[Wneg] = -1
  
  ;;; Test sign of numbers, form string of minus signs for negative vals
  NegSign = strarr(Nel) + (keyword_set(plusses) ? '+' : '')
  Negative = where(Num lt 0, Nneg)
  if Nneg gt 0 then NegSign[Negative] = '-'
  
  ;;; What's the order of magnitude of the values?
  Order = fix(sf_str(strmid(TestString, Epos+2, 2)))
  
  ;;; Initialize some parameters
  ;;; Make all values of order unity for rounding
  NumUnit = strmid(teststring,0,epos)
  
  ;;; Use TRANS_DEC to round unit values
  NumTrans = sf_trans_dec(NumUnit, Nfig)
  Len = strlen(NumTrans[0])
  
  if keyword_set(numerical) then begin
    NumRound = NegSign+NumTrans+'e'+StrOsign+sf_str(order)
    return, double(NumRound)
  endif
  if keyword_set(scientific) then begin
    NumRound = NegSign+NumTrans+'e'+StrOsign+sf_str(order)
    return, NumRound
  endif
  
  ;;; Make all values of order unity for rounding
  NumUnit = strmid(teststring,0,epos)
  
  ;;; Use TRANS_DEC to round unit values
  NumTrans = sf_trans_dec(NumUnit, Nfig, order_inc=order_inc)
  order += order_inc
  
  ;;; Remove decimal point
  NumNoDec = strmid(NumTrans,0,1)+strmid(NuMTrans,2,Len)
  
  ;;; Initialize the output array
  NumRound = strarr(Nel)
  
  ;;; There are four cases to test:
  
  w = where(order eq 0, nw)
  if nw gt 0 then NumRound[w] = NegSign[w]+NumTrans[w]
  
  w = where(order eq 1 and osign lt 0, nw)
  if nw gt 0 then NumRound[w] = NegSign[w]+'0.'+NumNoDec[w]
  
  w = where(order gt 1 and osign lt 0, nw)
  if nw gt 0 then begin
    Dif = order[w] - 1
    NumRound[w] = NegSign[w]+'0.'+strjoin(strarr(Dif)+'0','')+NumNoDec[w]
  endif
  
  w = where(order lt Len - 1 and osign gt 0, nw)
  if nw gt 0 then begin
    NumRound[w] = NegSign[w]+strmid(NumNoDec[w], 0, transpose(order[w])+1)
    w1 = where(len gt order[w]+1, nw1, comp=comp)
    if nw1 gt 0 then $
      NumRound[w[w1]] += '.'+strmid(NumNoDec[w[w1]], transpose(order[w[w1]])+1, Len)
  endif
  
  w = where(order ge Len - 1 and osign gt 0, nw)
  if nw gt 0 then begin
    NumNoDec += strjoin(strarr(order[w]-Len+2)+'0','')
    NumRound[w] = NegSign[w]+strmid(NumNoDec[w], 0, transpose(order[w])+1)
    w1 = where(len gt order+1, nw1, comp=comp)
    if nw1 gt 0 then $
      NumRound[w[w1]] += '.'+strmid(NumNoDec[w[w1]], transpose(order[w[w1]])+1, Len)
  endif
  
  ;;; Return an array or a scalar depending on input
  if Nel eq 1 then return,NumRound[0] else return, NumRound
end

function strnsignif,number,digits
  ;+
  ; NAME:
  ; STRNSIGNIF
  ; PURPOSE:
  ; Convert a number to a string with a fixed number of significant digits.
  ; EXPLANATION:
  ; Similar to strn(), this function converts a number to a string, but,
  ; unlike strn(), with a fixed number of significant digits.
  ;
  ; CALLING SEQEUNCE:
  ; tmp = STRNSIGNIF( number, digits )
  ;
  ; INPUT:
  ; NUMBER   This is the input number to be converted to a string.
  ;
  ; OUTPUT:
  ; tmp       The formatted string
  ;
  ; EXAMPLES:
  ; IDL> print,strnsignif(12345.6789,3)
  ; 12300
  ; IDL> print,strnsignif(1.23456789,3)
  ; 1.23
  ; IDL> print,strnsignif(.00123456789,3)
  ; 0.00123
  ;
  ; HISTORY:
  ; 1999-03-29 Version 1 written by Eric W. Deutsch & Brooke Skelton
  ;-


  if (n_params(0) lt 2) then begin
    print,'Call> str=strnsignif(number,digits)'
    print,'e.g.> str=strnsignif(33486.22,3)'
    return,'***'
  endif
  
  expon=fix(alog10(number))
  if (number lt 1) then expon=expon-1
  
  c=round(number/10.0^(expon-(digits-1)))*10.0^(expon-(digits-1))
  
  if (c gt 10^(digits-1)) then d = strn(round(c)) $
  else d = strn(string(c,format='(f20.'+strn(digits-1-expon)+')'))
  
  return,d
  
end
