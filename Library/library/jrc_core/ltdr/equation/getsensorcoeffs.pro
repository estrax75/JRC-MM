function getSensorCoeffs, sensor, sensorCode, SOIL=SOIL, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX

  coeffInfo=getCoeffStruct()

  CASE sensor OF
    'SEA' : BEGIN
      print,'Coefficients for seaWiFS '
      ; As in ATBD FAPAR SeaWifs (Version 2.0 - January 22,2002)
      coeffInfo.G0coeffs=[0.25130709,0.30589629,-0.0048298022,-0.32136740,0.31415914,-0.01074418]
      coeffInfo.G1coeffs=[-9.8725,-0.027458,2.9144,0.059376,10.904,0.,0.,0.,0.,0.,1.0]
      coeffInfo.G2coeffs=[-0.66956,-0.16930,-0.071256,-0.090485,-0.81353,-0.035440,-1.3438,-0.41673,-0.45123,-0.99648,0.]
      ;k,Theta,rhoc
      coeffInfo.RahmanCoeffs_BLUE=[0.56184,-0.04125,0.23265]
      coeffInfo.RahmanCoeffs_RED= [0.70535,0.03576,-0.44444]
      coeffInfo.RahmanCoeffs_NIR= [0.86644,-0.00102,0.63149]
    END
    'MER' : BEGIN
      print,'Coefficients for MERIS '
      ;
      ; 2011 VERSION
      ;
      ;Meris, as coded in BEAM
      ;
      ;
      coeffInfo.G0coeffs=[0.255,0.306,-0.0045,-0.32,0.32,-0.005]
      coeffInfo.G1coeffs=[-9.2615,-0.029011,3.2545,0.055845,9.8268,0.,0.,0.,0.,0.,1.0]
      coeffInfo.G2coeffs=[-0.47131,-0.21018,-0.045159,0.076505,-0.80707,-0.048362,-1.2471,-0.54507,-0.47602,-1.1027,0.]
      ;k,Theta,rhoc
      ;
      coeffInfo.RahmanCoeffs_BLUE=[0.56192,-0.04203,0.24012]
      coeffInfo.RahmanCoeffs_RED =[0.70879,0.037,-0.46273]
      coeffInfo.RahmanCoeffs_NIR =[0.86523,-0.00123,0.63841]
    END
    'MOD' : BEGIN
      print,'Coefficients for MODIS '
      ;Modis
      coeffInfo.G0coeffs=[0.26130709,0.33489629,-0.0038298022,-0.32136740,0.31415914,-0.010744180]
      coeffInfo.G1coeffs=[-13.860,-0.018273,1.5824,0.081450,17.092,0.,0.,0.,0.,0.,1.0]
      coeffInfo.G2coeffs=[-0.036557,-3.5399,8.3076,0.18702,-13.294,0.77034,-4.9048,-2.3630,-2.6733,-37.297,0.]

      ;k,Theta,rhoc
      coeffInfo.RahmanCoeffs_BLUE=[0.56177,-0.03204,0.13704]
      coeffInfo.RahmanCoeffs_RED =[0.70116,0.03376,-0.39924]
      coeffInfo.RahmanCoeffs_NIR =[0.86830,-0.00081,0.63537]
    END
    'OLCI' : BEGIN
      print,'Coefficients for OLCI '
      ;OLCI
      coeffInfo.G0coeffs=[0.254845,0.28550,-0.00440000,-0.322000,0.321000,-0.005079]
      coeffInfo.G1coeffs=[-9.1299,-0.028791,3.2,0.054,9.851,0.,0.,0.,0.,0.,1.0]
      coeffInfo.G2coeffs=[0.0082617,1.1027,0.64661,0.029443,-0.65340,0.19878,-0.95736,0.77296,0.054908,-1.6565,0.]
      ;
      ;     0.64295     1.02238    -0.09158
      ;    -0.35287     0.66978     0.03698
      ;k,Theta,rhoc
      coeffInfo.RahmanCoeffs_BLUE=[0.51669,-0.04434,0.30402]
      coeffInfo.RahmanCoeffs_RED =[0.66361,0.03840,-0.39471]
      coeffInfo.RahmanCoeffs_NIR =[0.86633,-0.00705,0.66537]
      ;
    END
    'AVHRR' : BEGIN
      res=getNOAAcoeff(sensorCode, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX)
      print,'looking for Coefficients for '+sensor+' '+string(sensorCode)
      ; MM 20160506 coeffs from table 9 pag. 28

      coeffInfo.G0coeffs=[res.g0Coeff1,res.g0Coeff2,res.g0Coeff3,res.g0Coeff4,res.g0Coeff5,res.g0Coeff6]

      ; MM 20160506 coeffs from table 8 pag. 28
      ; k, theta, rho
      if  keyword_set(SOIL) then begin
        coeffInfo.RahmanCoeffs_RED =[res.baresoilkiband1,res.baresoilThetahgBand1,res.baresoilRhoicBand1]
        coeffInfo.RahmanCoeffs_NIR =[res.baresoilkiband2,res.baresoilThetahgBand2,res.baresoilRhoicBand2]
      endif else begin
        coeffInfo.RahmanCoeffs_RED =[res.kiband1,res.ThetahgBand1,res.RhoicBand1]
        coeffInfo.RahmanCoeffs_NIR =[res.kiband2,res.ThetahgBand2,res.RhoicBand2]
      endelse

      ; MM 20160531 coeffs from table 8 pag. 28
      coeffInfo.soilCoeffs=[res.bareSoilsAs,res.bareSoilsBs]
    END
    'AVH': BEGIN
      res=getNOAAcoeff(sensorCode, MISSIONOVERLAPINDEX=MISSIONOVERLAPINDEX)
      print,'looking for Coefficients for '+sensor+' '+string(sensorCode)
      ; MM 20160506 coeffs from table 9 pag. 28
      coeffInfo.G0coeffs=[res.g0Coeff1,res.g0Coeff2,res.g0Coeff3,res.g0Coeff4,res.g0Coeff5,res.g0Coeff6]

      ; MM 20160506 coeffs from table 8 pag. 28
      ; k, theta, rho
      if  keyword_set(SOIL) then begin
        coeffInfo.RahmanCoeffs_RED =[res.baresoilkiband1,res.baresoilThetahgBand1,res.baresoilRhoicBand1]
        coeffInfo.RahmanCoeffs_NIR =[res.baresoilkiband2,res.baresoilThetahgBand2,res.baresoilRhoicBand2]
      endif else begin
        coeffInfo.RahmanCoeffs_RED =[res.kiband1,res.ThetahgBand1,res.RhoicBand1]
        coeffInfo.RahmanCoeffs_NIR =[res.kiband2,res.ThetahgBand2,res.RhoicBand2]
      endelse

      ; MM 20160531 coeffs from table 8 pag. 28
      coeffInfo.soilCoeffs=[res.bareSoilsAs,res.bareSoilsBs]
    END

  endcase
  return, coeffInfo
end
