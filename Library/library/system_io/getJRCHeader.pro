function getJRCHeader

  date=systime()
  
  return, { $
    title: '-', $
    source: 'JRC Elaboration from NASA data (LDTR/AVHRR)', $
    versionNumber: '1.1', $
    versionDate: '2016/06/16', $
    author: 'Ext. Consultant: Mirko Marioni', $
    institute: 'UE - JRC Unit H05', $
    conventions: '-', $
    date: date $
  }

end