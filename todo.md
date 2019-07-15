# TODO
* handle XML file extensions like .ext.adg.xml as .adg.xml
* FileXXX -> DataXXX
* peek ableton file type from xml data
* catch exceptions from GZip.decompress
* abletonxmlData to abletonxlText
* using String instead of Text for error messages? I think so. primitive and no need for localization
* qualified RIO.X as T exported from App?

# usage
abletonlive-xml push [REP] = 
  case repository in .xml/ (or --xml-dir) exists of
      True -> case REP argument of
                []  -> use .xml/ remote address
                remote address -> push to that address
      False -> case REP argument of 
          []  -> "not a Git repository. do you want to create one (yes|no)?
                  no -> exit
                  yes -> use Github (yes|no)
          remote address -> Git 
          


