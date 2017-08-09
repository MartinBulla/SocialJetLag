library(RODBC) 
db = "C:/Users/mbulla/test.accdb" #db = "Z:/2016-2017 Birds.accdb"
mycon = odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/mbulla/test.accdb")
con2 = odbcConnect(db)
channel - odbcConnectAccess("C:/Users/mbulla/test")