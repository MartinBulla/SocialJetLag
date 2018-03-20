# Luc or Martin
   Luc = FALSE
# based on DB (TRUE) or Excel file (FALSE)
DB = TRUE
{# TOOLS
	{# define working directories
		if(Luc == TRUE){
			wd0 = "C:/Users/ldemonte/Dropbox/data_entry/"	
			wd = "C:/Users/ldemonte/Dropbox/data_entry/ready_for_DB_upload/"	
			outdir = "C:/Users/ldemonte/Dropbox/data_entry/uploaded_to_DB/"
			wd2 = "C:/Users/ldemonte/Dropbox/AVESatNIOZ/"
			}else{
				wd0 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/"	
				wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/ready_for_DB_upload/"	
				outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/uploaded_to_DB/"	
				wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/AVESatNIOZ/"	
			}
	}		
	{# load packages
		require(plyr)
		require(XLConnect)
		require("RSQLite")
		#require("DBI")
		require('Hmisc')
	}
	{# DB connection
		db=paste(wd2,"AVESatNIOZ.sqlite",sep="")
		#db=paste(wd2,"test.sqlite",sep="")
		#db=paste(wd2,"test2.sqlite",sep="")
	}
	{# metadata
		# birds table
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			b = dbGetQuery(con, "SELECT*FROM BIRDS")   
			dbDisconnect(con)
		# captures table
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			z = dbGetQuery(con, "SELECT*FROM CAPTURES")   
			dbDisconnect(con)
			
	}
	{# !!!! DEFINE CONSTANTS
		catch =	c('Richel', 'Schier','Griend','Vistula', 'Mokbaai') # define off NIOZ catching locations
	}
}
{# PREPARE DATA
   if(DB==TRUE){
	con = dbConnect(dbDriver("SQLite"),dbname = db)
			v = dbGetQuery(con, "SELECT*FROM CONT_OBS")   
			
	dbDisconnect(con)
	}else{v = readWorksheetFromFile(paste(wd0,'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='continuous_observations', colTypes = 'character')}
	
	v=ddply(v,.(session), transform, after = c(datetime_[-1],datetime_[length(datetime_)]), prev = c(datetime_[1],datetime_[-length(datetime_)]))
	
		v$dur = difftime(v$after,v$datetime_, units = 'secs')
		
			
			#v[v$bird_ID == 'Z078553' ,]

		vv = ddply(v,.(bird_ID), summarise, sleep = length(sure[beh%in%c('sleep','rest')]),dur = sum(dur[beh%in%c('sleep','rest')]))
		vv$current_av = b$current_av[match(vv$bird_ID,b$bird_ID)] #b[b$bird_ID %in% vv$bird_ID[vv$sleep == 0],c('bird_ID','current_av')] # shows the current aviaries of the above birds		
		vv$home_av = b$home_av[match(vv$bird_ID,b$bird_ID)]
}		
 # CHECK length of single beh instances
	v[as.numeric(v$dur)>5*60,] # shows lines with behaviour that lasted longer than 5 min
 # OBSERVED, but need sleep observations	
	vv # shows duration of sleep/rest observation per bird
	vv[vv$sleep == 0, ]  # shows birds with ZEROE sleep/rest observation 

 # BIRDS LACKING ANY OBSERVATION
	x = unique(z$bird_ID[grepl('on',z$what, perl = TRUE)])
	aa = ddply(z[which(z$bird_ID%in%x & substring(z$what_ID,1,1)=='A'),],.(bird_ID), summarise, on = max(capture[grepl('on',what, perl = TRUE)]) , off = ifelse(length(capture[grepl('off',what, perl = TRUE)])==0, NA,max(capture[grepl('off',what, perl = TRUE)])))
	aa  = aa[is.na(aa$off) | aa$off<aa$on,]
	
	aa = aa[!aa$bird_ID%in%vv$bird_ID,]
	aa$current_av = b$current_av[match(aa$bird_ID,b$bird_ID)] 
	aa$home_av = b$home_av[match(aa$bird_ID,b$bird_ID)]
	aa
	aa$bird_ID