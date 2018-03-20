# TO DO - check all data for correct datetime - e.g. current cont_obs are running on winter time, while all the rest of data runs on summer time
{# INFO
#ALT+0

# when birds arrive to nioz
#### !!! if blood not indicated in what, it is asumed that we have no clue whether blood was taken - if blood was taken upon capture, please indicate this, as well as whether biometry and ful, crc done
#### enter fields f_mass, project,species, age and subspecies when birds brought in
}

# Luc please install:
#install.packages('scales')	

#### START HERE
# Luc or Martin
   Luc = FALSE
   
# indicate in DB_LOG
	dblog = TRUE   
	
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
		# biometry
			o = readWorksheetFromFile(paste(wd2, 'Biometry captive red knots 2017.xlsx', sep = ''), sheet=1)
			v = readWorksheetFromFile(paste(wd2, 'morphometrics+sex_2016.xlsx', sep = ''), sheet=1)
			v$RNR[v$RNR%in%o$RINGNR]
		# locations
			g = readWorksheetFromFile(paste(wd2, 'catch_locations.xlsx', sep = ''), sheet=1)
			
	}
	{# !!!! DEFINE CONSTANTS
		catch =	c('Richel', 'Schier','Griend','Vistula', 'Mokbaai') # define off NIOZ catching locations
	}
}

# CHECK BEFORE UPLOAD
	{# prepare
	con = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbGetQuery(con, "DROP TABLE IF EXISTS CAPTURES")   
			a = dbGetQuery(con, "SELECT*FROM CAPTURES")   
			oo = dbGetQuery(con, "SELECT*FROM DBLOG where DBLOG.'table' = 'CAPTURES'")   
			dbDisconnect(con)
			
	f = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
	f2 = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
	}
	{# check WHAT entries to know whether additional uploads needed
	  l = list()
	  j =NA
	  for(i in 1:length(f)){#{2){#
	  #i = 1
		m = readWorksheetFromFile(f[i], sheet=1)
			{#### if this part does not work hashtag it out
				#m[m==""] = NA
				#m[m==" "] = NA
				#m[m=="NA"] = NA 
			####
			}	
		#print(i)
		#print(unique(m$what)[!is.na(unique(m$what))])
	
		l[[i]] = data.frame(f = i , what = if(length(unique(m$what)[!is.na(unique(m$what))])==0){NA}else{unique(m$what)[!is.na(unique(m$what))]}, stringsAsFactors = FALSE)
		j = c(j,unique(m$what)[!is.na(unique(m$what))])
		
		}
	  #ll = do.call(rbind,l)
	  #f2[i]
	  print( unique(j))
	 }
	{# check HEALTH entries to know whether additional uploads needed - FINISH CLEANING
	  l = list()
	  j =NA
	  for(i in 1:length(f)){#{2){#
	  #i = 20
		m = readWorksheetFromFile(f[i], sheet=1)
		
		#print(i)
		#print(unique(m$health)[!is.na(unique(m$health))])
		l[[i]] = data.frame(f = i , health = if(length(unique(m$health)[!is.na(unique(m$health))])==0){NA}else{unique(m$health)[!is.na(unique(m$health))]}, stringsAsFactors = FALSE)
		j = c(j,unique(m$health)[!is.na(unique(m$health))])
		}
	   #ll = do.call(rbind,l)
	  #f2[i]
	  print(unique(j))
	 }


{# UPLOAD CAPTURES
	 f = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
	 f2 = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
	#for(i in 1:length(f)){#length(f)){#{2){#
	 i = 1
	 f2[i]
		{# prepare
		
		print(i)
		m = readWorksheetFromFile(f[i], sheet=1, colTypes = 'character')
		#names(m)[names(m) == 'now'] = 'at'
		names(m)[names(m) == 'CGF'] = 'molt_col'
		names(m)[names(m) == 'pk'] = 'c_pk'
		
		print(names(a)[!names(a)%in%names(m) & !names(a)%in%c('year_')]) # names that are in the DB but not in the data_entry file (with exception of year_)
		print(names(m)[!names(m)%in%names(a) & !names(m)%in%c('m_p','f_p','home')]) # names that are in the data_entry file but not in the DB (with exception of 'm_p','f_p','home')
		
		#m$capture = as.character(m$capture)
		#m$release = as.character(m$release)
		m$year_ = substring(m$capture, 1,4)
		
		{#### if this part does not work, hashtag it out
				m[m==""] = NA
				m[m==" "] = NA
				m[m=="NA"] = NA 
			####
			}
			
		if(length(names(m)[names(m)=='c_pk'])==0){m$c_pk = NA}
		if(length(names(m)[names(m)=='pic'])==0){m$pic = NA}
		if(length(names(m)[names(m)=='with'])==0){m$with = NA}	
		#m$capture = as.POSIXct(m$capture)
		#m$release = as.POSIXct(m$release)		
		}
		{# upload to captures
			#print(names(m)[!names(m)%in%c("year_", "capture", "at","release", "where", "bird_ID", "what",  "what_ID", "health", "feet","mass",  "remarks", "author", "plum", "molt","molt_col", "L01","L02","L03","L04","L05","L06","L07","L08","L09","L10","R01","R02","R03","R04","R05","R06","R07","R08","R09","R10","crc_now", "capture_pk")])
		  mm = m[,c("year_", "capture", "at","release", "where", "bird_ID", "what",  "what_ID", "health", "feet","mass", "with", "remarks", "author", "plum", "molt","molt_col", "L01","L02","L03","L04","L05","L06","L07","L08","L09","L10","R01","R02","R03","R04","R05","R06","R07","R08","R09","R10","crc_now","pic", "c_pk")]
		  #mm$capture = as.character(mm$capture)
		  #mm$release = as.character(mm$release)
		  if(f2[i]%in%oo$remarks){print('NO UPLOAD!!! - data already in DB - see DBLOG table')}else{
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			#print(names(z)[!names(z)%in%names(mm)])
			#print(names(mm)[!names(mm)%in%names(z)])
			  dbWriteTable(con, name = "CAPTURES", value = mm, row.names = FALSE, append = TRUE)
				dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'CAPTURES', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
				dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			  dbDisconnect(con)
			  print(paste(f2[i],'uploaded to captures'))
			}
		}	
	#}
}	
{# create/update BIRDS entries 
	 f = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
	 f2 = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
	#for(i in 1:length(f)){#{2){#
	 i = 1
	{# prepare
		print(i)
		m = readWorksheetFromFile(f[i], sheet=1, colTypes = 'character')
				
		#names(m)[names(m) == 'now'] = 'at'
		names(m)[names(m) == 'CGF'] = 'molt_col'
		names(m)[names(m) == 'pk'] = 'c_pk'
		
		#print(names(a)[!names(a)%in%names(m) & !names(a)%in%c('year_')]) # names that are in the DB but not in the data_entry file (with exception of year_)
		#print(names(m)[!names(m)%in%names(a) & !names(m)%in%c('m_p','f_p','home')]) # names that are in the data_entry file but not in the DB (with exception of 'm_p','f_p','home')
		
		m$year_ = substring(m$capture, 1,4)
		
			m[m==""] = NA
			m[m==" "] = NA
			m[m=="NA"] = NA 
			
		if(length(names(m)[names(m)=='pic'])==0){m$pic = NA}
		if(length(names(m)[names(m)=='with'])==0){m$with = NA}	
		#m$capture = as.POSIXct(m$capture)
		#m$release = as.POSIXct(m$release)		
		}
	
	{# IF BIRD ARRIVES to NIOZ - create its data entry line and if data missing create TO_DO
			mm = m[m$at%in%catch | grepl("capt",m$what, perl = TRUE),]
				if(nrow(mm)==0){print('no capt in what')}else{
				  # TO_DO entry if data missing
					mass_f = length(names(mm)[names(mm)=='mass_f'])
					project = length(names(mm)[names(mm)=='project'])
					species = length(names(mm)[names(mm)=='species'])
					subspecies = length(names(mm)[names(mm)=='subspecies'])
					age = length(names(mm)[names(mm)=='age'])
					
					if((mass_f+project+species+subspecies+age) < 5){
					mx = mm[,c('capture', 'bird_ID', 'what')]
					mx$what = paste(if(mass_f==0){'mass_f'}, if(age==0){'age'},if(species==0){'species'},if(subspecies==0){'subspecies'},if(project==0){'project'}, sep =",")
					mx$capture = as.character(mx$capture)
					mx$datetime_solved = mx$remarks = mx$todo_pk = NA
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "TO_DO", value = mx[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
					print(paste('no', paste(if(mass_f==0){'mass_f'}, if(age==0){'age'},if(species==0){'species'},if(subspecies==0){'subspecies'},if(project==0){'project'},sep=","), 'column names and data entry despite capt in what in capture sheet, to do created'))					
					}
				  # add data to birds 
					mm$capture = as.character(mm$capture)
					mm$release = as.character(mm$release)
					mm$year_ = substring(mm$capture, 1,4)
					mm$blood = ifelse(grepl("blood",mm$what, perl = TRUE), 'yes',NA) #### !!! if blood not indicated in what, it is asumed that we have no clue whether blood taken
					mm$sex_method = ifelse(grepl("blood",mm$what, perl = TRUE), 'blood',NA) 
					if(length(names(mm)[names(mm)=='wing']) == 0){mm$wing = mm$bill = mm$totalhead = mm$tarsus = mm$tartoe = mm$bio_datetime = mm$bio_author = NA}else{mm$bio_datetime == mm$capture; mm$bio_author = mm$author}
					if(length(names(mm)[names(mm)=='mass_f']) == 0){mm$mass_f = NA}
					if(length(names(mm)[names(mm)=='project']) == 0){mm$project = NA}
					if(length(names(mm)[names(mm)=='subspecies']) == 0){mm$subspecies = NA}
					if(length(names(mm)[names(mm)=='species']) == 0){mm$species = NA}
					if(length(names(mm)[names(mm)=='age']) == 0){mm$age = NA}
					if(length(names(mm)[names(mm)=='height_1']) == 0){mm$muscle = mm$height_1 = mm$width_1 = mm$height_2 = mm$width_2 = mm$ful_datetime = mm$ful_author = NA}else{mm$ful_datetime == mm$capture; mm$ful_author = mm$author}
					
					mm$end_ = mm$end_type = mm$site_r = mm$bird_pk = mm$sex = mm$lat_r = mm$lon_r = mm$site_r = NA
					# UPDATE CATCHING LOCATIONS
					names(mm)[names(mm)=='capture'] = 'caught'
					names(mm)[names(mm)=='release'] = 'start_'
					names(mm)[names(mm)=='at'] = 'site_c'
					names(mm)[names(mm)=='where'] = 'current_av'
					names(mm)[names(mm)=='mass'] = 'mass_c'
					
					mm$site_c = capitalize(tolower(mm$site_c))
					
					mm$home_av = mm$current_av
					mm$crc = mm$crc_now
					mm$lat_c = g$lat[match(mm$site_c,g$abb)]
					mm$lon_c = g$lon[match(mm$site_c,g$abb)]
					
					#x = c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_","end_","end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","ful_datetime","ful_author","remarks", 'bird_pk')
					#x[!x%in%names(mm)]
					
					
					v = mm[,c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_","end_","end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","ful_datetime","ful_author","project","remarks", 'bird_pk')]
					
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "BIRDS", value = v, row.names = FALSE, append = TRUE)
					#dbGetQuery(con, "UPDATE BIRDS SET 	caught = (SELECT temp.capture FROM temp WHERE temp.bird_ID = BIRDS.bird_ID,
					#									start_ = (SELECT temp.release FROM temp WHERE temp.bird_ID = BIRDS.bird_ID,
					#									site_c = (SELECT temp.at FROM temp WHERE temp.bird_ID = BIRDS.bird_ID
					#									")
					dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'major', script = 'DB_upload.R', remarks = 'new birds', stringsAsFactors = FALSE)
					dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
					print(paste('capt info uploaded to BIRDS for', mm$bird_ID))
				}
			} 
	{# IF BIRD ENDS at NIOZ
			mm = m[m$at%in%catch | grepl("free",m$what, perl = TRUE) | grepl("died",m$what, perl = TRUE) | grepl("dead",m$what, perl = TRUE) | grepl("killed",m$what, perl = TRUE) | grepl("killed",m$health, perl = TRUE) | grepl("died",m$health, perl = TRUE) | grepl("dead",m$health, perl = TRUE),]
			if(nrow(mm) > 0){
					mm$what = ifelse(!mm$what%in%c("free","died","killed"), mm$health, mm$what)
				 	mm$release = as.character(mm$release)
					mm$type = ifelse(grepl("free",mm$what, perl = TRUE), 'released', ifelse(grepl("dead",mm$what, perl = TRUE), 'died', ifelse(grepl("died",mm$what, perl = TRUE), 'died', ifelse(grepl("killed",mm$what, perl = TRUE), 'killed', NA))))
					mm$where = capitalize(tolower(mm$where))
					mm$lat_r = g$lat[match(mm$where,g$abb)]
					mm$lon_r = g$lon[match(mm$where,g$abb)]
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbWriteTable(con, name = "temp", value = mm[,c('bird_ID','release','where','type', 'lat_r', 'lon_r')], row.names = FALSE, append = FALSE)
					
					dbExecute(con, "UPDATE BIRDS SET 	end_ = (SELECT temp.release FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														site_r = (SELECT temp.'where' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														lat_r = (SELECT temp.lat_r FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														lon_r = (SELECT temp.lon_r FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														end_type = (SELECT temp.type FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
				 
				 
				 
				 dbDisconnect(con)	
				 print(paste('end info uploaded to BIRDS', mm$bird_ID))			 
				}else{print('no free, killed, died in what or health')}
			 
			}
	{# IF WHAT = SWITCH THEN UPDATE HOME AVIARY FROM WHERE
				mm = m[which(!is.na(m$what)),]
				mm = mm[grepl("switch",mm$what, perl = TRUE),c('bird_ID', 'capture','where', 'what','home')]
				mm = ddply(mm,.(bird_ID), summarise, where = where[capture == max(capture)])
				if(nrow(mm) > 0){
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbWriteTable(con, name = "temp", value = mm, row.names = FALSE, append = FALSE)
					dbExecute(con, "UPDATE BIRDS SET 	home_av = (SELECT temp.'where' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (SELECT * FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbDisconnect(con)	
					print(paste('home_av updated in BIRDS for',mm$bird_ID))
					}else{print('no switch in what')}
			  
			 } 
	
	{# update current aviary and mass values
	   {# update current aviary
		mm = m[!grepl("obs",m$what, perl = TRUE)| !grepl("cons",m$what, perl = TRUE),]
		mm = ddply(mm,.(bird_ID), summarise, where = where[capture == max(capture)])
		mm$where = ifelse(tolower(mm$where)%in%tolower(unique(g$abb[!is.na(g$abb)])), NA, mm$where)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dbWriteTable(con, name = "temp", value = mm, row.names = FALSE, append = FALSE)
		dbExecute(con, "UPDATE BIRDS SET 	current_av = (SELECT temp.'where' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
												WHERE
												EXISTS (SELECT * FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbDisconnect(con)	
					print('current_av updated in BIRDS') 
			}
	   {# update current mass
		m2=m
		m2$mass[is.na(m2$mass)] = m2$with[is.na(m2$mass)]
		m2 = ddply(m2[!is.na(m2$mass),],.(bird_ID), summarise, mass = mass[capture == max(capture)])
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dbWriteTable(con, name = "temp", value = m2, row.names = FALSE, append = FALSE)
		dbExecute(con, "UPDATE BIRDS SET 	mass_c = (SELECT temp.'mass' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
												WHERE
												EXISTS (SELECT * FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbDisconnect(con)	
					print('mass_c updated in BIRDS') 
	}
	 }
	{# update BIRDS, if data present, or TO_DO where data missing but what is cr,blood,bio,ul,ful- note that blood means that update to SEX is needed 
			# upadate crc_now
				mm = m[!(is.na(m$crc_now)| m$crc_now%in%c('yes_flag','no_flag','no_metal','',' ')),c('bird_ID', 'crc_now')]
				if(nrow(mm) > 0){
					if(nrow(mm[!is.na(mm$crc_now),]) == 0){
						mx = mm[,c('capture', 'bird_ID', 'crc_now')]
						mx$what = 'cr'
						mx$capture = as.character(mx$capture)
						mx$datetime_solved = mx$remarks = mx$todo_pk = NA
						con = dbConnect(dbDriver("SQLite"),dbname = db)
						dbWriteTable(con, name = "TO_DO", value = mx[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
						print('cr in what but not data in crc_now, TODO created')
					}else{
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbWriteTable(con, name = "temp", value = mm[!is.na(mm$crc_now),], row.names = FALSE, append = FALSE)
					dbExecute(con, "UPDATE BIRDS SET crc_now = (SELECT temp.crc_now FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
												WHERE
												EXISTS (SELECT * FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbDisconnect(con)
					print(paste('crc_now updated in birds for',mm$bird_ID))
					}
				  }else{print('no crc_now change')}
			# update blood	
				mm = m[which(grepl("blood",m$what, perl = TRUE)) ,] 
				if(nrow(mm) > 0){
					mm = mm[,c('capture', 'bird_ID', 'what')]
					mm$what = 'sex'
					mm$capture = as.character(mm$capture)
					mm$datetime_solved = mm$remarks = mm$todo_pk = NA
					mm$blood = 'yes'
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "TO_DO", value = mm[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbWriteTable(con, name = "temp", value = mm, row.names = FALSE, append = FALSE)
					 dbExecute(con, "UPDATE BIRDS SET 	
											blood = (SELECT temp.blood FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					
					#dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'TO_DO', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
					#dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
					print(paste('blood updated in BIRDS and TO_DO for sex created', mm$bird_ID))			 
				}else{print('no blood in what')}
			# update bio
				mm = m[ which(grepl("bio",m$what, perl = TRUE) & !grepl("capt",m$what, perl = TRUE)) ,]
				if(nrow(mm)==0){print('no bio in what')}else{
					if(length(names(mm)[names(mm)=='wing']) == 0){
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					mm = mm[,c('capture', 'bird_ID', 'what','author')]
					mm$what = 'bio'
					mm$capture = as.character(mm$capture)
					#mm$author = 'jh'
					mm$datetime_solved = mm$remarks = mm$todo_pk = NA
					dbWriteTable(con, name = "TO_DO", value = mm[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbWriteTable(con, name = "temp", value = mm, row.names = FALSE, append = FALSE)
					dbExecute(con, "UPDATE BIRDS SET 	
											bio_author = (SELECT temp.author FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
											bio_datetime = (SELECT temp.capture FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					dbDisconnect(con)
					print('no bio columns in capture sheet; to do created')
					print(paste('bio_datetime and bio_author updated in BIRDS for', mm$bird_ID))							
					}else{
					 con = dbConnect(dbDriver("SQLite"),dbname = db)
					 dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					 dbWriteTable(con, name = "temp", value = mm[,c('author','capture','bird_ID','wing','bill','totalhead', 'tarsus', 'tartoe')], row.names = FALSE, append = FALSE)
					
					 dbExecute(con, "UPDATE BIRDS SET 	
														bio_author = (SELECT temp.author FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														bio_datetime = (SELECT temp.capture FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														wing = (SELECT temp.wing FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														bill = (SELECT temp.'bill' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														totalhead = (SELECT temp.totalhead FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														tarsus = (SELECT temp.tarsus FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														tartoe = (SELECT temp.tartoe FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
				 dbDisconnect(con)		
				print(paste('bio updated in BIRDS for', mm$bird_ID))							 
					}
					}
			# update cr
				mm = m[ which(grepl("cr",m$what, perl = TRUE)& !grepl("capt",m$what, perl = TRUE) & !grepl("crc",m$what, perl = TRUE)) ,]
				if(nrow(mm)==0){print('no cr in what')}else{
					if(nrow(mm[is.na(mm$crc_now),]) > 0){
					
					mm = mm[,c('capture', 'bird_ID', 'what')]
					mm$what = 'cr'
					mm$capture = as.character(mm$capture)
					mm$datetime_solved = mm$remarks = mm$todo_pk = NA
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "TO_DO", value = mm[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
					print(paste('no crc_now entry despite cr in what in capture sheet, to do created for', mm$bird_ID))					
					}else{
					 con = dbConnect(dbDriver("SQLite"),dbname = db)
					 dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					 dbWriteTable(con, name = "temp", value = mm[,c('bird_ID','crc_now')], row.names = FALSE, append = FALSE)
					
					 dbExecute(con, "UPDATE BIRDS SET 	crc = (SELECT temp.crc_now FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
				 dbDisconnect(con)		
					print(paste('crc updated in birds', mm$bird_ID))			
					}
				}
			# update ful
				mm = m[ grepl("ful",m$what, perl = TRUE) & !grepl("capt",m$what, perl = TRUE) ,]
				if(nrow(mm)==0){print('no ful in what')}else{
					if(length(names(mm)[names(mm)=='height_1']) == 0){
					mm = mm[,c('capture', 'bird_ID', 'what')]
					mm$what = 'ful'
					mm$capture = as.character(mm$capture)
					mm$datetime_solved = mm$remarks = mm$todo_pk = NA
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "TO_DO", value = mm[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
					print(paste('no ful column names and data entry despite ful in what in capture sheet, to do created', mm$bird_ID))								
					}else{
					 con = dbConnect(dbDriver("SQLite"),dbname = db)
					 dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					 dbWriteTable(con, name = "temp", value = mm[,c('author','capture','bird_ID','muscle','height_1','width_1','height_2', 'width_2')], row.names = FALSE, append = FALSE)
					
					 dbExecute(con, "UPDATE BIRDS SET 	
														ful_author =  (SELECT temp.author FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														ful_datetime = (SELECT temp.capture FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														muscle = (SELECT temp.muscle FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														height_1 = (SELECT temp.'height_1' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														width_1 = (SELECT temp.width_1 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														height_2 = (SELECT temp.height_2 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														width_2 = (SELECT temp.width_2 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
					dbGetQuery(con, "DROP TABLE IF EXISTS temp")
				 dbDisconnect(con)		
				 print(paste('ful updated in birds for', mm$bird_ID))
					}
					}
		
		}
	
	{# make entry in DB_LOG 
		con = dbConnect(dbDriver("SQLite"),dbname = db)
				dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
				dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
				dbDisconnect(con)
			}
	print(paste(f2[i],'updated BIRDS'))
			}

{# update SPECIAL tables
  
	{# prepare
		f = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
		  f2 = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
		  #f = list.files(path=paste(outdir,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
		  #f2 = list.files(path=paste(outdir,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
		  con = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbGetQuery(con, "DROP TABLE IF EXISTS CAPTURES")   
			a = dbGetQuery(con, "SELECT*FROM CAPTURES")   
			oo = dbGetQuery(con, "SELECT*FROM DBLOG where DBLOG.'table' = 'CAPTURES'")   
			dbDisconnect(con)
 i = 1
 #for(i in 1:length(f)){#{2){#
		print(i)
		m = readWorksheetFromFile(f[i], sheet=1, colTypes = 'character')
				
		#names(m)[names(m) == 'now'] = 'at'
		names(m)[names(m) == 'CGF'] = 'molt_col'
		names(m)[names(m) == 'pk'] = 'c_pk'
		
		print(names(a)[!names(a)%in%names(m) & !names(a)%in%c('year_')]) # names that are in the DB but not in the data_entry file (with exception of year_)
		print(names(m)[!names(m)%in%names(a) & !names(m)%in%c('m_p','f_p','home')]) # names that are in the data_entry file but not in the DB (with exception of 'm_p','f_p','home')

		m$year_ = substring(m$capture, 1,4)
		
			#m[m==""] = NA
			#m[m==" "] = NA
			#m[m=="NA"] = NA 
			
		if(length(names(m)[names(m)=='c_pk'])==0){m$c_pk = NA}
		if(length(names(m)[names(m)=='pic'])==0){m$pic = NA}
		if(length(names(m)[names(m)=='with'])==0){m$with = NA}	
		#m$capture = as.POSIXct(m$capture)
		#m$release = as.POSIXct(m$release)		
		}
	
	{# update BIO_TRAIN if btrain or utrain or ult in WHAT
		mm = m[ grepl("btrain",m$what, perl = TRUE) | grepl("utrain",m$what, perl = TRUE) ,]
		mm = mm[ !is.na(mm$what) ,]
		
		if(nrow(mm)>0){
			mm$datetime_ = as.character(mm$capture)
			mm$year_ = substring(mm$capture, 1,4)
		  if(TRUE%in%unique(grepl("btrain",mm$what, perl = TRUE)) & TRUE%in%unique(grepl("utrain",mm$what, perl = TRUE))){
			mm = mm[,c('year_','author', 'datetime_', 'bird_ID','wing', 'bill', 'totalhead','tarsus','tartoe','muscle','height_1','width_1','height_2','width_2')]
			mm$remarks = mm$bio_pk = NA
		   }else{ if(TRUE%in%unique(grepl("btrain",mm$what, perl = TRUE))){
				mm = mm[,c('year_','author', 'datetime_', 'bird_ID','wing', 'bill', 'totalhead','tarsus','tartoe')]
				mm$muscle = mm$height_1 = mm$width_1 = mm$height_2 = mm$width_2 = mm$remarks = mm$bio_pk = NA
				}else{ if(TRUE%in%unique(grepl("utrain",mm$what, perl = TRUE))){
				mm$wing = mm$tarsus = mm$tartoe = mm$bill = mm$totalhead = NA
				mm = mm[,c('year_','author', 'datetime_', 'bird_ID','wing', 'bill', 'totalhead','tarsus','tartoe','muscle','height_1','width_1','height_2','width_2')]
				mm$remarks = mm$bio_pk = NA
			}}}
			
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			dbWriteTable(con, name = "BIO_TRAIN", value = mm[,c('year_','author', 'datetime_', 'bird_ID','wing', 'bill', 'totalhead','tarsus','tartoe','muscle', 'height_1','width_1','height_2','width_2','remarks','bio_pk')], row.names = FALSE, append = TRUE)
			dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIO_TRAIN', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
			dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			print(paste(mm$capture,'BIO_TRAIN data added for', mm$bird_ID))			
			dbDisconnect(con)
			
			
			}else{print("no btrain or utrain in WHAT")}
	}				
	
	{# update ULTRASOUND table if UL present
				mm = m[ grepl("ul",m$what, perl = TRUE) ,]
				mm = mm[ !is.na(mm$what) ,]
				mm = mm[ !grepl("ful",mm$what, perl = TRUE) ,]
				if(nrow(mm)==0){print('no ul in what')}else{
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					u = dbGetQuery(con, "SELECT*FROM DBLOG where DBLOG.'table' = 'ULTRASOUND'")   
					dbDisconnect(con)
				if(nrow(u)==0 | !f2[i]%in%u$remarks){	
					if(length(names(mm)[names(mm)=='height_1']) == 0){
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					mm = mm[,c('capture', 'bird_ID', 'what')]
					mm$what = 'ul'
					mm$capture = as.character(mm$capture)
					mm$datetime_solved = mm$remarks = mm$todo_pk = NA
					dbWriteTable(con, name = "TO_DO", value = mm[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					dbDisconnect(con)				
					print(paste('no ul column names and data entry despite ul in what in capture sheet, to do created for', mm$bird_ID))	
					}else{
						mm$ultra_pk=NA
					 con = dbConnect(dbDriver("SQLite"),dbname = db)
					 dbGetQuery(con, "DROP TABLE IF EXISTS temp")
					 dbWriteTable(con, name = "ULTRASOUND", value = mm[,c('author','capture','bird_ID','muscle','height_1','width_1','height_2', 'width_2','remarks','ultra_pk')], row.names = FALSE, append = FALSE)
					 v = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'ULTRASOUND', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
					dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
					 dbDisconnect(con)		
					print(paste('ul added to ULTRASOUND for', mm$bird_ID))					
					}
					}else{print('NO UPLOAD!!! - data already in ULTRASOUND table - see DBLOG table')}
					}
	}				
	{# update SAMPLE table 
				mm = m[ which((grepl("blood",m$what, perl = TRUE) & !is.na(m$what_ID))| (grepl("skin",m$what, perl = TRUE) & !is.na(m$what_ID))),]
				mm = mm[ !is.na(mm$what) ,]
				if(nrow(mm)==0){print('no blood or skin in what or no what_ID')}else{
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					u = dbGetQuery(con, "SELECT*FROM DBLOG where DBLOG.'table' = 'SAMPLES'")   
					dbDisconnect(con)
				if(nrow(u)==0 | !f2[i]%in%u$remarks){
					 mm$sample_pk=NA
					 mm$datetime_=as.character(mm$capture)
					 mm$type = ifelse(grepl("blood",mm$what, perl = TRUE), 'blood', ifelse(grepl("skin",mm$what, perl = TRUE), 'skin',NA))
					 mm$where = ifelse(mm$type == 'blood', 'NIOZ','MPIO')
					 mm$remarks = NA
					 con = dbConnect(dbDriver("SQLite"),dbname = db)
					 dbWriteTable(con, name = "SAMPLES", value = mm[,c('datetime_','type','what_ID','where','remarks','sample_pk')], row.names = FALSE, append = TRUE)
					 dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'SAMPLES', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
					dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
					 dbDisconnect(con)		
					print(paste('samples added to samples for', mm$bird_ID))					
					}else{print('NO UPLOAD!!! - data already in SAMPLES table - see DBLOG table')}
					}
	}				
	
	{# update HARN table if all HARN columns present and 'neck' value entered
	  if(length(names(m)[names(m)=='neck']) == 1){
		mm = m[!is.na(m$neck) & !m$neck %in% c(""," "),]
		if(nrow(mm)>0){
		
		mm = mm[,c('capture', 'bird_ID','what', 'what_ID', 'tilt','neck','armpit','back','size')]
		mm$harn_pk= NA
		mm$capture = as.character(mm$capture)

		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "HARN", value = mm, row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'HARN', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste(mm$capture,'HARN data added for', mm$bird_ID))			
		dbDisconnect(con)
		}else{print('no harn data although neck column present')}}else{print('no harn additional data = no neck columnt')}
	}		
}	

{# MOVE THE FILE TO DONE
    file.rename(f[i], paste(outdir,f2[i], sep = ''))
		}
 print(paste('uploaded',f2[i]))
	###}	

##### AFTER UPLOAD GREY OUT THE DATA IN THE SHEETS OF VISITS FILE

{# UPLOAD VISITS	- current way or date time based ---- NA what check		
	{# prepare	
		# current visits data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM VISITS")   
			dbDisconnect(con)
			d = d[ !grepl("session", d$remarks, perl = TRUE) ,]
			if(nrow(d)> 0){d$v_pk = 1:nrow(d)}
		# current visits data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='visits')
			v$v_pk = 1:nrow(v)
			if(nrow(d)>0){
			v = v[v$v_pk>max(d$v_pk),] # select only rows that are not in DB yet
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# NAs in authors
			v[is.na(v$author),]
		# author field - show those that are not in authors
				con = dbConnect(dbDriver("SQLite"),dbname = db)
				a = dbGetQuery(con, "SELECT*FROM AUTHORS")   
				a = unique(a$initials[a$initials!=""])
				dbDisconnect(con)
						
				g = unique(unlist(strsplit(v$author, ','))) 

				g[!g%in%c(a)] # "drew"   "ih"     "ms"     "kc"     "others"
		
		# check whether 'where' field has only allowed values
				v[!v$where%in%c(paste('o', seq(1,8,1), sep=""),paste('w', seq(1,7,1), sep=""), 'wu','out', 'hall', 'front', 'back','tech','attic'),] 
		
		# datetimes
			v[is.na(v$start),] # check if start time is NA
			v[is.na(v$end),] # check if start time is NA
			
			v[which((!is.na(v$start) | !is.na(v$end)) & v$start>v$end), ] # check whether end happened before start
			
			v[which(as.numeric(difftime(v$start,trunc(v$start,"day"), units = "hours"))<6),] # visits before 6:00
			v[which(as.numeric(difftime(v$end,trunc(v$end,"day"), units = "hours"))<6),] # visits before 6:00
			
			v[which(as.numeric(difftime(v$start,trunc(v$start,"day"), units = "hours"))>22),] # visits after 22:00
			v[which(as.numeric(difftime(v$end,trunc(v$end,"day"), units = "hours"))>22),] # visits after 22:00
			
		# check rows with NA in what
			v[is.na(v$what),]
		
		# check rows with multiple what info
			#v[!v$what%in%c(NA,"check","floor","feather","food","fff", "catch", "release", "process", "clean", "bleach","clhall", "logger","harness","dummies", "things", "obs", "cons","ul"),] 
		
		# check whether all in what is defined and show the entries which are not
			 g = unique(unlist(strsplit(v$what, ',')))
			 gg = g[!g%in%c(NA,"check","dcheck","floor","feather","food","fff", "flood","catch", "release", "process", "clean", "bleach","clhall", "logger","harness","dummies", "things", "obs", "cons","ul","repair", "prep","light_off","set","water","rinse","noise")]	
			 #gg
			 if(length(gg)>0){
				for(i in 1:length(gg)){	
					print(v[grepl(gg[i],v$what, perl = TRUE),])
					}
					}else{print('no undefined what')}
		}
	{# upload
		if(nrow(v)>0){
		v$v_pk = NA
		v$start = as.character(v$start)
		v$end = as.character(v$end)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "VISITS", value = v[,c("author","where","start","what","end","comments","v_pk")], row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'VISITS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_uploda.R', remarks = '', stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste('VISITS data uploaded from', v$start[1], 'to', v$start[nrow(v)]))			
		dbDisconnect(con)
		}else{print('no new data, no upload')}
	}	
}		
{# UPLOAD CONTINUOUS OBSERVATIONS - Z080710  needs SLEEP
	{# prepare	
		# current CONS data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM CONT_OBS")   
			dbDisconnect(con)
		# current con_obs data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='continuous_observations')
			v$cont_pk = 1:nrow(v)  
			if(nrow(d)>0){
			v = v[v$cont_pk>max(d$cont_pk),] # select only rows that are not in DB yet
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# NAs in authors
			v[is.na(v$author),]
		# author field - show those that are not in authors
				con = dbConnect(dbDriver("SQLite"),dbname = db)
				a = dbGetQuery(con, "SELECT*FROM AUTHORS")   
				a = unique(a$initials[a$initials!=""])
				dbDisconnect(con)
						
				g = unique(unlist(strsplit(v$author, ','))) 

				g[!g%in%a] # "drew"   "ih"     "ms"     "kc"     "others"
		
		# check aviary
			v[!v$aviary%in%c(paste('o', seq(1,8,1), sep=""),paste('w', seq(1,7,1), sep="")),] 
		# check unique new sessions
			unique(v$session)
		
		# check if bird_ID correct
			# birds table
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			b = dbGetQuery(con, "SELECT*FROM BIRDS")   
			dbDisconnect(con)
			
			v[!v$bird_ID%in%c(b$bird_ID),]			
		
		# check that each session has only one bird_ID
			vv = ddply(v,.(session, bird_ID), summarise, n = length(bird_ID))
			vv[duplicated(vv$session),]
		
		# datetimes
			v[is.na(v$datetime_),] # check if datetime_ is NA
				
		# check rows with NA in beh
			v[is.na(v$beh),]
			
		# check whether 'beh' field has only allowed values
			v[!v$beh%in%c('sleep', 'rest', 'stand', 'preen','stretch','hop', 'hh', 'walk','fly', 'run', 'active', 'eat', 'prob', 'peck','drink', 'ruffle'),] 
		
		# sure - y,n
			v[!v$sure%in%c('n','y'),]
		
		# check whether all birds observed have rest or sleep OR not wrong time and hence too long sleep
			v=ddply(v,.(session), transform, prev = c(datetime_[1],datetime_[-length(datetime_)]))
			v$dur = difftime(v$datetime_,v$prev, units = 'secs')
			v[as.numeric(v$dur)>5*60,] # shows lines with behaviour that lasted longer than 5 min
		
			#v[v$bird_ID == 'Z080704',]

			vv = ddply(v,.(bird_ID), summarise, sleep = length(sure[beh%in%c('sleep','rest')]),dur = sum(dur[beh%in%c('sleep','rest')]))
			vv # shows duration of sleep/rest observation per bird
		}
	{# upload
		if(nrow(v)>0){
		v$cont_pk = NA
		v$dur = v$prev = NULL
		v$datetime_ = as.character(v$datetime_)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		# to CONT_OBS
			dbWriteTable(con, name = "CONT_OBS", value = v, row.names = FALSE, append = TRUE)
		# to VISITS
			names(v)[names(v)=='aviary'] = 'where'
			vv = ddply(v,.(author, where, session, bird_ID), summarise, start = min(datetime_), what = 'cons', 'general_check' = 'n',  end = max(datetime_), comments = NA)
			vv$comments = paste('session', vv$session, 'bird_ID', vv$bird_ID)
			vv$session = vv$bird_ID = NULL
			vv$v_pk = NA
			
			dbWriteTable(con, name = "VISITS", value = vv[,c("author","where","start","what","end","comments","v_pk")], row.names = FALSE, append = TRUE)
		# update DBLOG
			dv1 = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'CONT_OBS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = NA, stringsAsFactors = FALSE)
			dv2 = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'VISITS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'cons', script = 'DB_uploda.R', remarks = NA, stringsAsFactors = FALSE)
			dv = rbind(dv1, dv2)
			dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			print(paste('CONT_OBS and VISITS data uploaded from', min(as.POSIXct(v$datetime_)), 'to', max(as.POSIXct(v$datetime))))
			dbDisconnect(con)
			}else{print('no new data, no upload')}
	}	
}

{# UPLOAD AUTHORS
	{# prepare	
		# current visits data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM AUTHORS")   
			dbDisconnect(con)
		# current visits data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='authors')
			v$authors_pk = 1:nrow(v)
			if(nrow(d)>0){
			v = v[v$authors_pk>max(d$authors_pk),] # select only rows that are not in DB yet
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# NAs in initials
			v[is.na(v$initials),]
		# NAs in initials
			v[is.na(v$name),]
		# NAs in initials
			v[is.na(v$surname),]
		# NAs in contact
			v[is.na(v$contact),]		
		# alias and project
			unique(unlist(strsplit(v$alias, ','))) 
			unique(unlist(strsplit(v$project, ','))) 
		
		# datetimes
			v$start_ = as.POSIXct(v$start_, format="%Y-%m-%d")
			v$end_ = as.POSIXct(v$end_, format="%Y-%m-%d")
			v[is.na(v$start_),] # check if start time is NA
			v[is.na(v$end_),] # check if start time is NA
			
			v[which((!is.na(v$start_) | !is.na(v$end_)) & v$start_>v$end_), ] # check whether end happened before start
			
		}
	{# upload
		if(nrow(v)>0){
		v$v_pk = NA
		v$start_ = as.character(v$start_)
		v$end_ = as.character(v$end_)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "VISITS", value = v, row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'AUTHORS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'updated', script = 'DB_uploda.R', remarks = NA, stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste('AUTHORS data uploaded'))			
		dbDisconnect(con)
		}else{print('no new data, no upload')}
	}	
}		

{# UPLOAD DEVICE 
	{# prepare	
		# current CONS data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM DEVICES")   
			dbDisconnect(con)
		# current con_obs data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='devices')
			v$devices_pk = 1:nrow(v)
			if(nrow(d)>0){
			v = v[v$devices_pk>max(d$devices_pk),] # select only rows that are not in DB yet
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# datetimes
			v[is.na(v$datetime_),] # check if datetime_ is NA
		# NAs in devices
			v[is.na(v$device),]
		
		# check whether 'devices' field has only allowed values
			v[!v$device%in%c('acc', 'toa', 'harn', 'dummie'),] 
		
		# check ID
			# # of characters shall be 3
			v[nchar(v$ID)!=3,] 
		
			# fist letter
			unique(substring(v$ID,1,1))
			# numbers
			unique(substring(v$ID,2,3))
			
		# what
			v[!v$what%in%c('on', 'off', 'dd','fail'),] 
		
		# batt
			unique(v$batt) 
		}
	{# upload
		if(nrow(v)>0){
		v$devices_pk = NA
		v$datetime_ = as.character(v$datetime_)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "DEVICES", value = v, row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'DEVICES', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_uploda.R', remarks = NA, stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste('DEVICES data uploaded from', min(as.POSIXct(v$datetime_)), 'to', max(as.POSIXct(v$datetime))))	
		dbDisconnect(con)
		}else{print('no new data, no upload')}
	}	
}
{# UPLOAD AVIARIES
	{# prepare	
		# current CONS data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM AVIARIES")   
			dbDisconnect(con)
		# current con_obs data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='aviaries')
			v$av_pk = 1:nrow(v)
			if(nrow(d)>0){
			v = v[v$av_pk>max(d$av_pk),] # select only rows that are not in DB yet
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# datetimes
			v[is.na(v$datetime_),] # check if datetime_ is NA
		
		# NAs in authors
			v[is.na(v$author),]
		# author field - show those that are not in authors
				con = dbConnect(dbDriver("SQLite"),dbname = db)
				a = dbGetQuery(con, "SELECT*FROM AUTHORS")   
				a = unique(a$initials[a$initials!=""])
				dbDisconnect(con)
						
				g = unique(unlist(strsplit(v$author, ','))) 

				g[!g%in%a] # "drew"   "ih"     "ms"     "kc"     "others"
		
		# NAs in aviary
			v[is.na(v$aviary),]
			
		# check aviary
			v[!v$aviary%in%paste('w', seq(1,7,1), sep=""),] 
		
		# check light_cycle
			v[!v$light_cycle%in%c('constant','natural', '12'),] 
		
		# check T_cycle
			v[!v$T_cycle%in%c('constant_seewater','natural', '12'),] 
		
		# light and T values
			summary(v)
	}
	{# upload
		if(nrow(v)>0){
		v$av_pk = NA
		v$datetime_ = as.character(v$datetime_)
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "AVIARIES", value = v, row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'AVIARIES', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_uploda.R', remarks = NA, stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste('AVIARIES data uploaded from', min(as.POSIXct(v$datetime_)), 'to', max(as.POSIXct(v$datetime))))			
		dbDisconnect(con)
		}else{print('no new data, no upload')}
	}	
}
{# UPLOAD TAGS
	{# prepare	
		# current CONS data from DB	
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			d = dbGetQuery(con, "SELECT*FROM TAGS")   
			dbDisconnect(con)
		# current con_obs data_entry file
			v = readWorksheetFromFile(paste(wd0, 'VISITS_cons_devices_aviary_ENTRY.xlsx', sep = ''), sheet='tags')
			v$tag_pk = 1:nrow(v)
		if(nrow(d)>0){
			v = v[v$tag_pk>max(d$tag_pk),] # select only rows that are not in DB yet
			#v = v[!is.na(v$start),]
			if(nrow(v)==0){'no need to check/upload - no new data'}
			}
	}	
	{# check
		# NAs in type
			v[is.na(v$type),]
		# types
			unique(v$type)
		# NAs in coating
			v[is.na(v$coating),]
		# coating
			unique(v$coating)
		# NAs in memmory
			v[is.na(v$memmory),]
		# memmory
			unique(v$memmory)
		# batt and mass values
			summary(v)
	}
	{# upload
		if(nrow(v)>0){
		v$tag_pk = NA
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbWriteTable(con, name = "TAGS", value = v, row.names = FALSE, append = TRUE)
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'TAGS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_uploda.R', remarks = NA, stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
		print(paste('TAGS data uploaded'))			
		dbDisconnect(con)
		}else{print('no new data, no upload')}
	}	
}



##### DONE 2018-01-31 13:45:29
# if re-run needed - please use first BIRD TABLE, then above CAPTURE BIRDS update and only then the remaining 2

{# 1. BIRDS TABLE - first upload 2015 - 2017
			#con = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbGetQuery(con, "DROP TABLE IF EXISTS BIRDS")   
			#bDisconnect(con)
			# then make the table a new directly in SQLiteStudio
	{# upload EVA's catches + RUFAs
			v = readWorksheetFromFile(paste(wd2, 'morphometrics+sex_2016.xlsx', sep = ''), sheet=1)
				v$RNR[v$RNR%in%o$RINGNR]
				r = readWorksheetFromFile(paste(wd2, 'ColourRings2016.xlsx', sep = ''), sheet=1)
				r$RNR = toupper(r$RNR)
				v$colcom = r$complete_cr[match(v$RNR,r$RNR)]  
				v$colcom_now = r$actual_cr[match(v$RNR,r$RNR)]  
				v$year_ = substring(v$CatchDate,1,4)
				v$CatchLocation[v$CatchLocation == 'Vistula Mouth'] = 'Vistula'
			
			v = data.frame(year_ = v$year_, species = 'REKN', subspecies = v$Species, bird_ID = v$RNR, crc = v$colcom, crc_now = v$colcom_no, age = v$Age, sex = v$Sex, caught = v$CatchDate, site_c = v$CatchLocation, wing = v$WING, bill = v$BILL, totalhead = v$TOTHD, tarsus = v$TARS, tartoe = v$TATO, stringsAsFactors = FALSE)

			v$home_av = v$current_av = v$start_ =  v$end_ = v$end_type = v$lat_c = v$lon_c = v$lat_r = v$lon_r = v$site_r = v$muscle = v$height_1 = v$width_1 = v$height_2 = v$width_2 = v$mass_f = v$mass_c = v$bio_author = v$ful_datetime = v$ful_author = v$remarks = v$bird_pk = v$blood = v$sex_method = v$bio_datetime = NA        
			v$project = 'MigrationOnthogeny'
			#v[duplicated(v$RNR),]
			
			xx =c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_",  "end_", "end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","ful_datetime","ful_author","project","remarks", "bird_pk")
			xx[!xx%in%names(v)]
				
			v$caught = as.character(v$caught)
			v$site_c = capitalize(tolower(v$site_c))
			v$lat_c = g$lat[match(v$site_c,g$abb)]
			v$lon_c = g$lon[match(v$site_c,g$abb)]
			
			vr = data.frame(species = 'REKN', subspecies = 'ruf', bird_ID = as.character(c('982284830', '982284831')), stringsAsFactors = FALSE)
			vx = merge(v,vr,all=TRUE)
			vx = vx[,c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_",  "end_",    "end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","ful_datetime","ful_author","project","remarks", "bird_pk")]
			#vr = vx[vx$bird_ID%in%c('982284830', '982284831'),]
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			dbWriteTable(con, name = "BIRDS", value = vx , row.names = FALSE, append = TRUE)
			
			if(dblog == TRUE){
			   dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'major', script = 'DB_upload.R', remarks = '2015-2016 catches')
			
			   dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			   }
			dbDisconnect(con)
		}	
	{# upload 2017 catches (except for last one)
			
			v = readWorksheetFromFile(paste(wd2, 'Biometry captive red knots 2017.xlsx', sep = ''), sheet=1)
			v = v[which(v$Nioz == 'Yes'),]
			v$DNA = ifelse(v$DNA==TRUE, 'yes', NA)
			v$CATCH_MONTH = ifelse(nchar(v$CATCH_MONTH)==1, paste(0, v$CATCH_MONTH, sep=""), v$CATCH_MONTH)
			
				vv = v[nchar(v$ULTRASOUN)>2,]
				u1 = data.frame(measured = paste(vv$CATCH_YEAR,'08',vv$CATCH_DAY, sep = '-'), bird_ID = vv$RINGNR, muscle = vv$PECTORAL.MUSCLE, height_1 = vv$SH1 , width_1 = vv$SW1, height_2 = vv$SH2 , width_2 = vv$SW2, stringsAsFactors = FALSE)
				u1$muscle = gsub(",", ".", u1$muscle)
				u1$height_1 = gsub(",", ".", u1$height_1)
				u1$width_1 = gsub(",", ".", u1$width_1)
				u1$width_2 = gsub(",", ".", u1$width_2)
				u1$height_2 = gsub(",", ".", u1$height_2)
			
				u2 = readWorksheetFromFile(paste(wd2, 'ultrasound.xlsx', sep = ''), sheet=1)
				u2$mass = u2$age = u2$comments = u2$where = u2$released = NULL
				u2$measured = as.character(u2$measured)
				u =	rbind(u1,u2)
				v = merge(v,u, by.x = 'RINGNR', by.y = 'bird_ID', all.x = TRUE)
				
				v$bio_datetime = ifelse(v$CATCH_MONTH == '09', '2017-10-04', ifelse(v$CATCH_MONTH == '08', '2017-09-04', paste(v$CATCH_YEAR,v$CATCH_MONTH,v$CATCH_DAY, sep = '-')))
				v$start_ = ifelse(v$CATCH_MONTH == '09', '2017-09-22', NA)
				v=v[v$CATCH_MONTH != '09',]

			v = data.frame(bird_pk = v$BIOKLRI_ID, year_ = v$CATCH_YEAR, species = 'REKN', subspecies = 'isl', bird_ID = v$RINGNR, crc = v$CR_CODE, crc_now = NA, age = v$AGE, sex = NA, caught = paste(v$CATCH_YEAR,v$CATCH_MONTH,v$CATCH_DAY, sep = '-'), site_c = v$CATCH_LOCATION, wing = v$WING, bill = v$BILL, totalhead = v$TOTHD, tarsus = v$TARS, tartoe = v$TATO, mass_f = v$MASS, giz_author = 'ad', bio_author = 'jth', blood = v$DNA, muscle = v$muscle, height_1 = v$height_1, width_1 = v$width_1, height_2 = v$height_2, width_2 = v$width_2, giz_datetime = v$measured,  bio_datetime = v$bio_datetime, start_ = v$start_  , stringsAsFactors = FALSE)
			
			v$home_av = v$current_av =  v$end_ = v$end_type = v$lat_c = v$lon_c = v$lat_r = v$lon_r = v$site_r = v$mass_c  = v$remarks = v$sex_method =  NA 
			
			v$site_c = ifelse(v$site_c == 'GRIEND', 'Griend', ifelse( v$site_c == 'DE RICHEL', 'Richel', 'Schier'))
			v$lat_c = g$lat[match(v$site_c,g$abb)]
			v$lon_c = g$lon[match(v$site_c,g$abb)]
			
				x = readWorksheetFromFile(paste(wd2, 'captive_knots_2017_12+moving_2018_01.xlsx', sep = ''), sheet=1)
				x = x[x$X2 == 'Martin',]
			v$project = ifelse(v$bird_ID%in%x$ID,'SocialJetLag','MigrationOnthogeny')
				xx =c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_",  "end_", "end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","giz_datetime","giz_author","project","remarks", "bird_pk")
				xx[!xx%in%names(v)]
			
			v1 = v[c("year_","species","subspecies","bird_ID","crc","crc_now","home_av","current_av","age","sex" ,"start_",  "end_", "end_type","caught",  "lat_c","lon_c",   "site_c",  "lat_r",   "lon_r",   "site_r","muscle",  "height_1","width_1", "height_2","width_2","mass_f",  "mass_c",  "wing","bill","totalhead", "tarsus",  "tartoe",  "blood","sex_method","bio_datetime","bio_author","giz_datetime","giz_author","project","remarks", "bird_pk")]
			
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			dbWriteTable(con, name = "BIRDS", value = v1, row.names = FALSE, append = TRUE)
			if(dblog == TRUE){
			  dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'major', script = 'DB_upload.R', remarks = '2017-07 and 08 catches')
			
			  dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			}
			dbDisconnect(con)
		}	
}	

# 2. capture BIRDs above

{# 3. update FUL from file, which has to have following info 'author','measured','bird_ID','muscle','height_1','width_1','height_2', 'width_2'
	ul_date = '2017-09-23' # DEFINE
	u = readWorksheetFromFile(paste(wd2, 'ultrasound_',ul_date,'.xlsx', sep = ''), sheet=1)
	u$measured = as.character(u$measured)
	
	con = dbConnect(dbDriver("SQLite"),dbname = db)	
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")
	dbWriteTable(con, name = "temp", value = u[,c('author','measured','bird_ID','muscle','height_1','width_1','height_2', 'width_2')], row.names = FALSE, append = FALSE)
					
	dbExecute(con, "UPDATE BIRDS SET 	
								ful_author = (SELECT temp.author FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								ful_datetime = (SELECT temp.measured FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								muscle = (SELECT temp.muscle FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								height_1 = (SELECT temp.'height_1' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								width_1 = (SELECT temp.width_1 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								height_2 = (SELECT temp.height_2 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
								width_2 = (SELECT temp.width_2 FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")
	
	# update DBLOG
	x = data.frame(bird_ID = u$bird_ID, datetime_ = as.character(Sys.time()), stringsAsFactors=FALSE)
	dbWriteTable(con, name = "temp", value = x, row.names = FALSE)
	dbExecute(con, "UPDATE TO_DO SET 	
								datetime_solved = (SELECT temp.datetime_ FROM temp WHERE temp.bird_ID = TO_DO.bird_ID and TO_DO.what like '%ful%')
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = TO_DO.bird_ID and TO_DO.what like '%ful%')			
														")
					dbWriteTable(con, name = "TO_DO", value = mx[,c('capture','bird_ID','what','datetime_solved','remarks','todo_pk')], row.names = FALSE, append = TRUE)
					dbDisconnect(con)	
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")
	
	if(dblog == TRUE){
			 dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'major', script = 'DB_upload.R', remarks = 'ful of 2017-09 catch')
			  dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			   }
	dbDisconnect(con)		
	print('ful updated in birds')		
}

{# 4. DONE update biometrics and other info from JOBs DB for BIRDS 2017-09 catch DATA
			v = readWorksheetFromFile(paste(wd2, 'Biometry captive red knots 2017.xlsx', sep = ''), sheet=1)
			v = v[which(v$Nioz == 'Yes'),]
			v$DNA = ifelse(v$DNA==TRUE, 'yes', NA)
			v$CATCH_MONTH = ifelse(nchar(v$CATCH_MONTH)==1, paste(0, v$CATCH_MONTH, sep=""), v$CATCH_MONTH)
			
			v = v[v$CATCH_MONT=='09',]
					
			v$site_c = ifelse(v$CATCH_LOCATION == 'GRIEND', 'Griend', ifelse( v$CATCH_LOCATION == 'DE RICHEL', 'Richel', 'Schier'))
			v$lat_c = g$lat[match(v$site_c,g$abb)]
			v$lon_c = g$lon[match(v$site_c,g$abb)]
			v$project = 'SocialJetLag'
			v$age = ifelse(v$AGE == 3, 'A', v$AGE)
			v$bio_author = 'jh'
			v$bird_ID = v$RINGNR
			v$species = 'REKN'
			v$subspecies = 'isl'
			
			
	# UPDATE BIRDS
	con = dbConnect(dbDriver("SQLite"),dbname = db)	
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")
	dbWriteTable(con, name = "temp", value = v[,c('bio_author','bird_ID','TOTHD','BILL','WING','TARS','TATO', 'age','project','site_c','lat_c','lon_c','DNA','MASS','species','subspecies')], row.names = FALSE, append = FALSE)
		#bio_datetime = (SELECT temp.capture FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
	dbExecute(con, "UPDATE BIRDS SET 	
														bio_author = (SELECT temp.bio_author FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														species = (SELECT temp.species FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														subspecies = (SELECT temp.subspecies FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														mass_f = (SELECT temp.MASS FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														age = (SELECT temp.age FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														blood = (SELECT temp.DNA FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														project = (SELECT temp.project FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														site_c = (SELECT temp.site_c FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														lat_c = (SELECT temp.lat_c FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														lon_c = (SELECT temp.lon_c FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														wing = (SELECT temp.WING FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														bill = (SELECT temp.'BILL' FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														totalhead = (SELECT temp.TOTHD FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														tarsus = (SELECT temp.TARS FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
														tartoe = (SELECT temp.TATO FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")				
	
	# UPDATE TO_DO
	x = data.frame(bird_ID = v$bird_ID, datetime_ = as.character(Sys.time()), stringsAsFactors=FALSE)
	dbWriteTable(con, name = "temp", value = x, row.names = FALSE)
	
	dbExecute(con, "UPDATE TO_DO SET 	
								datetime_solved = (SELECT temp.datetime_ FROM temp WHERE temp.bird_ID = TO_DO.bird_ID and TO_DO.what like '%mass_f%')
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = TO_DO.bird_ID and TO_DO.what like '%mass_f%')			
														")
		
	dbGetQuery(con, "DROP TABLE IF EXISTS temp")
	
	if(dblog == TRUE){
			 dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'major', script = 'DB_upload.R', remarks = 'ful of 2017-09 bio, age, species, etc')
			  dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
			   }
	dbDisconnect(con)		
	print('bio updated in birds')			  
	}

		{# update HARN table if 'harn' or 'on'/'off' and what_ID starting with 'H', 'P','D'
		mm = m[grepl("harn",m$what, perl = TRUE)| grepl("on",m$what, perl = TRUE) & substring(m$what_ID,1,1) %in%c('H','D','P') | grepl("off",m$what, perl = TRUE) & substring(m$what_ID,1,1) %in%c('H','D','P'),] 
		mm = mm[!is.na(mm$what),]
		if(nrow(mm)==0){print('no harn in what')}else{
					if(length(names(mm)[names(mm)=='tilt']) == 0){
						mm = mm[,c('capture', 'bird_ID','what', 'what_ID')]
						mm$tilt = mm$neck = mm$armpit  = mm$back  = mm$size  = mm$harn_pk= NA
						mm$capture = as.character(mm$capture)
					}else{
						mm = mm[,c('capture', 'bird_ID', 'what','what_ID','tilt', 'neck', 'armpit','back','size')]
						mm$harn_pk=NA
						mm$capture = as.character(mm$capture)
					}
					con = dbConnect(dbDriver("SQLite"),dbname = db)
					dbWriteTable(con, name = "HARN", value = mm, row.names = FALSE, append = TRUE)
					dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'HARN', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'weekly', script = 'DB_upload.R', remarks = f2[i], stringsAsFactors = FALSE)
					dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)
					print(paste(mm$capture,'HARN data added for', mm$bird_ID))			
					dbDisconnect(con)
					}
			}	
{# 5. update positions to decimals
	v = readWorksheetFromFile(paste(wd2, 'catch_locations.xlsx', sep = ''), sheet=1)		
				
	v[v==""] = NA
	v[v==" "] = NA
	v[v=="NA"] = NA 
	
	
	#conv_unit("6 13 51", from = 'deg_min_sec', to = 'dec_deg')
	#conv_unit("5 16 40", from = 'deg_min_sec', to = 'dec_deg')
	
	#v$lat_deg = gsub('.', ' ', v$lat_deg, fixed = TRUE)
	#v$lon_deg = gsub('.', ' ', v$lon_deg, fixed = TRUE)		
	#v$lat = ifelse(is.na(v$lat_deg), v$lat, conv_unit(v$lat_deg, from = 'deg_min_sec', to = 'dec_deg')) 
	
	con = dbConnect(dbDriver("SQLite"),dbname = db)
			b = dbGetQuery(con, "SELECT*FROM BIRDS")   
			dbDisconnect(con)
				
	b$lat_c = v$lat[match(b$site_c, v$abb)]
	b$lon_c = v$lon[match(b$site_c, v$abb)]
	
	b$lat_r = v$lat[match(b$site_r, v$abb)]
	b$lon_r = v$lon[match(b$site_r, v$abb)]
	
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dbWriteTable(con, name = "temp", value = b, row.names = FALSE, append = FALSE)
		
		dbExecute(con, "UPDATE BIRDS SET 	
							lat_c = (SELECT temp.lat_c FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
							lon_c = (SELECT temp.lon_c FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
							lat_r = (SELECT temp.lat_r FROM temp WHERE temp.bird_ID = BIRDS.bird_ID),
							lon_r = (SELECT temp.lon_r FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'minor', script = 'DB_upload.R: 5. update positions to decimals', remarks = 'updated lat and lon', stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)		 
				 
		dbDisconnect(con)	

}
{# 6. update color combos
	m = read.csv(paste(wd2,'BIOKLRI.csv', sep=""), stringsAsFactors=FALSE)
	
	con = dbConnect(dbDriver("SQLite"),dbname = db)
		b = dbGetQuery(con, "SELECT*FROM BIRDS where crc is null")   
	dbDisconnect(con)
	
	b$crc = m$CR_CODE[match(b$bird_ID, m$RINGNR)]	
	#b[,c('bird_ID','crc')]
	
	con = dbConnect(dbDriver("SQLite"),dbname = db)
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dbWriteTable(con, name = "temp", value = b[,c('bird_ID','crc')], row.names = FALSE, append = FALSE)
		
		dbExecute(con, "UPDATE BIRDS SET 	
							crc = (SELECT temp.crc FROM temp WHERE temp.bird_ID = BIRDS.bird_ID)
											WHERE
												EXISTS (
													SELECT *
													FROM temp
													WHERE temp.bird_ID = BIRDS.bird_ID)			
														")
		dbGetQuery(con, "DROP TABLE IF EXISTS temp")
		dv = data.frame(pk = NA, db = 'AVESatNIOZ', table = 'BIRDS', datetime_ = as.character(Sys.time()), author = if(Luc == TRUE){'lm'}else{'mb'}, type = 'minor', script = 'DB_upload.R: update color combos', remarks = 'updated crc', stringsAsFactors = FALSE)
		dbWriteTable(con, name = "DBLOG", value = dv, row.names = FALSE, append = TRUE)		 
				 
		dbDisconnect(con)	
}			