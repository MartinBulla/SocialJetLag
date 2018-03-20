 ##### ESSENTIAL ##### if new defnitions in columns (e.g. in what) please add them to relevant lines of code
 
 # Luc or Martin
   Luc = FALSE

{# TOOLS
	{# define working directories
		if(Luc == TRUE){
			wd = "C:/Users/ldemonte/Dropbox/data_entry/ready_for_checks/"	
			outdir = "C:/Users/ldemonte/Dropbox/data_entry/ready_for_DB_upload/"
			wd2 = "C:/Users/ldemonte/Dropbox/AVESatNIOZ/"
			}else{
				wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/ready_for_checks/"	
				#wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/ready_for_DB_upload"	
				outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/data_entry/ready_for_DB_upload/"	
				wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/AVESatNIOZ/"	
			}
	}		
	{# load packages
		require(plyr)
		require(XLConnect)
		require("RSQLite")
	}
	{# DB connection
		db=paste(wd2,"AVESatNIOZ.sqlite",sep="")
		#db=paste(wd2,"test.sqlite",sep="")
		#db=paste(wd2,"test2.sqlite",sep="")
	}
}
 
 
{# run first - set "i"
	f = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=TRUE)
	f2 = list.files(path=paste(wd,sep =''),pattern='data_entry', recursive=TRUE,full.names=FALSE)
	#for(i in 1:length(f)){#{2){#
	i = 1
	print(f2[i])
		m = readWorksheetFromFile(f[i], sheet=1)
		#str(m)
		#head(m)
		m$capture = as.character(m$capture)
		m$release = as.character(m$release)
			#m[m==""] = NA
			#m[m==" "] = NA
			#m[m=="NA"] = NA 
		m$capture = as.POSIXct(m$capture)
		m$release = as.POSIXct(m$release)		
		#m = m[!(is.na(m$capture) & is.na(m$at) & is.na(m$author)),]
}

# CHECKS
	{# check whether all general columns present
			con = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbGetQuery(con, "DROP TABLE IF EXISTS CAPTURES")   
			a = dbGetQuery(con, "SELECT*FROM CAPTURES")   
			dbDisconnect(con)
		print(names(a)[!names(a)%in%names(m) & !names(a)%in%c('year_')]) # names that are in the DB but not in the data_entry file (with exception of year_); pk and DFG are fine)
		print(names(m)[!names(m)%in%names(a) & !names(m)%in%c('m_p','f_p','home')]) # names that are in the data_entry file but not in the DB (molt_col, with, pic and c_pk are fine)
		
	# add columns to avoid issues with checking
		#if(length(names(m)[names(m)=='pic'])==0){m$pic = NA}
		#if(length(names(m)[names(m)=='with'])==0){m$with = NA}	
		#if(length(names(m)[names(m)=='health']) == 0){m$health = NA}
		}
	# what
		{# shows rows with multimple what info
		  m$what[!m$what%in%c("",NA,"bio","blood","capt","cr","crc", "free", "ful", "group", "harn", "obs","on", "off", "end","out_a","out_b", "single", "skin", "switch","ul", "on,out_b","btrain","utrain")] 
			#m$what[!m$what%in%c(NA,"bio","blood","capt","cr","crc", "free", "ful", "group", "harn", "obs","on", "off", "end","out_a","out_b", "single", "skin", "switch","ul")] 
		}
		{# shows whether all what entries are defined
		  g = unique(unlist(strsplit(m$what, ',')))
		  gg = g[!g%in%c(NA,"bio","blood","capt","cr","crc", "free", "ful", "group", "harn", "obs","on", "off", "end", "out_a","out_b", "pic", "single", "skin", "switch","ul","cons","btrain","utrain")]		
		  if(length(gg)>0){		  
			for(j in 1:length(gg)){	
					print(m[grepl(gg[j],m$what, perl = TRUE),])
					}
				}else{'all defined'}
		}
		# if what is the following then what_ID needs to be something  
			m[grepl("blood",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("skin",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("on",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("off",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("dead",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("toa",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("acc",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("dtag",m$what, perl = TRUE) & is.na(m$what_ID),]
			m[grepl("harn",m$what, perl = TRUE) & is.na(m$what_ID),]
			
			#mm = m
			#mm$what = ifelse(grepl('skin',mm$what, perl = TRUE), 'skin', ifelse(grepl('blood,bio,cr',mm$what, perl = TRUE), 'blood',mm$what))
		    #mm[mm$what%in%c("blood","skin","on", "off","dead","skin","toa", "acc","dtag","harn") & is.na(m$what_ID),] # "blood,bio,cr","switch,blood,bio,cr","skin,switch"
		# if what is 'on' then 'with' filled
			m[grepl("on",m$what, perl = TRUE) & is.na(m$with),]
		
		# if what or health is pic then pic needs to have somethin 	
			m[grepl("pic",m$what, perl = TRUE) & is.na(m$pic) | grepl("pic",m$health, perl = TRUE) & is.na(m$pic),]
			
		# if what is observation than health needs to be something # not that continuous observation are 'cons' 	
		    m[grepl("obs",m$what, perl = TRUE) & is.na(m$health),]
		
		# if what is crc then crc_now needs to be something	
			m[grepl('crc',m$what, perl = TRUE) & is.na(m$crc_now),]
		
		# if what is something than additional columns with data should be there -  if not, it will give warnings
			if(TRUE%in%grepl('capt',m$what, perl = TRUE)){
			print(m[grepl('capt',m$what, perl = TRUE) & is.na(m$project),] )
			print(m[grepl('capt',m$what, perl = TRUE) & is.na(m$species),])
			print(m[grepl('capt',m$what, perl = TRUE) & is.na(m$subspecies),])
			print(m[grepl('capt',m$what, perl = TRUE) & is.na(m$age),])
			print(m[grepl('capt',m$what, perl = TRUE) & is.na(m$mass_f),])
			}else{'no capt in what'}
			
			if(TRUE%in%grepl('ful',m$what, perl = TRUE)){
			print(m[grepl('ful',m$what, perl = TRUE) & is.na(m$muscle),])
			print(m[grepl('ful',m$what, perl = TRUE) & is.na(m$height_1),])
			print(m[grepl('ful',m$what, perl = TRUE) & is.na(m$width_1),])
			print(m[grepl('ful',m$what, perl = TRUE) & is.na(m$height_2),])
			print(m[grepl('ful',m$what, perl = TRUE) & is.na(m$width_2),])
			 }else{'no ful in what'}
			
			if(TRUE%in%grepl('ul',m$what, perl = TRUE)){ 
			print( m[grepl('ul',m$what, perl = TRUE) & is.na(m$muscle),])
			print(m[grepl('ul',m$what, perl = TRUE) & is.na(m$height_1),])
			print(m[grepl('ul',m$what, perl = TRUE) & is.na(m$width_1),])
			print(m[grepl('ul',m$what, perl = TRUE) & is.na(m$height_2),])
			print(m[grepl('ul',m$what, perl = TRUE) & is.na(m$width_2),])
			}else{'no ul in what'}
			
			if(TRUE%in%grepl('bio',m$what, perl = TRUE)){ 
			print(m[grepl('bio',m$what, perl = TRUE) & is.na(m$wing),])
			print(m[grepl('bio',m$what, perl = TRUE) & is.na(m$bill),])
			print(m[grepl('bio',m$what, perl = TRUE) & is.na(m$tarsus),])
			print(m[grepl('bio',m$what, perl = TRUE) & is.na(m$tartoe),])
			}else{'no bio in what'}
			
			if(TRUE%in%grepl('btrain',m$what, perl = TRUE)){ 
			print(m[grepl('btrain',m$what, perl = TRUE) & is.na(m$wing),])
			print(m[grepl('btrain',m$what, perl = TRUE) & is.na(m$bill),])
			print(m[grepl('btrain',m$what, perl = TRUE) & is.na(m$tarsus),])
			print(m[grepl('btrain',m$what, perl = TRUE) & is.na(m$tartoe),])
			}else{'no btrain in what'}
			
			if(TRUE%in%grepl('utrain',m$what, perl = TRUE)){ 
		   print( m[grepl('utrain',m$what, perl = TRUE) & is.na(m$muscle),])
			print(m[grepl('utrain',m$what, perl = TRUE) & is.na(m$height_1),])
			print(m[grepl('utrain',m$what, perl = TRUE) & is.na(m$width_1),])
			print(m[grepl('utrain',m$what, perl = TRUE) & is.na(m$height_2),])
			print(m[grepl('utrain',m$what, perl = TRUE) & is.na(m$width_2),])
			}else{'no utrain in what'}
			

	# captured at
		m[!m$at%in%c(paste('o', seq(1,8,1), sep=""),paste('w', seq(1,7,1), sep=""), 'griend', 'richel', 'box', 'crate'),] 
	
	# released where 
		m[!m$where%in%c(paste('o', seq(1,8,1), sep=""),paste('w', seq(1,7,1), sep=""), 'griend','box', 'crate', 'freezer','mokbaai') & m$what!='obs',] 
	
	# datetimes
		m[is.na(m$capture),] # check if capture time is NA
		m[is.na(m$release) & !m$what%in%c('obs') ,] # check if release time is NA
		
		m[which((!is.na(m$capture) | !is.na(m$release)) & m$capture>m$release), ] # check whether release happened before capture
		
		m[which(as.numeric(difftime(m$capture,trunc(m$capture,"day"), units = "hours"))<6),] # working time below 6:00
		m[which(as.numeric(difftime(m$release,trunc(m$release,"day"), units = "hours"))<6),] # working time below 6:00
		m[which(as.numeric(difftime(m$capture,trunc(m$capture,"day"), units = "hours"))>22),] # working time > 22:00
		m[which(as.numeric(difftime(m$release,trunc(m$release,"day"), units = "hours"))>22),] # working time > 22:00
		 
	# health
		# shows all rows with multiple health entries !!!!
		m[!m$health%in%c(NA,"","antib","bet","bf","care","check","dead","ffed","limp","mass","ok","pic","size","stuck","surg","vet","wear","wet","pic", "bleed", "cut","cramp"),]
				
		{# shows whether all health entries are defined
		 m$health = as.character(m$health)
		 g = unique(unlist(strsplit(m$health, ',')))
		 gg = g[!g%in%c(NA,"antib","bet","bf","care","check","dead","ffed","limp","mass","ok","pic","size","stuck","surg","vet","wet","wear","pic","bleed","cut","cramp")]
		  if(length(gg)>0){		  
			for(j in 1:length(gg)){	
					print(m[grepl(gg[j],m$health, perl = TRUE),])
					}
				}else{'all defined'}
	}
		
	{# is author defined 
		# get all authors from db and check whether authors in captures are the same (or whether some need to be entered into authors
		con = dbConnect(dbDriver("SQLite"),dbname = db)
		a = dbGetQuery(con, "SELECT*FROM AUTHORS")   
		a = unique(a$initials[a$initials!=""])
		dbDisconnect(con)
				
  		g = unique(unlist(strsplit(m$author, ',')))
	if(length(g[!g%in%a])>1){g[!g%in%a]}else{print('ok')}
	}	
	# shall be only one author per line with two letters
		m[nchar(m$author)!=2,]
		
	# bird_ID
		m[!nchar(m$bird_ID)%in%c(7,9),]
	
	# feet
		m[!m$feet%in%c(seq(0,30,1),NA),] 
	
	# plum
		m[!m$plum%in%c(seq(1,7,1),NA),]
	
	# molt
		m[!m$molt%in%c(seq(0,3,1),NA),]	
	# CFG	
		m[!m$CGF%in%c('w','b','wb','-',NA),]	
	#L
		m[!m$L01%in%c(seq(0,5,1),'-',NA),]
		m[!m$L02%in%c(seq(0,5,1),'-',NA),]
		m[!m$L03%in%c(seq(0,5,1),'-',NA),]
		m[!m$L04%in%c(seq(0,5,1),'-',NA),]
		m[!m$L05%in%c(seq(0,5,1),'-',NA),]
		m[!m$L06%in%c(seq(0,5,1),'-',NA),]
		m[!m$L07%in%c(seq(0,5,1),'-',NA),]
		m[!m$L08%in%c(seq(0,5,1),'-',NA),]
		m[!m$L09%in%c(seq(0,5,1),'-',NA),]
		m[!m$L010%in%c(seq(0,5,1),'-',NA),]
	#R
		m[!m$R01%in%c(seq(0,5,1),'-',NA),]
		m[!m$R02%in%c(seq(0,5,1),'-',NA),]
		m[!m$R03%in%c(seq(0,5,1),'-',NA),]
		m[!m$R04%in%c(seq(0,5,1),'-',NA),]
		m[!m$R05%in%c(seq(0,5,1),'-',NA),]
		m[!m$R06%in%c(seq(0,5,1),'-',NA),]
		m[!m$R07%in%c(seq(0,5,1),'-',NA),]
		m[!m$R08%in%c(seq(0,5,1),'-',NA),]
		m[!m$R09%in%c(seq(0,5,1),'-',NA),]
		m[!m$R010%in%c(seq(0,5,1),'-',NA),]
	
	# mass
		#min_ = 
		#max_ =
		m$mass = as.numeric(m$mass)
		m[which(m$mass<90 | m$mass>250), ] #m$mass < min_ | m$mass > max_
		
		m$with = as.numeric(m$with)
		m[which(m$with<90 | m$with>250), ] 
		
	{# mass 'with' only, if on and what ID entered but not off in captures and if on and off and what ID for all old instances 
		con = dbConnect(dbDriver("SQLite"),dbname = db)  
		a = dbGetQuery(con, "SELECT*FROM CAPTURES")   
		dbDisconnect(con)
		x = unique(a$bird_ID[grepl('on',a$what, perl = TRUE)])
		aa = ddply(a[a$bird_ID%in%x,],.(bird_ID), summarise, on = max(capture[grepl('on',what, perl = TRUE)]) , off = ifelse(length(capture[grepl('off',what, perl = TRUE)])==0, NA,max(capture[grepl('off',what, perl = TRUE)])))
		aa  = aa[is.na(aa$off) | aa$off<aa$on,]
		
		# these birds should have 'with' instead of 'mass' 
		#!!!!! CAREFUL, WORKS ONLY IF ALL PREVIOUS DATA IN DB  !!!!!
			m[which(m$bird_ID %in% aa$bird_ID & is.na(m$with) & !m$what%in%c("obs","off")),]
	}
	# home
		m[!m$home%in%c(paste('o', seq(1,8,1), sep=""),paste('w', seq(1,7,1), sep=""),NA),] 
		
	# MOVE THE FILE - Have you reloaded f and f2
	file.rename(f[i], paste(outdir,f2[i], sep = ''))
	##################
	}
