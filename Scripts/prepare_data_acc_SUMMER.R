{# TOOLS
	{# define working directory
	     #wd = "M:/Science/Projects/MC/test_data_axy4/"	
		 #wdd=wd
	     wd = "M:/Science/Projects/MC/data/"	
		 #wd = "//ds/grpkempenaers/Martin/" 
	    # wd = "H:/data/"	
		 outdir = 'C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/acc'
		 #outdir = "//ds/grpkempenaers/Martin/"
	     wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
		 #wd2 = "//ds/grpkempenaers/Martin/"
	     #wdd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/accelerometer output"	
		 #outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/visualized/"	
	}
	{# load packages
	require(xlsx)
	require(data.table)
	require(raster)
	#require(bigmemory)
	#require(biganalytics)
	#require(bigtabulate)
	#require(ff)
	#install.packages('ff')
	}
	{# assign more memory
		memory.limit()
		memory.limit(size = 200000)
	}
	{# define constants
		varnames = c("tag", "datetime_", "x", "y","z", "temp", "batt")
		sep_ = '\t'#',' #\t#
	}
	{# load functions
		 odba <- function(x){
			  d.x <- x - mean(x, na.rm = T)   
				return(sum(abs(d.x), na.rm = T)/length(x))
}
	}
}

#load(file="//ds/grpkempenaers/Martin/rdata/H745_A30_2017-10-31.RData")
{# PREPARAE DATA 
   f = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
   f2 = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)] f2='Z697_A11_S1.csv'
   for(i in 1:length(f)){
	d = fread(f[i],sep=sep_,  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
		#d[,batt:= as.numeric(substring(batt, 1,nchar(batt)-1))]
		#d$batt = as.numeric(substring(d$batt, 1,nchar(d$batt)-1))
	#if(grepl('/', d$datetime_[1])){d$datetime_ = gsub('/','-',d$datetime_)}	
	#as.POSIXct(c('2017/10/29 01:00:00','2017/10/29 02:00:00','2017/10/29 03:00:00','2017/10/29 04:00:00'), format = '%Y/%m/%d %H:%M:%OS',tz="CES")

	# per min
	  bb = d[,list(odbaX = odba(x), odbaY = odba(y), odbaZ = odba(z), m_x = median(x), m_y = median(y), m_z = median(z)), by = .(substring(datetime_,1,16))] ## cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),

					#bb = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,16))]
					#bb = ddply(d,. (datetime_=substring(d$datetime_,1,16)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
	  names(bb)[1] = 'datetime_'
	  #bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]
	  if(grepl('/', bb$datetime_[1])){bb[,datetime_:= as.character(as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M',tz="UTC"))]}#else{bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M',tz="UTC")]}
	  
	  bb$bird_ID=substring(f2[i],1,4)
	  bb$tag=substring(f2[i],6,8)					
								
	# per second
	  aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z),  m_x = median(x), m_y = median(y), m_z = median(z)), by = . (substring(datetime_,1,19))] # cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),
	
				#aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,19))]
				#dd = d[substring(datetime_,1,19)=="2017-08-01 22:16:15",]			
	  names(aa)[1] = 'datetime_'
		#aa$datetime_ = as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')
	  
	   #aa[,datetime_:= as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')]#
	   if(grepl('/', aa$datetime_[1])){aa[,datetime_:= as.character(as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M:%S',tz="UTC"))]}#else{aa[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%S',tz="UTC")]}
	   aa$bird_ID=substring(f2[i],1,4)
	   aa$tag=substring(f2[i],6,8)
			
				
	save(aa,bb, file=paste(wd2, 'odba/to_do/',aa$bird_ID[1],'_',aa$tag[1],'_',substring(f2[i],10,19),"_odba.Rdata",sep=""))
		rm(aa)
		rm(bb)
		gc()
		
	if(grepl('/', d$datetime_[1])){d[,datetime_:= as.character(format(as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M:%OS',tz="UTC"),"%Y-%m-%d %H:%M:%OS2"))]}
	save(d, file = paste(wd,'rdata/',substring(f2[i],1,4),'_',substring(f2[i],6,8),'_',substring(f2[i],10,19),'.RData',sep=''))
	 # make subset if needed
	 #d[,datetime_1:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%OS')]# for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
	 #op <- options(digits.secs=2)
	 # d_ = d[d$datetime_1>as.POSIXct('2017-08-03 00:00:00') & d$datetime_1<as.POSIXct('2017-08-04 00:00:00'),]
	  #save(  d_, file = paste(wd,'rdata/',substring(f2[i],1,4),'_',substring(f2[i],6,8),'_',substring(f2[i],10,19),'subset.RData',sep=''))
	rm(d)
	#rm(d)
	#rm(list = ls(all.names = TRUE))
	#d[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%OS')]	
				#d$datetime_ = as.POSIXct(d$datetime_, format = '%Y-%m-%d %H:%M:%OS') 	
	#op <- options(digits.secs=2)
	#save(d, file = paste(wd,'rdata/',aa$bird_ID[1],'_',aa$tag[1],'posix.RData',sep=''))	
	#save(d, file = paste(wd,'rdata/','_H517_A19_2017-08-22_posix.RData',sep=''))
	file.rename(f[i], paste(wd,'csv/', f2[i], sep = ''))	
	print(f2[i])
	}
		
}

{# PREPARE DATA - server
	 load(file="//ds/grpkempenaers/Martin/rdata/H745_A30_2017-10-31.RData")
	 f = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
	f2 = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)] f2='Z697_A11_S1.csv'
		i=1
		#d[,batt:= as.numeric(substring(batt, 1,nchar(batt)-1))]
		#d$batt = as.numeric(substring(d$batt, 1,nchar(d$batt)-1))
	if(grepl('/', d$datetime_[1])){d$datetime_ = gsub('/','-',d$datetime_)}	
	# per min
	  bb = d[,list(odbaX = odba(x), odbaY = odba(y), odbaZ = odba(z), m_x = median(x), m_y = median(y), m_z = median(z)), by = .(substring(datetime_,1,16))] ## cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),

					#bb = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,16))]
					#bb = ddply(d,. (datetime_=substring(d$datetime_,1,16)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
	  names(bb)[1] = 'datetime_'
	  bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]#if(grepl('/', bb$datetime_[1])){bb[,datetime_:= as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M')]}else{bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]}
	  
	  bb$bird_ID=substring(f2[i],1,4)
	  bb$tag=substring(f2[i],6,8)					
								
	# per second
	  aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z),  m_x = median(x), m_y = median(y), m_z = median(z)), by = . (substring(datetime_,1,19))] # cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),
	
				#aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,19))]
				#dd = d[substring(datetime_,1,19)=="2017-08-01 22:16:15",]			
	  names(aa)[1] = 'datetime_'
		#aa$datetime_ = as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')
	  
	   aa[,datetime_:= as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')]#if(grepl('/', aa$datetime_[1])){aa[,datetime_:= as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M')]}else{aa[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]}
	  aa$bird_ID=substring(f2[i],1,4)
	  aa$tag=substring(f2[i],6,8)
			
				
	save(aa,bb, file=paste(wd2, 'odba/to_do/',aa$bird_ID[1],'_',aa$tag[1],'_',substring(f2[i],10,19),"_odba.Rdata",sep=""))
	
}
{# PREPARAE DATA - from RData file
   load(file="//ds/grpkempenaers/Martin/rdata/H745_A30_2017-10-31.RData")
   f = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
   f2 = list.files(path=paste(wd,'csv/to_do/', sep=''),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)] f2='Z697_A11_S1.csv'
   i = 1
	d = fread(f[i],sep=sep_,  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
		#d[,batt:= as.numeric(substring(batt, 1,nchar(batt)-1))]
		#d$batt = as.numeric(substring(d$batt, 1,nchar(d$batt)-1))
	#if(grepl('/', d$datetime_[1])){d$datetime_ = gsub('/','-',d$datetime_)}	
	# per min
	  bb = d[,list(odbaX = odba(x), odbaY = odba(y), odbaZ = odba(z), m_x = median(x), m_y = median(y), m_z = median(z)), by = .(substring(datetime_,1,16))] ## cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),

					#bb = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,16))]
					#bb = ddply(d,. (datetime_=substring(d$datetime_,1,16)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
	  names(bb)[1] = 'datetime_'
	  #bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]
	  if(grepl('/', bb$datetime_[1])){bb[,datetime_:= as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M')]}else{bb[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]}
	  
	  bb$bird_ID=substring(f2[i],1,4)
	  bb$tag=substring(f2[i],6,8)					
								
	# per second
	  aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z),  m_x = median(x), m_y = median(y), m_z = median(z)), by = . (substring(datetime_,1,19))] # cv sometimes gives NAs, so not used cv_x = cv(x, aszero = TRUE), cv_y = cv(y, aszero = TRUE), cv_z = cv(z, aszero = TRUE),
	
				#aa = d[,list(odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp), bat = median(as.numeric(batt))), by = .(substring(datetime_,1,19))]
				#dd = d[substring(datetime_,1,19)=="2017-08-01 22:16:15",]			
	  names(aa)[1] = 'datetime_'
		#aa$datetime_ = as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')
	  
	   #aa[,datetime_:= as.POSIXct(aa$datetime_, format = '%Y-%m-%d %H:%M:%S')]
	  if(grepl('/', aa$datetime_[1])){aa[,datetime_:= as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M')]}else{aa[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M')]}
	  aa$bird_ID=substring(f2[i],1,4)
	  aa$tag=substring(f2[i],6,8)
			
				
	save(aa,bb, file=paste(wd2, 'odba/to_do/',aa$bird_ID[1],'_',aa$tag[1],'_',substring(f2[i],10,19),"_odba.Rdata",sep=""))
		rm(aa)
		rm(bb)
		gc()
	if(grepl('/', d$datetime_[1])){d$datetime_ = gsub('/','-',d$datetime_)}		
		
	save(d, file = paste(wd,'rdata/',substring(f2[i],1,4),'_',substring(f2[i],6,8),'_',substring(f2[i],10,19),'.RData',sep=''))
	
	rm(d)
	file.rename(f[i], paste(wd,'csv/', f2[i], sep = ''))	
	print(f2[i])
		
		
		
}

d = fread(f[i],sep=sep_,  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
if(grepl('/', d$datetime_[1])){d[,datetime_:= as.POSIXct(datetime_, format = '%Y/%m/%d %H:%M:%OS')]}else{d[,datetime_1:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%OS')]}	
d[,datetime_:= as.character(datetime_)]
	save(d, file = paste(wd,'rdata/',substring(f2[i],1,4),'_',substring(f2[i],6,8),'_',substring(f2[i],10,19),'.RData',sep=''))
	 # make subset if needed
	 #d[,datetime_1:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%OS')]# for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
	 #op <- options(digits.secs=2)
	 # d_ = d[d$datetime_1>as.POSIXct('2017-08-03 00:00:00') & d$datetime_1<as.POSIXct('2017-08-04 00:00:00'),]
	  #save(  d_, file = paste(wd,'rdata/',substring(f2[i],1,4),'_',substring(f2[i],6,8),'_',substring(f2[i],10,19),'subset.RData',sep=''))
	rm(d)
	#rm(d)
	#rm(list = ls(all.names = TRUE))
	#d[,datetime_:= as.POSIXct(datetime_, format = '%Y-%m-%d %H:%M:%OS')]	
				#d$datetime_ = as.POSIXct(d$datetime_, format = '%Y-%m-%d %H:%M:%OS') 	
	#op <- options(digits.secs=2)
	#save(d, file = paste(wd,'rdata/',aa$bird_ID[1],'_',aa$tag[1],'posix.RData',sep=''))	
	#save(d, file = paste(wd,'rdata/','_H517_A19_2017-08-22_posix.RData',sep=''))
	file.rename(f[i], paste(wd,'csv/', f2[i], sep = ''))	
	print(f2[i])
	
	
{# not used exception
		if(substring(f2[i],1,4)=='Z526'){
		d1 = fread(f[i],sep="\t",  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
		d2 = fread(f[i+1],sep="\t",  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
		d = rbind(d1,d2)
		}else{
		d = fread(f[i],sep="\t",  col.names = varnames[1:5], stringsAsFactors = FALSE, colClasses = c('character', 'POSIXct',"numeric", "numeric","numeric","numeric","numeric"), drop = 6:7)
		}

}
{# old + testing
{# CV
		f = list.files(path=paste(wdd),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
		f2 = list.files(path=paste(wdd),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)]
			
			
		i=3	
		
           
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d1 = d[!is.na(d$batt),]
		
		i=4		
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d2 = d[!is.na(d$batt),]
				
				d = rbind(d1,d2) 
		
				d$datetime_ = as.POSIXct(strptime(d$datetime_[2], '%Y-%m-%d %H:%M:%OS')) # for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
				op <- options(digits.secs=2)
				aa = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,16)),summarise, cv_x = cv(x), cv_y = cv(y), cv_z = cv(z), temp=median(temp))
				save(aa, file=paste(wd, 'Rdata/',aa$bird_ID[1],"_tag",aa$tag[1],".Rdata",sep=""))
				
		i = 5
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d1 = d[!is.na(d$batt),]
				
		i = 6		
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d2 = d[!is.na(d$batt),]
				
				d = rbind(d1,d2) 				
				d$datetime_ = as.POSIXct(strptime(d$datetime_, '%Y-%m-%d %H:%M:%OS')) # for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
				aa = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,16)),summarise, cv_x = cv(x), cv_y = cv(y), cv_z = cv(z), temp=median(temp))
				save(aa, file=paste(wd, 'Rdata/',aa$bird_ID[1],"_tag",aa$tag[1],".Rdata",sep=""))
		}		
		{# odba
		f = list.files(path=paste(wdd),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
		f2 = list.files(path=paste(wdd),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)]
			
			
		i=3	
		
           
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d1 = d[!is.na(d$batt),]
		
		i=4		
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d2 = d[!is.na(d$batt),]
				
				d = rbind(d1,d2) 
		
				
				
				aa = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,19)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
					aa$datetime_ = as.POSIXct(strptime(aa$datetime_, '%Y-%m-%d %H:%M:%S'))
				bb = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,16)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
					bb$datetime_ = as.POSIXct(strptime(bb$datetime_, '%Y-%m-%d %H:%M'))				
					save(aa,bb, file=paste(wd, 'Rdata/',aa$bird_ID[1],"_obda_tag",aa$tag[1],".Rdata",sep=""))
				
				
		i = 5
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d1 = d[!is.na(d$batt),]
				
		i = 6		
				d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d2 = d[!is.na(d$batt),]
				
				d = rbind(d1,d2) 			
				
				aa = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,19)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
					aa$datetime_ = as.POSIXct(strptime(aa$datetime_, '%Y-%m-%d %H:%M:%S'))
				bb = ddply( d,. (tag,bird_ID, datetime_=substring(d$datetime_,1,16)),summarise, odbaX = odba(x), odbaY = odba(y),odbaZ = odba(z), temp=median(temp))
					bb$datetime_ = as.POSIXct(strptime(bb$datetime_, '%Y-%m-%d %H:%M'))				
					save(aa,bb, file=paste(wd, 'Rdata/',aa$bird_ID[1],"_obda_tag",aa$tag[1],".Rdata",sep=""))
		}		
	{# test 2017-07-07
		f = list.files(path=paste(wd),pattern='.csv', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
		f2 = list.files(path=paste(wd),pattern='.csv', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)]
			
			
		i=1
				#d = read.csv(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				d = fread(f[i],sep="\t",  col.names = varnames, stringsAsFactors = FALSE)
				load(file = paste(wd,substring(f2[i],1,8),'.RData',sep=''))
				d$datetime_ = as.POSIXct(d$datetime_, format = '%Y-%m-%d %H:%M:%OS')) # for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
				op <- options(digits.secs=2)
				
				save(d, file = paste(wd,substring(f2[i],1,8),'.RData',sep=''))
				d$bird_ID=substring(f2[i],1,7)
				d$tag=substring(f2[i],12,13)
				d1 = d[!is.na(d$batt),]
	
			require(rbenchmark)
			
		benchmark(R=save(d, file = paste(wd,substring(f2[i],1,8),'.RData',sep='')),
				  RDS = saveRDS(d, file = paste(wd,substring(f2[i],1,8),'.rds',sep='')),
                  replications=1,
                  #order=c('relative')
				  )
				  
		benchmark(R = load(file = paste(wd,substring(f2[i],1,8),'.RData',sep='')),
				  #RDS = readRDS(file = paste(wd,substring(f2[i],1,8),'.rds',sep='')),
                   #order=c('relative'),
				  replications=1
				  )		
		
		benchmark(#R = load(file = paste(wd,substring(f2[i],1,8),'.RData',sep='')),
				  RDS = readRDS(file = paste(wd,substring(f2[i],1,8),'.rds',sep='')),
                   #order=c('relative'),
				  replications=1
				  )
save
  test replications elapsed relative user.self sys.self user.child sys.child
1    R            1  126.25    1.000    106.77     1.89         NA        NA
2  RDS            1  129.06    1.022    109.03     1.86         NA        NA
				  
load  

test replications elapsed relative user.self sys.self user.child sys.child
1    R            1   73.55        1     72.78     0.55         NA        NA
1  RDS            1   69.24        1     68.69     0.45         NA        NA

				  
			tests = list(rep=expression(random.replicate(100, 100)), 
				arr=expression(random.array(100, 100)))
		do.call(benchmark,
			c(tests, list(replications=100,
                      columns=c('test', 'elapsed', 'replications'),
                      order='elapsed'))
		}
		
	
}