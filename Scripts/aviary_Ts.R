require(XLConnect)
require(ggplot2)
wd =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/T/current"
outdir =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/aviaryT/"

f = list.files(path=paste(wd),pattern='.xls', recursive=TRUE,full.names=TRUE)
			#f=f[order(substring(f,nchar(f)-8))]
f2 = list.files(path=paste(wd),pattern='.xls', recursive=TRUE,full.names=FALSE)
			#f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			#f2=f2[order(f2)]

dev.new(width = 6, height = 9)
# see and room together
for(i in 1 : 5){
		# room 
		df <- readWorksheetFromFile(f[i],  
		                            sheet=1,
		                            startRow = 7,
		                            endCol = 2
		                            )

		colnames(df) = c('datetime_','T')
		df = df[2:nrow(df),]
		if(nchar(df$datetime_[1])<19){df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d-%m-%y %H:%M"))
			}else{df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d.%m.%Y %H:%M:%S"))}
		
		df$time_ = as.numeric(difftime(df$datetime_,trunc(df$datetime_,"day"), units = "hours"))
		df$day = substring(df$datetime_, 1,10) 	
		df$T = as.numeric(gsub(",", ".", df$T))	
		r = df[!is.na(df$datetime_),]
		r$type = if(substring(f2[i],1,3) == 'Rui'){'room'}else{'see'}

		# see
		df <- readWorksheetFromFile(f[i+5],  
		                            sheet=1,
		                            startRow = 7,
		                            endCol = 2
		                            )

		colnames(df) = c('datetime_','T')
		df = df[2:nrow(df),]
		if(nchar(df$datetime_[1])<19){df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d-%m-%y %H:%M"))
			}else{df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d.%m.%Y %H:%M:%S"))}
		
		df$time_ = as.numeric(difftime(df$datetime_,trunc(df$datetime_,"day"), units = "hours"))
		df$day = substring(df$datetime_, 1,10) 	
		df$T = as.numeric(gsub(",", ".", df$T))	
		s = df[!is.na(df$datetime_),]
		s$type = if(substring(f2[i+5],1,3) == 'Rui'){'room'}else{'see'}

		df = rbind(r,s)
		ggplot(df,aes(x  = time_, y = T, col = type)) + geom_line() + facet_grid(day ~ .) + labs(title = paste("aviary",i+2))
		ggsave(file=paste(outdir, 'aviary_',i+2,'_T.png', sep=""),width = 6, height = 9)
		print(i)
	}	

# see and room separate
for(i in 1 : length(f)){
		df <- readWorksheetFromFile(f[i],  
		#df <- readWorksheetFromFile(paste(wd,"see3_2017-07-17",".xls", sep=''),  
		                            sheet=1,
		                            startRow = 7,
		                            endCol = 2
		                            )

		colnames(df) = c('datetime_','T')
		df = df[2:nrow(df),]
		if(nchar(df$datetime_[1])<19){df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d-%m-%y %H:%M"))
			}else{df$datetime_ = as.POSIXct(strptime(df$datetime_,format="%d.%m.%Y %H:%M:%S"))}
		
		df$time_ = as.numeric(difftime(df$datetime_,trunc(df$datetime_,"day"), units = "hours"))
		df$day = substring(df$datetime_, 1,10) 	
		df$T = as.numeric(gsub(",", ".", df$T))	
		df = df[!is.na(df$datetime_),]


		ggplot(df,aes(x  = time_, y = T)) + geom_line(col = 'red') + facet_grid(day ~ .) + labs(title = paste("aviary",i+2))
		ggsave(file=paste(outdir, substring(f2[i],1,3), if(substring(f2[i],1,3) == 'Rui'){i+2}else{i+2-5},'.png', sep=""),width = 6, height = 9)
		print(i)
	}	