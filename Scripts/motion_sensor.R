
{# define working  directories
wd =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"
outdir =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/"
}
{# define constants
		min_ = 0
		max_ = 1
		
		wr_col="grey50"
		ln_col="grey80"
		disturb_in='#5eab2b'
		disturb_out='#84d350'
		cv_x = "#99c978"
		cv_y = "#f0b2b2"
		cv_z = "#ADD8E6"
		tem = 'dodgerblue'
	}
{# define functions
		act_actogram = function(dfr, type = "PNG", min_=0, max_=1, line_=FALSE) {
				# dfr - data frame
				# figCap - figure captions
				# latlon - latitude longitude
				# day - panel labels as day or as day of incubation period and inc constancy
				# ins = start of incubation period
				# inp = lenght of incubation period
				# type = PNG -created, PDF - created, SAVE - also Rdata file saved
				# min_/max_ - limits of y-axis in the panel
				# UTC/UTC_met -  shall data and metadata be transformed to longitudinal time (yes = TRUE, no=FALSE) 
				# signal - are data based on automated tracking?
				#cut_ = cut off point activity/no activity
			 
			 dfr$act=0.8
			 dfr$col_='black'
			
 			 dfr$day = as.Date(trunc(dfr$datetime_, "day"))
			 dfr$time = as.numeric(difftime(dfr$datetime_, trunc(dfr$datetime_,"day"), units = "hours"))
			
			 sl1 = unique(dfr$day)
			 sl1=sl1[order(sl1)]
			 					strip.left1 = function(which.panel, ...) {
										LAB = format(sl1[which.panel], "%b-%d")
										grid.rect(gp = gpar(fill = "grey95", col=ln_col))
										ltext(0.5, 0.5, cex = 0.6, LAB, col=wr_col)
										}
								ylab_=list('Date',cex=0.7, col=wr_col, vjust=1, hjust=0.5)		
										
								scales1 = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , cex = 0.6, tck=0.4,
										limits = c(0,24),col=wr_col,col.line = ln_col, alternating=3), 
										y = list(limits = c(min_, max_),
										at =c(round(min_*0.1),round(max_*0.74)),draw=FALSE), 
										col=wr_col,cex=0.5, tck=0.4,alternating=2, col.line=ln_col)
								#ylab_right=list('log(odba)',cex=0.7, col=wr_col,vjust=-0.3)
				
			   panel1 = function(...) {
							 
								# activity
									dfri= dfr[which(dfr$day == sl1[panel.number()]),]
									#dfri= dfr[which(dfr$day == sl1[1]),]
									panel.xyplot(dfri$time, dfri$act, col = dfri$col_, type='h')
								# disturbance
									vji = vj[which(vj$day == sl1[panel.number()]),] 
									if(nrow(vji)>0) {panel.rect(xleft=vji$start_, ybottom=min_, xright=vji$end_, ytop=vji$top, col=vji$col_, border=0)
													}
								panel.xyplot(...)
							}
			{# key
				clr_cap=list(text=list(c(dfr$bird_ID[1],paste("aviary",dfr$aviary[1])),cex=0.6, col=c(wr_col)))	
				clr_1=list(text = list(c('active','disturbance inside aviary', 'disturbance outside aviary'),col=c(wr_col),cex=0.6),
												text = list(c("|","|","|"), cex=c(0.6), font="bold", col = c('black', disturb_in, disturb_out)))
					
				{# adds buffer around legend columns by creating fake legend columns
						clr_n=list(text = list(c("")))				
					}									
				key1 = c(  # captions
									#clr_0,
									clr_cap,
									clr_n,
								# used
									clr_1,
									clr_n,
								# ending to compensate for captions
								#	clr_e,
								rep=FALSE, between=0.9 #just=-0.3#, 	padding.text=1.1,#, border=ln_col #columns
								)									
			}
			{# plot				
			#dev.new(width=8.26771654, height=11.6929134)
						
			rfidsplot = xyplot(1 ~ time | day, 
									data = dfr, 
									type='p',
									cex = 0.1, cex.title=0.5, main = NULL,
									layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
									strip.left = strip.left1, 
									scales = scales1,
									panel=panel1, 
									key=key1,
									ylab=ylab_,
									#ylab.right=ylab_right,
									#auto.key=TRUE,
									xlab.top=list('Time [h]',cex=0.7,col=wr_col, vjust=1),
									xlab=NULL,
									par.settings=list(axis.components=list(left=list(tck=0)), layout.widths=list(right.padding=2),axis.line = list(col = ln_col)), #box.3d=list(col = wr_col)), #top=list(tck=0.4),
									as.table=TRUE,
									#aspect = "fill", 
									strip = FALSE, distribute.type = TRUE,    
									lattice.options = list(layout.widths = list(strip.left = list(x = 3)))
									)
									
			
				
			}	
			if(type %in% c('SAVE')) {
					save(rfidsplot,dfr, sl1, strip.left1,scales1,panel1,key1,nt, is_,ie_, act_c, ex,latlon, figCap, file=paste("C:/Users/mbulla/Documents/ownCloud/ACTOSforHTML/",paste("rfid",i,sep="_"),".Rdata",sep=""))
					tf = paste0(outdir,pkk$act_ID[i], paste("_",pkk$pkk[i],sep=""), "_%03d.png",sep="") 
														png(tf,width = 210, height = 57+8*length(sl1), units = "mm", res = 600)	
									par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)
									print(rfidsplot)
									vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1))) # creates area for map	 	##y=0.9575 for 297mm height							
									pushViewport(vp)
									print(gg, newpage=FALSE) # prints the map
									dev.off()
				}else{if(type %in% c('PNG')) {
									tf = paste0(outdir, paste(dfr$bird_ID[1],"_",dfr$aviary[1],sep=""), ".png",sep="") 
									png(tf,width = 210, height = 57+8*length(sl1), units = "mm", res = 600)	#png(tf,width = 210, height = 297, units = "mm", res = 600)	
									par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)#par(pin = c(8.26771654, 11.6929134)) 
									print(rfidsplot)
									#vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1)))#vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 					
									#pushViewport(vp)
									#print(gg, newpage=FALSE) # prints the map
									dev.off()
									}else{
									#dev.new(widht=8.26771654,height=11.6929134)
									par(pin = c(8.26771654, 11.6929134)) 
									print(rfidsplot)
									#vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 	vp <- viewport(x=0.89,y=0.94,width=0.11*1.5, height=0.11) # creates area for map	 						
									#pushViewport(vp)
									#print(gg, newpage=FALSE) # prints the map
									}}
				}	
		 
	}

{# load packages
 sapply(c('ggplot2', 'ggthemes','grid','gridExtra','plyr','lattice', 'latticeExtra','magrittr','matrixStats','maptools','raster', 'rgeos', 'rgdal', 'XLConnect','zoo'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))  
}			
{# load metadata
	m = readWorksheetFromFile(paste(wd, 'SocialJetLag_DB.xlsx', sep = ''), sheet='devices')
	m = m[m$device == 'ms',]
	m = m[m$what == 'on' & m$ID %in% m$ID[m$what == 'off'],]
	
	b = readWorksheetFromFile(paste(wd, 'SocialJetLag_DB.xlsx', sep = ''), sheet='birds')
	b$datetime_r = as.POSIXct(strptime(paste(b$date, substring(b$released, 12)),format ="%Y-%m-%d %H:%M:%S" ))# released time 
	
	v = readWorksheetFromFile(paste(wd, 'SocialJetLag_DB.xlsx', sep = ''), sheet='visits')
	v$start_ = as.POSIXct(strptime(paste(v$date, substring(v$start, 12)),format ="%Y-%m-%d %H:%M:%S" ))
	v$end_ = as.POSIXct(strptime(paste(v$date, substring(v$end, 12)),format ="%Y-%m-%d %H:%M:%S" ))
	v = v[v$aviary %in% c('w1','w2', 'w3','w4','w5','w6','w7','mud','tech','wu'),]
	v$day = as.Date(trunc(v$date, "day"))
	#v = v[!is.na(v$start_) & !is.na(v$end_),]
	v$start_ = as.numeric(difftime(v$start_, trunc(v$start_,"day"), units = "hours"))
	v$end_ = as.numeric(difftime(v$end_, trunc(v$end_,"day"), units = "hours"))
	
	#v[v$start_>v$end_,]
	#v[v$start_==v$end_,]
}

{# visualise	
	
	f = list.files(path=paste(wd, 'ms/', sep = ''),pattern='.TXT', recursive=TRUE,full.names=TRUE)
	f2 = list.files(path=paste(wd, 'ms/', sep = ''),pattern='.TXT', recursive=TRUE,full.names=FALSE)
	for(i in length(f)){
		d = read.table(f[i], skip = 10,  stringsAsFactors = FALSE,
							col.names = c('time_','date_', 'del', 'del2', 'aviary'))
		 
		d$aviary = paste('w',d$aviary+3,sep='')
		# bring datetime to real CES time
			mi = m[m$ID == paste('ms',i, sep =''),]
			tst = as.numeric(difftime(as.POSIXct(mi$datetime_), as.POSIXct(strptime('00:00:00 01/01/00',format="%H:%M:%S %d/%m/%y")), units = 'sec')) # difference to real CES time on the computer
			d$datetime_ =as.POSIXct(strptime(paste(d$time_, d$date_),format="%H:%M:%S %d/%m/%y")) + tst
			d$del = d$del2 = d$time_ = d$date_ = NULL  
		# bring bird IDs
			bi = b[b$date>=as.POSIXct(mi$datetime_)-1*24*60*60 & b$date<=as.POSIXct(mi$datetime_)+6*24*60*60 & b$treat == 'single',]
			d$bird_ID = bi$bird_ID[match(d$aviary, bi$aviary)]
			
		for(j in 4:7){
				dj = d[d$aviary == paste('w',j,sep =''),]
				dj = dj[dj$datetime_>bi$datetime_r[bi$aviary == paste('w',j,sep ='')],]
				vj = v
				vj$where = ifelse(vj$aviary!=paste('w',j,sep =''), 'outside',vj$where)
				vj$col_ = ifelse(vj$where == 'in', disturb_in, disturb_out)
				vj$top = ifelse(vj$where == 'in', max_, min_+0.25*(max_-min_))
				act_actogram(dfr = dj)
				print(paste('w',j,sep =''))
				}
		}
}		