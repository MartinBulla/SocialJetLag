{# TOOLS
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
	     wdd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/accelerometer output"	
		 outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/visualized/"	
	}
	{# load packages
	require(grid)
	require(plyr)
	require(raster)
	require(lattice)
	require(xlsx)
	}
	
	{# define constants
		varnames = c("tag", "datetime_", "x", "y","z", "temp", "batt")
		
		min_ = -0.1
		max_ = 6
		
		wr_col="grey50"
		ln_col="grey80"
		disturb='#5eab2b'
		cv_x = "#99c978"
		cv_y = "#f0b2b2"
		cv_z = "#ADD8E6"
		tem = 'dodgerblue'
	}
	
	{# load functions
	
		 odba <- function(x){
			  d.x <- x - mean(x, na.rm = T)   
				return(sum(abs(d.x), na.rm = T)/length(x))
}

		 Accelerometer_actogram = function(dfr, type = "PNG", min_=-0.1, max_=6, line_=FALSE) {
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
				
		
			 dfr$cv_x_la = log(abs(dfr$cv_x))
			 dfr$cv_y_la = log(abs(dfr$cv_y))
			 dfr$cv_z_la = log(abs(dfr$cv_z))
			 
			 dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")
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
										limits = c(0,24),col=wr_col,col.line = ln_col, alternating=3), y = list(limits = c(min_, max_),at =c(0,5),draw=TRUE), col=wr_col,cex=0.5, tck=0.4,alternating=2, col.line=ln_col)
								ylab_right=list('Ln(abs(cv(xyz)) & (Temperature-33C)*4',cex=0.7, col=wr_col,vjust=-0.3)
												
							
				
		
			   panel1 = function(...) {
							 
							    {# disturbance
									vi_ = v[which(v$day == sl1[panel.number()]),] 
									if(nrow(vi_)>0) {panel.rect(xleft=vi_$start, ybottom=min_, xright=vi_$end, ytop=0.5, col=disturb, border=0)
													}
									
								}							
								# activity
									panel.xyplot(...)
							}
			{# key
				clr_cap=list(text=list(c(dfr$bird_ID[1],paste("tag",dfr$tag[1])),cex=0.6, col=c(wr_col)))	
				clr_1=list(text = list(c('ln(abs(cv(x)))','ln(abs(cv(y)))','ln(abs(cv(z)))','ln(temperature)','visits or disturbance'),col=c(wr_col),cex=0.6),
												text = list(c("o","o","o","o", "|"), cex=c(0.6,0.6,0.6,0.6,0.6), font="bold", col = c(cv_x,cv_y,cv_z,tem,disturb)))
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
						
			rfidsplot = xyplot(log(abs(cv_x)) + log(abs(cv_y)) + log(abs(cv_z)) + (temp-33)*4 ~ time | day, 
									data = dfr, 
									type=ifelse(line_==TRUE, 'l','p'),
									col = c(cv_x,cv_y,cv_z, tem),
									cex = 0.1, cex.title=0.5, main = NULL,
									layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
									strip.left = strip.left1, 
									scales = scales1,
									panel=panel1, 
									key=key1,
									ylab=ylab_,
									ylab.right=ylab_right,
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
									tf = paste0(outdir, paste(dfr$bird_ID[1],"_tag",dfr$tag[1],ifelse(line_==TRUE, '_L','_P'),sep=""), ".png",sep="") 
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
		 temperature_actogram = function(dfr, type = "PNG", min_=33, max_=34.5) {
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
				
		
			 dfr$cv_x_la = log(abs(dfr$cv_x))
			 dfr$cv_y_la = log(abs(dfr$cv_y))
			 dfr$cv_z_la = log(abs(dfr$cv_z))
			 
			 dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")
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
										limits = c(0,24),col=wr_col,col.line = ln_col, alternating=3), y = list(limits = c(min_, max_),at =c(min_,min_+(max_-min_)*0.40,min_+(max_-min_)*0.80),draw=TRUE), col=wr_col,cex=0.5, tck=0.4,alternating=2, col.line=ln_col)
								ylab_right=list('Temperature [°C]',cex=0.7, col=wr_col,vjust=-0.3)
												
							
				
		
			   panel1 = function(...) {
							 
							    {# disturbance
									vi_ = v[which(v$day == sl1[panel.number()]),] 
									if(nrow(vi_)>0) {panel.rect(xleft=vi_$start, ybottom=min_, xright=vi_$end, ytop=min_+(max_-min_)*0.05, col=disturb, border=0)
													}
									
								}							
								# activity
									panel.xyplot(...)
							}
			{# key
				clr_cap=list(text=list(c(dfr$bird_ID[1],paste("tag",dfr$tag[1])),cex=0.6, col=c(wr_col)))	
				clr_1=list(text = list(c('temperature','visits or disturbance'),col=c(wr_col),cex=0.6),
												text = list(c("o","|"), cex=c(0.6,0.6), font="bold", col = c(tem,disturb)))
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
						
			rfidsplot = xyplot(temp ~ time | day, 
									data = dfr, 
									col = c(tem),
									cex = 0.1, cex.title=0.5, main = NULL,
									layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
									strip.left = strip.left1, 
									scales = scales1,
									panel=panel1, 
									key=key1,
									ylab=ylab_,
									ylab.right=ylab_right,
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
									tf = paste0(outdir, paste(dfr$bird_ID[1],"_tag",dfr$tag[1],sep=""), "T.png",sep="") 
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
		 odba_actogram = function(dfr, type = "PNG", min_=log(min(dfr$odba)-0.05*(min(dfr$odba))), max_=log(max(dfr$odba)+0.05*(max(dfr$odba))), line_=FALSE) {
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
				
		
			 dfr$odba=dfr$odbaX+dfr$odbaY+dfr$odbaZ
			 
			 dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")
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
										at =c(round(min_*0.42),round(max_*0.74)),draw=TRUE), col=wr_col,cex=0.5, tck=0.4,alternating=2, col.line=ln_col)
								ylab_right=list('log(odba)',cex=0.7, col=wr_col,vjust=-0.3)
				
			   panel1 = function(...) {
							 
							    {# disturbance
									vi_ = v[which(v$day == sl1[panel.number()]),] 
									if(nrow(vi_)>0) {panel.rect(xleft=vi_$start, ybottom=min_, xright=vi_$end, ytop=min_+0.25*(max_-min_), col=disturb, border=0)
													}
									
								}							
								# activity
									panel.xyplot(...)
							}
			{# key
				clr_cap=list(text=list(c(dfr$bird_ID[1],paste("tag",dfr$tag[1])),cex=0.6, col=c(wr_col)))	
				clr_1=list(text = list(c('log(odba)','visits or disturbance'),col=c(wr_col),cex=0.6),
												text = list(c("o","|"), cex=c(0.6,0.6), font="bold", col = c(tem,disturb)))
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
						
			rfidsplot = xyplot(log(odba) ~ time | day, 
									data = dfr, 
									type=ifelse(line_==TRUE, 'l','p'),
									col = tem,
									cex = 0.1, cex.title=0.5, main = NULL,
									layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
									strip.left = strip.left1, 
									scales = scales1,
									panel=panel1, 
									key=key1,
									ylab=ylab_,
									ylab.right=ylab_right,
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
									tf = paste0(outdir, paste(dfr$bird_ID[1],"odba_tag",dfr$tag[1],ifelse(line_==TRUE, '_L','_P'),sep=""), ".png",sep="") 
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
		 act_actogram = function(dfr, type = "PNG", min_=0, max_=1, line_=FALSE, cut_=0.201904, only_act=TRUE, res='1min') {
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
			 
			 dfr$odba=dfr$odbaX+dfr$odbaY+dfr$odbaZ	
			 if(only_act==TRUE){
					 dfr$act=ifelse(log(dfr$odba)>=cut_,0.8,NA)
					 dfr$col_=ifelse(log(dfr$odba)>=cut_,'black', NA)
				}else{
					 dfr$act=ifelse(log(dfr$odba)>=cut_,0.8, 0.2)
					 dfr$col_=ifelse(log(dfr$odba)>=cut_,'deepskyblue', 'red')
					 }
			 #dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")
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
							 
							    {# disturbance
									vi_ = v[which(v$day == sl1[panel.number()]),] 
									if(nrow(vi_)>0) {panel.rect(xleft=vi_$start, ybottom=min_, xright=vi_$end, ytop=min_+0.25*(max_-min_), col=disturb, border=0)
													}
									
								}							
								# activity
									dfri= dfr[which(dfr$day == sl1[panel.number()]),]
									if(only_act==TRUE){panel.xyplot(dfri$time, dfri$act, col = dfri$col_, type='h')
										}else{panel.xyplot(dfri$time, dfri$act, col = dfri$col_, cex = 0.1, type='p')}
									panel.xyplot(...)
							}
			{# key
				clr_cap=list(text=list(c(dfr$bird_ID[1],paste("tag",dfr$tag[1])),cex=0.6, col=c(wr_col)))	
				if(only_act==TRUE){clr_1=list(text = list(c('active','visits or disturbance'),col=c(wr_col),cex=0.6),
												text = list(c("|","|"), cex=c(0.6), font="bold", col = c('black', disturb)))
					}else{clr_1=list(text = list(c('active','inactive','visits or disturbance'),col=c(wr_col),cex=0.6),
												text = list(c("o","o","|"), cex=c(0.6), font="bold", col = c('deepskyblue', 'red',disturb)))}
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
									type=ifelse(line_==TRUE, 'l','p'),
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
									tf = paste0(outdir, paste(dfr$bird_ID[1],"tag",dfr$tag[1],'_',only_act,'_',res,ifelse(line_==TRUE, '_L','_P'),sep=""), ".png",sep="") 
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
	
	{# load metadata\
			{# disturbance
			v = read.csv(paste(wd,'visits.csv',sep=''), stringsAsFactors = FALSE)	
			v$day = as.Date(v$date)
			v$start = sapply(strsplit(v$start,":"), # converst time to decimals
					function(x) {
						x <- as.numeric(x)
						x[1]+(x[2]+x[3]/60)/60
						}
						)

			v$end = sapply(strsplit(v$end,":"), # converst time to decimals
					function(x) {
						x <- as.numeric(x)
						x[1]+(x[2]+x[3]/60)/60
						}
						)
			v = v[-is.na(v$end),]			
		}
		
	}
}
{# Visualise		
			{# CV	
				p = list.files(path=paste(wd),pattern='.Rdata', recursive=TRUE,full.names=TRUE)
				i=3
				load(p[i])
				Accelerometer_actogram(dfr=aa, line_=FALSE)
				Accelerometer_actogram(dfr=aa, line_=TRUE)
				temperature_actogram(dfr=aa)
				
				summary((aa$temp-33)*4)
				densityplot(~(aa$temp-33)*4)
				
				pairs(aa[4:6], pch=21)
				densityplot(aa$cv_x)
				wireframe(cv_x ~ cv_y * cv_z, data=aa)

				
				wireframe(swin ~ sway * yaw, data=d)
				plot(d$swin~d$sway)			
				
				}
			{# obda
				p = list.files(path=paste(wd),pattern='obda', recursive=TRUE,full.names=TRUE)
				i=2
				load(p[i])
				#odba_actogram(dfr=aa, line_=FALSE)
				
				{# 1 min
				dfr_=bb
				dfr_$odba=dfr_$odbaX+dfr_$odbaY+dfr_$odbaZ	
					d=data.frame(den=density(log(dfr_$odba))$y, x=density(log(dfr_$odba))$x)
					d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])] 
				act_actogram (dfr=dfr_, line_=FALSE, only_act=FALSE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1min')
				act_actogram (dfr=dfr_, line_=FALSE, only_act=TRUE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1min')
				}
				{# 1 sec
				dfr_=aa
				dfr_$odba=dfr_$odbaX+dfr_$odbaY+dfr_$odbaZ	
					d=data.frame(den=density(log(dfr_$odba))$y, x=density(log(dfr_$odba))$x)
					d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])] 
				
				act_actogram (dfr=dfr_, line_=FALSE, only_act=TRUE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1s')
				act_actogram (dfr=dfr_, line_=FALSE, only_act=FALSE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1s')
				densityplot(~log(dfr_$odba))
				}
				{# 5 min median from 1s
					aa$datetime_<-as.POSIXlt(round(as.double(aa$datetime_)/(5*60))*(5*60),origin=(as.POSIXlt('1970-01-01')))
					aa$odba=aa$odbaX+aa$odbaY+aa$odbaZ
					dfr_=ddply(aa,.(tag, bird_ID, datetime_), summarise, odbaX=median(odbaX), odbaY=median(odbaY), odbaZ=median(odbaZ),odba_m=median(odba))
					dfr_$odba=dfr_$odbaX+dfr_$odbaY+dfr_$odbaZ
					
					d=data.frame(den=density(log(dfr_$odba))$y, x=density(log(dfr_$odba))$x)
					d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])] 
					#d=data.frame(den=density(log(dfr_$odba_m))$y, x=density(log(dfr_$odba))$x)
					#d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])] 
				
					act_actogram (dfr=dfr_, line_=FALSE, only_act=TRUE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='5m_median')
					act_actogram (dfr=dfr_, line_=FALSE, only_act=FALSE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='5m_median')
					
					densityplot(~log(dfr_$odba))
					dev.new()
					densityplot(~log(dfr_$odba_m))
				}
				{# 1 min median from 1s
					load(p[i])
					aa$odba=aa$odbaX+aa$odbaY+aa$odbaZ
					dfr_=ddply(aa,.(tag, bird_ID, datetime_=substring(aa$datetime_,1,16)), summarise, odbaX=median(odbaX), odbaY=median(odbaY), odbaZ=median(odbaZ),odba_m=median(odba))
					dfr_$datetime_ = as.POSIXct(strptime(dfr_$datetime_, '%Y-%m-%d %H:%M'))	
					dfr_$odba=dfr_$odbaX+dfr_$odbaY+dfr_$odbaZ
					
					d=data.frame(den=density(log(dfr_$odba))$y, x=density(log(dfr_$odba))$x)
					d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])] 
					g=data.frame(den=density(log(dfr_$odba_m))$y, x=density(log(dfr_$odba))$x)
					g$x[g$den==min(g$den[g$x>(-3.5) & g$x<(-2)])] 
				
					act_actogram (dfr=dfr_, line_=FALSE, only_act=TRUE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1m_median')
					act_actogram (dfr=dfr_, line_=FALSE, only_act=FALSE, cut_=d$x[d$den==min(d$den[d$x>(-3.5) & d$x<(-2)])], res='1m_median')
					
					densityplot(~log(dfr_$odba))
					dev.new()
					densityplot(~log(dfr_$odba_m))
				}
	
	}
}				


{# prepare Rdata DONE	
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
		
				d$datetime_ = as.POSIXct(strptime(d$datetime_, '%Y-%m-%d %H:%M:%OS')) # for milliseconds see http://stackoverflow.com/questions/2150138/how-to-parse-milliseconds-in-r
				
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
}