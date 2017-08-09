{# TOOLS
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Experimental_set_up/"	
	}
	{# load packages
	require(ggplot2)
	require(plyr)
	require(XLConnect)
	}
	{# define date
		date_ = '2017-08-07'
	}
	}
{# weekly schedule bird shifts
	week = as.POSIXct(c('2017-07-10','2017-07-17','2017-07-24','2017-07-31','2017-08-07','2017-08-14','2017-08-21','2017-08-28','2017-09-04','2017-09-11','2017-09-18','2017-09-25','2017-10-02','2017-10-09'))
	w = data.frame(w1 = week[1:7], w2 = week[2:8], w3 = week[3:9], w4 = week[4:10], stringsAsFactors = FALSE)
	s = readWorksheetFromFile(paste(wd,'metadata_&_weekly_acc_attachment.xls', sep = ''), sheet=1)
		
		s$bmr = s$ex = s$giz = NULL
		s$now = s$cage
		s$to_go = s$cage
		s$treatment = 'out_b'
		
		s2 = s
		s2$now = s2$cage
		s2$to_go = paste('wu',s2$exp_wu, sep='')
		s2$week = w$w2[match(s2$week, w$w1)]	
		s2$treatment = 'single'
		
		s3 = s2
		s3$now = s3$to_go
		s3$to_go = ifelse(s3$exp_wu%in%c(3,7), s3$cage, 'wu3')
		s3$treatment = ifelse(s3$exp_wu%in%c(3,7), 'out_a', 'group')
		s3$week = w$w3[match(s3$week, w$w2)]	
		
		
		s4 = s3
		s4$now = 'wu3'
		s4$to_go = ifelse(s4$to_go == 'wu3', s4$cage, NA)
		s4 = s4[!is.na(s4$to_go),]
		s4$week = w$w4[match(s4$week, w$w3)]	
		s4$treatment = 'out_a'
		
		s = rbind(s,s2,s3,s4)
		s$treatment = factor(s$treatment,levels = c("single","out_b","out_a", "group"))
		s = s[order(s$week, s$treatment, s$now),]	
		
		s$cage = s$exp_wu = NULL
		#write.csv(s, paste(outdir,'schedule_all_weeks.csv', sep = ''),row.names = FALSE)
		s = s[which(s$week == as.POSIXct(date_)),]
		write.csv(s, paste(outdir,'weekly_schedule_', date_,'.csv', sep = ''),row.names = FALSE)
		#write.table(s, paste(wd,'weekly_schedulle.txt', sep = ''),sep = "\t",row.names = FALSE)
		}
	
{# explore 2017-08-01 10:39:25 based on shifting/release of 50 birds	
e = readWorksheetFromFile(paste(wd,'knot release august 2017.xlsx', sep = ''), sheet=1, endCol = 8)
e = readWorksheetFromFile(paste(wd,'knot release august 2017_MB_suggestions.xlsx', sep = ''), sheet=1, endCol = 10)
	d = readWorksheetFromFile(paste(wd,'birds_sex.xlsx', sep = ''), sheet=1)
	e$sex = d$Sex[match(e$ID,d$bird_id)]
	#d = read.table(paste(wd, 'Eva_Kim/formartin_tochoosebirds.txt', sep=''), header = TRUE, stringsAsFactors = FALSE)
	#e$sex = d$sex[match(e$ID,d$bird_id)]	
	s$bird_id[!s$bird_id%in%e$ID[!is.na(e$rhythm)]]
	e$ID[!e$ID%in%s$bird_id & !is.na(e$rhythm)] # H523 included, but not part
	
summary(factor(e$sex))
table(e$sex,e$to.cage.)
summary(factor(e$to.cage.))
table(e$Species,e$to.cage.)
summary(factor(e$rhythm))
table(e$rhythm,e$to.cage.)
table(e$Age,e$to.cage.)
table(e$Age,e$to.cage., e$Species)

summary(factor(e$new))
table(e$sex,e$new)
table(e$Species,e$new)
table(e$rhythm,e$new)
table(e$Age,e$new)

summary(factor(e$new2))
table(e$sex,e$new2)
table(e$Species,e$new2)
table(e$rhythm,e$new2)
table(e$Age,e$new2)
table(e$Age,e$new2, e$Species)
}
{# adjusted cages
	e = readWorksheetFromFile(paste(wd,'new_cage_distributions_2017-08-01.xlsx', sep = ''), sheet=1, endCol = 8)
	d = readWorksheetFromFile(paste(wd,'birds_sex.xlsx', sep = ''), sheet=1)
	e$sex = d$Sex[match(e$ID,d$bird_id)]
	
	wb <- loadWorkbook(paste(wd,"new_cage_distributions_2017-08-01_.xlsx",sep=''), create = TRUE)
	#creating sheets within an Excel workbook
	createSheet(wb, name = "plan")
	writeWorksheet(wb,e,sheet = "plan")
	saveWorkbook(wb)
}
{# new temporary schedulle
e = readWorksheetFromFile(paste(wd,'new_cage_distributions_2017-08-01_.xlsx', sep = ''), sheet=1, endCol = 9)
d = read.csv(paste(outdir,'weekly_schedule_', date_,'.csv', sep = ''),stringsAsFactors = FALSE)
e$to_go = d$to_go[match(e$ID, d$bird_id)]
e$treatment = d$treatment[match(e$ID, d$bird_id)]

wb <- loadWorkbook(paste(wd,"new_cage_distributions_2017-08-01_with_T.xlsx",sep=''), create = TRUE)
	#creating sheets within an Excel workbook
	createSheet(wb, name = "plan")
	writeWorksheet(wb,e,sheet = "plan")
	saveWorkbook(wb)
	
d = read.csv(paste(outdir,'weekly_schedule_', date_,'.csv', sep = ''),stringsAsFactors = FALSE)
d$to_go[nchar(d$to_go)<3] = e$new[match(d$bird_id[nchar(d$to_go)<3],e$ID)]	
write.csv(d, paste(outdir,'weekly_schedule_', date_,'_.csv', sep = ''),row.names = FALSE)
}



