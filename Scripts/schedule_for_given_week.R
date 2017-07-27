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
		date_ = '2017-07-31'
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
		write.csv(s, paste(outdir,'schedule_all_weeks.csv', sep = ''),row.names = FALSE)
		s = s[which(s$week == as.POSIXct(date_)),]
		write.csv(s, paste(outdir,'weekly_schedule_', date_,'.csv', sep = ''),row.names = FALSE)
		#write.table(s, paste(wd,'weekly_schedulle.txt', sep = ''),sep = "\t",row.names = FALSE)
		}
	