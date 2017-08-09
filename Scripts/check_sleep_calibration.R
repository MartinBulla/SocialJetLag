date_ = '2017-07-31'

require(XLConnect)
wd =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"
wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Experimental_set_up/"	
outdir =  "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/"
	
d = readWorksheetFromFile(paste(wd,'SocialJetLag_DB.xlsx',sep=''), sheet = 'activity_calibration')
	unique(d$beh)
	d = d[d$beh %in% c('sleeping', 'resting'),]
	d = d[!duplicated(paste(d$aviary,d$bird_id)),c('aviary','bird_id')]
	
w = read.csv(paste(wd2,'schedule_all_weeks.csv', sep = ''), stringsAsFactors = FALSE)
	w = w[as.Date(w$week) == as.Date(date_),]
	w = w[!w$bird_id %in% d$bird_id,]
	w = w[!w$treatment%in%c('single','group'),c('bird_id','actual_CR','sp','to_go', 'treatment')]
write.csv(w,paste(wd2, 'check_sleep_week_', date_,'.csv', sep=""), row.names = FALSE)