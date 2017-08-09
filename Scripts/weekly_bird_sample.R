{# TOOLS
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/"	
	}
	{# load packages
	require(ggplot2)
	require(plyr)
	}
}
{# DATA	
	d  = read.csv(paste(wd, 'experimental_birds.csv', sep=''), header = TRUE, stringsAsFactors = FALSE)
	cr = d[d$type%in%c('can','ruf'),]
	d = d[!d$type%in%c('can','ruf'),]
	week = c('2017-07-10','2017-07-17','2017-07-24','2017-07-31','2017-08-07','2017-08-14','2017-08-21','2017-08-28','2017-09-04','2017-09-11','2017-09-18','2017-09-25','2017-10-02','2017-10-09')

}

{# distributions
	d$al = ifelse(is.na(d$bmr), 'n','y')
	ggplot(d, aes(y = exp, x = age, fill = sex)) + geom_boxplot()
		ggsave(file=paste(outdir, '23birds_exp_age-sex.png', sep=""))
		
	ggplot(d, aes(y = giz, x = age, fill = sex)) + geom_boxplot()
		ggsave(file=paste(outdir, '23birds_giz_age-sex.png', sep=""))

	ggplot(d, aes(y = bmr, x = age, fill = sex)) + geom_boxplot()
		ggsave(file=paste(outdir, '23birds_bmr_age-sex.png', sep=""))

		
	ggplot(d, aes(x = giz, fill = al)) + geom_histogram(position = 'dodge')
		ggsave(file=paste(outdir, '23birds_all-or-no-bmr_gizz-hist.png', sep=""))
	ggplot(d, aes(x = exp, fill = al)) + geom_histogram(position = 'dodge')
		ggsave(file=paste(outdir, '23birds_all-or-no-bmr_exp-hist.png', sep=""))
	ggplot(d, aes(x = exp, y = giz, col = al)) + geom_point()
	
	ggplot(d, aes(y = giz, x = al)) + geom_boxplot()
	ggplot(d, aes(y = exp, x = al)) + geom_boxplot()
	
	ggplot(d, aes(y = giz, x = al, fill = sex)) + geom_boxplot()
	ggplot(d, aes(y = exp, x = al, fill = sex)) + geom_boxplot()
	
	ggplot(d, aes(y = giz, x = al, fill = age)) + geom_boxplot()
	ggplot(d, aes(y = exp, x = al, fill = age)) + geom_boxplot()
}

{# DONE - weekly schedule sample 
	summary(factor(d$type))
	l = list()
	for (i in 1:6){
		if(i == 1){x = data.frame(week = week[i], aviary = 4:6, sp = 'isl', bird_id = sample(d$bird_id, 3, replace = F), stringsAsFactors = FALSE)
					x$sex = d$sex[match(x$bird_id, d$bird_id)]
					x$age = d$age[match(x$bird_id, d$bird_id)]
					x$bmr = d$bmr[match(x$bird_id, d$bird_id)]
					x$ex = d$exp[match(x$bird_id, d$bird_id)]
					x$giz = d$giz[match(x$bird_id, d$bird_id)]
				   l[[i]] = x
			}else{y = do.call(rbind,l)
				  x = data.frame(week = week[i],aviary = 4:7, sp = 'isl', bird_id = sample(d$bird_id[!d$bird_id%in%y$bird_id], 4, replace = F), stringsAsFactors = FALSE)
					x$sex = d$sex[match(x$bird_id, d$bird_id)]
					x$age = d$age[match(x$bird_id, d$bird_id)]
					x$bmr = d$bmr[match(x$bird_id, d$bird_id)]
					x$ex = d$exp[match(x$bird_id, d$bird_id)]
					x$giz = d$giz[match(x$bird_id, d$bird_id)]
					l[[i]] = x		
				}
			}	
	w = do.call(rbind,l)	
	
	r = cr[, c('type','bird_id','sex','age','bmr','exp','giz')]
	names(r)[names(r)%in%c('type','exp')] = c('sp','ex')
	r$week = c(week[7],week[7],week[7],week[1],week[1])
	r$aviary = c(4,5,6,3,7)
	
	s = rbind(w,r)
	s = s[order(s$week, s$aviary),]
	
# add last week sample as canutus is not going to be together (later manually moved toward later screening)
	s = read.csv(paste(wd,'s1.csv', sep = ''))	
		x = data.frame(week = week[7], aviary = 4:6, sp = 'isl', bird_id = sample(as.character(s$bird_id[s$sp=='isl' & s$aviary == 7]), 3, replace = F), stringsAsFactors = FALSE)
					x$sex = d$sex[match(x$bird_id, d$bird_id)]
					x$age = d$age[match(x$bird_id, d$bird_id)]
					x$bmr = d$bmr[match(x$bird_id, d$bird_id)]
					x$ex = d$exp[match(x$bird_id, d$bird_id)]
					x$giz = d$giz[match(x$bird_id, d$bird_id)]
					
	y = s[s$bird_id%in%x$bird_id,]
	y$bird_id = cr$bird_id[cr$type=='can']
	y$age = cr$age[cr$type=='can']
	y$sp = 'can'
	y$sex = y$bmr = y$ex = y$giz = NA
	
	ss = s[s$sp!='can' | !s$bird_id%in%x$bird_id,]	
	sf = rbind(ss,x,y)		
	sf = sf[order(sf$week, sf$aviary),]
	#write.csv(sf, paste(wd,'freeze/s1.csv', sep = ''), row.names = FALSE)
	#write.csv(sf, paste(wd,'weekly_acc_attachment.csv', sep = ''), row.names = FALSE)
	#s = read.csv(paste(wd,'s2.csv', sep = ''))
}
{# explore
	for (i in 2:3){
		if(i == 2){s = read.csv(paste(wd,'s2.csv', sep = ''))}
		if(i == 3){s = read.csv(paste(wd,'s3.csv', sep = ''))}
			
		ggplot(s[s$aviary%in%c(4:6),], aes( x = ex, fill = sex, col = age)) + geom_histogram() + facet_grid(week ~ .) +scale_color_grey()
		ggsave(file=paste(outdir, i,'weekly_groups_exp_sex-age.png', sep=""))	
		ggplot(s[s$aviary%in%c(4:6),], aes( x = giz, fill = sex, col = age)) + geom_histogram() + facet_grid(week ~ .) +scale_color_grey()
		ggsave(file=paste(outdir, i,'weekly_groups_giz_sex-age.png', sep=""))	
		ggplot(s[s$aviary%in%c(4:6),], aes( x = bmr, fill = sex, col = age)) + geom_histogram() + facet_grid(week ~ .) +scale_color_grey()
		ggsave(file=paste(outdir, i,'weekly_groups_bmr_sex-age.png', sep=""))	
	
	}
	s$al = ifelse(is.na(s$bmr), 'n','y')
	ggplot(s[s$aviary%in%c(4:6),], aes( x = ex, fill = factor(al))) + geom_histogram() + facet_grid(week ~ .) 
	
	
	ggplot(s[s$aviary%in%c(4:6),], aes( x = exp, fill = factor(age))) + geom_histogram() + facet_grid(week ~ .) 
	ggsave(file=paste(outdir, 'weekly_groups_exp_sex.png', sep=""))	
	ggplot(s[s$aviary%in%c(4:6),], aes( x = giz, fill = factor(sex))) + geom_histogram() + facet_grid(week ~ .) 
	ggplot(s[s$aviary%in%c(4:6),], aes( x = bmr, fill = factor(sex))) + geom_histogram() + facet_grid(week ~ .) 
	
	ggplot(s[s$aviary%in%c(4:6),], aes( x = ex, y = giz, col = factor(week))) + geom_point() + geom_line()
	ggplot(s[s$aviary%in%c(4:6),], aes( x = ex, y = bmr, col = factor(week))) + geom_point() + geom_line()
	
	ggplot(s[s$aviary%in%c(4:6),], aes( x = ex, fill = sex, col = age)) + geom_histogram() + facet_grid(week ~ .) +scale_color_grey()	
	
	dev.new()
	ggplot(s[s$sp=='isl' & s$aviary == 7,], aes( x = ex, fill = sex, col = age)) + geom_histogram() + scale_color_grey()
	}
