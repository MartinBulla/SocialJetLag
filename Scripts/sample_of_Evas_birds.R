{# define working directories
	     #wd = "M:/Science/Projects/MC/test_data_axy4/"	
		 #wdd=wd
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
	  	 outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Output/"	
}
{# load data
	d = read.table(paste(wd, 'formartin_tochoosebirds.txt', sep=''), header = TRUE, stringsAsFactors = FALSE)
	o = read.csv(paste(wd, 'info bonus birds.csv', sep=''), header = TRUE, stringsAsFactors = FALSE)
	r = read.csv(paste(wd, 'birds.csv', sep=''), header = TRUE, sep = ';', stringsAsFactors = FALSE)
	r = r[!r$dead.released%in%c('dead dead/euthamized', 1, 'd'),]
	r = r[r$Species == 'isl',]
	r = r[!r$ID%in%o$ID,]
	#summary(factor(r$dead.released))
	#r = read.table(paste(wd, 'birds.txt', sep=''), header = TRUE, sep ='\t', stringsAsFactors = FALSE)
}
{# explore relationships on diet level
	b = ddply(d,.(bird_id, sex, age, diet), summarise, giz = median(gizzard), exp = median(exp))

	# distributions
		ggplot(b, aes(x = exp)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp)) + geom_histogram()
		ggplot(b[b$diet=='T',], aes(x = exp)) + geom_density(alpha = 0.5)
		ggplot(b[b$diet=='T',], aes(x = exp)) + geom_histogram()
		ggplot(b[b$diet=='H',], aes(x = exp)) + geom_density(alpha = 0.5)
		ggplot(b[b$diet=='H',], aes(x = exp)) + geom_histogram()
		
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp, fill = diet)) + geom_density(alpha = 0.5)
		
		ggplot(b, aes(x = giz)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz)) + geom_histogram()
		
		ggplot(b[b$diet=='T',], aes(x = giz)) + geom_density(alpha = 0.5)
		ggplot(b[b$diet=='T',], aes(x = giz)) + geom_histogram()
		ggplot(b[b$diet=='H',], aes(x = giz)) + geom_density(alpha = 0.5)
		ggplot(b[b$diet=='H',], aes(x = giz)) + geom_histogram()
		
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz, fill = diet)) + geom_density(alpha = 0.5)		
	
	# relationships
		ggplot(b, aes(x = diet, y = exp, fill = age, col = sex)) + geom_boxplot(alpha = 0.5)
		ggplot(b, aes(x = diet, y = giz, fill = age, col = sex)) + geom_boxplot(alpha = 0.5)
		
		ggplot(b, aes(x = diet, y = exp, fill = age)) + geom_boxplot(alpha = 0.5)
		ggplot(b, aes(x = diet, y = giz, fill = age)) + geom_boxplot(alpha = 0.5)	
		
		ggplot(b, aes(x = giz, y = exp, col = age, shape = diet)) + geom_point(alpha = 0.5) + stat_smooth(se = FALSE)
		ggplot(b[b$diet == 'H',], aes(x = giz, y = exp, col = age, shape = sex)) + geom_point(alpha = 0.5) + stat_smooth(se = FALSE)
		
		ggplot(b[b$diet == 'H',], aes(x = giz, y = exp, col = sex)) + geom_point(alpha = 0.5) + stat_smooth()
		ggplot(b[b$diet == 'H',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth()
		
		ggplot(b[b$diet == 'T',], aes(x = giz, y = exp, col = sex)) + geom_point(alpha = 0.5) + stat_smooth()
		ggplot(b[b$diet == 'T',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth()	
	
}
{# explore means per bird_id
	d$n = 1
	b = ddply(d,.(bird_id, sex, age), summarise, giz = median(gizzard), exp = median(exp), n = sum(n))

	# distributions
		ggplot(b, aes(x = exp)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp)) + geom_histogram()
		
		ggplot(b, aes(x = exp, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sex ~ .)
		
		ggplot(b, aes(x = exp, fill = age)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5)
			
		ggplot(b, aes(x = giz)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz)) + geom_histogram()
		
		ggplot(b, aes(x = giz, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sex ~ .)
		
		ggplot(b, aes(x = giz, fill = age)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5)
	
	# relationships
		ggplot(b, aes(x = age, y = exp, fill = sex)) + geom_boxplot(alpha = 0.5)
		ggplot(b, aes(x = age, y = giz, fill =  sex)) + geom_boxplot(alpha = 0.5)
		
		
		ggplot(b, aes(x = giz, y = exp, col = age, shape = sex)) + geom_point(alpha = 0.5) + stat_smooth(se = FALSE)
		ggplot(b[b$sex == 'M',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth(se = TRUE)
		ggplot(b[b$sex == 'F',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth(se = TRUE)
		
	}
{# within individual
	d$n = 1
	b = ddply(d,.(bird_id, sex, age), summarise, giz = median(gizzard), exp = median(exp), n = sum(n))
	nrow(b)
	e = b[b$bird_id%in%b$bird_id[duplicated(b$bird_id)],]
	ggplot(e, aes(x = age, y = exp, fill = sex)) + 
				geom_boxplot( alpha = 0.5) + 
				geom_line(aes(group = bird_id),	alpha = 0.5, colour = "black") +
				facet_grid(sex ~ .) 
	
	ggplot(e, aes(x = age, y = giz, fill = sex)) + 
				geom_line(aes(group = bird_id),	alpha = 0.5, colour = "black") +
				facet_grid(sex ~ .) 
				
	ggplot(e, aes(x = age, y = giz, fill = sex)) + 
				geom_boxplot( alpha = 0.5) + 
				geom_line(aes(group = bird_id),	alpha = 0.5, colour = "black") +
				facet_grid(sex ~ .) 
	
	
}
{# use A and C2 data were possible

	d$n = 1
	
	b = ddply(d,.(bird_id, sex, age), summarise, giz = median(gizzard), exp = median(exp), n = sum(n))
	nrow(b)
	b1 = b[b$age%in%c('A', '2CY'),]
	b1[duplicated(b1$bird_id),]
	b2 = b[b$age == 'J' & !b$bird_id%in%b1$bird_id,]
	b = rbind(b1,b2)
	
	# distributions
		ggplot(b, aes(x = exp)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp)) + geom_histogram()
		
		ggplot(b, aes(x = exp, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sex ~ .)
		
		ggplot(b, aes(x = exp, fill = age)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5)
			
		ggplot(b, aes(x = giz)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz)) + geom_histogram()
		
		ggplot(b, aes(x = giz, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sex ~ .)
		
		ggplot(b, aes(x = giz, fill = age)) + geom_histogram(alpha = 0.5, position="dodge")
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5)
	
	# relationships
		ggplot(b, aes(x = age, y = exp, fill = sex)) + geom_boxplot(alpha = 0.5)
		ggplot(b, aes(x = age, y = giz, fill =  sex)) + geom_boxplot(alpha = 0.5)
		
		
		ggplot(b, aes(x = giz, y = exp, col = age, shape = sex)) + geom_point(alpha = 0.5) + stat_smooth(se = FALSE)
		ggplot(b[b$sex == 'M',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth(se = TRUE)
		ggplot(b[b$sex == 'F',], aes(x = giz, y = exp, col = age)) + geom_point(alpha = 0.5) + stat_smooth(se = TRUE)
		

}
{# use A and C2 data were possible, and only for available birds

	d$n = 1
	
	b = ddply(d,.(bird_id, sex, age), summarise, giz = median(gizzard), exp = median(exp), n = sum(n))
	nrow(b)
	b1 = b[b$age%in%c('A', '2CY'),]
	b1[duplicated(b1$bird_id),]
	b2 = b[b$age == 'J' & !b$bird_id%in%b1$bird_id,]
	b = rbind(b1,b2)
	b$type = NA
	b$type[b$bird_id%in%o$ID] = 'bonus'
	b$type[b$bird_id%in%r$ID] = 'avail'
	b = b[!is.na(b$type),]
	write.csv(b[, c('bird_id','sex')], paste(wd, 'birds_for_BMR_info.csv', sep=''),row.names = FALSE) 
	
	table(b$sex,b$type)
	table(b$age,b$type)
	table(b$age[b$type=='avail'],b$sex[b$type=='avail'])
	# distributions
		ggplot(b, aes(x = exp, fill = type)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = exp, fill = type)) + geom_histogram(position="dodge")
		
		ggplot(b, aes(x = exp, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge") + facet_grid(type ~ .)
		ggplot(b, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(type ~ .)
			ggsave(file=paste(outdir, 'av_bonus_exp-sex.png', sep=""))
		ggplot(b, aes(x = exp, fill = age)) + geom_density(alpha = 0.5) + facet_grid(type ~ .)
		ggplot(b, aes(x = exp, fill = age)) + geom_histogram(alpha = 0.5, position="dodge") + facet_grid(type ~ .)
		ggplot(b[b$type == 'bonus', ], aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b[b$type == 'avail', ], aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		

		ggplot(b, aes(x = giz, fill = type)) + geom_density(alpha = 0.5)
		ggplot(b, aes(x = giz, fill = type)) + geom_histogram(position="dodge")
		
		ggplot(b, aes(x = giz, fill = sex)) + geom_histogram(alpha = 0.5, position="dodge") + facet_grid(type ~ .)
		ggplot(b, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(type ~ .)
			ggsave(file=paste(outdir, 'av_bonus_gizz-sex.png', sep=""))
		ggplot(b, aes(x = giz, fill = age)) + geom_density(alpha = 0.5) + facet_grid(type ~ .)
		ggplot(b, aes(x = giz, fill = age)) + geom_histogram(alpha = 0.5, position="dodge") + facet_grid(type ~ .)
		ggplot(b[b$type == 'bonus', ], aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
		ggplot(b[b$type == 'avail', ], aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(age ~ .)
}

{# sample
	
	d$n = 1
	
	b = ddply(d,.(bird_id, sex, age), summarise, giz = median(gizzard), exp = median(exp), n = sum(n))
	nrow(b)
	b1 = b[b$age%in%c('A', '2CY'),]
	b1[duplicated(b1$bird_id),]
	b2 = b[b$age == 'J' & !b$bird_id%in%b1$bird_id,]
	b = rbind(b1,b2)
	b = b[b$bird_id%in%r$ID, ]
	b$pk=1:nrow(b)
	
	l = list()
	for( i in 1:10){
		s = data.frame(pk = sample(seq(1,nrow(b)), 12, replace = F))
		s$exp = b$exp[match(s$pk, b$pk)]
		s$giz = b$giz[match(s$pk, b$pk)]
		s$sex = b$sex[match(s$pk, b$pk)]
		s$age = b$age[match(s$pk, b$pk)]
		s$sample = i
		l[[i]] = s
	}
	s = do.call(rbind,l)
	write.csv(s, paste(wd, 'sample_2017-07-07_1-4best-6.csv', sep=''),row.names = FALSE) 
	# available
	ggplot(b, aes(x = giz)) + geom_histogram() 
		ggsave(file=paste(outdir, 'av_gizz-hist.png', sep=""))
	ggplot(b, aes(x = giz)) + geom_density() 
		ggsave(file=paste(outdir, 'av_gizz-den.png', sep=""))
	ggplot(b, aes(x = exp)) + geom_histogram() 
		ggsave(file=paste(outdir, 'av_exp-hist.png', sep=""))
	ggplot(b, aes(x = exp)) + geom_density() 
		ggsave(file=paste(outdir, 'av_exp-den.png', sep=""))	
	
	# sample 6
		s6=s[s$sample == 6,]	
		table(s6$sex)
		table(s6$age, s6$sex)
		s4=s[s$sample == 4,]
		table(s4$sex)
		table(s4$age, s4$sex)
		s1=s[s$sample == 1,]
		table(s1$sex)
		table(s1$age, s1$sex)
		
		ggplot(s[s$sample == 6,], aes(x = giz)) + geom_histogram() 
		ggsave(file=paste(outdir, 'sample6_gizz-hist.png', sep=""))
		ggplot(s[s$sample == 6,], aes(x = giz)) + geom_density() 
		ggsave(file=paste(outdir, 'sample6_gizz-dens.png', sep=""))
		
		ggplot(s[s$sample == 6,], aes(x = exp)) + geom_histogram() 
		ggsave(file=paste(outdir, 'sample6_exp-hist.png', sep=""))
		ggplot(s[s$sample == 6,], aes(x = exp)) + geom_density() 
		ggsave(file=paste(outdir, 'sample6_exp-dens.png', sep=""))
		
	# sample
	ggplot(s, aes(x = giz)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_gizz-hist.png', sep=""))
	ggplot(s, aes(x = giz)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_gizz.png', sep=""))
	ggplot(s, aes(x = giz, fill = sex)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_gizz-sex_hist.png', sep=""))
	ggplot(s, aes(x = giz, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_gizz-sex.png', sep=""))
	
	ggplot(s, aes(x = exp)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_exp_hist.png', sep=""))
	ggplot(s, aes(x = exp)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_exp.png', sep=""))
	ggplot(s, aes(x = exp, fill = sex)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_exp-sex_hist.png', sep=""))
	ggplot(s, aes(x = exp, fill = sex)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
		ggsave(file=paste(outdir, '12sample_exp-sex.png', sep=""))
	
	ggplot(s, aes(x = giz, fill = age)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
	ggplot(s, aes(x = giz, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
	ggplot(s, aes(x = giz, fill = interaction(age, sex))) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
	ggplot(s, aes(x = giz, fill = interaction(age, sex))) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
	
	
	ggplot(s, aes(x = exp, fill = age)) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
	ggplot(s, aes(x = exp, fill = age)) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
	ggplot(s, aes(x = exp, fill = interaction(age, sex))) + geom_histogram(position = 'dodge') + facet_grid(sample ~ .) 
	ggplot(s, aes(x = exp, fill = interaction(age, sex))) + geom_density(alpha = 0.5) + facet_grid(sample ~ .) 
	
	
	
	

}
# sampling
	g = b[b$type == 'general',]
	b$pk = 1:nrow(b)
	
	length(unique(b$pk))