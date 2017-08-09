{# TOOLS
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Data/"	
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MC/Experimental_procedure/"	
	}
	{# load packages
	require(ggplot2)
	require(plyr)
	}
}
{# weekly schedule with cages
	s = read.csv(paste(wd,'weekly_acc_attachment.csv', sep = ''),stringsAsFactors = FALSE)
	b = read.csv(paste(wd,'birds_per_cage.csv', sep = ''),stringsAsFactors = FALSE)
	s$cage = b$Cage[match(s$bird_id, b$ID)]
	
	x = data.frame( bird_id = c(	'H324','Z564', 'Z555', 'Z558',
									'Z716',
									'Z543','Z546',
									'Z697',
									'H517',
									'Z698', 'Z907',
									'Z523',
									'H745','Z041'
									),
					cage = c(		1, 1,1,1,
									2,
									3,3,
									4,
									5,
									6,6,
									7,
									8,8
									),
									stringsAsFactors = FALSE)
	s$cage[nchar(s$cage)==3 & !is.na(s$cage)] = x$cage[match(s$bird_id[nchar(s$cage)==3 & !is.na(s$cage)], x$bird_id)]
	s$cage[s$sp == 'ruf'] = 'wu2'
	s$rnr = b$RNR[match(s$bird_id, b$ID)]
	names(s)[2] = 'exp_wu'
	write.csv(s[,c("week","rnr","bird_id","sp","sex","age","cage","exp_wu","bmr","ex" ,"giz")], paste(wd,'weekly_acc_attachment.csv', sep = ''),row.names = FALSE)
	
	s = read.csv(paste(wd,'weekly_acc_attachment.csv', sep = ''),stringsAsFactors = FALSE)
	b = read.csv(paste(wd,'ColourRings.csv', sep = ''),stringsAsFactors = FALSE)
		b$RNR = toupper(b$RNR)
	s$actual_CR = b$actual_cr[match(s$rnr, b$RNR)]	
	s$actual_CR[is.na(s$actual_CR)] = '------'
	
	write.csv(s[,c("week","rnr","bird_id","actual_CR","sp","sex","age","cage","exp_wu","bmr","ex" ,"giz")], paste(wd,'weekly_acc_attachment.csv', sep = ''),row.names = FALSE)
	write.csv(s[,c("week","rnr","bird_id","actual_CR","sp","sex","age","cage","exp_wu")], paste(wd,'weekly_acc_attachment_for_Luc.csv', sep = ''),row.names = FALSE)
}

{# weekly schedule bird shifts
	w = data.frame(w1 = week[1:7], w2 = week[2:8], w3 = week[3:9], w4 = week[4:10], stringsAsFactors = FALSE)
	s = read.csv(paste(wd,'weekly_acc_attachment.csv', sep = ''),stringsAsFactors = FALSE)
		
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
		s$treatment = factor(s$treatment,levels = c("out_a", "group", "out_b","single"))
		s = s[order(s$week, s$treatment, s$now),]	
		
		s$cage = s$exp_wu = NULL
		write.csv(s, paste(outdir,'weekly_schedulle.csv', sep = ''),row.names = FALSE)
		#write.table(s, paste(wd,'weekly_schedulle.txt', sep = ''),sep = "\t",row.names = FALSE)
		}
	