library('ez')
library('plyr')
library('ggplot2')

setwd("/Users/janosbotyanszki/Dropbox/bubs/RCP_datasets")


cleaned_datafile = 'cleaned_data.csv'

df <- read.csv(file=cleaned_datafile, fileEncoding="latin1")



#### PRE DATA ANALYSIS
for (cut in c("2_5", "3_0", "None")){
	rcp1_c = df[(df$cut == cut) & (df$section == 1),]
	rcp2_rt_c = df[(df$cut == cut) & (df$section == 2),]
	rcp3_c = df[(df$cut == cut) & (df$section == 3),]

	pre1 = rcp1_c[rcp1_c$time == 'pre',]
	pre2 = rcp2_rt_c[rcp2_rt_c$time == 'pre',]
	pre3 = rcp3_c[rcp3_c$time == 'pre',]

	pre1_avgs = ddply(pre1, .(SA.load, distractor_type), summarise, rt_avg = mean(rt_avg)*1000)
	pre2_avgs = ddply(pre2, .(SA.load, distractor_type), summarise, rt_avg = mean(rt_avg)*1000)
	pre3_avgs = ddply(pre3, .(wm_load_fix, distractor_type), summarise, rt_avg = mean(rt_avg)*1000)

	print(c("##### CUT: ", cut))
	print(pre1_avgs)
	print(pre1_avgs[pre1_avgs$distractor_type == 'incompatible',]$rt_avg - pre1_avgs[pre1_avgs$distractor_type == 'neutral',]$rt_avg)
	print(pre2_avgs)
	print(pre2_avgs[pre2_avgs$distractor_type == 'incompatible',]$rt_avg - pre2_avgs[pre2_avgs$distractor_type == 'neutral',]$rt_avg)
	print(pre3_avgs)
	print(pre3_avgs[pre3_avgs$distractor_type == 'incompatible',]$rt_avg - pre3_avgs[pre3_avgs$distractor_type == 'neutral',]$rt_avg)

}




#### AVG RT ANOVAS

# reread data and drop compatible trials
df <- read.csv(file=cleaned_datafile, fileEncoding="latin1")
df = df[df$distractor_type != 'compatible',]

for (cut in c("None", "2_5", "3_0", "None")){
	rcp1_c = df[(df$cut == cut) & (df$section == 1),]
	rcp2_rt_c = df[(df$cut == cut) & (df$section == 2),]
	rcp3_c = df[(df$cut == cut) & (df$section == 3),]

	# AVG RTs
	print(c("RCP1 average RT. CUT: ", cut))
	z = ezANOVA(data = rcp1_c, wid = Subject.ID, within= c(SA.load, time, distractor_type), between = Group, dv = rt_avg)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]
	print(c("RCP2 average RT. CUT: ", cut))
	z = ezANOVA(data = rcp2_rt_c, wid = Subject.ID, within= c(SA.load, time, distractor_type), between = Group, dv = rt_avg)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]
	print(c("RCP3 average RT. CUT: ", cut))
	z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, time, distractor_type), between = Group, dv = rt_avg)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]
	print(c("RCP3 average WM RT. CUT: ", cut))
	z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, time, distractor_type), between = Group, dv = wm_rt_avg)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]

	# MEDIAN RTs
	print(c("RCP1 median RT. CUT: ", cut))
	z = ezANOVA(data = rcp1_c, wid = Subject.ID, within= c(SA.load, time, distractor_type), between = Group, dv = rt_med)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]
	print(c("RCP2 median RT. CUT: ", cut))
	z = ezANOVA(data = rcp2_rt_c, wid = Subject.ID, within= c(SA.load, time, distractor_type), between = Group, dv = rt_med)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]
	print(c("RCP3 median RT. CUT: ", cut))
	z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, time, distractor_type), between = Group, dv = rt_med)
	z$ANOVA[z$ANOVA$"p<.05" == "*",]

	# ACCURACIES

	##
	##
	##

}





#### Pre RCP1 RT by load, distractor type (no group)
#ezANOVA(data = rcp1_c[rcp1_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
#ezANOVA(data = rcp1_2_5_c[rcp1_2_5_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
#ezANOVA(data = rcp1_3_0_c[rcp1_3_0_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)






