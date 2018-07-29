library('ez')
library('plyr')
library('ggplot2')
options(warn=-1)

source('config.r')

setwd(working_dir)

cleaned_datafile = 'cleaned_data.csv'


get_complete_data <- function(df) {
    #
    #  return only rows in which both pre and post data are available
    #
    
    df_pre = df[(df$time == 'pre'),]
    df_post = df[(df$time == 'post'),]
    
    # find unique IDs where pre and post are available, even if you remove compatible distractor types
    prepost_ids = unique((merge(df_pre[df_pre$distractor_type != 'compatible',], 
                                df_post[df_post$distractor_type != 'compatible',], by = "Subject.ID"))$Subject.ID)
    df_pre_complete = df_pre[is.element(df_pre$Subject.ID, prepost_ids),]
    df_post_complete = df_post[is.element(df_post$Subject.ID, prepost_ids),]
    
    return(rbind(df_pre_complete, df_post_complete))

}

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


for (cut in c("2_5", "3_0", "None")){
	rcp1_c = df[(df$cut == cut) & (df$section == 1) & (df$time == 'pre'),]
	rcp2_rt_c = df[(df$cut == cut) & (df$section == 2) & (df$time == 'pre'),]
	rcp3_c = df[(df$cut == cut) & (df$section == 3) & (df$time == 'pre'),]

	# drop data where we don't have PRE and POST
	#rcp1_c = get_complete_data(rcp1_c)
	#rcp2_rt_c = get_complete_data(rcp2_rt_c)
	#rcp3_c = get_complete_data(rcp3_c)


	# Run AVG RTs
	print(c("RCP1 average RT. CUT: ", cut))
	z = ezANOVA(data = rcp1_c, wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
	print(z$ANOVA[z$ANOVA$"p<.05" == "*",])
	print(c("RCP2 average RT. CUT: ", cut))
	z = ezANOVA(data = rcp2_rt_c, wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
	print(z$ANOVA[z$ANOVA$"p<.05" == "*",])
	#print(c("RCP3 average RT. CUT: ", cut))
	#z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, distractor_type), dv = rt_avg)
	#print(z$ANOVA[z$ANOVA$"p<.05" == "*",])


	# Run MEDIAN RTs
	print(c("RCP1 median RT. CUT: ", cut))
	z = ezANOVA(data = rcp1_c, wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_med)
	print(z$ANOVA[z$ANOVA$"p<.05" == "*",])
	print(c("RCP2 median RT. CUT: ", cut))
	z = ezANOVA(data = rcp2_rt_c, wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_med)
	print(z$ANOVA[z$ANOVA$"p<.05" == "*",])
	#print(c("RCP3 median RT. CUT: ", cut))
	#z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, distractor_type), dv = rt_med)
	#print(z$ANOVA[z$ANOVA$"p<.05" == "*",])

	# ACCURACIES

	##
	##
	##

	# WM 

	#print(c("RCP3 average WM RT. CUT: ", cut))
	#z = ezANOVA(data = rcp3_c, wid = Subject.ID, within= c(wm_load_fix, distractor_type), between = Group, dv = wm_rt_avg)
	#print(z$ANOVA[z$ANOVA$"p<.05" == "*",])

}





#### Pre RCP1 RT by load, distractor type (no group)
#ezANOVA(data = rcp1_c[rcp1_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
#ezANOVA(data = rcp1_2_5_c[rcp1_2_5_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)
#ezANOVA(data = rcp1_3_0_c[rcp1_3_0_c$time == 'pre',], wid = Subject.ID, within= c(SA.load, distractor_type), dv = rt_avg)






