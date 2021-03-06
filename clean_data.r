library('ez')
library('plyr')
library('ggplot2')

source('config.r')

setwd(working_dir)

calculate_rt_diff <- function(raw_df) {
	#
	# group by subject, load, group, wm_load_fix, and create dimension rt_incomp_minus_comp: 
	#   incompatible RTs - compatible RTs
	#
	
  	rt1 = ddply(raw_df[raw_df$distractor_type == 'compatible',], .(Subject.ID, SA.load, Group, wm_load_fix), summarise, rt_avg_compatible = mean(rt))
	rt2 = ddply(raw_df[raw_df$distractor_type == 'incompatible',], .(Subject.ID, SA.load, Group, wm_load_fix), summarise, rt_avg_incompatible = mean(rt))

	merged_df = merge(rt1, rt2, by = c("Subject.ID", "SA.load", "Group", "wm_load_fix"))
	merged_df['rt_incomp_minus_comp'] = merged_df['rt_avg_incompatible'] - merged_df['rt_avg_compatible']
	return(merged_df[!is.na(merged_df$Subject.ID),])
}

calculate_rt_diff_of_diff <- function(df1, df2) {
	#
	# merge two dataframes and calculate the difference of their 'rt_incomp_minus_comp' columns
	#
	
	merged_df = merge(df1, df2, by = c("Subject.ID", "SA.load", "Group", "wm_load_fix"))
	merged_df['rt_diff_of_diff'] = merged_df['rt_incomp_minus_comp.x'] - merged_df['rt_incomp_minus_comp.y']
	return(merged_df[!is.na(merged_df$Subject.ID),])
}

calculate_accuracy_diff <- function(df1, df2) {
  	#
  	# get average accuracy by subject for two dataframes
  	# merge and calculate the difference in accuracies
  	#
  	
  	corr1 = ddply(df1, .(Subject.ID), summarise, accuracy = mean(correct*1.0))
  	corr2 = ddply(df2, .(Subject.ID), summarise, accuracy = mean(correct*1.0))

	merged_df = merge(corr1, corr2, by = "Subject.ID", suffixes = c("_1", "_2"))
	merged_df['delta_accuracy'] = merged_df['accuracy_2'] - merged_df['accuracy_1']
	return(merged_df[!is.na(merged_df$Subject.ID),])
}

get_thresholds <- function(arr, log=FALSE) {
    #
    # returns the mean, 2.5sigma, and 3.0sigma thresholds of the given array after log-transforming it
    #
    
    arr = arr[!is.na(arr)]
    len = length(arr)
    if (log == TRUE) {
        arr = log10(arr)
        m = mean(arr)
        s = sd(arr)
        
        return(c(10^m, 10^(m - 3*s), 10^(m - 2.5*s), 10^(m + 2.5*s), 10^(m + 3*s)))
    }
    m = mean(arr)
    s = sd(arr)
    return(c(m, m - 3*s, m - 2.5*s, m + 2.5*s, m + 3*s))
}

get_thresholds_dict <- function(arr, log=FALSE) {
    #
    # returns a hash map with the thresholds and mean of the given array
    #
    
    threshold_arr = get_thresholds(arr, log=log)
    e = new.env(hash=TRUE)
    
    e$mu = threshold_arr[1]
    e$minus_3_0_sigma = threshold_arr[2]
    e$minus_2_5_sigma = threshold_arr[3]
    e$plus_2_5_sigma = threshold_arr[4]
    e$plus_3_0_sigma = threshold_arr[5]
    
    return(e)
}


print_fraction_below <- function(arr, thresh) {
    arr = arr[!is.na(arr)]
    print('Percent data below threshold: ')
    print(length(arr[arr < thresh])/length(arr)*100.)
}


df <- read.csv(file="rcp_data_v2.csv", fileEncoding="latin1")

# drop TBI subjects, compatible distractor types
df = df[df$Group != 'tbi',]
df = df[df$Group != 'unknown',]
df$Subject.ID <- as.factor(df$Subject.ID)
df$section <- as.factor(df$section)


# get subsets of the data for RCP 1, 2, and 3
# drop NAs 
rcp1 = df[(df$section == 1) & (!is.na(df$rt)),]
# remove no-go trials from RCP2. Cannot use this for accuracies of course
rcp2_rt = df[(df$section == 2) & (df$rt < 3.0) & (!is.na(df$rt)),]
rcp2_acc = df[(df$section == 2) & (!is.na(df$rt)),]
rcp3 = df[(df$section == 3) & (!is.na(df$rt)),]


# RT OUTLIER DELETION

rcp1_env = get_thresholds_dict(rcp1$rt, log=TRUE)
rcp2_rt_env = get_thresholds_dict(rcp2_rt$rt, log=TRUE)
rcp3_env = get_thresholds_dict(rcp3$rt, log=TRUE)

rcp1_2_5 = rcp1[(!is.na(rcp1$rt)) 
                        & (rcp1$rt > rcp1_env$minus_2_5_sigma) 
                        & (rcp1$rt < rcp1_env$plus_2_5_sigma),]
rcp1_3_0 = rcp1[(!is.na(rcp1$rt)) 
                        & (rcp1$rt > rcp1_env$minus_3_0_sigma) 
                        & (rcp1$rt < rcp1_env$plus_3_0_sigma),]

rcp2_rt_2_5 = rcp2_rt[(!is.na(rcp2_rt$rt)) 
                      & (rcp2_rt$rt > rcp2_rt_env$minus_2_5_sigma) 
                      & (rcp2_rt$rt < rcp2_rt_env$plus_2_5_sigma),]
rcp2_rt_3_0 = rcp2_rt[(!is.na(rcp2_rt$rt)) 
                      & (rcp2_rt$rt > rcp2_rt_env$minus_3_0_sigma) 
                      & (rcp2_rt$rt < rcp2_rt_env$plus_3_0_sigma),]


rcp3_2_5 = rcp3[(!is.na(rcp3$rt)) 
                      & (rcp3$rt > rcp3_env$minus_2_5_sigma) 
                      & (rcp3$rt < rcp3_env$plus_2_5_sigma),]
rcp3_3_0 = rcp3[(!is.na(rcp3$rt)) 
                      & (rcp3$rt > rcp3_env$minus_3_0_sigma) 
                      & (rcp3$rt < rcp3_env$plus_3_0_sigma),]


# AGGREGATE DATA OVER TRIALS 

rcp1_avg = ddply(rcp1, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp1_avg$cut = 'None'
rcp1_2_5_avg = ddply(rcp1_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp1_2_5_avg$cut = '2_5'
rcp1_3_0_avg = ddply(rcp1_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp1_3_0_avg$cut = '3_0'

rcp2_rt_avg = ddply(rcp2_rt, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp2_rt_avg$cut = 'None'
rcp2_rt_2_5_avg = ddply(rcp2_rt_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.)) 
rcp2_rt_2_5_avg$cut = '2_5'
rcp2_rt_3_0_avg = ddply(rcp2_rt_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp2_rt_3_0_avg$cut = '3_0'

rcp2_acc_avg = ddply(rcp2_acc, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp2_acc_avg$cut = 'None'

rcp3_avg = ddply(rcp3, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp3_avg$cut = 'None'
rcp3_2_5_avg = ddply(rcp3_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp3_2_5_avg$cut = '2_5'
rcp3_3_0_avg = ddply(rcp3_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, 
                  rt_avg = mean(rt), 
                  rt_med = median(rt), 
                  wm_rt_avg = mean(wm_rt..RCP.3.only.),
                  acc_avg = mean(correct), 
                  wm_acc_avg = mean(wm_correct..RCP.3.only.))
rcp3_3_0_avg$cut = '3_0'


table_to_output = rbind(rcp1_avg, rcp1_2_5_avg, rcp1_3_0_avg, rcp2_rt_avg, rcp2_rt_2_5_avg, rcp2_rt_3_0_avg, rcp3_avg, rcp3_2_5_avg, rcp3_3_0_avg)
write.table(table_to_output, file = "cleaned_data_RT.csv", row.names=FALSE, na="",col.names=TRUE, sep=",")


# Accuracy cuts

rcp1_acc = rcp1_avg[rcp1_avg$acc_avg >= 0.75,]
rcp1_acc$cut = 'rcp1_0_75'
rcp2_acc = rcp2_acc_avg[rcp2_acc_avg$acc_avg >= 0.80,]
rcp2_acc$cut = 'rcp2_0_80'
rcp3_acc = rcp3_avg[rcp3_avg$acc_avg >= 0.80,]
rcp3_acc$cut = 'rcp3_0_80'
rcp3_WM_acc = rcp3_avg[rcp3_avg$wm_acc_avg >= 0.70,]
rcp3_WM_acc$cut = 'rcp3_WM_0_70'

table_to_output = rbind(rcp1_acc, rcp2_acc, rcp3_acc, rcp3_WM_acc)
write.table(table_to_output, file = "cleaned_data_ACC.csv", row.names=FALSE, na="",col.names=TRUE, sep=",")



