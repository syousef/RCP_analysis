library('ez')
library('plyr')
library('ggplot2')

setwd("/Users/janosbotyanszki/Dropbox/bubs/RCP_datasets")


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

df_pre1 = rcp1[(rcp1$time == 'pre'),]
df_pre2_rt = rcp2_rt[(rcp2_rt$time == 'pre'),]
df_pre2_acc = rcp2_acc[(rcp2_acc$time == 'pre'),]
df_pre3 = rcp3[(rcp3$time == 'pre'),]
df_post1 = rcp1[(rcp1$time == 'post'),]
df_post2_rt = rcp2_rt[(rcp2_rt$time == 'post'),]
df_post2_acc = rcp2_acc[(rcp2_acc$time == 'post'),]
df_post3 = rcp3[(rcp3$time == 'post'),]



#! update for go/all rcp2
##### Get healthy data but only the ones where both pre and post are present
prepost_ids1 = unique((merge(df_pre1, df_post1, by = "Subject.ID"))$Subject.ID)
prepost_ids2 = unique((merge(df_pre2_rt, df_post2_rt, by = "Subject.ID"))$Subject.ID)
prepost_ids3 = unique((merge(df_pre3, df_post3, by = "Subject.ID"))$Subject.ID)

df_pre1_complete = df_pre1[is.element(df_pre1$Subject.ID, prepost_ids1),]
df_pre2_rt_complete = df_pre2_rt[is.element(df_pre2_rt$Subject.ID, prepost_ids2),]
df_pre3_complete = df_pre3[is.element(df_pre3$Subject.ID, prepost_ids3),]
df_pre3_complete = df_pre3_complete[df_pre3_complete$Subject.ID != 7623,]

df_post1_complete = df_post1[is.element(df_post1$Subject.ID, prepost_ids1),]
df_post2_rt_complete = df_post2_rt[is.element(df_post2_rt$Subject.ID, prepost_ids2),]
df_post3_complete = df_post3[is.element(df_post3$Subject.ID, prepost_ids3),]
df_post3_complete = df_post3_complete[df_post3_complete$Subject.ID != 7623,]


big_df <- rbind(df_pre1_complete, df_post1_complete, df_pre2_rt_complete, df_post2_rt_complete, df_pre3_complete, df_post3_complete)





# Calculate aggregate statistics for PRE and POST per subject
rcp1_by_subject = ddply(rcp1, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
rcp2_by_subject = ddply(rcp2_acc, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
rcp3_by_subject = ddply(rcp3, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))

# Calculate aggregate statistics for PRE or POST per subject

pre1_by_subject = ddply(df_pre1, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
post1_by_subject = ddply(df_post1, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
pre2_by_subject_acc = ddply(df_pre2_acc, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
post2_by_subject_acc = ddply(df_post2_acc, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
pre2_by_subject_rt = ddply(df_pre2_rt, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
post2_by_subject_rt = ddply(df_post2_rt, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct))
pre3_by_subject = ddply(df_pre3, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), wm_rt_avg = mean(wm_rt..RCP.3.only.), acc_avg = mean(correct), wm_acc_avg = mean(wm_correct..RCP.3.only.))
post3_by_subject = ddply(df_post3, .(Subject.ID), summarise, rt_avg = mean(rt), rt_med = median(rt), wm_rt_avg = mean(wm_rt..RCP.3.only.), acc_avg = mean(correct), wm_acc_avg = mean(wm_correct..RCP.3.only.))


# OUTLIER DELETION

rcp1_env = get_thresholds_dict(rcp1$rt, log=TRUE)
rcp2_rt_env = get_thresholds_dict(rcp2_rt$rt, log=TRUE)
#rcp2_acc_env = get_thresholds_dict(rcp2_acc$rt, log=TRUE)
rcp3_env = get_thresholds_dict(rcp3$rt, log=TRUE)

rcp1_2_5 = rcp1[(!is.na(rcp1$rt)) & (rcp1$rt > rcp1_env$minus_2_5_sigma) & (rcp1$rt < rcp1_env$plus_2_5_sigma),]
rcp1_3_0 = rcp1[(!is.na(rcp1$rt)) & (rcp1$rt > rcp1_env$minus_3_0_sigma) & (rcp1$rt < rcp1_env$plus_3_0_sigma),]

rcp2_rt_2_5 = rcp2_rt[(!is.na(rcp2_rt$rt)) & (rcp2_rt$rt > rcp2_rt_env$minus_2_5_sigma) & (rcp2_rt$rt < rcp2_rt_env$plus_2_5_sigma),]
rcp2_rt_3_0 = rcp2_rt[(!is.na(rcp2_rt$rt)) & (rcp2_rt$rt > rcp2_rt_env$minus_3_0_sigma) & (rcp2_rt$rt < rcp2_rt_env$plus_3_0_sigma),]

rcp3_2_5 = rcp3[(!is.na(rcp3$rt)) & (rcp3$rt > rcp3_env$minus_2_5_sigma) & (rcp3$rt < rcp3_env$plus_2_5_sigma),]
rcp3_3_0 = rcp3[(!is.na(rcp3$rt)) & (rcp3$rt > rcp3_env$minus_3_0_sigma) & (rcp3$rt < rcp3_env$plus_3_0_sigma),]



# AGGREGATE DATA OVER TRIALS 
rcp1_avg = ddply(rcp1, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp1_2_5_avg = ddply(rcp1_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp1_3_0_avg = ddply(rcp1_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))

rcp2_rt_avg = ddply(rcp2_rt, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp2_rt_2_5_avg = ddply(rcp2_rt_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp2_rt_3_0_avg = ddply(rcp2_rt_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))

rcp3_avg = ddply(rcp3, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp3_2_5_avg = ddply(rcp3_2_5, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))
rcp3_3_0_avg = ddply(rcp3_3_0, .(Subject.ID, wm_load_fix, SA.load, distractor_type, time, Group, section), summarise, rt_avg = mean(rt), rt_med = median(rt), acc_avg = mean(correct), wm_rt_avg = mean(wm_rt..RCP.3.only.))


# drop data where we don't have PRE and POST
rcp1_c = get_complete_data(rcp1_avg)
rcp1_c$cut = 'None'
rcp1_2_5_c = get_complete_data(rcp1_2_5_avg)
rcp1_2_5_c$cut = '2_5'
rcp1_3_0_c = get_complete_data(rcp1_3_0_avg)
rcp1_3_0_c$cut = '3_0'

rcp2_rt_c = get_complete_data(rcp2_rt_avg)
rcp2_rt_c$cut = 'None'
rcp2_rt_2_5_c = get_complete_data(rcp2_rt_2_5_avg)
rcp2_rt_2_5_c$cut = '2_5'
rcp2_rt_3_0_c = get_complete_data(rcp2_rt_3_0_avg)
rcp2_rt_3_0_c$cut = '3_0'

rcp3_c = get_complete_data(rcp3_avg)
rcp3_c$cut = 'None'
rcp3_2_5_c = get_complete_data(rcp3_2_5_avg)
rcp3_2_5_c$cut = '2_5'
rcp3_3_0_c = get_complete_data(rcp3_3_0_avg)
rcp3_3_0_c$cut = '3_0'

table_to_output = rbind(rcp1_c, rcp1_2_5_c, rcp1_3_0_c, rcp2_rt_c, rcp2_rt_2_5_c, rcp2_rt_3_0_c, rcp3_c, rcp3_2_5_c, rcp3_3_0_c)
write.table(table_to_output, file = "cleaned_data.csv", row.names=FALSE, na="",col.names=TRUE, sep=",")

