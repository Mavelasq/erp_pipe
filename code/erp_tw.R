library (tidyverse)
install.packages("pracma")
library(pracma)

#open final data sets
dat_final_comp = read.csv("data/dat_final_comp.csv")
dat_final_dif = read.csv("data/dat_final_dif.csv")

#prepare data####
#change names
#comparison waves
dt.points.nam <- vector()
x =seq(from = -98, to = 900, by =2)
for (i in 1:length(x)) {
  dt.points.nam[[i]]<- paste0("DataPoint_",x[i])
}
names(dat_final_comp)[6:505] = dt.points.nam

#difference waves
dat_final_dif = relocate(dat_final_dif,Task, .after = ElectrodeSite)#relocate task
dt.points.nam.dif <- vector()
x =seq(from = -98, to = 900, by =2)
for (i in 1:length(x)) {
  dt.points.nam[[i]]<- paste0("DataPoint_",x[i])
}
names(dat_final_dif)[5:504] = dt.points.nam


#choose timewindows p3 = 320-380, n450 = 450-550, p2 = 220 - 260
which(names(dat_final_comp)== "DataPoint_260") #look up data point

#initialize time windows####

#initialize tw 
#creating dfs with the inc and con waves and difference waves for each erp
which(names(dat_final_dif) == "DataPoint_550")


tw_p3     = dat_final_comp[,c(1:5,215:245)] #320-380
tw_p3_dif = dat_final_dif[,c(1:4,215:245)]

tw_N450     = dat_final_comp[,c(1:5,279:329)] #500 - 580
tw_N450_dif = dat_final_dif[,c(1:4,279:329)]

tw_p3     = dat_final_comp[,c(1:5,215:245)] #320-380
tw_p3_dif = dat_final_dif[,c(1:4,215:245)]

tw_N450     = dat_final_comp[,c(1:5,305:345)] #500 - 580
tw_N450_dif = dat_final_dif[,c(1:4,305:345)]

tw_p2     = dat_final_comp[,c(1:5,165:185)] #220 - 260
tw_p2_dif = dat_final_dif[,c(1:4,165:185)]

#initialize tw mean df for comparison waves
tw_p3_mean = tw_p3[,1:5]
tw_p3_mean$tw_mean = 0

tw_N450_mean = tw_N450 [,1:5]
tw_N450_mean$tw_mean = 0

tw_p2_mean = tw_p2[,1:5]
tw_p2_mean$tw_mean = 0

#initialize tw mean df for difference waves
tw_p3_difmean = tw_p3_dif[,1:4]
tw_p3_difmean$tw_mean = 0

tw_N450_difmean = tw_N450_dif[,1:4]
tw_N450_difmean$tw_mean = 0

tw_p2_difmean = tw_p2_dif[,1:4]
tw_p2_difmean$tw_mean = 0

#loop to calculate tw means####
#comparison waves#
#p2
for (i in 1:nrow(tw_p2)){
  tw_p2_mean[i,6] = mean(as.numeric(tw_p2[i, 6:ncol(tw_p2)]))
}
#p3
for (i in 1:nrow(tw_p3)){
  tw_p3_mean[i,6] = mean(as.numeric(tw_p3[i, 6:ncol(tw_p3)]))
}
#n450
for (i in 1:nrow(tw_N450)){
  tw_N450_mean[i,6] = mean(as.numeric(tw_N450[i, 6:ncol(tw_N450)]))
}

#difference waves#
for (i in 1:nrow(tw_p2_dif)){
  tw_p2_difmean[i,5] = mean(as.numeric(tw_p2_dif[i, 5:ncol(tw_p2_dif)]))
}
for (i in 1:nrow(tw_p3_dif)){
  tw_p3_difmean[i,5] = mean(as.numeric(tw_p3_dif[i, 5:ncol(tw_p3_dif)]))
}
for (i in 1:nrow(tw_N450_dif)){
  tw_N450_difmean[i,5] = mean(as.numeric(tw_N450_dif[i, 5:ncol(tw_N450_dif)]))
}


#summarize mean####

#comparison waves
comp_mean_mus = dat_final_comp %>%
  group_by(Task, ElectrodeSite, mus_code, Condition) %>%
  summarize(
    across(starts_with("DataPoint_"), mean)
  )

#difference waves
diff_mean_mus = dat_final_dif %>%
  group_by(Task, ElectrodeSite, mus_code) %>%
  summarize(
    across(starts_with("DataPoint_"), mean)
  )

write.csv(diff_mean, file = "diff_mean.csv")
write.csv(diff_mean_mus, file = "dif_mean_mus.csv")


#ANOVA mean amplitude####
summary(aov(tw_mean ~ mus_code, filter(tw_N450_difmean, ElectrodeSite == "fz" & Task == "simon")))
summary(aov(tw_mean ~ mus_code, filter(tw_p2_mean, ElectrodeSite == "pz" & Task == "simon")))

#bar graph
tw_p3_sum = tw_p3_mean %>%
  group_by(Task, Condition, ElectrodeSite) %>%
  summarise(
    n = n(), 
    mean = mean(tw_mean,na.rm = TRUE), 
    sd = sd(tw_mean,na.rm = TRUE), 
  ) %>%
  mutate(se = sd/sqrt(n))

filter(tw_p3_sum, ElectrodeSite == "cz" & Task == "simon") %>%
ggplot(aes(x = Condition, y = mean))+
  geom_bar(stat = )

#correction for multiple comparison
#p300 simon ps
p_p3_si = c(0.0338,0.0244,0.104)
p_n450_si = c(0.0307,0.99)
p_n450_si = c(0.001,0.066,0.00001)

p.adjust(p_n450_si, method = "fdr")


#Slope analysis comparison waves####

##get p2, p3, n450 peaks per participant####
#open dataframe with peak and latency info
pl_data = read.csv("data/dat.erp.csv")
pl_data = pl_data[,c(1:2, 4:ncol(pl_data))] #delete extra condition column
colnames(pl_data)[2] = "Condition" #change name of condition column from "Stroop.congruence" to "Condition"

#restructure pl_data to match dat_final
pl_data_long <- pl_data %>%
  pivot_longer(
    cols = -c(id, Condition),
    names_to = "key",
    values_to = "value"
  ) %>%
  separate(
    col = key,
    into = c("Task", "erp", "ElectrodeSite", "measurement_type"),
    sep = "\\."
  )%>%
  unite(
    col = "erp_measure",
    c("erp", "measurement_type"),
    sep = "."
  )%>%
  pivot_wider(
    id_cols = c(id, Condition, Task, ElectrodeSite),
    names_from = erp_measure,
    values_from = value
  )


#prepare slope_erp to hold slope data####
#make column of R2 or beta values
slope_erp = tw_p3[,1:5]
slope_erp$dip_n300 = 0
slope_erp$dip_p3 = 0
slope_erp$peak_p3 = 0
slope_erp$dip_n400 = 0
slope_erp$dip_n1 = 0
slope_erp$peak_p2 = 0
slope_erp$beta_p2 = 0
slope_erp$beta_p3 = 0
slope_erp$beta_n450 = 0
slope_erp$r2_p2 = 0
slope_erp$r2_p3 = 0
slope_erp$r2_n450 = 0

#change condition names
slope_erp$Condition <- ifelse(slope_erp$Condition == "c", "congruent", "incongruent")

#change name of participant column to id
colnames(slope_erp)[1] <- "id"

#remove outliers and missing ids
pl_data_long.f = pl_data_long[pl_data_long$id %in% slope_erp$id,] #match all data_long in slope erp

pl_data_long.f[pl_data_long.f == "" | pl_data_long.f == "???"] <- NA #remove id if it has missing values
pl_data_long.f = filter(pl_data_long.f, !is.na(ern.l))
pl_data_long.f[which(arrange(pl_data_long.f, id, Task)[,1] != arrange(slope_erp, id, Task)[,1]),]#test match

dat_final_comp$Condition = ifelse(dat_final_comp$Condition == "c", "congruent", "incongruent") #change name of condition in dat_final
colnames(dat_final_comp)[1] = "id" #change participant to id

#arrange dfs to have same order
slope_erp = arrange(slope_erp, id, Task, Condition, ElectrodeSite)
pl_data_long.f = arrange(pl_data_long.f, id, Task, Condition, ElectrodeSite)
dat_final_comp = arrange(dat_final_comp, id, Task, Condition, ElectrodeSite)

#get latencies for dips and peaks####
for (i in 1:nrow(dat_final_comp)){
    #check if info matches in both dfs
    id_f   <- dat_final_comp$id[i] 
    task_f <- dat_final_comp$Task[i]
    cond_f <- dat_final_comp$Condition[i] 
    es_f   <- dat_final_comp$ElectrodeSite[i]
    id_p   <- pl_data_long.f$id[i] 
    task_p <- pl_data_long.f$Task[i]
    cond_p <- pl_data_long.f$Condition[i] 
    es_p   <- pl_data_long.f$ElectrodeSite[i]
    
  if (id_f == id_p & task_f == task_p & cond_f == cond_p & es_f == es_p){ #if info matches
    #p2
      idx_col = grep("p2.l", colnames(pl_data_long.f)) #get index of p2 peak latency
      lat = pl_data_long.f[i,idx_col] #get latency value
      #find latency in timeline
      lat_idx = grep(lat, names(dat_final_comp))
      slope_erp$peak_p2[i] = dat_final_comp[i,lat_idx] #store peak value at slope_erp
      #get p2 dip (n1)
      idx_col_d = grep("n1.l", colnames(pl_data_long.f)) #get index of erp
      lat_d = pl_data_long.f[i,idx_col_d] #get latency
      #find latency in timeline
      lat_idx_d = grep(lat_d, names(dat_final_comp))
      slope_erp$dip_n1[i] = dat_final_comp[i,lat_idx_d] #store peak value at slope_erp
      #dip - 200
      #minx=lat_idx-100
      #slope_erp$dip_p3[i] = min(as.numeric(dat_final_comp[i,minx:lat_idx]))
    #p3
      idx_col = grep("p3.l", colnames(pl_data_long.f)) #get index of p2 peak latency
      lat = pl_data_long.f[i,idx_col] #get latency value
      #find latency in timeline
      lat_idx = grep(lat, names(dat_final_comp))
      slope_erp$peak_p3[i] = dat_final_comp[i,lat_idx] #store peak value at slope_erp
      #get p2 dip (n1)
      idx_col_d = grep("n300.l", colnames(pl_data_long.f)) #get index of erp
      lat_d = pl_data_long.f[i,idx_col_d] #get latency
      #find latency in timeline
      lat_idx_d = grep(lat_d, names(dat_final_comp))
      slope_erp$dip_n300[i] = dat_final_comp[i,lat_idx_d] #store peak value at slope_erp
      #dip - 200
      minx=lat_idx-100
      slope_erp$dip_p3[i] = min(as.numeric(dat_final_comp[i,minx:lat_idx]))
    #n450
      idx_col = grep("n400.l", colnames(pl_data_long.f)) #get index of p2 peak latency
      lat = pl_data_long.f[i,idx_col] #get latency value
      #find latency in timeline
      lat_idx = grep(lat, names(dat_final_comp))
      slope_erp$dip_n400[i] = dat_final_comp[i,lat_idx] #store peak value at slope_erp
     
    }
 }

# replace n300 with dip values
slope_erp$dip_n300[8] = slope_erp$dip_p3[8]
slope_erp$dip_n300[12] = slope_erp$dip_p3[12]
slope_erp$dip_n300[20] = slope_erp$dip_p3[20]
slope_erp$dip_n300[23] = slope_erp$dip_p3[23]
slope_erp$dip_n300[53] = slope_erp$dip_p3[53]
slope_erp$dip_n300[175] = slope_erp$dip_p3[175]

#get slopes####
#n450
for (i in 1:nrow(slope_erp)){
  xmin = which(dat_final_comp[i,] == slope_erp$peak_p3[i])
  xmax = which(dat_final_comp[i,] == slope_erp$dip_n400[i])

  x1_test = as.numeric(dat_final_comp[i,xmin:xmax])
  x1_test_length = 1:length(xmin:xmax)
  test_lm = lm(x1_test ~ x1_test_length)
  slope_erp$beta_n450[i] = as.numeric(test_lm$coefficients[2])
  slope_erp$r2_n450[i] = summary(test_lm)$r.squared
}

#run function
slope_erp = calculate_slopes(dat_final_comp, slope_erp, "1", "2")

#function
calculate_slopes <- function(dat_final_comp, slope_erp, dip_suffix, peak_suffix) {
  dip_col <- paste0("dip_n", dip_suffix)
  peak_col <- paste0("peak_p", peak_suffix)
  beta_col <- paste0("beta_p", peak_suffix)
  r2_col <- paste0("r2_p", peak_suffix)
  n_rows <- nrow(dat_final_comp)
  
  for (i in 1:n_rows){
    xmin = which(dat_final_comp[i,] == slope_erp[i, dip_col])
    xmax = which(dat_final_comp[i,] == slope_erp[i, peak_col])
    
    x1_test = as.numeric(dat_final_comp[i, xmin:xmax])
    x1_test_length = 1:length(xmin:xmax)
    test_lm = lm(x1_test ~ x1_test_length)
    slope_erp[i, beta_col] = as.numeric(test_lm$coefficients[2])
    slope_erp[i, r2_col] = summary(test_lm)$r.squared
  }
  
  return(slope_erp)
}

#latencies comparison 
full_dat_comp = merge(pl_data_long.f, slope_erp, by = c("id", "ElectrodeSite", "Task", "Condition"))

summary(aov(beta_p3 ~ mus_code, filter(full_dat_comp, ElectrodeSite == "pz" & Task == "stroop" & Condition == "incongruent")))

str_p_com_mus = c(0.024, .711, .55)
p.adjust(str_p_com_mus, method = "fdr")

#Slope Analysis difference waves####
#make column of R2 or beta values
slope_erp_dif = dat_final_dif[,1:4]
slope_erp_dif$min_p3 = 0
slope_erp_dif$max_p3 = 0
slope_erp_dif$min_p2 = 0
slope_erp_dif$max_p2 = 0
slope_erp_dif$min_n450 = 0
slope_erp_dif$max_n450 = 0
slope_erp_dif$beta_p2 = 0
slope_erp_dif$beta_p3 = 0
slope_erp_dif$beta_n450 = 0
slope_erp_dif$r2_p2 = 0
slope_erp_dif$r2_p3 = 0
slope_erp_dif$r2_n450 = 0

#get dips and peaks based on time####
#arrange to match
colnames(slope_erp_dif)[1] = "id" #change col names
colnames(dat_final_dif)[1] = "id"

#rearrange to match
slope_erp_dif = arrange(slope_erp_dif, id, Task, ElectrodeSite)
dat_final_dif = arrange(dat_final_dif, id, Task, ElectrodeSite)



#get index
which(names(dat_final_dif) == "DataPoint_274")

#get peak
#p2 : 110-250 (129:179)
for (i in 1: nrow(slope_erp_dif)){
  slope_erp_dif$max_p3[i] = findpeaks(as.numeric(dat_final_dif[i, 119:191]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]
  lat_idx = which(dat_final_dif[i,] == slope_erp_dif$max_p3[i])
  minx=lat_idx-100
  slope_erp_dif$min_p3[i] = -findpeaks(as.numeric(-dat_final_dif[i, minx:lat_idx]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]

which(names(dat_final_dif) == "DataPoint_550")

#get peak
#p2 : 150-250 (129:179)
for (i in 1: nrow(slope_erp_dif)){
  slope_erp_dif$max_p3[i] = max(dat_final_dif[i, 129:179])
  lat_idx = which(dat_final_dif[i,] == max(dat_final_dif[i, 129:179]))
  minx=lat_idx-100
  slope_erp_dif$min_p3[i] = min(dat_final_dif[i, minx:lat_idx])
  

  xmax = which(dat_final_dif[i,] == slope_erp_dif$max_p3[i])
  xmin = which(dat_final_dif[i,] == slope_erp_dif$min_p3[i])
  
  x1_test = as.numeric(dat_final_dif[i, xmin:xmax])
  x1_test_length = 1:length(xmin:xmax)
  test_lm = lm(x1_test ~ x1_test_length)
  slope_erp_dif$beta_p3[i] = as.numeric(test_lm$coefficients[2])
  slope_erp_dif$r2_p3[i] = summary(test_lm)$r.squared
}

#p3 : 270-420 (189:264), min: max - 100
for (i in 1: nrow(slope_erp_dif)){
  slope_erp_dif$min_p3[i] = -findpeaks(as.numeric(-dat_final_dif[i, 189:264]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]
  lat_idx = which(dat_final_dif[i,] == slope_erp_dif$min_p3[i])
  maxx=lat_idx-100
  slope_erp_dif$max_p3[i] = findpeaks(as.numeric(dat_final_dif[i, maxx:lat_idx]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]
  slope_erp_dif$min_p3[i] = min(dat_final_dif[i, 189:264])
  lat_idx = which(dat_final_dif[i,] == min(dat_final_dif[i, 189:264]))
  maxx=lat_idx-100
  slope_erp_dif$max_p3[i] = max(dat_final_dif[i, maxx:lat_idx])
  

  xmax = which(dat_final_dif[i,] == slope_erp_dif$max_p3[i])
  xmin = which(dat_final_dif[i,] == slope_erp_dif$min_p3[i])
  
  x1_test = as.numeric(dat_final_dif[i, xmax:xmin])
  x1_test = as.numeric(dat_final_dif[i, xmin:xmax])
  
  x1_test_length = 1:length(xmax:xmin)
  test_lm = lm(x1_test ~ x1_test_length)
  slope_erp_dif$beta_p3[i] = as.numeric(test_lm$coefficients[2])
  slope_erp_dif$r2_p3[i] = summary(test_lm)$r.squared
}

#n450 : 400-550 (254:329), min: max - 100
for (i in 1: nrow(slope_erp_dif)){
  slope_erp_dif$max_n450[i] = findpeaks(as.numeric(dat_final_dif[i, 254:329]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]
  lat_idx = which(dat_final_dif[i,] == slope_erp_dif$max_n450[i])
  minx=lat_idx-100
  slope_erp_dif$min_n450[i] = -findpeaks(as.numeric(-dat_final_dif[i, minx:lat_idx]), nups = 1, ndowns = 1, npeaks = 1, sortstr = TRUE)[1]


#n450 : 400-550 (254:329), min: max - 100
for (i in 1: nrow(slope_erp_dif)){
  slope_erp_dif$max_n450[i] = max(dat_final_dif[i, 254:329])
  lat_idx = which(dat_final_dif[i,] == max(dat_final_dif[i, 254:329]))
  minx=lat_idx-100
  slope_erp_dif$min_n450[i] = min(dat_final_dif[i, minx:lat_idx])

  xmax = which(dat_final_dif[i,] == slope_erp_dif$max_n450[i])
  xmin = which(dat_final_dif[i,] == slope_erp_dif$min_n450[i])
  
  x1_test = as.numeric(dat_final_dif[i, xmin:xmax])
  x1_test_length = 1:length(xmin:xmax)
  test_lm = lm(x1_test ~ x1_test_length)
  slope_erp_dif$beta_n450[i] = as.numeric(test_lm$coefficients[2])
  slope_erp_dif$r2_n450[i] = summary(test_lm)$r.squared
}

#latencies####

#p2
for (i in 1:nrow(slope_erp_dif)){
  p = colnames(dat_final_dif)[which(dat_final_dif[i,] == slope_erp_dif$max_p2[i])]
  slope_erp_dif$p2_lat[i] = as.numeric(regmatches(p,regexpr("(?<=DataPoint_)[0-9]+", p, perl = TRUE)))
}
#p3
for (i in 1:nrow(slope_erp_dif)){
  p = colnames(dat_final_dif)[which(dat_final_dif[i,] == slope_erp_dif$min_p3[i])]
  slope_erp_dif$p3_lat[i] = as.numeric(regmatches(p,regexpr("(?<=DataPoint_)[0-9]+", p, perl = TRUE)))
}
#n450
for (i in 1:nrow(slope_erp_dif)){
  p = colnames(dat_final_dif)[which(dat_final_dif[i,] == slope_erp_dif$max_n450[i])]
  slope_erp_dif$n450_lat[i] = as.numeric(regmatches(p,regexpr("(?<=DataPoint_)[0-9]+", p, perl = TRUE)))
}

#ANOVA slope####
summary(aov(tw_mean ~ mus_code, filter(slope_erp, ElectrodeSite == "fz" & Task == "simon" & Condition == "incongruent")))

dif_summary = slope_erp_dif %>%
  group_by(mus_code, ElectrodeSite, Task) %>%
  summarise(
    mean_p2_lat   = mean(p2_lat),
    mean_p3_lat   = mean(p3_lat),
    mean_n450_lat = mean(n450_lat),
    sd_p2_lat   = sd(p2_lat),
    sd_p3_lat   = sd(p3_lat),
    sd_n450_lat = sd(n450_lat)
  )

p_p3_si_dif = c(0.02, .48, 0.06)
p_n450_si_dif = c(0.38, .84, 0.01)

p.adjust(p_n450_si_dif, method = "fdr")

#ANOVA beta####


#ANOVA slope####
summary(aov(beta_n450 ~ mus_code, filter(slope_erp_dif, ElectrodeSite == "fz" & Task == "simon")))


#plot waves####
plot(seq(from = -98, to = 900, by = 2), dat_final_dif[134, 5:ncol(dat_final_dif)])


#plot waves####
plot(seq(from = -98, to = 900, by = 2), dat_final_comp[12, 6:ncol(dat_final_comp)])
