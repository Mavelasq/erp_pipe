library (tidyverse)

dat_final_comp = read.csv("data/dat_final_comp.csv")
dat_final_dif = read.csv("data/dat_final_dif.csv")

#prepare data####
#change names
dt.points.nam <- vector()
x =seq(from = -98, to = 900, by =2)
for (i in 1:length(x)) {
  dt.points.nam[[i]]<- paste0("DataPoint_",x[i])
}
names(dat_final_comp)[6:505] = dt.points.nam

#choose timewindows p3 = 320-380, n450 = 450-550, p2 = 220 - 260
which(names(dat_final_comp)== "DataPoint_260") #look up data point

#initialize tw
tw_p3     = dat_final_comp[,c(1:5,215:245)] #320-380
tw_p3_dif = dat_final_dif[,c(1:4,215:245)]

tw_N450     = dat_final_comp[,c(1:5,305:345)] #500 - 580
tw_N450_dif = dat_final_dif[,c(1:4,305:345)]

tw_p2     = dat_final_comp[,c(1:5,165:185)]
tw_p2_dif = dat_final_dif[,c(1:4,165:185)]

#initialize tw  mean df
tw_p3_mean = tw_p3[,1:5]
tw_p3_mean$tw_mean = 0

tw_N450_mean = tw_N450 [,1:5]
tw_N450_mean$tw_mean = 0

tw_p2_mean = tw_p2[,1:5]
tw_p2_mean$tw_mean = 0

#initialize tw p3 mean dif df
tw_p3_difmean = tw_p3_dif[,1:4]
tw_p3_difmean$tw_mean = 0

#initialize tw n450 mean dif df
tw_N450_difmean = tw_N450_dif[,1:4]
tw_N450_difmean$tw_mean = 0

#initialize tw p2 dif df
tw_p2_difmean = tw_p2_dif[,1:4]
tw_p2_difmean$tw_mean = 0

#loop to calculate tw means####
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


#loop to calculate tw difference means####
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
diff_mean_mus = dat_final_dif %>%
  group_by(Task, ElectrodeSite, mus_code) %>%
  summarize(
    across(starts_with("DataPoint_"), mean)
  )

write.csv(diff_mean, file = "diff_mean.csv")
write.csv(diff_mean_mus, file = "dif_mean_mus.csv")


#ANOVA####
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
p_n450_si = c(0.001,0.066,0.00001)

p.adjust(p_n450_si, method = "fdr")


#Slope analysis#####

#get p2, p3, n450 peaks per participant

#open dataframe with peak and latency info

pl_data = read.csv("data/dat.erp.csv")
pl_data = pl_data[,c(1:2, 4:ncol(pl_data))]
colnames(pl_data)[2] = "Condition"

#restructure pl_data
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

#change name of participant column to id
colnames(slope_erp)[1] <- "id"


#remove outliers and missing
pl_data_long.f = pl_data_long[pl_data_long$id %in% slope_erp$id,]

pl_data_long.f[pl_data_long.f == "" | pl_data_long.f == "???"] <- NA

pl_data_long.f = filter(pl_data_long.f, !is.na(ern.l))

pl_data_long.f[which(arrange(pl_data_long.f, id, Task)[,1] != arrange(slope_erp, id, Task)[,1]),]#test match

pl_data_long.f$p3.l[1]

dat_final_comp$Condition = ifelse(dat_final_comp$Condition == "c", "congruent", "incongruent") #change name of condition
colnames(dat_final_comp)[1] = "id" #change participant to id

#arrange dfs to have same order
slope_erp = arrange(slope_erp, id, Task, Condition, ElectrodeSite)
pl_data_long.f = arrange(pl_data_long.f, id, Task, Condition, ElectrodeSite)
dat_final_comp = arrange(dat_final_comp, id, Task, Condition, ElectrodeSite)

function {
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
      idx_col = grep("p2.l", colnames(pl_data_long.f)) #get index of erp
      lat = pl_data_long.f[i,idx_col] #get latency
      #find latency in timeline
      lat_idx = grep(lat, names(dat_final_comp))
      slope_erp$peak_p2[i] = dat_final_comp[i,lat_idx] #store peak value at slope_erp
      #n300
      idx_col_d = grep("n1.l", colnames(pl_data_long.f)) #get index of erp
      lat_d = pl_data_long.f[i,idx_col_d] #get latency
      #find latency in timeline
      lat_idx_d = grep(lat_d, names(dat_final_comp))
      slope_erp$dip_n1[i] = dat_final_comp[i,lat_idx_d] #store peak value at slope_erp
      #dip - 200
      #minx=lat_idx-100
      #slope_erp$dip_p3[i] = min(as.numeric(dat_final_comp[i,minx:lat_idx]))
    }
  }
}

# replace n300 with dip values
slope_erp$dip_n300[8] = slope_erp$dip_p3[8]
slope_erp$dip_n300[12] = slope_erp$dip_p3[12]
slope_erp$dip_n300[20] = slope_erp$dip_p3[20]
slope_erp$dip_n300[23] = slope_erp$dip_p3[23]
slope_erp$dip_n300[53] = slope_erp$dip_p3[53]
slope_erp$dip_n300[175] = slope_erp$dip_p3[175]



#make column of R2 or beta values
slope_erp = tw_p3[,1:5]
slope_erp$max_p3 = 0
slope_erp$min_p3 = 0
slope_erp$beta = 0
slope_erp$r2 = 0
slope_erp$beta_n3 = 0
slope_erp$r2_n3 = 0
slope_erp$peak_p3 = 0
slope_erp$dip_p3 = 0
slope_erp$dip_n300 = 0
slope_erp$dip_n400 = 0
slope_erp$dip_n1 = 0
slope_erp$peak_p2 = 0



slope_erp$Condition <- ifelse(slope_erp$Condition == "c", "congruent", "incongruent")

#get peak
for (i in 1: nrow(tw_p3)){
  slope_erp$max_p3[i] = max(tw_p3[i, 6:36])
}

#get dip
which(names(dat_final_comp) == "DataPoint_300")

for (i in 1:nrow(dat_final_comp)){
  slope_erp$min_p3[i] = min(dat_final_comp[i, 155:205])
}

for (i in 1:nrow(slope_erp)){
  xmin = which(dat_final_comp[i,] == slope_erp$dip_n1[i])
  xmax = which(dat_final_comp[i,] == slope_erp$peak_p2[i])

  x1_test = as.numeric(dat_final_comp[i,xmin:xmax])
  x1_test_length = 1:length(xmin:xmax)
  test_lm = lm(x1_test ~ x1_test_length)
  slope_erp$beta_p2[i] = as.numeric(test_lm$coefficients[2])
  slope_erp$r2_p2[i] = summary(test_lm)$r.squared
}

plot(seq(from = -98, to = 900, by = 2), dat_final_comp[12, 6:ncol(dat_final_comp)])