#################################################     BACKGROUND
#################################################     
# This script re-produces Tables 2 & 3 and Figures 1 & 2 of the following Manuscript:
#     Title: Lesion size matters: A validation dataset for melanoma detection
#     Author list: Emily A. Cowen, Paras Mehta, Nicholas Raymond Kurtansky, Erik P Duhaime, 
#                  Pascale Guitera, Qishen Ha, Alan Halpern, Kivanc Kose, Bo Liu, Fuxu Liu, 
#                  Michael A. Marchetti, Jochen Weber, Veronica Rotemberg
#     TL;DR: We present a dataset of consecutive biopsies for melanoma across year 2020 
#            with novel annotations to validate melanoma classifier algorithms 
#            according to size and lesion difficulty.
#
# Image dataset URL: https://doi.org/10.34970/151324
#
# NOTE: If you are assessing your algorithm's differentiability on this dataset
#       against these benchmark methods (crowd annotation and an existing classifier),
#       please see the section 'NEW PREDICTION SCORES'


#################################################     
#################################################     CLEAR ENVIRONMENT
#################################################     
rm(list=ls())



#################################################     
#################################################     NEW PREDICTION SCORES
#################################################     
# specify location of new model output scores
newmodel_file = "SPECIFY LOCATION"
# specify column indexes of
#   Image name
column.image_name = 1
#   Model output
column.modelscore = 2



#################################################     
#################################################     DATA FOMRATTING 
#################################################     
# load functions
library(readr)
library(dplyr)
library(gtsummary)
library(tidyr)
library(pROC)
library(ggplot2)
library(wesanderson)
library(plotROC)
library(mskRvis)
library(gridExtra)
library(flextable)
# define operations
`%notin%` <- Negate(`%in%`)

# Dataset URL
urlfile <- "https://raw.githubusercontent.com/ISIC-Research/lesion-size-2020/main/lesion-size-2020_metadata.csv"

# load algorithm scores
if(newmodel_file == "SPECIFY LOCATION"){
  myscores <- read_csv(url(urlfile)) %>%
    select(image_name, new_model = adae_score)
} else{
  myscores <- read_csv(urlfile) %>%
    select(image_name = column.image_name, new_model = column.modelscore)
}

# load metadata and merge
mydata <- merge(
  x = read_csv(url(urlfile)),
  y = myscores,
  by = "image_name",
  all.x = T, all.y = F) %>%
  # define new variables
  mutate(age_cat = case_when(
    age <= 18 ~ "0-18",
    age >= 18 & age < 50 ~ "19-49",
    age >= 50 & age < 60 ~ "50-59",
    age >= 60 & age < 70 ~ "60-69",
    age >= 70 ~ "70+"
  )) %>%
  mutate(crowd_score_cat = case_when(
    crowd_score <= .10 ~ "0-10%",
    crowd_score > .10 & crowd_score <= .50 ~ "11-50%",
    crowd_score > .50 & crowd_score <= .90 ~ "51-90%",
    crowd_score > .90 ~ "91-100%"
  )) %>%
  mutate(tiny = case_when(
    tiny == 0 ~ ">5mm",
    tiny == 1 ~ "<=5mm"
  )) %>%
  mutate(mpath_cat = factor(case_when(
    mpath == 1 ~ "1",
    mpath == 2 ~ "2",
    mpath == 2.5 ~ "2.5",
    mpath == 3 ~ "3",
    mpath == 4 ~ "4",
    mpath == 4.5 ~ "4.5",
    mpath == 5 ~ "5",
    is.na(mpath) ~ "NA"
  ), levels = c("NA", "1", "2", "2.5", "3", "4", "4.5", "5")))



#################################################     
#################################################     FIGURE 1 - ROC CURVES
#################################################     
roc_tiny_new.model = roc(data = filter(mydata, tiny == "<=5mm"), response = target, predictor = new_model)
roc_large_new.model = roc(data = filter(mydata, tiny == ">5mm"), response = target, predictor = new_model)

roc_tiny_adae = roc(data = filter(mydata, tiny == "<=5mm"), response = target, predictor = adae_score)
roc_large_adae = roc(data = filter(mydata, tiny == ">5mm"), response = target, predictor = adae_score)

roc_tiny_crowd = roc(data = filter(mydata, tiny == "<=5mm"), response = target, predictor = crowd_score)
roc_large_crowd = roc(data = filter(mydata, tiny == ">5mm"), response = target, predictor = crowd_score)

SeSp <- rbind(
  # tiny ~ new model
  setNames(data.frame(
    roc_tiny_adae$sensitivities,  roc_tiny_adae$specificities, rep("model <= 5mm", length(roc_tiny_adae$sensitivities))
  ), c("tpf", "tnf", "model")),
  # large ~ new model
  setNames(data.frame(
    roc_large_adae$sensitivities,  roc_large_adae$specificities, rep("model > 5mm", length(roc_large_adae$sensitivities))
  ), c("tpf", "tnf", "model")),
  # tiny ~ ADAE
  setNames(data.frame(
    roc_tiny_adae$sensitivities,  roc_tiny_adae$specificities, rep("ADAE <= 5mm", length(roc_tiny_adae$sensitivities))
  ), c("tpf", "tnf", "model")),
  # large ~ ADAE
  setNames(data.frame(
    roc_large_adae$sensitivities,  roc_large_adae$specificities, rep("ADAE > 5mm", length(roc_large_adae$sensitivities))
  ), c("tpf", "tnf", "model")),
  # tiny ~ crowd
  setNames(data.frame(
    roc_tiny_crowd$sensitivities, roc_tiny_crowd$specificities, rep("Crowd <= 5mm", length(roc_tiny_crowd$sensitivities))
  ), c("tpf", "tnf", "model")),
  # large ~ crowd
  setNames(data.frame(
    roc_large_crowd$sensitivities,  roc_large_crowd$specificities, rep("Crowd > 5mm", length(roc_large_crowd$sensitivities))
  ), c("tpf", "tnf", "model"))) %>%
  arrange(model, tnf)
  
roc_frame <- mydata %>%
  select(image_name, tiny, target, new_model, adae_score, crowd_score) %>%
  gather(key = "model", value = "predictor", c(4:6)) %>%
  mutate(factor = factor(case_when(
    model == "new_model" & tiny == "<=5mm" ~ "New model on lesions \u2264 5mm",
    model == "new_model" & tiny == ">5mm" ~ "New model on lesions > 5mm",
    model == "adae_score" & tiny == "<=5mm" ~ "ADAE on lesions \u2264 5mm",
    model == "adae_score" & tiny == ">5mm" ~ "ADAE on lesions > 5mm",
    model == "crowd_score" & tiny == "<=5mm" ~ "Crowd on lesions \u2264 5mm",
    model == "crowd_score" & tiny == ">5mm" ~ "Crowd on lesions > 5mm",
  ), levels = c("New model on lesions > 5mm", "New model on lesions \u2264 5mm", "ADAE on lesions > 5mm", "ADAE on lesions \u2264 5mm", "Crowd on lesions > 5mm", "Crowd on lesions \u2264 5mm")))

# plot figure 1
if(newmodel_file == "SPECIFY LOCATION"){
  fig.1 <- ggplot(data = filter(roc_frame, model != "new_model"), aes(d = target, m = predictor, color = factor, linetype = factor)) +
    geom_roc(n.cuts = 0) +
    style_roc() +
    scale_color_manual(values = c("#FD6467", "#FD6467", "#5B1A18", "#5B1A18")) +
    scale_linetype_manual(values = rep(c("solid", "dashed"), 3)) +
    theme(legend.position = c(0.8, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(linetype = "solid", color = "black", size = .5),
          legend.key.width = unit(1.5,"cm"))
} else{
  fig.1 <- ggplot(data = roc_frame, aes(d = target, m = predictor, color = factor, linetype = factor)) +
    geom_roc(n.cuts = 0) +
    style_roc() +
    scale_color_manual(values = c("#F1BB7B", "#F1BB7B", "#FD6467", "#FD6467", "#5B1A18", "#5B1A18")) +
    scale_linetype_manual(values = rep(c("solid", "dashed"), 3)) +
    theme(legend.position = c(0.8, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(linetype = "solid", color = "black", size = .5),
          legend.key.width = unit(1.5,"cm"))
}



#################################################     
#################################################     TABLE 2 - AUC, SPECIFICITY, AND SENSITIVITY
#################################################     
c.subset = rep(NA, 6)
c.model = rep(NA, 6)
c.auc = rep(NA, 6)
c.auc.lb = rep(NA, 6)
c.auc.ub = rep(NA, 6)
c.se = rep(NA, 6)
c.se.lb = rep(NA, 6)
c.se.ub = rep(NA, 6)

sp_newmodel.large = ci(roc_large_new.model, of = "sp", sensitivities = c(.95))
se_newmodel.large = ci(roc_large_new.model, of = "se", specificities = c(.50))
sp_newmodel.tiny = ci(roc_tiny_new.model, of = "sp", sensitivities = c(.95))
se_newmodel.tiny = ci(roc_tiny_new.model, of = "se", specificities = c(.50))

sp_adae.large = ci(roc_large_adae, of = "sp", sensitivities = c(.95))
se_adae.large = ci(roc_large_adae, of = "se", specificities = c(.50))
sp_adae.tiny = ci(roc_tiny_adae, of = "sp", sensitivities = c(.95))
se_adae.tiny = ci(roc_tiny_adae, of = "se", specificities = c(.50))

sp_crowd.large = ci(roc_large_crowd, of = "sp", sensitivities = c(.95))
se_crowd.large = ci(roc_large_crowd, of = "se", specificities = c(.50))
sp_crowd.tiny = ci(roc_tiny_crowd, of = "sp", sensitivities = c(.95))
se_crowd.tiny = ci(roc_tiny_crowd, of = "se", specificities = c(.50))

newmod.large = c(
  ">5mm",
  "New model",
  ci(roc_large_new.model)[2],
  ci(roc_large_new.model)[1],
  ci(roc_large_new.model)[3],
  sp_newmodel.large[2],
  sp_newmodel.large[1],
  sp_newmodel.large[3],
  se_newmodel.large[2],
  se_newmodel.large[1],
  se_newmodel.large[3]
)

newmod.tiny = c(
  "<=5mm",
  "New model",
  ci(roc_tiny_new.model)[2],
  ci(roc_tiny_new.model)[1],
  ci(roc_tiny_new.model)[3],
  sp_newmodel.tiny[2],
  sp_newmodel.tiny[1],
  sp_newmodel.tiny[3],
  se_newmodel.tiny[2],
  se_newmodel.tiny[1],
  se_newmodel.tiny[3]
)

adae.large = c(
  ">5mm",
  "ADAE",
  ci(roc_large_adae)[2],
  ci(roc_large_adae)[1],
  ci(roc_large_adae)[3],
  sp_adae.large[2],
  sp_adae.large[1],
  sp_adae.large[3],
  se_adae.large[2],
  se_adae.large[1],
  se_adae.large[3]
)

adae.tiny = c(
  "<=5mm",
  "ADAE",
  ci(roc_tiny_adae)[2],
  ci(roc_tiny_adae)[1],
  ci(roc_tiny_adae)[3],
  sp_adae.tiny[2],
  sp_adae.tiny[1],
  sp_adae.tiny[3],
  se_adae.tiny[2],
  se_adae.tiny[1],
  se_adae.tiny[3]
)

crowd.large = c(
  ">5mm",
  "Crowd",
  ci(roc_large_crowd)[2],
  ci(roc_large_crowd)[1],
  ci(roc_large_crowd)[3],
  sp_crowd.large[2],
  sp_crowd.large[1],
  sp_crowd.large[3],
  se_crowd.large[2],
  se_crowd.large[1],
  se_crowd.large[3]
)

crowd.tiny = c(
  "<=5mm",
  "Crowd",
  ci(roc_tiny_crowd)[2],
  ci(roc_tiny_crowd)[1],
  ci(roc_tiny_crowd)[3],
  sp_crowd.tiny[2],
  sp_crowd.tiny[1],
  sp_crowd.tiny[3],
  se_crowd.tiny[2],
  se_crowd.tiny[1],
  se_crowd.tiny[3]
)

# Table 2
if(newmodel_file == "SPECIFY LOCATION"){
  table.2 = t(data.frame(
    adae.large,
    adae.tiny,
    crowd.large,
    crowd.tiny
  )) %>% as.data.frame()
  colnames(table.2) <- c("Subset", "Model", "AUC", "LB_auc", "UP_auc", "SP_at95se", "LB_sp", "UB_sp", "SE_at50sp", "LB_se", "UB_se")
} else{
  table.2 = t(data.frame(
    newmod.large,
    newmod.tiny,
    adae.large,
    adae.tiny,
    crowd.large,
    crowd.tiny
  )) %>% as.data.frame()
  colnames(table.2) <- c("Subset", "Model", "AUC", "LB_auc", "UP_auc", "SP_at95se", "LB_sp", "UB_sp", "SE_at50sp", "LB_se", "UB_se")
}

# format table 2
table.2 = table.2 %>%
  mutate(AUC = as.numeric(AUC)) %>%
  mutate(LB_auc = as.numeric(LB_auc)) %>%
  mutate(UP_auc = as.numeric(UP_auc)) %>%
  mutate(SP_at95se = as.numeric(SP_at95se)) %>%
  mutate(LB_sp = as.numeric(LB_sp)) %>%
  mutate(UB_sp = as.numeric(UB_sp)) %>%
  mutate(SE_at50sp = as.numeric(SE_at50sp)) %>%
  mutate(LB_se = as.numeric(LB_se)) %>%
  mutate(UB_se = as.numeric(UB_se)) %>%
  flextable() %>%
  colformat_double(j = c("AUC", "LB_auc", "UP_auc", "SP_at95se", "LB_sp", "UB_sp", "SE_at50sp", "LB_se", "UB_se"), digits = 3) %>%
  add_header_row(top = TRUE, values = c(rep("", 2), rep("Area under the curve", 3), rep("Specificity (@ 95% se)", 3), rep("Sensitivity (@ 50% sp)", 3))) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_caption(caption = "Table 2: ROC curves")



#################################################     
#################################################     FIGURE 2 - SCORES BY MPATH AND LESION SIZE
#################################################     
temp <- mydata %>%
  filter(mpath_cat %notin% c("2.5", "4.5")) %>%
  mutate(tiny_ = case_when(
    tiny == ">5mm" ~ "> 5mm",
    tiny == "<=5mm" ~ "\u2264 5mm"
  ))

# Figure 2a - ADAE output score by MPATH and lesion size
f2a <- ggplot(temp, aes(x = mpath_cat, y = log(adae_score), fill = tiny_)) +
  geom_boxplot(alpha = .80) +
  scale_fill_manual(values = msk_cols(c("medium_blue", "light_yellow"))) +
  coord_cartesian(ylim = c(-10, 0)) +
  labs(x = "MPATH", y = "ADAE Score (ln)") +
  theme_light() +
  #theme(legend.position = c(0.8, 0.2),
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(linetype = "solid", color = "black", size = .5),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1, "cm")
  )

# Figure 2a - Crowd score by MPATH and lesion size
f2b <- ggplot(temp, aes(x = mpath_cat, y = crowd_score, fill = tiny_)) +
  geom_boxplot(alpha = .80) +
  scale_fill_manual(values = msk_palette("contrast_colorblind")[3:4]) +
  labs(x = "MPATH", y = "Crowd Score") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(linetype = "solid", color = "black", size = .5),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1, "cm")
  )

# Figure 2c - Crowd score by MPATH and lesion size
f2c <- ggplot(temp, aes(x = mpath_cat, y = new_model, fill = tiny_)) +
  geom_boxplot(alpha = .80) +
  scale_fill_manual(values = msk_palette("contrast_colorblind")[5:6]) +
  labs(x = "MPATH", y = "New Model Score") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(linetype = "solid", color = "black", size = .5),
        legend.key.width = unit(1,"cm"),
        legend.key.height = unit(1, "cm")
  )

# arrange sub-figures
if(newmodel_file == "SPECIFY LOCATION"){
  fig.2 <- grid.arrange(f2a, f2b, ncol = 2)
} else{
  fig.2 <- grid.arrange(f2a, f2b, f2c, ncol = 3)
}

# print figure 2
plot(fig.2)



#################################################     
#################################################     TABLE 3 - SUMMARY OF FIGURE 2
#################################################     
mpath_classes <- c("Unknown/NA", "1", "2", "3", "4", "5")
cols <- c("<=5mm", ">5mm", "pvalue")

mat.adae <- matrix(NA, nrow = length(mpath_classes), ncol = 3)
rownames(mat.adae) <- mpath_classes
colnames(mat.adae) <- cols
mat.crowd <- matrix(NA, nrow = length(mpath_classes), ncol = 3)
rownames(mat.crowd) <- mpath_classes
colnames(mat.crowd) <- cols
mat.newmodel <- matrix(NA, nrow = length(mpath_classes), ncol = 3)
rownames(mat.newmodel) <- mpath_classes
colnames(mat.newmodel) <- cols

for(m in mpath_classes){
  tempdata <- mydata %>%
    filter(mpath_cat %notin% c("2.5", "4.5")) %>%
    mutate(mpath_cat = case_when(
      mpath_cat == "NA" ~ "Unknown/NA",
      mpath_cat == 1 ~ "1",
      mpath_cat == 2 ~ "2",
      mpath_cat == 3 ~ "3",
      mpath_cat == 4 ~ "4",
      mpath_cat == 5 ~ "5"
    )) %>%
    filter(mpath_cat == m)
  
  mat.adae[m, 1] <- log(median(tempdata$adae_score[tempdata$tiny == "<=5mm"]))
  mat.adae[m, 2] <- log(median(tempdata$adae_score[tempdata$tiny == ">5mm"]))
  mat.adae[m, 3] <- wilcox.test(data = tempdata, adae_score ~ tiny)$p.value

  mat.crowd[m, 1] <- median(tempdata$crowd_score[tempdata$tiny == "<=5mm"])
  mat.crowd[m, 2] <- median(tempdata$crowd_score[tempdata$tiny == ">5mm"])
  mat.crowd[m, 3] <- wilcox.test(data = tempdata, crowd_score ~ tiny)$p.value
  
  mat.newmodel[m, 1] <- log(median(tempdata$new_model[tempdata$tiny == "<=5mm"]))
  mat.newmodel[m, 2] <- log(median(tempdata$new_model[tempdata$tiny == ">5mm"]))
  mat.newmodel[m, 3] <- wilcox.test(data = tempdata, new_model ~ tiny)$p.value
}

# Table 3a (ADAE)
table.3a = as.data.frame(mat.adae) %>%
  mutate(MPATH = row.names(mat.adae)) %>%
  select("MPATH", "<=5mm", ">5mm", "pvalue") %>%
  flextable() %>%
  colformat_double(j = c("<=5mm", ">5mm", "pvalue"), digits = 3) %>%
  add_header_row(top = TRUE, values = c("MPATH", "Median score (ln)", "Median score (ln)", "pvalue")) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_caption(caption = "Table 3a: ADAE scores")

# Table 3b (Crowd)
table.3b = as.data.frame(mat.crowd) %>%
  mutate(MPATH = row.names(mat.crowd)) %>%
  select("MPATH", "<=5mm", ">5mm", "pvalue") %>%
  flextable() %>%
  colformat_double(j = c("<=5mm", ">5mm", "pvalue"), digits = 3) %>%
  add_header_row(top = TRUE, values = c("MPATH", "Median score", "Median score", "pvalue")) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_caption(caption = "Table 3b: Crowd scores")

# Table 3c (New Model)
table.3c = as.data.frame(mat.newmodel) %>%
  mutate(MPATH = row.names(mat.newmodel)) %>%
  select("MPATH", "<=5mm", ">5mm", "pvalue") %>%
  flextable() %>%
  colformat_double(j = c("<=5mm", ">5mm", "pvalue"), digits = 3) %>%
  add_header_row(top = TRUE, values = c("MPATH", "Median score", "Median score", "pvalue")) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_caption(caption = "Table 3c: New model scores")



#################################################     
#################################################     PRINT TABLES AND FIGURES
#################################################     
plot(fig.1)  
print(table.2)
plot(fig.2)
print(table.3a)
print(table.3b)
print(table.3c)
