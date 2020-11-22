library(readstata13)
library(RStata)
library(dplyr)

######## SANITATION ############

# ANALYSIS TO DECIDE ON IMPUTING
# balance_vars <- sanitation_data %>% mutate(missing_data=is.na(bl_hyg_own)) %>%
#   select(starts_with('vil'), starts_with('cen'), missing_data)

# summary(lm(missing_data ~ ., data=balance_vars)) # drop some villages or???
# balance_vars %>% ggplot(aes(vil_share_landless, fill = missing_data)) + geom_density(alpha=0.2)


# filtered_sanitation_data %>% group_by(treat_cat_1) %>%
#   summarize(effect = mean(r4_hyg_access, na.rm = T), sd = sd(r4_hyg_access, na.rm = T))


prepare_sanitation_data <- function() {
  sanitation_data <- read.dta13('Sanitation/BD-SAN-FINAL.dta')
  sanitation_data$treatment <- sanitation_data$treat_cat_4 == 'Subsidy Won'
  levels(sanitation_data$treat_cat_2) = c('Control', 'Control', 'Control', 'Low', 'Med', 'High')
  
  for (column_name in colnames(sanitation_data)) {
    if (column_name == 'eligible') {
      sanitation_data[[column_name]] = sanitation_data[[column_name]] == 'Eligible'
    }
    else if(is.factor(sanitation_data[[column_name]]) & !grepl('treat', column_name, fixed=TRUE)) {
      sanitation_data[[column_name]] = sanitation_data[[column_name]] == 'Yes'
    }
  }
  filtered_sanitation_data <- sanitation_data %>% filter(eligible & !is.na(r4_any_od_adults)) # or hyg
  all_vars <- filtered_sanitation_data %>% select(starts_with('cen'), starts_with('vil'), starts_with('bl_c'), 'treat_cat_1', 'r4_hyg_access', 'bl_hyg_access', 'treatment')
  filtered_sanitation_data <- filtered_sanitation_data[complete.cases(all_vars),]
  
  pretreatment_vars <- filtered_sanitation_data %>% select(starts_with('cen'), starts_with('vil'), starts_with('bl_c'))
  treatment <- filtered_sanitation_data$treatment
  Y2 <- ((filtered_sanitation_data$r4_hyg_access) > (filtered_sanitation_data$bl_hyg_access)) * treatment # > (filtered_sanitation_data$bl_hyg_access)
  Y1 <- filtered_sanitation_data$r4_hyg_access
  cluster <- as.numeric(filtered_sanitation_data$cid)
  stratify_data <- data.frame(
    cid=cluster,
    treatment=treatment,
    pretr=filtered_sanitation_data$bl_c_hyg_own) %>%
    group_by(cid) %>% summarise(
      treatment=treatment[1],
      pretr=mean(pretr),
      n=n()
    )
  
  list(pretreatment_vars=pretreatment_vars, Y2=Y2, Y1=Y1, cluster=cluster, treatment=treatment, stratify_data=stratify_data)
}


############## MALARIA ##################


# NOTHING WORKS HERE

prepare_malaria_data <- function() {
  malaria_data <- read.dta13('Malaria/ACT_AllMain_FINAL_pub.dta')
  malaria_data <- malaria_data[malaria_data$coartemprice %in% c(100, 40),]
  malaria_data$treatment <- malaria_data$act100 == 0
  malaria_data$num_actv[is.na(malaria_data$num_actv)] <- 0
  
  malaria_data %>% group_by(coartemprice) %>% summarize(
    sought_treat=mean(sought_treat, na.rm=T), num_actv=mean(num_actv, na.rm=T), LOG_mal_prob21=mean(LOG_mal_prob21, na.rm=T), E_tabJ_illness=mean(E_tabJ_illness, na.rm=T) # NUM ACT -- NA == 0
  )
  
  malaria_data$B_antimal_cost_na <- is.na(malaria_data$B_antimal_cost)
  malaria_data$B_antimal_cost[is.na(malaria_data$B_antimal_cost)] <- 0
  
  malaria_data$B_adultteen_na <- is.na(malaria_data$B_adultteen)
  malaria_data$B_adultteen[is.na(malaria_data$B_adultteen)] <- 0
  malaria_data$LOG_mal_prob21[is.na(malaria_data$LOG_mal_prob21)] <- 0
  malaria_data$LOG_mal_prob21[is.na(malaria_data$LOG_mal_prob2)] <- 0
  
  pretreatment_vars <- malaria_data %>% select(starts_with('B_'))
  treatment <- malaria_data$treatment[complete.cases(pretreatment_vars)]
  Y2 <- malaria_data$num_actv[complete.cases(pretreatment_vars)] * treatment
  Y1 <- (malaria_data$used_act_teen[complete.cases(pretreatment_vars)] +
           malaria_data$used_act_kid[complete.cases(pretreatment_vars)] +
           malaria_data$used_act_baby[complete.cases(pretreatment_vars)]) +
    (malaria_data$used_act_teen2[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_kid2[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_baby2[complete.cases(pretreatment_vars)]) +
    (malaria_data$used_act_teen3[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_kid3[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_baby3[complete.cases(pretreatment_vars)]) +
    (malaria_data$used_act_teen4[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_kid4[complete.cases(pretreatment_vars)] +
       malaria_data$used_act_baby4[complete.cases(pretreatment_vars)])
  Y2[is.na(Y2)] <- 0
  Y1[is.na(Y1)] <- 0
  Y2 <- Y2
  Y1 <- Y1
  pretreatment_vars <- pretreatment_vars[complete.cases(pretreatment_vars),]
  stratify_data <- data.frame(treatment=treatment, B_act_best=pretreatment_vars$B_act_best)
  cluster <- as.numeric(rownames(stratify_data))
  stratify_data$cid <- cluster
  list(pretreatment_vars=pretreatment_vars, Y2=Y2, Y1=Y1, cluster=cluster, treatment=treatment, stratify_data=stratify_data)
}


############# IMUNIZATION #############

# RStata::stata("dataverse_/1_Merge Data.do",
#               stata.path = "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
#               stata.version = 13,
#               stata.echo = TRUE)



prepare_immunization_data <- function() {
  final_data <- read.dta13('dataverse_/imm_mergedwHHandBaseline.dta')
  complete <- final_data[!is.na(final_data$complete1to3),]
  Y2 <- (4) * complete$treat3 * complete$complete1to3 + complete$d2_5 # costs
  Y1 <- complete$complete1to3
  treatment <- complete$treat3
  X <- complete %>% select(matches('^d.*__1$'))
  X <- X[,sapply(X, mode) != 'character']
  X <- X[,sapply(X, function (x) {sum(is.na(x)) != length(x)})]
  X[is.na(X)] <- -100000
  pretreatment_vars <- X
  cluster <- as.numeric(rownames(complete))
  stratify_data <- data.frame(treatment=treatment, cid=cluster)
  list(pretreatment_vars=pretreatment_vars, Y2=Y2, Y1=Y1, cluster=cluster, treatment=treatment, stratify_data=stratify_data)
}

