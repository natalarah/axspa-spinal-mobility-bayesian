library(tidyverse)
#library(sets)
library(brms)
library(caret)

explanatory_vars <- names(datos)[1:11]

2^(length(explanatory_vars))

set.seed(123)
power_set <- unlist(lapply(1:length(explanatory_vars),
                           # Get all combinations
                           combinat::combn,
                           x = explanatory_vars,
                           simplify = FALSE),
                    recursive = FALSE)
power_set     

paste(power_set[[100]], collapse = " + ")
formula1 <- paste('CONTROLLED', paste(power_set[[100]], collapse = " + "), sep = ' ~ ')
as.formula(formula1)


datos %>%
  group_by(CONTROLLED) %>%
  count()

confusionMatrix(as_factor(rep('SI', len = nrow(datos))), datos$CONTROLLED)

all_log_reg_models_sensitivity <- NA
all_log_reg_models_specificity <- NA
len <- NA
BIC <- NA

steps <- c(1:length(power_set))

for(i in steps){
  
  len[i] = length(power_set[[i]])
  
  if(len[i] == 1){
    formula1 <- as.formula(paste('CONTROLLED', power_set[[i]], sep = ' ~ '))
    print(formula1)
    
    log_reg_model <- glm(formula1, data = datos, family = "binomial")
    BIC[i] = BIC(log_reg_model)
    log_reg_model_pred <- predict(log_reg_model, datos, type = 'response')
    log_reg_model_pred <- as_factor(if_else(log_reg_model_pred < 0.5, 'SI', 'NO'))
    
    Conf_Mat <- confusionMatrix(log_reg_model_pred, datos$CONTROLLED)
    all_log_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_reg_models_specificity[i])
    print(message)
    
  }
  else if(len[i] >= 2){
    
    formula1 <- as.formula(paste('CONTROLLED', paste(power_set[[i]], collapse = ' + '), sep = ' ~ '))
    print(formula1)
    
    log_reg_model <- glm(formula1, data = datos, family = "binomial")
    BIC[i] = BIC(log_reg_model)
    log_reg_model_pred <- predict(log_reg_model, datos, type = 'response')
    log_reg_model_pred <- as_factor(if_else(log_reg_model_pred < 0.5, 'SI', 'NO'))
    
    Conf_Mat <- confusionMatrix(log_reg_model_pred, datos$CONTROLLED)
    all_log_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_reg_models_specificity[i])
    print(message)
    
  }
}

hist(all_log_reg_models_specificity)

best_subset_raw_data <- data.frame(
  'id' = (1:length(all_log_reg_models_specificity)),
  'Sensitivity' = all_log_reg_models_sensitivity,
  'Specificity' = all_log_reg_models_specificity,
  'Num_Variables' = as_factor(len),
  'BIC' = BIC
)

write_csv(best_subset_raw_data, 'best_subset_raw_data.csv')

best_specificity <- which.max(all_log_reg_models_specificity)

best_subset_raw_data[best_specificity, ]

power_set[[best_specificity]]


best_subset_raw_data %>%
  filter(is.na(Specificity) == F) %>%
  group_by(Num_Variables) %>%
  summarize(Max_Spec = max(Specificity),
            Mean_Spec = mean(Specificity),
            SD_Spec = sd(Specificity),
            Max_Sens = max(Sensitivity)) %>%
  unique()

fig_orig <- best_subset_raw_data %>%
  ggplot(aes(x = Num_Variables, y = Specificity, group = Num_Variables)) +
  geom_boxplot(aes(fill = Num_Variables)) +
  xlab('Numero de Covariables') +
  ggtitle('Variables Originales:') +
  ylab('Especifidad') +
  ylim(0, 1) +
  theme(legend.position = 'none')

#best_subset_raw_data %>%
#  ggplot(aes(x = Specificity, y = Sensitivity)) +
#  geom_point(aes(color = Num_Variables)) +
  


Best_Models_by_Num <- best_subset_raw_data %>%
  group_by(Num_Variables) %>%
  filter(Specificity == max(Specificity)) %>%
  select(id, Num_Variables)

write_csv(Best_Models_by_Num, 'Best_Models_by_Num.csv')
#-------------------------------------------------------------------------------
library(brms)

df_priors = datos %>% mutate(CONTROLLED = if_else(CONTROLLED == 'SI', 0, 1))

all_log_bayes_reg_models_sensitivity <- NA
all_log_bayes_reg_models_specificity <- NA

for(i in (1:nrow(Best_Models_by_Num))){
  
  len[i] = length(power_set[[Best_Models_by_Num$id[i]]])
  
  if(len[i] == 1){
    
    selected_model <- power_set[[Best_Models_by_Num$id[i]]][1]
    
    formula1 <- as.formula(paste('CONTROLLED', paste('1', selected_model, sep = ' + '), sep = ' ~ '))
    print(formula1)
    
    priors <- get_prior(formula1,
                        data = df_priors,
                        family = binomial())
    
    print(priors)
    
    bayesian_model <- brm(formula1,
                          data = df_priors,
                          family = 'binomial',
                          cores = 4,
                          prior = priors)
    
    bayesian_model_pred <- predict(bayesian_model, df_priors)
    bayesian_model_pred1 <- as_factor(if_else(bayesian_model_pred[,1] < 0.5,
                                              'SI', 'NO'))
    true_obs <- as_factor(if_else(df_priors$CONTROLLED == 0, 'SI', 'NO'))
    Conf_Mat <- confusionMatrix(bayesian_model_pred1, true_obs)
    all_log_bayes_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_bayes_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_bayes_reg_models_specificity[i])
    print(message)
    
  }
  else if(len[i] >= 2){
    
    selected_model1 <- power_set[[Best_Models_by_Num$id[i]]]
    selected_model <- paste('CONTROLLED', paste(c('1', as.character(power_set[[Best_Models_by_Num$id[i]]])), collapse = ' + '), sep = '~')
    formula1 <- as.formula(selected_model)
    print(formula1)
    
    priors <- get_prior(formula1,
                        data = df_priors,
                        family = binomial())
    
    print(priors)
    
    bayesian_model <- brm(formula1,
                          data = df_priors,
                          family = 'binomial',
                          cores = 4,
                          prior = priors)
    
    bayesian_model_pred <- predict(bayesian_model, df_priors, type = 'response')
    bayesian_model_pred1 <- as_factor(if_else(bayesian_model_pred[,1] < 0.5,
                                              'SI', 'NO'))
    true_obs <- as_factor(if_else(df_priors$CONTROLLED == 0, 'SI', 'NO'))
    Conf_Mat <- confusionMatrix(bayesian_model_pred1, true_obs)
    all_log_bayes_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_bayes_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_bayes_reg_models_specificity[i])
    print(message)
    
  }
  
}


best_bayes_data2 <- data.frame(
  'Sensitivity' = all_log_bayes_reg_models_sensitivity,
  'Specificity' = all_log_bayes_reg_models_specificity
)


best_bayes_data2 <- cbind(Best_Models_by_Num, best_bayes_data2)
glimpse(best_bayes_data2)

best_bayes_data2$id <- as_factor(best_bayes_data2$id)
best_subset_raw_data$id <- as_factor(best_subset_raw_data$id)

best_models_performances <- left_join(best_bayes_data2, best_subset_raw_data, by = 'id')
# Remove Num_Variables.y column because it is redundant:
best_models_performances <- best_models_performances[,-(7)]

names(best_models_performances) <- c('id', 'Num_Variables', 'Bayes_Sensitivity',
                                     'Bayes_Specificity', 'Sensitivity', 'Specificity',
                                     'BIC')

best_models_performances %>%
  ggplot(aes(x = Num_Variables, y = Bayes_Specificity)) +
  geom_point(aes(color = 'Bayes Model'), shape = 15, size = 3) +
  geom_point(aes(x = Num_Variables, y = Specificity, 
                 color = 'Classic'), shape = 18, size = 3) +
  theme(legend.position = 'bottom')

best_models_performances %>%
  ggplot(aes(x = Num_Variables, y = Bayes_Specificity)) +
  geom_point(aes(color = 'Bayes Model', shape = 'Bayes', size = 4)) +
  geom_point(aes(x = Num_Variables, y = Specificity, 
                 color = 'Classic', shape = 'Classic', size = 4)) +
  xlab('Numero de Variables') +
  ylab('Especificidad') +
  theme(legend.position = 'none')

best_models_performances %>%
  ggplot(aes(x = Num_Variables, y = Bayes_Specificity)) +
  geom_bar(aes(fill = 'Bayes Model'), stat = 'identity') +
  geom_bar(aes(x = Num_Variables, y = Specificity,  fill = 'Classic Model'), stat = 'identity') +
  theme(legend.position = 'none')


#-------------------------------------------------------------------------------
library(brms)
library(caret)
formula1 <- as.formula('CONTROLLED ~ 1 + Mass_kg + CERV_ROT_LEF +CERV_ROT_RIG + CERV_FRONT_FLEX + LAT_CERV_FLEX_RIG + SCHOBERT_MOD + LAT_LUMBAR_FLEX_RIG + LAT_LUMBAR_FLEX_LEF')
print(formula1)

priors <- get_prior(formula1,
                    data = df_priors,
                    family = binomial())

print(priors)

bayesian_model <- brm(formula1,
                      data = df_priors,
                      family = 'binomial',
                      cores = 4,
                      prior = priors)

summary(bayesian_model)

bayesian_model_pred <- predict(bayesian_model, df_priors)
bayesian_model_pred_df <- data.frame(bayesian_model_pred)
#--------------------------------------------------------
#bayesian_model_pred_df$Observed <- datos$CONTROLLED
bayesian_model_pred_df$Observed <- true_obs
#--------------------------------------------------------
bayesian_model_pred1 <- as_factor(if_else(bayesian_model_pred[,1] < 0.5,
                                          'SI', 'NO'))
true_obs <- as_factor(if_else(df_priors$CONTROLLED == 0, 'SI', 'NO'))
true_obs <- as_factor(if_else(df_priors$CONTROLLED == 0, 'NO', 'SI'))
Conf_Mat <- confusionMatrix(bayesian_model_pred1, true_obs)

#-------------------------------------------------------------------------------



log_reg_model <- glm(formula1, data = datos,
                     family = 'binomial')

summary(log_reg_model)
