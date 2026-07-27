library(tidyverse)
library(sets)
library(caret)
library(PerformanceAnalytics)
library(pROC)
library(ISLR2)
library(leaps)
library(bayestestR)
library(bestglm)
library(glmulti)


pca_df <- data.frame(data_pca[['x']])

explanatory_vars <- names(pca_df)

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

pca_df$CONTROLLED <- datos$CONTROLLED

confusionMatrix(as_factor(rep('SI', len = nrow(pca_df))), pca_df$CONTROLLED)

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
    
    log_reg_model <- glm(formula1, data = pca_df, family = "binomial")
    BIC[i] = bic(log_reg_model)
    log_reg_model_pred <- predict(log_reg_model, pca_df, type = 'response')
    log_reg_model_pred <- as_factor(if_else(log_reg_model_pred < 0.5, 'SI', 'NO'))
    
    Conf_Mat <- confusionMatrix(log_reg_model_pred, pca_df$CONTROLLED)
    all_log_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_reg_models_specificity[i])
    print(message)
    
  }
  else if(len[i] >= 2){
    
    formula1 <- as.formula(paste('CONTROLLED', paste(power_set[[i]], collapse = ' + '), sep = ' ~ '))
    print(formula1)
    
    log_reg_model <- glm(formula1, data = pca_df, family = "binomial")
    BIC[i] = bic(log_reg_model)
    log_reg_model_pred <- predict(log_reg_model, pca_df, type = 'response')
    log_reg_model_pred <- as_factor(if_else(log_reg_model_pred < 0.5, 'SI', 'NO'))
    
    Conf_Mat <- confusionMatrix(log_reg_model_pred, pca_df$CONTROLLED)
    all_log_reg_models_sensitivity[i] = Conf_Mat[['byClass']][['Sensitivity']]
    all_log_reg_models_specificity[i] = Conf_Mat[['byClass']][['Specificity']]
    message = paste0('Specificity: ', all_log_reg_models_specificity[i])
    print(message)
    
  }
}


hist(all_log_reg_models_specificity)

best_subset_pca_raw_data <- data.frame(
  'id' = (1:length(all_log_reg_models_specificity)),
  'Sensitivity' = all_log_reg_models_sensitivity,
  'Specificity' = all_log_reg_models_specificity,
  'Num_Variables' = as_factor(len),
  'BIC' = BIC
)


write_csv(best_subset_pca_raw_data, 'best_subset_pca_raw_data.csv')

best_pca_specificity <- which.max(all_log_reg_models_specificity)

best_subset_pca_raw_data[best_pca_specificity, ]

power_set[[best_pca_specificity]]


best_subset_pca_raw_data %>%
  #filter(is.na(Specificity) == F) %>%
  group_by(Num_Variables) %>%
  summarize(Max_Spec = max(Specificity),
            Mean_Spec = mean(Specificity),
            SD_Spec = sd(Specificity),
            Max_Sens = max(Sensitivity)) %>%
  unique()

fig_pca <- best_subset_pca_raw_data %>%
  ggplot(aes(x = Num_Variables, y = Specificity, group = Num_Variables)) +
  geom_boxplot(aes(fill = Num_Variables)) +
  xlab('Numero de Componentes') +
  ggtitle('PCA:') +
  ylab('Especifidad') +
  ylim(0, 1) +
  theme(legend.position = 'none')


library(cowplot)

plot_grid(fig_orig, fig_pca)
