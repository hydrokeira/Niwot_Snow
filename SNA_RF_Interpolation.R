#gap fill SNA sensors
library(tidyverse)
library(lubridate)
library(ranger)
library(EflowStats)

setwd("/Users/keirajohnson/Library/CloudStorage/OneDrive-UCB-O365/Keira_Johnson_CU/Niwot/SNA_updated")

files_list <- list.files(path = ".", pattern = "sn_", full.names = TRUE)

sn_snow <- list()

for (i in seq_along(files_list)) {
  
  print(i)
  
  sn <- read.csv(files_list[i])
  
  sn$date <- as.POSIXct(sn$date, format = "%Y-%m-%d %H:%M:%S")
  
  sn_snow[[i]] <- sn %>% 
    filter(flag_snowdepth_median=="n") %>%
    dplyr::select(date, snowdepth_median) %>% 
    mutate(sn = basename(files_list[i]))
}

sn_all<-do.call(bind_rows, sn_snow)

sn_all <- sn_all %>%
  mutate(date = floor_date(date, unit = "hour")) %>% 
  group_by(date, sn) %>% 
  summarise(snowdepth_median = mean(snowdepth_median, na.rm = TRUE), .groups = "drop") %>% 
  #mutate(snowdepth_median = ifelse(is.nan(snowdepth_median), NA, snowdepth_median)) %>% 
  #complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "hour")) %>% 
  #arrange(date) %>%
  mutate(waterYear=get_waterYear(date)) %>%
  filter(waterYear > 2022 & waterYear < 2026) %>%
  filter(!is.na(sn))

ggplot(sn_all, aes(date, snowdepth_median))+geom_line()+facet_wrap(~sn, scales="free_y")+theme_bw()+
  theme(text = element_text(size = 20))

write.csv(sn_all, "All_SNA_compiled.csv")

snow_wide <- bind_rows(sn_snow) %>% 
  mutate(date = floor_date(date, unit = "hour")) %>% 
  group_by(date, sn) %>% 
  summarise(snowdepth_median = mean(snowdepth_median, na.rm = TRUE), .groups = "drop") %>% 
  mutate(snowdepth_median = ifelse(is.nan(snowdepth_median), NA, snowdepth_median)) %>% 
  pivot_wider(names_from = sn, values_from = snowdepth_median) %>% 
  complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "hour")) %>% 
  arrange(date) %>%
  mutate(waterYear=get_waterYear(date)) %>%
  filter(waterYear > 2022 & waterYear < 2026)

ggplot(snow_wide, aes(date, sn_01_tenminute.jm.data.csv))+geom_line()

snow_wide <- snow_wide %>% mutate(day_of_year = yday(date), 
                                  sin_doy = sin(2 * pi * day_of_year / 365.25), 
                                  cos_doy = cos(2 * pi * day_of_year / 365.25))

snow_filled <- snow_wide

sensor_cols <- setdiff(names(snow_wide), c("date", "waterYear", "day_of_year", "sin_doy", "cos_doy"))

set.seed(123)

validation_results <- list()
importance_list<-list()

for (sensor in sensor_cols) {
  
  print(sensor)
  
  predictors <- c(setdiff(sensor_cols, sensor), "sin_doy", "cos_doy")
  observed_rows <- which(!is.na(snow_wide[[sensor]]))
  n_holdout <- floor(0.2 * length(observed_rows))
  holdout_rows <- integer(0)
  attempts <- 0
  
  while (length(holdout_rows) < n_holdout && attempts < 10000) {
    start_row <- sample(observed_rows, 1)
    block_rows <- intersect(start_row:(start_row + 23), observed_rows)
    holdout_rows <- union(holdout_rows, block_rows)
    attempts <- attempts + 1
  }
  
  holdout_rows <- head(holdout_rows, n_holdout)
  train_rows <- setdiff(observed_rows, holdout_rows)
  train_data <- snow_wide[train_rows, , drop = FALSE]
  
  predictor_use <- predictors[sapply(train_data[predictors], function(x) mean(!is.na(x)) >= 0.5)]
  train_data <- train_data %>% dplyr::select(all_of(c(sensor, predictor_use)))
  predict_rows <- c(holdout_rows, which(is.na(snow_wide[[sensor]])))
  
  if (nrow(train_data) > 50 && length(predict_rows) > 0 && length(predictor_use) > 0) {
    
    medians <- sapply(train_data[predictor_use], median, na.rm = TRUE)
    keep <- is.finite(medians)
    predictor_use <- predictor_use[keep]
    medians <- medians[keep]
    
    for (p in predictor_use) train_data[[p]][is.na(train_data[[p]])] <- medians[p]
    train_data <- train_data %>% filter(if_all(all_of(predictor_use), ~ is.finite(.x)))
    
    predict_data <- snow_wide[predict_rows, predictor_use, drop = FALSE]
    for (p in predictor_use) predict_data[[p]][is.na(predict_data[[p]])] <- medians[p]
    
    rf_model <- ranger(as.formula(paste(sensor, "~", paste(predictor_use, collapse = " + "))), 
                       data = train_data %>% dplyr::select(all_of(c(sensor, predictor_use))), 
                       num.trees = 500, mtry = max(1, floor(sqrt(length(predictor_use)))), min.node.size = 5,
                       importance = "permutation")
    
    importance_df <- data.frame(predictor = names(rf_model$variable.importance), importance = rf_model$variable.importance, target_sensor = sensor)
    
    predictions <- predict(rf_model, data = predict_data)$predictions
    validation_predictions <- predictions[seq_along(holdout_rows)]
    gap_predictions <- predictions[(length(holdout_rows) + 1):length(predictions)]
    
    validation_results[[sensor]] <- tibble(date = snow_wide$date[holdout_rows], 
                                           observed = snow_wide[[sensor]][holdout_rows], 
                                           predicted = validation_predictions,
                                           sensor = sensor)
    
    gap_rows <- which(is.na(snow_wide[[sensor]]))
    
    if (length(gap_rows) > 0) snow_filled[[sensor]][gap_rows] <- gap_predictions
    
    importance_list[[sensor]] <- importance_df
  }
}

validation_df <- bind_rows(validation_results)

ggplot(validation_df, aes(x = observed, y = predicted)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  facet_wrap(~sensor, scales = "free") + 
  labs(x="Observed", y="Predicted")+
  theme_bw()+
  theme(text = element_text(size = 20))


snow_long <- snow_filled %>% 
  pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "snow_depth") %>% 
  left_join(snow_wide %>% pivot_longer(cols = all_of(sensor_cols), names_to = "sensor", values_to = "observed_depth"), by = c("date", "sensor")) %>% 
  mutate(type = ifelse(is.na(observed_depth) & !is.na(snow_depth), "Predicted", "Observed")) %>% 
  dplyr::select(date, sensor, snow_depth, type)

ggplot(snow_long, aes(date, snow_depth))+geom_point(aes(col=type), size=0.5)+facet_wrap(~sensor, scales="free_y")+theme_bw()+
  theme(text = element_text(size = 20))+scale_color_manual(values = c("blue", "red"))

ggplot(snow_long, aes(date, snow_depth))+geom_line()+facet_wrap(~sensor, scales="free_y")+theme_bw()+
  theme(text = element_text(size = 20))

snow_long %>%
  filter(sensor=="sn_01_tenminute.jm.data.csv") %>%
  ggplot(aes(date, snow_depth))+geom_line()+theme_bw()+
  theme(text = element_text(size = 20))+
  xlim(as.POSIXct("2024-05-15"), as.POSIXct("2024-07-01"))

importance_all <- bind_rows(importance_list)

ggplot(importance_all, aes(x = predictor, y = importance)) +
  geom_col(fill="black") +
  coord_flip() +
  facet_wrap(~ target_sensor, scales = "free_x") +
  labs(y = "Permutation importance")+theme_bw()+
  theme(text=element_text(size = 15))

write.csv(snow_long, "SNA_Interpolated.csv")

