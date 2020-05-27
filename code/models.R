#models.R


#correlations - DP still working on this -> issues with NaN values
moded_vars <- c("pct_change", "med_age", "med_income","education_HS_p", "education_BA_p", "education_MS_p", "no_car_pct", "yes_car_pct", 
                "white_pct", "black_pct", "asian_pct", "latinx_pct" )
model_df <- comp[moded_vars]
model_df$geometry <- NULL
cor(model_df)

#models with med_age, med_income, and % variables on education attainment, car ownership, and race
mod_a1 <- lm(pct_change ~ med_age + med_income + education_HS_p + education_BA_p + education_MS_p 
           + no_car_pct + yes_car_pct + white_pct + black_pct + asian_pct + latinx_pct, data = comp)
summary(mod_a1)

#use stepwise backward function to get the best fiting model
step(mod_a1, direction = "backward")

mod_a2 <- lm(pct_change ~ med_age + med_income + education_HS_p + 
             education_MS_p + white_pct + black_pct + latinx_pct, data = comp)
summary(mod_a2)


#misc model
mod_a3 <- lm(pct_change ~ med_age + med_income + education_HS_p + black_pct + latinx_pct, data = comp)
summary(mod_a3)

