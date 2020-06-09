#models.R

#correlations
install.packages("corrplot")
library(corrplot)

model_vars <- c("pct_change", "med_age", "med_income","education_HS_p", "education_BA_p", "education_MS_p", "no_car_pct", "yes_car_pct", 
                "white_pct", "black_pct", "asian_pct", "latinx_pct", "car_pct", "transit_pct", "bike_pct", "walk_pct", "wfh_pct",
                "industry1_pct", "industry2_pct", "industry3_pct", "industry4_pct", "industry5_pct", "industry6_pct", "industry7_pct",
                "industry8_pct", "industry9_pct", "industry10_pct", "industry11_pct", "industry12_pct", "industry13_pct")
model_df <- comp[model_vars]
model_df$geometry <- NULL
model_vars <- cor(model_df, use = "pairwise.complete.obs")
corrplot(model_vars, method='number')


model_vars1 <- c("pct_change", "med_age", "med_income")
model_vars1_df <- comp[model_vars1]
model_vars1_df$geometry <- NULL
model_var1 <- cor(model_vars1_df, use = "pairwise.complete.obs")
corrplot(model_var1, method='number')

model_vars2 <- c("pct_change", "education_HS_p", "education_BA_p", "education_MS_p")
model_vars2_df <- comp[model_vars2]
model_vars2_df$geometry <- NULL
model_var2 <- cor(model_vars2_df, use = "pairwise.complete.obs")
corrplot(model_var2, method='number')

model_vars3 <- c("pct_change", "no_car_pct", "yes_car_pct")
model_vars3_df <- comp[model_vars3]
model_vars3_df$geometry <- NULL
model_var3 <- cor(model_vars3_df, use = "pairwise.complete.obs")
corrplot(model_var6, method='number')

model_vars4 <- c("pct_change", "white_pct", "black_pct", "asian_pct", "latinx_pct")
model_vars4_df <- comp[model_vars4]
model_vars4_df$geometry <- NULL
model_var4 <- cor(model_vars4_df, use = "pairwise.complete.obs")
corrplot(model_var4, method='number')

model_vars5 <- c("pct_change", "car_pct", "transit_pct", "bike_pct", "walk_pct", "wfh_pct")
model_vars5_df <- comp[model_vars5]
model_vars5_df$geometry <- NULL
model_var5 <- cor(model_vars5_df, use = "pairwise.complete.obs")
corrplot(model_var5, method='number')

model_vars6 <- c("pct_change", "industry1_pct", "industry2_pct", "industry3_pct", "industry4_pct", "industry5_pct", "industry6_pct", "industry7_pct",
                 "industry8_pct", "industry9_pct", "industry10_pct", "industry11_pct", "industry12_pct", "industry13_pct")
model_vars6_df <- comp[model_vars6]
model_vars6_df$geometry <- NULL
model_var6 <- cor(model_vars6_df, use = "pairwise.complete.obs")
corrplot(model_var6, method='number')



#models with med_age, med_income, and % variables on education attainment, car ownership, and race
mod_a1 <- lm(pct_change ~ med_age + med_income + education_HS_p + education_BA_p + education_MS_p 
           + no_car_pct + yes_car_pct + white_pct + black_pct + asian_pct + latinx_pct, data = comp)
summary(mod_a1)


step(mod_a1, direction = "backward") #use stepwise backward function to get the best fiting model

mod_a2 <- lm(pct_change ~ med_age + med_income + education_HS_p + 
             education_MS_p + white_pct + black_pct + latinx_pct, data = comp)
summary(mod_a2)


#model with med_age, med_income, and % variables on education attainment, car ownership, and race, and commute
mod_b1 <- lm(pct_change ~ med_age + med_income + education_HS_p + education_BA_p + education_MS_p 
             + no_car_pct + yes_car_pct + white_pct + black_pct + asian_pct + latinx_pct 
             + car_pct + transit_pct + bike_pct + walk_pct + wfh_pct, data = comp)
summary(mod_b1)

step(mod_b1, direction = "backward") #use stepwise backward function to get the best fiting model

mod_b2 <- lm(pct_change ~ med_income 
                        + education_MS_p 
                        + no_car_pct 
                        + car_pct, 
                        data = comp)
summary(mod_b2)


#model with med_age, med_income, and % variables on education attainment, car ownership, and race, commute, and industry data

mod_c1 <- lm(pct_change ~ med_age + med_income + education_HS_p + education_BA_p + education_MS_p 
             + no_car_pct + yes_car_pct + white_pct + black_pct + asian_pct + latinx_pct + car_pct + transit_pct + bike_pct + walk_pct + wfh_pct 
             + industry1_pct + industry2_pct + industry3_pct + industry4_pct + industry5_pct + industry6_pct + industry7_pct
             + industry8_pct + industry9_pct + industry10_pct + industry11_pct + industry12_pct + industry13_pct, data = comp)
summary(mod_c1)

step(mod_c1, direction = "backward") #use stepwise backward function to get the best fiting model

mod_c2 <- lm(pct_change ~ med_age 
                        + med_income 
                        + education_MS_p 
                        + no_car_pct 
                        + car_pct 
                        + wfh_pct 
                        + industry2_pct 
                        + industry3_pct 
                        + industry5_pct 
                        + industry9_pct 
                        + industry10_pct 
                        + industry12_pct, 
                          data = comp)
summary(mod_c2)

## take out the non-significant variables from "mod_c2"
mod_c3 <- lm(pct_change ~ 
             + med_income 
             + education_MS_p 
             + no_car_pct 
             + car_pct 
             + industry5_pct 
             + industry9_pct 
             + industry10_pct 
             + industry12_pct, 
              data = comp)
summary(mod_c3)

#model with variables of interest

mod_d1<- lm(pct_change ~ med_income + education_BA_p + education_MS_p
             + yes_car_pct + white_pct + asian_pct + car_pct + transit_pct
             + industry1_pct + industry2_pct + industry3_pct + industry4_pct + industry5_pct + industry6_pct + industry7_pct
             + industry8_pct + industry9_pct + industry10_pct + industry11_pct + industry12_pct + industry13_pct, data = comp)
summary(mod_d1)

step(mod_d1, direction = "backward") #use stepwise backward function to get the best fiting model

mod_d2 <- lm(pct_change ~ med_income + education_MS_p + yes_car_pct + 
               car_pct + transit_pct + industry2_pct + industry3_pct + industry5_pct + 
               industry9_pct + industry10_pct + industry12_pct, data = comp)
summary(mod_d2)

#looking at correlations of the variables in the final model 
mod_d2_vars <- c("pct_change", "med_income", "education_MS_p", "yes_car_pct", "car_pct", "transit_pct", "industry2_pct", "industry3_pct",
                 "industry5_pct", "industry9_pct", "industry10_pct", "industry12_pct")
mod_d2_vars_df <- comp[mod_d2_vars]
mod_d2_vars_df$geometry <- NULL
mod_d2_vars <- cor(mod_d2_vars_df, use = "pairwise.complete.obs")
corrplot(mod_d2_vars, method='number')



stargazer(mod_a1, mod_a2, mod_b1, mod_b2, mod_c1, mod_c2, mod_c3, mod_d1, mod_d2, type = "text")


