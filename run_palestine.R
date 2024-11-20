# Robert Kubinec

# Run analyses for Palestine data

library(dplyr)
library(ggplot2)
library(tidyr)
library(qualtRics)
library(googlesheets4)
library(missRanger)
library(rstanarm)
library(brms)
library(tidybayes)
library(readr)

# if you have cmdstanr installed

options(brms.backend="cmdstanr",brms.threads=8)

# set to TRUE to re-run models and reproduce results

run_model <- TRUE

pal_data <- read_csv("data/pal_data_clean.csv") %>% 
  mutate(att=ordered(Q13,levels=c("كتير سلبي",
                                            "شوي سلبي",
                                            "محايد",
                                            "شوي إيجابي",
                                            "كتيرإيجابي"),
                               labels=c("Very\nNegative",
                                        "Somewhat\nNegative",
                                        "Neutral",
                                        "Somewhat\nPositive",
                                        "Very\nPositive")))


# pal_data <- rename(pal_data,
#                    sex="Q6",
#                    age="Q5") %>% 
#   mutate(sex=recode(sex,
#                     أنثى = "Female",
#                     ذكر = "Male"),
#          age=factor(age,labels=c("18-24",
#                                  "55-64",
#                                  "25-34",
#                                  "45-54",
#                                  "35-44",
#                                  "75-85",
#                                  "65-74")),
#          age=factor(age,levels=c("18-24",
#                                  "25-34",
#                                  "35-44",
#                                  "45-54",
#                                  "55-64",
#                                  "65-74",
#                                  "75-85")))

# impute age/gov/sex so we can appropriately do adjustment

# need to clean up names

old_names <- names(pal_data)

names(pal_data) <- paste0("V",1:length(pal_data))

pal_data <- missRanger(pal_data,V22 + V23 + V28~.)

names(pal_data) <- old_names

dem_data <- read_csv("data/pal_census_data.csv") %>%
  rename(Male="Males",
         Female="Females") %>% 
  select(-matches("%")) %>% 
  gather(key = "age",value = "pop",-محافظات, 
                            -Governates,-`Total Population (2017)`,
         -`Total Population (2007)`,-Male,-Female) %>% 
  mutate(Male2=(Male/(Male+Female))*pop,
         Female2=(Female/(Male+Female))*pop) %>% 
  select(-Male,-Female) %>% 
  rename(Male="Male2",Female="Female2") %>% 
  gather(key="sex",value="pop2",Male,Female) %>% 
  select(-pop) %>% 
  select(pop="pop2",everything()) %>% 
  ungroup %>% 
  mutate(prop=pop/sum(pop))

# add in 18-19 year olds to 20-24

add <- filter(dem_data,age=="15-19") %>% 
  mutate(prop2=prop*(2/5),
         age="20-24")

dem_data <- left_join(dem_data,select(add,age,Governates,sex,prop2),
                      by=c("age","sex","Governates")) %>% 
  mutate(prop=case_when(is.na(prop2)~prop,
                        TRUE~prop + prop2),
         age=recode(age,
                    `15-19`="20-24"),
         age=recode(age,
                    `20-24`="18-24",
                    `25-29`="25-34",
                    `30-34`="25-34",
                    `35-39`="35-44",
                    `40-44`="35-44",
                    `45-49`="45-54",
                    `50-54`="45-54",
                    `55-59`="55-64",
                    `60-64`="55-64",
                    `65-69`="65-74",
                    `70-74`="65-74",
                    `75-79`="75-85",
                    `80-84`="75-85",
                    `85-89`="75-85",
                    `90-94`="75-85",
                    `Age 95+`="75-85")) %>% 
  group_by(محافظات,age,sex) %>% 
  summarize(prop=mean(prop))

# recode pal data to match demo data

pal_data <- left_join(pal_data,dem_data,by=c("age","sex","Q11"="محافظات"))

# redo figure 1

# calculate bayesian models for each combination of age/sex & then posterior predict to get
# weighted answers

if(run_model) {
  att_mod <- stan_glmer(Q12_1~att + (1|Q11) + sex + (1|age),data=pal_data,chains=1)
  saveRDS(att_mod,"att_mod.rds")
  } else {
  att_mod <- readRDS("att_mod.rds")
}



# figure out weighted answers for attitude levels given proportions

att_dist_est <- lapply(unique(pal_data$att), function (a) {
  
  if(!is.na(a)) {
    this_data <- mutate(dem_data,att=a) %>% 
      select(att,age,sex,Q11="محافظات")
    
    post_num1 <- try(posterior_epred(att_mod, 
                                       newdata=this_data,
                                       draws=100))
    
    post_out <- post_num1 %*% dem_data$prop
    
    tibble(estimate=post_out[,1]) %>% 
      mutate(att=a)
  }
  
    }) %>% bind_rows


att_dist_est %>% 
  group_by(att) %>% 
  summarize(med_est=median(estimate),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05)) %>% 
  ggplot(aes(y=med_est,x=att)) +
  geom_pointrange(aes(ymin=low_est,ymax=high_est)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Average Kilometer Distance from Settlement") +
  xlab("Attitude Towards Settlement")

ggsave("Figure_3.jpg",dpi=600)

# histogram

pal_data %>% 
  filter(!is.na(att)) %>%
  ggplot(aes(x=Q12_1)) +
  geom_histogram() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Count of Responses") +
  xlab("Distance from Settlement in Kilometers")

ggsave("Figure_2.jpg",dpi=600)

# do some other analyses
# create behavioral score

pal_data <- mutate(pal_data,beh_score=as.numeric(Q1=="صح") + as.numeric(Q2=="صح") +
                      as.numeric(Q3=="صح") + 
                     as.numeric(Q4=="صح"),
                   beh_score=as.numeric(factor(beh_score)))

# we can do the behavior analysis with ordered logit

pal_data <- mutate(pal_data,
                   work_settle=recode(Q14,
                                      `لا`="No",
                                      `نعم`="Yes"),
                   interact_settle=recode(Q10,
                                          `لا`="No",
                                          `نعم`="Yes"),
                   distance=Q12_1/100,
                   Q14=factor(Q14),
                   Q14=factor(Q14, levels=rev(levels(Q14))),
                   Q10=factor(Q10),
                   covid=factor(Q20,levels=c("لا",
                                             "نعم")),
                   covid_job=factor(Q22,levels=c("لا",
                                             "نعم")),
                   covid_job=ifelse(!is.na(covid) & is.na(covid_job),"لا",
                                    covid_job),
                   Q10=factor(Q10, levels=rev(levels(Q10))),
                   Q9=factor(Q9,labels=c("0-1500 NIS",
                                         "1500-3000 NIS",
                                         "3000-4500 NIS",
                                         "4500-6000 NIS",
                                         ">6000 NIS")),
                   Q7=factor(Q7,labels=c("Illiterate",
                                         "University",
                                         "M.A.",
                                         "PhD",
                                         "Elementary School",
                                         "High School")),
                   Q7=factor(Q7,levels=c("Illiterate",
                                         "Elementary School",
                                         "High School",
                                         "University",
                                         "M.A.",
                                         "PhD"))) %>% 
  select(everything(),education="Q7",
         income="Q9",
         district="Q11")

# Q10 = interact with settler
# Q14 = work in a settlement

if(run_model) {
  beh_score_mod <- brm(bf(beh_score ~ distance + interact_settle + 
                                work_settle + 
                         I(distance^2) +
                                interact_settle*distance +
                                work_settle*distance +
                         interact_settle*I(distance^2) + 
                         work_settle*I(distance^2) + 
                                (1|age) +
                                sex + (1|district) + education + income,
                         center=T),
                       family=cumulative(),
                              data=pal_data,chains=1,iter=1000)
  
  saveRDS(beh_score_mod,"beh_score_mode.rds")
  
  att_score_mod <- brm(bf(att ~ distance + interact_settle + work_settle + 
                         interact_settle*distance + 
                         work_settle*distance + 
                         I(distance^2) +
                         interact_settle*I(distance^2) + 
                         work_settle*I(distance^2) + 
                         (1|age) +
                         sex + (1|district) + education + income),
                       data=pal_data,chains=1,family=cumulative(),iter=1000)
  
  saveRDS(att_score_mod,"att_score_mode.rds")
  
  # do the same, but predict work with attitudes/behavior
  
  work_att <- brm(bf(work_settle ~ distance +
                       I(distance^2) +
                       att + 
                       (1|age) +
                       sex + (1|district) + education + income),
                  data=mutate(pal_data,
                              att=factor(as.character(att),
                                         levels=c("Very\nNegative",
                                                  "Somewhat\nNegative",
                                                  "Neutral",
                                                  "Somewhat\nPositive",
                                                  "Very\nPositive"),
                                         labels=c("Very Negative",
                                                  "Somewhat Negative",
                                                  "Neutral",
                                                  "Somewhat Positive",
                                                  "Very Positive"))),
                  chains=1,family=bernoulli,iter=1000)
  
  saveRDS(work_att,"work_att.rds")
  
  work_beh <- brm(bf(interact_settle ~ distance +
                       I(distance^2) +
                       att + 
                       (1|age) +
                       sex + (1|district) + education + income),
                  data=mutate(pal_data,
                         att=factor(as.character(att),
                                    levels=c("Very\nNegative",
                                             "Somewhat\nNegative",
                                             "Neutral",
                                             "Somewhat\nPositive",
                                             "Very\nPositive"),
                                    labels=c("Very Negative",
                                             "Somewhat Negative",
                                             "Neutral",
                                             "Somewhat Positive",
                                             "Very Positive"))),
                              chains=1,family=bernoulli,iter=1000)
  
  saveRDS(work_beh,"work_beh.rds")
  
  # other alternate hypothesis
  
  dist_work_alt <- brm(bf(distance ~ interact_settle + (1|age) +
                        sex + (1|district) + education + income + 
                        att),
                   data=mutate(pal_data,
                                    att=factor(as.character(att),
                                               levels=c("Very\nNegative",
                                                        "Somewhat\nNegative",
                                                        "Neutral",
                                                        "Somewhat\nPositive",
                                                        "Very\nPositive"),
                                               labels=c("Very Negative",
                                                        "Somewhat Negative",
                                                        "Neutral",
                                                        "Somewhat Positive",
                                                        "Very Positive"))),
                   chains=1,family="Normal",iter=1000)
  
  saveRDS(dist_work_alt,"dist_work_alt.rds")
  
  dist_int_alt <- brm(bf(distance ~ work_settle + (1|age) +
                           sex + (1|district) + education + income + 
                           att),
                     data=mutate(pal_data,
                                 att=factor(as.character(att),
                                            levels=c("Very\nNegative",
                                                     "Somewhat\nNegative",
                                                     "Neutral",
                                                     "Somewhat\nPositive",
                                                     "Very\nPositive"),
                                            labels=c("Very Negative",
                                                     "Somewhat Negative",
                                                     "Neutral",
                                                     "Somewhat Positive",
                                                     "Very Positive"))),
                     chains=1,family="Normal",iter=1000)
  
  saveRDS(dist_int_alt,"dist_int_alt.rds")
  
  
  
  # predict work/interact just with distance
  
  work_dist <- brm(bf(work_settle ~ s(distance)),
                  data=filter(pal_data,
                              complete.cases(select(pal_data,distance))),
                                             chains=1,family=bernoulli,iter=1000)
  
  saveRDS(work_dist,"work_dist.rds")
  
  beh_dist <- brm(bf(interact_settle ~ distance +
                       I(distance^2) +
                       (1|age) +
                       sex + (1|district) + education + income),
                  data=pal_data,chains=1,family=bernoulli,iter=1000)
  
  saveRDS(beh_dist,"beh_dist.rds")
  
  
  
} else {
  beh_score_mod <- readRDS("beh_score_mode.rds")
  att_score_mod <- readRDS("att_score_mode.rds")
  work_dist <- readRDS("work_dist.rds")
  work_att <- readRDS("work_att.rds")
  work_beh <- readRDS("work_beh.rds")
  dist_work_alt <- readRDS("dist_work_alt.rds")
  dist_int_alt <- readRDS("dist_int_alt.rds")
}

# need a simple transform

x100_func <- function(x) {x*100}
x100_func_inv <- function(x) {x/100}

conditional_effects(work_dist,"distance")$distance %>% 
  mutate(distance=x100_func(distance)) %>% 
  ggplot(aes(y=estimate__,
             x=distance)) +
  geom_ribbon(aes(ymin=lower__,
                  ymax=upper__),fill="blue",alpha=0.5) + 
  scale_x_continuous() +
  geom_line(linetype=2) +
  labs(y="Proportion Working in a Settlement",
       x="Distance in Kilometers",
       caption=stringr::str_wrap("Results of regression of working in a settlement on distance from a settlement in kilometers. Distance is modeled as a spline.",
                                 width=75)) +
  ggthemes::theme_tufte() +
  scale_y_continuous(labels=scales::percent)

ggsave("Figure_7.jpg",dpi=600)

# need the draws in long form

beh_mod_plot <- gather_draws(beh_score_mod,b_distance,b_interact_settleYes,
                             b_work_settleYes,
                             `b_distance:interact_settleYes`,
                             `b_distance:work_settleYes`,
                             `b_interact_settleYes:IdistanceE2`,
                             `b_work_settleYes:IdistanceE2`) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                          b_interact_settleYes="Interact with Settler",
                          b_distance="Distance to Settlement",
                          b_work_settleYes="Work in Settlement",
                          `b_distance:interact_settleYes`="Distance X Interact",
                          `b_distance:work_settleYes`="Distance X Work",
                          `b_interact_settleYes:IdistanceE2`="Distance Sq. X Interact",
                          `b_work_settleYes:IdistanceE2`="Distance Sq. X Work"),
         .variable=factor(.variable,levels=c("Interact with Settler",
                                             "Distance to Settlement",
                                             "Work in Settlement",
                                             "Distance X Interact",
                                             "Distance Sq. X Interact",
                                             "Distance X Work",
                                             "Distance Sq. X Work"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Behavior Score") 

att_mod_plot <- gather_draws(att_score_mod,b_distance,b_interact_settleYes,
                             b_work_settleYes,
                             `b_distance:interact_settleYes`,
                             `b_distance:work_settleYes`,
                             `b_interact_settleYes:IdistanceE2`,
                             `b_work_settleYes:IdistanceE2`) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                          b_interact_settleYes="Interact with Settler",
                          b_distance="Distance to Settlement",
                          b_work_settleYes="Work in Settlement",
                          `b_distance:interact_settleYes`="Distance X Interact",
                          `b_distance:work_settleYes`="Distance X Work",
                          `b_interact_settleYes:IdistanceE2`="Distance Sq. X Interact",
                          `b_work_settleYes:IdistanceE2`="Distance Sq. X Work"),
         .variable=factor(.variable,levels=c("Interact with Settler",
                                             "Distance to Settlement",
                                             "Work in Settlement",
                                             "Distance X Interact",
                                             "Distance Sq. X Interact",
                                             "Distance X Work",
                                             "Distance Sq. X Work"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Attitude Score")




bind_rows(att_mod_plot,beh_mod_plot) %>% 
  ggplot(aes(y = Outcome, x = .value, xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(shape=sig,linetype=sig),size=2,fatten_point=4,
                     position=position_dodge(width=0.5)) +
  scale_y_discrete() +
  facet_wrap(~.variable,scales="free_x") +
  ylab("") +
  xlab("Logit Coefficient") +
  guides(shape=guide_legend(title="Significant"),
         linetype=guide_legend(title="Significant")) +
  geom_vline(xintercept = 0,linetype=2) +
  scale_color_brewer(type="qual") +
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.box = "vertical") 

ggsave("Figure_8.jpg",dpi=600)


# Same but for alternate hypotheses ---------------------------------------

alt_interact <- gather_draws(work_beh,b_distance,b_IdistanceE2,
                           b_attSomewhatNegative,
                           b_attNeutral,
                           b_attSomewhatPositive,
                           b_attVeryPositive) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                          b_IdistanceE2="Distance Sq.",
                          b_distance="Distance to Settlement",
                          b_work_settleYes="Work in Settlement",
                          b_attSomewhatNegative="Somewhat Negative Affect",
                          b_attNeutral="Neutral Affect",
                          b_attSomewhatPositive="Somewhat Positive Affect",
                          b_attVeryPositive="Very Positive Affect"),
         .variable=factor(.variable,levels=c("Somewhat Negative Affect",
                                             "Neutral Affect",
                                             "Somewhat Positive Affect",
                                             "Very Positive Affect"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Interact with Settlers",Control="Distance") 

alt_work <- gather_draws(work_att,b_distance,b_IdistanceE2,
                             b_attSomewhatNegative,
                             b_attNeutral,
                             b_attSomewhatPositive,
                             b_attVeryPositive) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                                           b_IdistanceE2="Distance Sq.",
                                           b_distance="Distance to Settlement",
                                           b_work_settleYes="Work in Settlement",
                                           b_attSomewhatNegative="Somewhat Negative Affect",
                                           b_attNeutral="Neutral Affect",
                                           b_attSomewhatPositive="Somewhat Positive Affect",
                                           b_attVeryPositive="Very Positive Affect"),
                          .variable=factor(.variable,levels=c("Somewhat Negative Affect",
                                                              "Neutral Affect",
                                                              "Somewhat Positive Affect",
                                                              "Very Positive Affect"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Employment in Settlement",Control="Distance")

alt_dist_int <- gather_draws(dist_int_alt,
                             b_attSomewhatNegative,
                             b_attNeutral,
                             b_attSomewhatPositive,
                             b_attVeryPositive) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                          b_attSomewhatNegative="Somewhat Negative Affect",
                          b_attNeutral="Neutral Affect",
                          b_attSomewhatPositive="Somewhat Positive Affect",
                          b_attVeryPositive="Very Positive Affect"),
         .variable=factor(.variable,levels=c("Somewhat Negative Affect",
                                             "Neutral Affect",
                                             "Somewhat Positive Affect",
                                             "Very Positive Affect"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Distance",Control="Interact with Settlers") 

alt_dist_work <- gather_draws(dist_work_alt,
                         b_attSomewhatNegative,
                         b_attNeutral,
                         b_attSomewhatPositive,
                         b_attVeryPositive) %>% 
  mutate(.value=ifelse(grepl(x=.variable,pattern="distance"),.value*10,.value)) %>% 
  median_qi() %>% 
  mutate(.variable=recode(.variable,
                          b_attSomewhatNegative="Somewhat Negative Affect",
                          b_attNeutral="Neutral Affect",
                          b_attSomewhatPositive="Somewhat Positive Affect",
                          b_attVeryPositive="Very Positive Affect"),
         .variable=factor(.variable,levels=c("Somewhat Negative Affect",
                                             "Neutral Affect",
                                             "Somewhat Positive Affect",
                                             "Very Positive Affect"),
                          ordered = T),
         sig=sign(.lower)==sign(.upper),
         Outcome="Distance",Control="Work in a Settlement")




bind_rows(alt_interact,alt_work,alt_dist_int,
          alt_dist_work) %>% 
  filter(!grepl(x=.variable,pattern="Distance"),
         !is.na(.variable)) %>% 
  mutate(Outcome=paste("Outcome = ",Outcome)) %>% 
  ggplot(aes(y = reorder(.variable,desc(.variable)), x = .value, xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(shape=sig,colour=Control),size=2,fatten_point=4,
                     position="dodge") +
  scale_y_discrete() +
  ylab("") +
  xlab("Logit Coefficient") +
  guides(shape="none") +
  geom_vline(xintercept = 0,linetype=2) +
  scale_color_brewer(type="qual") +
  facet_wrap(~Outcome,scales="free_x",ncol=1) +
  ggthemes::theme_tufte()

ggsave("Figure_9.jpg",dpi=600)


# More MRP adjustments ----------------------------------------------------



# cycle through different bivariate relationships:
# prev int with settler and attitudes
# education and attitudes
# employment and attitudes

# use MRP to adjust and make representative

library(tidybayes)
library(purrr)
library(modelr)
library(RColorBrewer)
library(ggthemes)

# first interaction

if(run_model) {
  int_mod <- stan_polr(att ~ interact_settle + district + sex + age,
                       data=pal_data,prior=NULL,
                       chains=1)
  saveRDS(int_mod,"int_mod.rds")
} else {
  int_mod <- readRDS("int_mod.rds")
}

# figure out weighted answers for attitude levels given proportions
# fix an error with district in dem_data

# missing factor level

miss_fac <- unique(dem_data$محافظات)[(!(unique(dem_data$محافظات) %in% pal_data$district))]

dem_data$محافظات[dem_data$محافظات==miss_fac] <- unique(pal_data$district)[(!(unique(pal_data$district) %in% dem_data$محافظات))][2]

int_est <- lapply(unique(pal_data$interact_settle), function (a) {
  
  if(!is.na(a)) {
    this_data <- mutate(ungroup(dem_data),interact_settle=a) %>% 
      select(interact_settle,age,sex,district="محافظات") %>% 
      mutate(interact_settle=factor(interact_settle,
                                    levels=levels(int_mod$model$interact_settle)),
             age=factor(age,
                                    levels=levels(int_mod$model$age)),
             sex=factor(sex,
                                    levels=levels(int_mod$model$sex)),
             district=factor(district,
                                    levels=levels(int_mod$model$district)))
    
    post_num1 <- try(posterior_linpred(int_mod, 
                                       newdata=this_data))
    
    post_out <- post_num1 %*% dem_data$prop
    
    tibble(estimate=post_out[,1]) %>% 
      mutate(interact_settle=a,
             .draw=1:n())
  }
  
}) %>% bind_rows

# need probability of lowest category

thresholds <- int_mod %>%
  gather_draws(`.*[|].*`, regex = TRUE) %>%
  group_by(.draw) %>%
  select(.draw, threshold = .value) %>%
  summarise_all(list) %>%
  mutate(threshold = map(threshold, ~ c(., Inf)))

int_est %>% 
  inner_join(thresholds, by = ".draw") %>%
  mutate(pry = map2(threshold, estimate, function(alpha, beta_x)
    # this part is logit^-1(alpha_j - beta*x) - logit^-1(alpha_j-1 - beta*x)
    plogis(alpha - beta_x) -
      plogis(lag(alpha, default = -Inf) - beta_x)
  )) %>%
  mutate(.category = list(levels(pal_data$att))) %>%
  unnest(c(threshold, pry, .category)) %>% 
  mutate(.category=factor(.category,levels=levels(pal_data$att))) %>% 
  ggplot(aes(x = .category, y = pry, color = interact_settle)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_brewer(type="qual") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Estimated Population Proportion") +
  xlab("Attitudes Towards Settlers") +
  theme_tufte() +
  guides(color=guide_legend(title="Previous\nInteraction\nwith Settlers"))



ggsave("Figure_4.jpg",dpi=600,scale=.8)

# now education

if(run_model) {
  edu_mod <- stan_polr(att ~ education + district + sex + age,
                        data=pal_data,
                       prior=NULL,
                        chains=1)
  saveRDS(edu_mod,"edu_mod.rds")
} else {
  edu_mod <- readRDS("edu_mod.rds")
}

# figure out weighted answers for attitude levels given proportions

edu_est <- lapply(unique(edu_mod$model$education), function (a) {
  
  if(!is.na(a)) {
    this_data <- mutate(dem_data,education=a) %>% 
      select(education,age,sex,district="محافظات") %>% 
      mutate(education=factor(education,
                                    levels=levels(edu_mod$model$education)),
             age=factor(age,
                        levels=levels(edu_mod$model$age)),
             sex=factor(sex,
                        levels=levels(edu_mod$model$sex)),
             district=factor(district,
                             levels=levels(edu_mod$model$district)))
    
    post_num1 <- try(posterior_linpred(edu_mod, 
                                       newdata=this_data))
    
    post_out <- post_num1 %*% dem_data$prop
    
    tibble(estimate=post_out[,1]) %>% 
      mutate(education=a,
             .draw=1:n())
  }
  
}) %>% bind_rows

# need probability of lowest category

thresholds <- edu_mod %>%
  gather_draws(`.*[|].*`, regex = TRUE) %>%
  group_by(.draw) %>%
  select(.draw, threshold = .value) %>%
  summarise_all(list) %>%
  mutate(threshold = map(threshold, ~ c(., Inf)))

  edu_est %>% 
  inner_join(thresholds, by = ".draw") %>%
  mutate(pry = map2(threshold, estimate, function(alpha, beta_x)
    # this part is logit^-1(alpha_j - beta*x) - logit^-1(alpha_j-1 - beta*x)
    plogis(alpha - beta_x) -
      plogis(lag(alpha, default = -Inf) - beta_x)
  )) %>%
  mutate(.category = list(levels(pal_data$att)),
         education=recode(education,
                          `Elementary School`="Elementary\nSchool",
                          `High School`="High\nSchool")) %>%
  unnest(c(threshold, pry, .category)) %>% 
    mutate(.category=factor(.category,levels=levels(pal_data$att))) %>% 
    ggplot(aes(x = .category, y = pry, color = education)) +
    stat_pointinterval(position = position_dodge(width = .4)) +
    scale_size_continuous(guide = FALSE) +
    scale_color_manual(values = brewer.pal(8, "Blues")[-c(1,2)]) +
    scale_y_continuous(labels=scales::percent) +
  ylab("Estimated Population Proportion") +
  xlab("Attitude Towards Settlers") +
    theme_tufte() +
    guides(color=guide_legend(title="Education"))
    


ggsave("Figure_5.jpg",scale=.8,dpi=600)

# next is employment

if(run_model) {
  work_mod <- stan_polr(att ~ work_settle + district + sex + age,
                       data=pal_data,
                       prior=NULL,
                       chains=1)
  saveRDS(work_mod,"work_mod.rds")
} else {
  work_mod <- readRDS("work_mod.rds")
}

# figure out weighted answers for attitude levels given proportions

work_est <- lapply(unique(pal_data$work_settle), function (a) {
  
  if(!is.na(a)) {
    this_data <- mutate(dem_data,work_settle=a) %>% 
      select(work_settle,age,sex,district="محافظات") %>% 
      mutate(work_settle=factor(work_settle,levels=levels(work_mod$model$work_settle))) %>% 
      mutate(age=factor(age,
                        levels=levels(work_mod$model$age)),
             sex=factor(sex,
                        levels=levels(work_mod$model$sex)),
             district=factor(district,
                             levels=levels(work_mod$model$district)))
    
    post_num1 <- try(posterior_linpred(work_mod, 
                                       newdata=this_data))
    
    post_out <- post_num1 %*% dem_data$prop
    
    tibble(estimate=post_out[,1]) %>% 
      mutate(work_settle=a,
             .draw=1:n())
  }
  
}) %>% bind_rows

# need probability of lowest category

thresholds <- work_mod %>%
  gather_draws(`.*[|].*`, regex = TRUE) %>%
  group_by(.draw) %>%
  select(.draw, threshold = .value) %>%
  summarise_all(list) %>%
  mutate(threshold = map(threshold, ~ c(., Inf)))

work_est %>% 
  inner_join(thresholds, by = ".draw") %>%
  mutate(pry = map2(threshold, estimate, function(alpha, beta_x)
    # this part is logit^-1(alpha_j - beta*x) - logit^-1(alpha_j-1 - beta*x)
    plogis(alpha - beta_x) -
      plogis(lag(alpha, default = -Inf) - beta_x)
  )) %>%
  mutate(.category = list(levels(pal_data$att))) %>%
  unnest(c(threshold, pry, .category)) %>% 
  mutate(.category=factor(.category,levels=levels(pal_data$att))) %>% 
  ggplot(aes(x = .category, y = pry, color = work_settle)) +
  stat_pointinterval(position = position_dodge(width = .4)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_brewer(type="qual") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Estimated Population Proportion") +
  xlab("Attitudes Towards Israeli Settlers") +
  theme_tufte() +
  guides(color=guide_legend(title="Previous\nEmployment"))



ggsave("Figure_6.jpg",scale=.8,dpi=600)

# look at COVID

if(run_model) {
  covid_mod <- stan_glm(covid_job ~ district + sex + age,
                        data=mutate(pal_data,covid_job=as.numeric(covid_job)-1),
                        family="binomial",
                        chains=1)
  saveRDS(covid_mod,"covid.rds")
} else {
  covid_mod <- readRDS("covid.rds")
}

# figure out weighted answers for attitude levels given proportions

this_data <- dem_data %>% 
  select(prop,age,sex,district="محافظات") %>% 
  filter(district %in% covid_mod$data$district)

post_num1 <- try(posterior_linpred(covid_mod, 
                                   newdata=this_data))

post_out <- post_num1 %*% this_data$prop

covid_est <- tibble(estimate=plogis(post_out[,1]))

quantile(covid_est$estimate,probs = c(0.05,.5,.95))

