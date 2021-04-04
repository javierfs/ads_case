library(tidyverse)
library(here)
library(dplyr)
library(patchwork)
library(ggrepel)
library(directlabels)
library(ggbeeswarm)
library(ggjoy)
library(ggExtra)



data <- read_csv(here('data','cleaned_merged_data_v1.csv'))


str(data)

summary(data$PUBLISH_DTT)

selected_data <- data %>%
                  select(AD_ID, Ad_minutes_alive, Ad_hours_alive)

##############################
# Q1: How big is the share of users who inserted an ad in each category section? 
##############################

summary_data <- data %>%
                group_by(CATEGORY_SECTION) %>%
                summarise(nmr_users = n_distinct(USER_ID)) %>%
                mutate(share_users = (nmr_users/sum(nmr_users))*100)
write.csv(summary_data,here('data', 'q1_unique_users_category_section.csv'), row.names = FALSE)               
summary_data %>%
  ggplot(aes(y = share_users, x = reorder(CATEGORY_SECTION, -share_users))) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(
    aes(label = paste('n =', nmr_users), y = share_users + 0.35),
    position = position_dodge(0.9),
    vjust = 0,
    size=3
  ) +
  labs(x='',
    y = "% of Share of users by category section",
       title = "How big is the share of users who inserted an ad in each category section? ",
       subtitle = "Home&Garden is the section where more distinct users post")+
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(labels = function(share_users) paste0(share_users, '%'))

  
##############################
# Q2: How Common is for users to Switch payment method after inserting their first ad? Compare 1st and 2nd ad 
##############################
data_q2 <- read_csv(here('data','Q2_data.csv'))
viz_data_q2 <- data_q2 %>%
  mutate(PAYMENT_2FIRSTADS = case_when(PAYMENT_2FIRSTADS=='Equal payment-method' ~ 'Keeping Payment Method',
                                       PAYMENT_2FIRSTADS=='payment-method switched' ~ 'Switched Payment Method'
                                       )) %>%
  group_by(PAYMENT_2FIRSTADS) %>%
  summarise(nmr_users = n_distinct(USER_ID)) %>%
  mutate(percent_users = (nmr_users/sum(nmr_users))*100) %>%
  mutate(lab.ypos = cumsum(percent_users) - 0.6*percent_users) %>%
  mutate_if(is.numeric, round, digits = 2)

#mycols <- c("#86CaC2", "#E15053")
mycols <- c("#96B89D", "#E15053")
viz_data_q2 %>%
  mutate(pos = (cumsum(c(0, viz_data_q2$percent_users)) + c(viz_data_q2$percent_users / 2, .01))[1:nrow(viz_data_q2)]) %>%
  ggplot(aes(x = 2, y = percent_users, fill = PAYMENT_2FIRSTADS)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  #geom_text(aes(y = lab.ypos, label = paste(percent_users, '%')), color = "white")+
  #geom_text_repel(aes(x = 2.4, y = c(42.58,3.37), label = PAYMENT_2FIRSTADS), 
  #                nudge_x = .9, 
  #                segment.size = .3, 
  #                show.legend = FALSE) +
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
  
##############################
# Q3: How willing are users to insert accross sections and groups? 
##############################

data_q3 <- read_csv(here('data','Q3_data.csv'))
viz_data_q3_1 <- data_q3 %>%
  group_by(CHANGE_CATEGORIES_2FIRSTADS) %>%
  summarise(nmr_users = n_distinct(USER_ID)) %>%
  mutate(percent_users = (nmr_users/sum(nmr_users))*100) %>%
  #mutate(lab.ypos = cumsum(percent_users) - 0.6*percent_users) %>%
  mutate_if(is.numeric, round, digits = 2)

mycols <- c("#96B89D", "#96B89D", "#E15053")
viz_data_q3_1 %>%
  ggplot(aes(x = 2, y = percent_users, fill = CHANGE_CATEGORIES_2FIRSTADS)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 89.2) +
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)

users_by_section <- data_q3 %>%
  filter(CHANGE_CATEGORIES_2FIRSTADS=='same section - group changed')%>%
  group_by(CATEGORY_SECTION1) %>%
  summarise(nmr_users_section = n_distinct(USER_ID)) 

users_by_groups  <- data_q3 %>%
  filter(CHANGE_CATEGORIES_2FIRSTADS=='same section - group changed')%>%
  group_by(CATEGORY_SECTION1,CATEGORY_GROUP1) %>%
  summarise(nmr_users_group = n_distinct(USER_ID))  

#viz_data_q3_2 <- merge(users_by_section,users_by_groups,by="CATEGORY_SECTION1")

users <- data_q3 %>%
  filter(CHANGE_CATEGORIES_2FIRSTADS=='same section - group changed')%>%
  summarise(nmr_users_2ads= n_distinct(USER_ID))

users_by_section %>%
  mutate(percent_users = (users_by_section$nmr_users_section/users$nmr_users_2ads)*100)%>%
  arrange(desc(percent_users)) %>%
  ggplot(aes(x=percent_users,y=reorder(CATEGORY_SECTION1, percent_users)))+
  geom_col(fill='#96B89D', width = 0.04) +
  geom_point(size=3, color='#96B89D') +
  labs(y=NULL,
       x='Percentage of users from the first ad group that inserts differently in the second ad')+
  theme_minimal()

  
##############################
# Q4: How add-on products perform? Does that differ between various categories?
##############################
  
data_q4 <- read_csv(here('data','Q4_data.csv'))

mycols<- c("#96B89D", "#E15053")
data_q4 %>%
  na.omit(data_q4$ADDITIONAL_PRODUCT_TYPE)%>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins= difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days= difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  filter(difference_days< 80)%>%
  mutate(ADDITIONAL_PRODUCT_TYPE = case_when(ADDITIONAL_PRODUCT_TYPE=='GALLERY' ~ 'Gallery',
                                             ADDITIONAL_PRODUCT_TYPE=='AUTOBUMP' ~ 'Autobump'
  )) %>%
  #ggplot(aes(x=Ad_days_alive, y=CATEGORY_SECTION, fill=factor(ADDITIONAL_PRODUCT_TYPE))) +
  ggplot(aes(x=difference_days, y=ADDITIONAL_PRODUCT_TYPE))+
  geom_jitter(aes(col=factor(ADDITIONAL_PRODUCT_TYPE)), position = position_jitter(height = .2, width = .02), alpha = .25)+
  geom_boxplot(alpha = 0.6)+
  labs(y=NULL,
       x=NULL)+
  facet_wrap(~ CATEGORY_SECTION, ncol = 4, scales = "free") +
  scale_x_reverse()+
  scale_fill_manual(values = mycols) +
  scale_colour_manual(values = mycols)+
  theme_minimal()+theme(legend.position="none")
##############################
# Q5: How fast are items getting sold in different categories? 
# BOX plots
##############################



data_q4$CATEGORY_SECTION <- factor(data_q4$CATEGORY_SECTION,levels = rev(c("Others", "Business", "Vehicles", "Leisure & Hobby","Electronics","Real Estate", "Home & Garden", "Personal Items" )))
data_q4 %>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins= difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days= difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  ggplot(aes(x=difference_days, y=CATEGORY_SECTION)) +
  geom_boxplot(alpha = 0.6)+
  scale_x_reverse(limits = c(75, -5), breaks = seq(75, 0, by = -5))+
  labs(y=NULL,
       x=NULL) +
  scale_fill_manual(values=c("#E15053", "#E15053", "#E15053", "#E15053",
                             "#E15053", "#E15053", "#E15053", "#E15053"))+
  
  theme_minimal()+
  theme(legend.position="none")


  



##############################
# Q5: How fast are items getting sold in different categories? 
# Density plots
##############################

# Add mean line by groups
mu <- iris %>%
  group_by(Species) %>%
  summarise(grp.mean = mean(Sepal.Length))





data_q4 %>%
  filter(Ad_days_alive<65)%>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins= difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days= difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  ggplot(aes(x=difference_days, group=CATEGORY_SECTION, color='#E15053')) +
  #geom_histogram(aes(y = ..density..), alpha = 0.4, fill = "#E15053", bins=65)+
  geom_density(adjust=1.5, alpha=.2) +
  
  #facet_wrap(~CATEGORY_SECTION, ncol=1)+
  facet_wrap(~ CATEGORY_SECTION, ncol = 1, labeller=labeller(Index = as_labeller(c('1'="Bussiness",
                                                                  '2'="Electronics",
                                                                  '3'="Home & Garden", 
                                                                  '4'='Leisure & Hobby',
                                                                  '5'='Others', 
                                                                  '6'='Personal Items', 
                                                                  '7'='Real Estate', 
                                                                  '8'='Vehicles'))), strip.position="left") +
  #geom_violin()+
  #geom_quasirandom()+
  #geom_jitter(position = position_jitter(height = .2, width = .02), alpha = .25, color='#E15053')+
  #geom_boxplot(alpha = 0.6) +
  labs(y=NULL,
       x='Selling time (days)')+
  scale_x_reverse()+
  theme_minimal()+
  theme(legend.position="none", strip.placement = "outside")

##############################
# Q5: How fast are items getting sold in different categories? 
# Density plots (joyplots)
##############################


data_density_plots <- data_q4 %>% 
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins= difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days= difftime(DELETE_DTT, PUBLISH_DTT, units = "days")) 

# lock in factor level order
data_density_plots$CATEGORY_SECTION <- factor(data_density_plots$CATEGORY_SECTION, levels = unique(data_density_plots$CATEGORY_SECTION))

custom.col <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
data_density_plots %>%
 ggplot(aes(x=difference_days,color=CATEGORY_SECTION)) +
 #scale_colour_identity(guide="legend",breaks=unique(data_density_plots$CATEGORY_SECTION))+
 geom_density()+
 labs(y='P(Selling Time)',
     x='Selling Time (days)')+
scale_x_continuous(breaks=seq(1, 75, 1), limits = c(-5, 75))+
scale_colour_manual(values=custom.col)+
  theme_minimal()
  

#E15053
data_density_plots %>% 
  ggplot(aes(x=difference_days, y=CATEGORY_SECTION, fill=CATEGORY_SECTION))+
  geom_joy(scale = 2, alpha=0.5) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_reverse(limits = c(75, -5), breaks = seq(75, 0, by = -5))+
  labs(y=NULL,
       x=NULL) +
  scale_fill_manual(values=c("#E15053", "#E15053", "#E15053", "#E15053",
                             "#E15053", "#E15053", "#E15053", "#E15053"))+
  
  theme_minimal()+
  theme(legend.position="none")
 

 



##############################
# Q5: How fast are items getting sold in different categories? 
# CDF
##############################

data_q4 %>%
  filter(Ad_days_alive<65)%>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins= difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days= difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  ggplot(aes(x=Ad_days_alive, group=CATEGORY_SECTION, color='#E15053')) +
  stat_ecdf(geom = "step")+
  #geom_histogram(aes(y = ..density..), alpha = 0.4, fill = "#E15053", bins=65)+
  #geom_density(adjust=1.5, alpha=.2) +
  #facet_wrap(~CATEGORY_SECTION, ncol=1)+
  facet_wrap(~ CATEGORY_SECTION, ncol = 1, labeller=labeller(Index = as_labeller(c('1'="Bussiness",
                                                                                   '2'="Electronics",
                                                                                   '3'="Home & Garden", 
                                                                                   '4'='Leisure & Hobby',
                                                                                   '5'='Others', 
                                                                                   '6'='Personal Items', 
                                                                                   '7'='Real Estate', 
                                                                                   '8'='Vehicles'))), strip.position="left") +
  #geom_violin()+
  #geom_quasirandom()+
  #geom_jitter(position = position_jitter(height = .2, width = .02), alpha = .25, color='#E15053')+
  #geom_boxplot(alpha = 0.6) +
  labs(y=NULL,
       x='Selling time (days)')+
  scale_x_reverse()+
  theme_minimal()+
  theme(legend.position="none", strip.placement = "outside")




##############################
# Q5: How fast are items getting sold in different categories? 
# Barcharts + grouping 2  bars
##############################

# lock in factor level order
data_q4$derma <- factor(df$derma, levels = df$derma)

data_q4_viz_groups_bars <- data_q4 %>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins = difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days = difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  mutate(selling_periods = case_when(difference_days < 30  ~ "< 1 Month",#difference_hours<= 24 ~ "<= 1 Day",
                                     difference_days>1 & difference_days <= 7  ~ "<= 1 Week",
                                     difference_days>7 & difference_days <= 14  ~ "<= 2 Weeks",
                                     difference_days>7 & difference_days <= 30 ~ "< 1 Month",
                                     difference_days > 30  ~ "> 1 Month"))%>%
  group_by(CATEGORY_SECTION,selling_periods) %>%
  summarise(nmr_ads = n()) %>%
  mutate(percent_users = (nmr_ads/sum(nmr_ads))*100) 

data_q4_viz_groups_bars$selling_periods <- factor(data_q4_viz_groups_bars$selling_periods,levels = c("<= 1 Day", "<= 1 Week", "<= 2 Weeks", "< 1 Month", "> 1 Month"))
#data_q4_viz_groups_bars$selling_periods <- factor(data_q4_viz_groups_bars$selling_periods,levels = c("< 1 Month","> 1 Month"))

data_q4_viz_groups_bars%>%
  ggplot(aes(y=percent_users,  x = selling_periods)) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(
    aes(label = paste('n =', nmr_ads), y = percent_users + 0.35),
    position = position_dodge(0.9),
    vjust = 0,
    size=3
  ) +
  facet_wrap(~ CATEGORY_SECTION, ncol = 1, labeller=labeller(Index = as_labeller(c('1'="Bussiness",
                                                                                   '2'="Electronics",
                                                                                   '3'="Home & Garden", 
                                                                                   '4'='Leisure & Hobby',
                                                                                   '5'='Others', 
                                                                                   '6'='Personal Items', 
                                                                                   '7'='Real Estate', 
                                                                                   '8'='Vehicles'))), strip.position="left") +
  theme_minimal()+
  theme(legend.position="none", 
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(labels = function(share_users) paste0(share_users, '%'))

##############################
# Q5: How fast are items getting sold in different categories? 
# Barcharts + grouping 5  bars
##############################

# lock in factor level order
data_q4$derma <- factor(df$derma, levels = df$derma)

data_q4_viz_groups_bars <- data_q4 %>%
  mutate(difference_hours= difftime(DELETE_DTT, PUBLISH_DTT, units = "hours"),
         difference_mins = difftime(DELETE_DTT, PUBLISH_DTT, units = "mins"),
         difference_days = difftime(DELETE_DTT, PUBLISH_DTT, units = "days"))%>%
  mutate(selling_periods = case_when(difference_hours<= 24 ~ "<= 1 Day",
                                     difference_days>1 & difference_days <= 2  ~ "<= 2 Days",
                                     difference_days>2 & difference_days <= 3  ~ "<= 3 Days",
                                     difference_days>3 & difference_days <= 4  ~ "<= 4 Days",
                                     difference_days>4 & difference_days <= 5  ~ "<= 5 Days",
                                     difference_days>5 & difference_days <= 6  ~ "<= 6 Days",
                                     difference_days>6 & difference_days <= 7  ~ "<= 7 Days"))%>%
                                     #difference_days > 30  ~ "> 1 Month"))%>%
  group_by(CATEGORY_SECTION,selling_periods) %>%
  summarise(nmr_ads = n()) 
  #mutate(percent_users = (nmr_ads/sum(nmr_ads))*100) 

data_q4_viz_groups_bars$selling_periods <- factor(data_q4_viz_groups_bars$selling_periods,levels = c("<= 1 Day", "<= 2 Days", "<= 3 Days", "<= 4 Days", "<= 5 Days", '<= 6 Days','<= 7 Days'))
#data_q4_viz_groups_bars$selling_periods <- factor(data_q4_viz_groups_bars$selling_periods,levels = c("< 1 Month","> 1 Month"))

data_q4_viz_groups_bars%>%
  ggplot(aes(y=nmr_ads,  x = selling_periods)) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(
    aes(label = paste('n =', nmr_ads), y = nmr_ads + 0.35),
    position = position_dodge(0.9),
    vjust = 0,
    size=3
  ) +
  facet_wrap(~ CATEGORY_SECTION, ncol = 1, labeller=labeller(Index = as_labeller(c('1'="Bussiness",
                                                                                   '2'="Electronics",
                                                                                   '3'="Home & Garden", 
                                                                                   '4'='Leisure & Hobby',
                                                                                   '5'='Others', 
                                                                                   '6'='Personal Items', 
                                                                                   '7'='Real Estate', 
                                                                                   '8'='Vehicles'))), strip.position="left") +
  theme_minimal()+
  theme(legend.position="none", 
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
  #scale_y_continuous(labels = function(share_users) paste0(share_users, '%'))



##############################
# Q5.2: Is number of received views a good indicator of selling time?
# Marginal Plots
##############################




#Getting data for data_q5_1 just views, and difference
data_q5_1_2sample <-data_q4 %>%
  mutate(difference_days= as.numeric(difftime(DELETE_DTT, PUBLISH_DTT, units = "days")))%>%
  select(CATEGORY_SECTION, AD_VIEWS, difference_days)


#SAMPLEANDO 
set.seed(1)
df_sampled <- data_q5_1_2sample %>%
  group_by(CATEGORY_SECTION) %>%
  sample_frac(.15)

df_sampled%>%
  group_by(CATEGORY_SECTION)%>%
  summarise(max(AD_VIEWS), n())



data_q4$CATEGORY_SECTION <- factor(data_q4$CATEGORY_SECTION,levels = rev(c("Others", "Business", "Vehicles", "Leisure & Hobby","Electronics","Real Estate", "Home & Garden", "Personal Items" )))
data_q5_1 <- data_q5_1_2sample %>%
  filter(CATEGORY_SECTION=="Business") %>%
  ungroup()%>%
  select(AD_VIEWS, difference_days)
  
p <- ggplot(data_q5_1,aes(x=difference_days, y=AD_VIEWS)) +
geom_point(alpha = 0.5,size = 0.03, color='#E15053')+
geom_smooth(method='loess',se=FALSE,aes(color='#d10f13', fill ='#837f7f'))+
labs(y=NULL,
     x=NULL) +
theme_minimal()+
theme(legend.position="none")+
scale_x_continuous(limits = c(-5, 60), breaks = seq(0, 60, by = 5))+
scale_y_continuous(limits = c(0,2500))

ggMarginal(p, type='histogram', xparams = list(binwidth=1, fill='#E15053',alpha = 0.7), yparams = list(binwidth=75, fill='#E15053',alpha = 0.7))

 


data_q5_1_2sample%>%
  group_by(CATEGORY_SECTION)%>%
  summarise(max(AD_VIEWS), n())
                