library(tidyverse)
library(here)
library(dplyr)
library(patchwork)
library(ggrepel)


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
                #mutate(share_users = (nmr_users/sum(nmr_users))*100)
                
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


data_q4 %>%
  na.omit(data_q4$ADDITIONAL_PRODUCT_TYPE)%>%
  filter(Ad_days_alive< 80)%>%
  mutate(ADDITIONAL_PRODUCT_TYPE = case_when(ADDITIONAL_PRODUCT_TYPE=='GALLERY' ~ 'Gallery',
                                             ADDITIONAL_PRODUCT_TYPE=='AUTOBUMP' ~ 'Autobump'
  )) %>%
  #ggplot(aes(x=Ad_days_alive, y=CATEGORY_SECTION, fill=factor(ADDITIONAL_PRODUCT_TYPE))) +
  ggplot(aes(x=Ad_days_alive, y=ADDITIONAL_PRODUCT_TYPE)) +
  geom_jitter(position = position_jitter(height = .2, width = .02), alpha = .25, color='#E15053')+
  geom_boxplot(alpha = 0.6)+
  labs(y=NULL,
       x='Selling time (days)')+
  facet_wrap(~ CATEGORY_SECTION, ncol = 4, scales = "free") +
  scale_x_reverse()+
  theme_minimal()

  
  




  #select(CATEGORY_SECTION1, CATEGORY_GROUP1, USER_ID)
  #mutate(percent_users = (nmr_users/sum(nmr_users_section))*100) %>%
  #mutate(lab.ypos = cumsum(percent_users) - 0.6*percent_users) %>%
  #mutate_if(is.numeric, round, digits = 2)

                