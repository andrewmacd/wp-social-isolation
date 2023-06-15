load("../china_isolation_data.RData")

names(china_isolation_data)

#Variable Coding

#Social Isolation

china_isolation_data$isolation = (china_isolation_data$Q7_A_rc + china_isolation_data$Q7_B_rc + china_isolation_data$Q7_C_rc)

mod2 = lm(connected ~ gen_social_med*urbanicity + urbases + female + CCP_member + age + jump_wall, data = china_isolation_data)
summary(mod2)

mod3 = lm(connected ~ gen_social_med*jump_wall + urbanicity + ses + female + CCP_member + age + jump_wall, data = china_isolation_data)
summary(mod3)

library(psy)
isolation.matrix = cbind(
  china_isolation_data$Q7_A_rc,
  china_isolation_data$Q7_B_rc,
  china_isolation_data$Q7_C_rc
)
cronbach(isolation.matrix)

library(scales)
china_isolation_data$isolation = rescale(china_isolation_data$isolation, to = c(0, 1))
summary(china_isolation_data$isolation)


#Digital Human Interaction
table(china_isolation_data$Q8_1_rc)
table(china_isolation_data$Q8_2_rc)
table(china_isolation_data$Q8_3_rc)
table(china_isolation_data$Q8_4_rc)
table(china_isolation_data$Q8_5_rc)
table(china_isolation_data$Q8_6_rc)

library(scales)
china_isolation_data$human_interaction = rescale((china_isolation_data$Q8_1_rc + china_isolation_data$Q8_2_rc + china_isolation_data$Q8_3_rc
                                                 + china_isolation_data$Q8_4_rc + china_isolation_data$Q8_5_rc + china_isolation_data$Q8_6_rc),
                                    t0 = c(0,1))
summary(china_isolation_data$human_interaction)

china_isolation_data$human_interaction_cat = factor(china_isolation_data$human_interaction,
                                             labels=c('0 Connections','1 Connection','2 Connections','3 Connections','4 Connections','5 Connections',
                                                      '6 Connections'))
table(china_isolation_data$human_interaction_cat)

#Online to Offline Relationships
table(china_isolation_data$Q9)
library(labelled)
var_label(china_isolation_data$Q9) <- NULL
table(china_isolation_data$Q9)

china_isolation_data$Q9_n = as.numeric(china_isolation_data$Q9)
summary(china_isolation_data$Q9_n)

library(scales)
china_isolation_data$online_offline = rescale(china_isolation_data$Q9_n, to = c(1,0))
table(china_isolation_data$online_offline)

china_isolation_data$online_offline_cat = factor(china_isolation_data$online_offline,
                                                    labels=c('No, never','Yes, once','Yes, several times','Yes, many times'))
table(china_isolation_data$online_offline_cat)


#Social Media Use
china_isolation_data$gen_social_med = (china_isolation_data$Q3_rc + china_isolation_data$Q5_rc + china_isolation_data$Q11_rc)


library(scales)
china_isolation_data$gen_social_med = rescale(china_isolation_data$gen_social_med, to = c(0, 1))
summary(china_isolation_data$gen_social_med)

#Urbanicity
table(china_isolation_data$Q38)
china_isolation_data$Q38_n = as.numeric(china_isolation_data$Q38)
summary(china_isolation_data$Q38_n)
china_isolation_data$Q38_n[china_isolation_data$Q38_n==6] <- NA
summary(china_isolation_data$Q38_n)

library(scales)
china_isolation_data$urbanicity = rescale(china_isolation_data$Q38_n, to = c(0, 1))
table(china_isolation_data$urbanicity)

china_isolation_data$urbanicity_cat = factor(china_isolation_data$urbanicity,
                                       labels=c('Village','Small City','Mid-Sized City','Suburban','Big City'))
table(china_isolation_data$urbanicity_cat)


#Analysis##########################################################################################################
#Descriptives###############
#Primary Variables
library(ggplot2)
figure1 = ggplot(china_isolation_data, aes(x=isolation)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="seashell", size=1)+
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Social Isolationism") +
  ylab("Density") +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
figure1

summary(china_isolation_data$isolation)
sd(china_isolation_data$isolation, na.rm = TRUE)

figure2a = ggplot(data = subset(china_isolation_data, !is.na(human_interaction_cat)), aes(x = human_interaction_cat)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Human Interaction", y = "Percent") +
  coord_flip() +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
figure2a

local({
  .Table <- with(china_isolation_data, table(human_interaction_cat))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


figure2b = ggplot(data = subset(china_isolation_data, !is.na(online_offline_cat)), aes(x = online_offline_cat)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Online to Offline Relationships", y = "Percent") +
  coord_flip() +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
figure2b


library(ggpubr)
library(grid)
figure2 = ggarrange(figure2a, figure2b, ncol=1, nrow=2, common.legend = FALSE, legend="right")
figure2


figure2c = ggplot(data = subset(china_isolation_data, !is.na(gen_social_med)), aes(x=gen_social_med)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="seashell", size = 1) +
  geom_density(alpha=.2, fill="#FF6666") +   
  xlab("Social Media Use") +
  ylab("Density")  +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
figure2c

summary(china_isolation_data$gen_social_med)
sd(china_isolation_data$gen_social_med, na.rm = TRUE)

figure2d = ggplot(data = subset(china_isolation_data, !is.na(urbanicity_cat)), aes(x = urbanicity_cat)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Urbanicity", y = "Percent") +
  coord_flip() +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
figure2d

local({
  .Table <- with(china_isolation_data, table(urbanicity_cat))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#Analysis########################################################################
#Models of Online Activity - Profiling ###################################
mod1 = lm(human_interaction ~ ses + female + CCP_member + age, data = china_isolation_data)
summary(mod1)
par(mfrow = c(2, 2))
plot(mod1)

library(MASS)
mod1_ord = polr(human_interaction_cat ~ ses + female + CCP_member + age, data = china_isolation_data)
summary(mod1_ord)
(ctable1 <- coef(summary(mod1_ord)))
## calculate and store p values
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable1 <- cbind(ctable1, "p value" = p))

library(DescTools)
PseudoR2(mod1_ord, c("McFadden", "Nagel"))

mod2 = lm(online_offline ~ ses + female + CCP_member + age, data = china_isolation_data)
summary(mod2)
par(mfrow = c(2, 2))
plot(mod2)

library(MASS)
mod2_ord = polr(online_offline_cat ~ ses + female + CCP_member + age, data = china_isolation_data)
summary(mod2_ord)
(ctable2 <- coef(summary(mod2_ord)))
## calculate and store p values
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable2 <- cbind(ctable2, "p value" = p))

library(DescTools)
PseudoR2(mod2_ord, c("McFadden", "Nagel"))

mod3 = lm(gen_social_med ~ ses + female + CCP_member + age, data = china_isolation_data)
summary(mod3)

library(stargazer)
stargazer(mod1_ord, mod2_ord, mod3,
          digits = 2, 
          dep.var.labels = c("Models"),
          star.cutoffs = c(0.0549999),
          omit.stat=c("LL","ser","f"), 
          no.space =TRUE)

#Models of isolationism
mod4 = lm(isolation ~ human_interaction + online_offline + gen_social_med + urbanicity + ses + female + CCP_member + age, data = china_isolation_data)
summary(mod4)

mod5 = lm(isolation ~ human_interaction*urbanicity + online_offline + gen_social_med + ses + female + CCP_member + age, data = china_isolation_data)
summary(mod5)

mod6 = lm(isolation ~ online_offline*urbanicity + human_interaction + gen_social_med + ses + female + CCP_member + age, data = china_isolation_data)
summary(mod6) 

mod7 = lm(isolation ~ gen_social_med*urbanicity + human_interaction + online_offline + ses + female + CCP_member + age, data = china_isolation_data)
summary(mod7)

library(stargazer)
stargazer(mod4, mod5, mod6, mod7,
          digits = 2, 
          dep.var.labels = c("Models"),
          star.cutoffs = c(0.0549999),
          omit.stat=c("LL","ser","f"), 
          no.space =TRUE)


library(ggplot2)
library(sjPlot)

a = plot_model(mod5, type = "pred", title = "", terms = c("human_interaction", "urbanicity")
               , legend.title = "Urbanicity") +
  scale_color_discrete(labels = c("Village", "Small City", "Mid-sized City", "Suburb", "Big City")) +
  ylab("") +
  xlab("Digital Human Interaction") +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
a

b = plot_model(mod6, type = "pred", title = "", terms = c("online_offline", "urbanicity")
               , legend.title = "Urbanicity") +
  scale_color_discrete(labels = c("Village", "Small City", "Mid-sized City", "Suburb", "Big City")) +
  ylab("") +
  xlab("Online to Offline Relationships") +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
b

c = plot_model(mod7, type = "pred", title = "", terms = c("gen_social_med", "urbanicity")
               , legend.title = "Urbanicity") +
  scale_color_discrete(labels = c("Village", "Small City", "Mid-sized City", "Suburb", "Big City")) +
  ylab("") +
  xlab("Social Media Use") +
  theme(panel.border = element_rect(color = "black",fill = NA, size = 1))
c

library(ggpubr)
library(grid)
figure5 = ggarrange(a, b, c, ncol=2, nrow=2, common.legend = FALSE, legend="right")
figure5 = annotate_figure(figure5, left = textGrob("Feelings of Isolation",  rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
figure5












