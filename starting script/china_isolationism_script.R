load("C:/Users/jbg43/Dropbox/Research/Internet Politics/China/Isolationism/china_isolation_data.RData")

names(china_isolation_data)
table(china_isolation_data$connected)
cor.test(china_isolation_data$gen_social_med,china_isolation_data$connected)

mod1 = lm(connected ~ gen_social_med + urbanicity + ses + female + CCP_member + age + jump_wall, data = china_isolation_data)
summary(mod1)

mod2 = lm(connected ~ gen_social_med*urbanicity + ses + female + CCP_member + age + jump_wall, data = china_isolation_data)
summary(mod2)

mod3 = lm(connected ~ gen_social_med*jump_wall + urbanicity + ses + female + CCP_member + age, data = china_isolation_data)
summary(mod3)

library(ggplot2)
library(sjPlot)

a = plot_model(mod2, type = "pred", title = "", terms = c("gen_social_med", "urbanicity")
               , legend.title = "Urbanicity") +
  scale_color_discrete(labels = c("Village", "Small City", "Mid-sized City", "Suburb", "Big City")) +
  ylab("Feelings of Isolation") +
  xlab("Social Media Use") 
a

b = plot_model(mod3, type = "pred", title = "", terms = c("gen_social_med", "jump_wall")
               , legend.title = "Globally Connected") +
  scale_color_discrete(labels = c("Never", "", "", "A Moderate Amount", "", "", "Very Often")) +
  ylab("Feelings of Isolation") +
  xlab("Social Media Use") 
b

library(ggpubr)
library(grid)
figure1 = ggarrange(a, b, ncol=2, nrow=1, common.legend = FALSE, legend="top")
figure1 = annotate_figure(figure1, left = textGrob("Feelings of Isolation",  rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
figure1


library(stargazer)
stargazer(mod1, mod2, mod3,
          digits = 2, 
          dep.var.labels = c("Models"),
          star.cutoffs = c(0.0549999),
          omit.stat=c("LL","ser","f"), 
          no.space =TRUE)

#Invert Connected
table(china_isolation_data$Q7_A)
table(china_isolation_data$Q7_A_rc)

china_isolation_data$connected = rescale(nrc_tweet$percent, to = c(0, 1))
summary(nrc_tweet$percent_rc)

china_isolation_data$xxx = (china_isolation_data$Q7_A_rc + china_isolation_data$Q7_B_rc + china_isolation_data$Q7_C_rc)
cor.test(china_isolation_data$xxx, china_isolation_data$connected)

library(scales)
china_isolation_data$urbanicity_cat = as.factor(rescale(china_isolation_data$urbanicity, to = c(1, 5)))
table(china_isolation_data$urbanicity_cat)
