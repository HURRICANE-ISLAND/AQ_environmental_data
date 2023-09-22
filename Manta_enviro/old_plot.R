#parameters#
library(tidyverse)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(ggthemes)
theme_set(theme_tufte())
theme_update(plot.title =element_text(hjust=0.5),
             axis.line.x = element_line(color="black", size=1), 
             element_line(color="black", size=1),
             text = element_text(size=20),
             axis.text = element_text(size=15)) 

gsi.dat <- read_csv("~/Desktop/WD/GSI/GSI.tidy.csv")
parameters <- read_csv("~/Desktop/WD/GSI/Jack/parameters.csv")
parameters$Date <- as.Date(parameters$Date)
parameters <- parameters %>% filter(!`Gear Type` =="Cage")

parameters$`Gear Type` <- as.factor(parameters$`Gear Type`)
parameters$Type <- as.factor(parameters$Type)
parameters$pH <- as.numeric(parameters$pH)

summary(parameters)


HI_AQwild <- parameters %>% filter(parameters$Site == "HI") %>% filter(Year %in% c("2020", "2021", "2022"))
NH_AQwild <- parameters %>% filter(parameters$Site == "NH") %>% filter(Year %in% c("2020", "2021", "2022"))
ST_AQwild <- parameters %>% filter(parameters$Site == "ST") %>% filter(Year %in% c("2020", "2021", "2022"))

#figure out 2020 max GSI, aquaculture#

HI_AQwild2020 <- HI_AQwild %>% filter(Year == "2020")
HI_AQwild2020[which.max(HI_AQwild2020$GSI_Avg),]

HI_AQwild2020 <- HI_AQwild %>% filter(Year == "2020") %>% filter(!Type== "wild")
HI_AQwild2020[which.max(HI_AQwild2020$GSI_Avg),]

HI_temp <- HI_AQwild %>%
  ggplot(aes(x = Date, y = Temp_Net)) + 
  geom_point(size =2, color = "red3") +
  ylim(c(0,20))+ xlim(as.Date(c("2020-06-01","2020-10-30")))+
  ggtitle("","2020")+ 
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ylab("Average Temperature (C)")  

HI_temp

plot_HIAQwild <- HI_AQwild %>% 
  ggplot(aes(x = Date, y = GSI_Avg, color = Type)) + 
  scale_color_manual(values = c("#619CFF","#F8766D")) + 
  geom_line(data = filter(HI_AQwild, Type == "AQ"), size= 0.50, linetype = "solid") +
  geom_point(aes(shape = Type), size = 1) + scale_shape_manual(values = c(16,17)) + 
  ylim(c(0,30)) +  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ggtitle("Hurricane Island (HI)", "2020") + ylab("%GSI") 
plot_HIAQwild

p1 <- ggarrange(plot_HIAQwild, HI_temp, ncol = 1, common.legend = TRUE)
p1

HI_chlorophyll <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `Chlorophyll RFU...11`)) +
  geom_point(color = "darkgreen") + ylim(c(0,2))+
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("Chlorophyll (RFU)") 

HI_chlorophyll

HI_cond <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `Cond µS/cm`)) + 
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ylim(c(36000, 46000))+
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("Conductivity (µS/cm) ")  
HI_cond

HI_ODO <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `ODO % sat`)) +
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ylim(c(90,120))+
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("ODO (% saturation) ") 

HI_ODO  

HI_TDS <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `TDS mg/L`)) +
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("TDS (mg/L) ") 

HI_TDS

HI_pH <-  HI_AQwild %>% filter(!pH == "NA") %>%
  ggplot(aes(x = Date, y = pH)) +
  geom_point() + ylim(c(7.5, 8.5)) +
  xlim(as.Date(c("2020-06-01","2020-10-30"))) +
  geom_vline(xintercept = as.Date("2020-08-19"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-09-08"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("pH")  
HI_pH

p2020 <- ggarrange(plot_HIAQwild, HI_temp, HI_chlorophyll, HI_cond, HI_ODO, HI_pH, ncol = 1, common.legend = TRUE)
p2020

#HI2021#

HI_AQwild2021 <- HI_AQwild %>% filter(Year == "2021")
HI_AQwild2021[which.max(HI_AQwild2021$GSI_Avg),]

HI_AQwild2021 <- HI_AQwild %>% filter(Year == "2021") %>% filter(!Type== "wild")
HI_AQwild2021[which.max(HI_AQwild2021$GSI_Avg),]

HI_temp <- HI_AQwild %>%
  ggplot(aes(x = Date, y = Temp_Net)) + 
  geom_point(size =2, color = "red3") +
  ylim(c(0,20))+ xlim(as.Date(c("2021-06-01","2021-10-30")))+
  ggtitle("","2021")+ 
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ylab("Average Temperature (C)")  

HI_temp

plot_HIAQwild <- HI_AQwild %>% 
  ggplot(aes(x = Date, y = GSI_Avg, color = Type)) + 
  scale_color_manual(values = c("#619CFF","#F8766D")) + 
  geom_line(data = filter(HI_AQwild, Type == "AQ"), size= 0.50, linetype = "solid") +
  geom_point(aes(shape = Type), size = 1) + scale_shape_manual(values = c(16,17)) + 
  ylim(c(0,30)) +  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ggtitle("Hurricane Island (HI)", "2021") + ylab("%GSI") 
plot_HIAQwild

p1 <- ggarrange(plot_HIAQwild, HI_temp, ncol = 1, common.legend = TRUE)
p1

HI_chlorophyll <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `Chlorophyll RFU...11`)) +
  geom_point(color = "darkgreen") + ylim(c(0,2))+
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("Chlorophyll (RFU)") 

HI_chlorophyll

HI_cond <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `Cond µS/cm`)) + 
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ylim(c(36000, 46000))+
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("Conductivity (µS/cm) ")  
HI_cond

HI_ODO <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `ODO % sat`)) +
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ylim(c(90,120))+
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("ODO (% saturation) ") 

HI_ODO  

HI_TDS <- HI_AQwild %>%
  ggplot(aes(x = Date, y = `TDS mg/L`)) +
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("TDS (mg/L) ") 

HI_TDS

HI_pH <-  HI_AQwild %>% filter(!pH == "NA") %>%
  ggplot(aes(x = Date, y = pH)) +
  geom_point() + ylim(c(7.5, 8.5)) +
  xlim(as.Date(c("2021-06-01","2021-10-30"))) +
  geom_vline(xintercept = as.Date("2021-08-31"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-09-10"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("pH")  
HI_pH

p2021 <- ggarrange(plot_HIAQwild, HI_temp, HI_chlorophyll, HI_cond, HI_ODO, HI_pH, ncol = 1, common.legend = TRUE)
p2021

##NH###################################################################################################################
#2020####

NH_AQwild2021 <- NH_AQwild %>% filter(Year == "2020")
NH_AQwild2021[which.max(NH_AQwild2021$GSI_Avg),]

NH_AQwild2021 <- NH_AQwild %>% filter(Year == "2020") %>% filter(!Type== "wild")
NH_AQwild2021[which.max(NH_AQwild2021$GSI_Avg),]

NH_temp <- NH_AQwild %>%
  ggplot(aes(x = Date, y = Temp_Net)) + 
  geom_point(size =2, color = "red3") +
  ylim(c(0,20))+ xlim(as.Date(c("2020-06-01","2020-10-30")))+
  ggtitle("","2020")+
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ylab("Average Temperature (C)")  

NH_temp

plot_NHAQwild <- NH_AQwild %>% 
  ggplot(aes(x = Date, y = GSI_Avg, color = Type)) + 
  scale_color_manual(values = c("#619CFF","#F8766D")) + 
  geom_line(data = filter(NH_AQwild, Type == "AQ"), size= 0.50, linetype = "solid") +
  geom_point(aes(shape = Type), size = 1) + scale_shape_manual(values = c(16,17)) + 
  ylim(c(0,30)) +  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ggtitle("North Haven (NH)", "2020") + ylab("%GSI") 
plot_NHAQwild

p1 <- ggarrange(plot_NHAQwild, NH_temp, ncol = 1, common.legend = TRUE)
p1

NH_chlorophyll <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `Chlorophyll RFU...11`, color = Type)) +
  geom_point() + ylim(c(0,2))+
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("Chlorophyll (RFU)") 

NH_chlorophyll

NH_cond <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `Cond µS/cm`, color = Type)) + 
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ylim(c(36000, 46000))+
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("Conductivity (µS/cm) ")  
NH_cond

NH_ODO <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `ODO % sat`, color = Type)) +
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  ylim(c(90,120))+
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("ODO (% saturation) ") 

NH_ODO  

NH_TDS <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `TDS mg/L`)) +
  geom_point() + 
  xlim(as.Date(c("2020-06-01","2020-10-30")))+  
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("TDS (mg/L) ") 

NH_TDS

NH_pH <-  NH_AQwild %>% filter(!pH == "NA") %>%
  ggplot(aes(x = Date, y = pH, color = Type)) +
  geom_point() + ylim(c(7.5, 8.5)) +
  xlim(as.Date(c("2020-06-01","2020-10-30"))) +
  geom_vline(xintercept = as.Date("2020-08-04"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2020-08-26"), color = "#F8766D") +
  ggtitle( "", "2020") + ylab("pH")  
NH_pH

p2020 <- ggarrange(plot_NHAQwild, NH_temp, NH_chlorophyll, NH_cond, NH_ODO, NH_pH, ncol = 1, common.legend = TRUE)
p2020


##2021###
NH_AQwild2021 <- NH_AQwild %>% filter(Year == "2021")
NH_AQwild2021[which.max(NH_AQwild2021$GSI_Avg),]

NH_AQwild2021 <- NH_AQwild %>% filter(Year == "2021") %>% filter(Type== "wild")
NH_AQwild2021[which.max(NH_AQwild2021$GSI_Avg),]

NH_temp <- NH_AQwild %>%
  ggplot(aes(x = Date, y = Temp_Net)) + 
  geom_point(size =2, color = "red3") +
  ylim(c(0,20))+ xlim(as.Date(c("2021-06-01","2021-10-30")))+
  ggtitle("","2021")+
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ylab("Average Temperature (C)")  

NH_temp

plot_NHAQwild <- NH_AQwild %>% 
  ggplot(aes(x = Date, y = GSI_Avg, color = Type)) + 
  scale_color_manual(values = c("#619CFF","#F8766D")) + 
  geom_line(data = filter(NH_AQwild, Type == "AQ"), size= 0.50, linetype = "solid") +
  geom_point(aes(shape = Type), size = 1) + scale_shape_manual(values = c(16,17)) + 
  ylim(c(0,30)) +  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ggtitle("North Haven (NH)", "2021") + ylab("%GSI") 
plot_NHAQwild

p1 <- ggarrange(plot_NHAQwild, NH_temp, ncol = 1, common.legend = TRUE)
p1

NH_chlorophyll <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `Chlorophyll RFU...11`, color = Type)) +
  geom_point() + ylim(c(0,2))+
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("Chlorophyll (RFU)") 

NH_chlorophyll

NH_cond <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `Cond µS/cm`, color = Type)) + 
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ylim(c(36000, 46000))+
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("Conductivity (µS/cm) ")  
NH_cond

NH_ODO <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `ODO % sat`, color = Type)) +
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  ylim(c(90,120))+
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("ODO (% saturation) ") 

NH_ODO  

NH_TDS <- NH_AQwild %>%
  ggplot(aes(x = Date, y = `TDS mg/L`)) +
  geom_point() + 
  xlim(as.Date(c("2021-06-01","2021-10-30")))+  
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("TDS (mg/L) ") 

NH_TDS

NH_pH <-  NH_AQwild %>% filter(!pH == "NA") %>%
  ggplot(aes(x = Date, y = pH, color = Type)) +
  geom_point() + ylim(c(7.5, 8.5)) +
  xlim(as.Date(c("2021-06-01","2021-10-30"))) +
  geom_vline(xintercept = as.Date("2021-09-25"), color = "#619CFF") +
  geom_vline(xintercept = as.Date("2021-08-25"), color = "#F8766D") +
  ggtitle( "", "2021") + ylab("pH")  
NH_pH

p2021 <- ggarrange(plot_NHAQwild, NH_temp, NH_chlorophyll, NH_cond, NH_ODO, NH_pH, ncol = 1, common.legend = TRUE)
p2021
