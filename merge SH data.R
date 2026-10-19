#open JV trts
#open POXC
#open PMC
#clean files
#merge files


library(tidyverse)

#treatment file
trt = read.csv("JV trts.csv", header = TRUE)
trt = trt[-which(is.na(trt$Plot)),]

trt$Date= as.Date("2022-05-06")
trt$K = as.numeric(trt$K)

trt =
  trt %>% mutate(across(where(is.character), as.factor),
                     across(where(is.integer), as.factor))


#PMC file

pmc = read.csv("SH data 8.28.23.csv", header = TRUE)


pmc$Date= as.Date("2022-05-06")

pmc =
  pmc %>% mutate(across(where(is.character), as.factor),
                 across(where(is.integer), as.factor))


#poxc file

poxc = read.csv("poxc 8.29.23.csv", header = TRUE)

poxc$Date= as.Date(poxc$Date, "%m/%d/%Y")

poxc =
  poxc %>% mutate(across(where(is.character), as.factor),
                 across(where(is.integer), as.factor))%>%
    group_by(Date, Plot, Depth)%>%
  summarize(poxc_mg_kg_mean = mean(poxc_mg_kg))


#merge

shdat = merge(poxc, trt)

shdat = merge(shdat, pmc)

write.csv(shdat, 
          "sh data 8.29.23.csv",
          row.names = FALSE)

#graph

ggplot(shdat,
       aes(x = N.rate, 
           y = minC,
           color = cover, #))+
           shape = crop))+
  geom_point()+
  facet_grid(.~crop, scales = "free")+
  geom_smooth(method= "lm", se= FALSE)+
  # geom_boxplot()+
  theme_bw()
  

ggplot(shdat,
       aes(x = N.rate, 
           y = minC,
           color = Rep))+
  # shape = crop))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se= FALSE)+
  facet_grid(Depth~crop, scales = "free")+
  # geom_boxplot()+
  theme_bw()+
  labs(y = "24-hour C mineralization")


ggplot(shdat,
       aes(x = N.rate, 
           y = minC,
           color = cover, #))+
           shape = crop))+
  geom_point(size = 2)+
  facet_grid(Depth~crop, scales = "free")+
  geom_smooth(method= "lm", se= FALSE)+
  # geom_boxplot()+
  theme_bw()
  labs(y = "Permanganate Oxidizeable C")
