### Tripartite Plant Biomass Script ###

library(ggplot2); library(ggpubr)

# Import data

DF <- read.csv("SD.Master.csv")

# remove samples with no data
DF.1 <- subset(DF, Biomass != "NA")

# Percent Colonization

my_comps <- list(c("Suillus", "Burkholderia+Suillus"), c("Suillus", "Burkholderia.HT+Suillus"),
                 c("Burkholderia+Suillus", "Burkholderia.HT+Suillus"))

ggplot(DF, aes(x=Condition, y=Perc.Colon)) + geom_boxplot() + theme_bw() +
  stat_compare_means(comparisons = my_comps)

# Change order of X axis variables

Condition.Order <- c("Control", "Burkholderia", "Burkholderia.HT", "Suillus", "Burkholderia+Suillus", "Burkholderia.HT+Suillus")

# PERCENT COLONIZATION (this plot uses ggpubr stats - just exploratory)

ggplot(DF, aes(x=factor(Condition, level = Condition.Order), y=Perc.Colon, color=Condition)) + 
  geom_boxplot() + theme_bw() + stat_compare_means(comparisons = my_comps) + xlab("") + 
  ylab("Mycorrhizal Colonization %") + geom_jitter(alpha=0.4) + coord_flip() + 
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.x = element_text(size = 12)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5)

# AOV
PC.aov <- aov(Perc.Colon ~ Condition, data = DF.1)
summary(PC.aov)

# Assign Tukey test letter codes to illustrate significant differences
### install.packages("multcompView") ###
library(multcompView)
library(tidyverse)
PC.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Perc.Colon ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(PC.letters.df)[1] <- "Letter" #Reassign column name
PC.letters.df$Condition <- rownames(PC.letters.df) #Create column based on rownames
PC.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Perc.Colon), sd=sd(Perc.Colon)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
PC.letter.df2 <- left_join(PC.letters.df, PC.placement)

# Add faceting variables
DF.1$Colonization <- "Mycorrhizal Colonization"
DF.1$Mass <- "Plant Biomass"
DF.1$SH <- "Shoot Height"
DF.1$RL <- "Root Length"
DF.1$RS <- "Root:Shoot Ratio"

# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Perc.Colon, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=PC.letter.df2, aes(label=Letter, x=Condition, y = 100, size=12)) +
  ylab("Mycorrhizal Colonization %") + geom_jitter(alpha=0.4) + coord_flip() + 
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10, face = "bold")) + theme(axis.title.x = element_text(size = 12)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~Colonization) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)"))

ggsave(filename = "Perc.Colon.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

pbsp <- subset(DF.1, Condition == "Burkholderia+Suillus")
sp <- subset(DF.1, Condition == "Suillus")
mean(pbsp$Perc.Colon)
mean(sp$Perc.Colon)
# BIOMASS (DRY)

my_comps2 <- list(c("Control", "Burkholderia"), c("Control", "Burkholderia.HT"), c("Control", "Suillus"),
                  c("Control","Burkholderia+Suillus"),c("Control", "Burkholderia.HT+Suillus"),
                  c("Burkholderia", "Burkholderia.HT"), c("Burkholderia", "Suillus"),
                  c("Burkholderia", "Burkholderia+Suillus"), c("Burkholderia", "Burkholderia.HT+Suillus"),
                  c("Suillus", "Burkholderia+Suillus"), c("Suillus", "Burkholderia.HT+Suillus"), 
                  c("Burkholderia+Suillus", "Burkholderia.HT+Suillus"))

ggplot(DF, aes(x=factor(Condition, level = Condition.Order), y=Biomass, color=Condition)) + 
  geom_boxplot() + theme_bw() + stat_compare_means(comparisons = my_comps2) + xlab("")  +
  ylab("Dry Plant Biomass (g)") + geom_jitter(alpha=0.4) + 
  scale_color_manual(values = c("ivory3", "red3", "red3", "steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) + 
  theme(axis.text.y = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust=1.5, face = "bold")) +
  geom_hline(yintercept=0.14, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5)

# TUKEY test and letters to graph
BM.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Biomass ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(BM.letters.df)[1] <- "Letter" #Reassign column name
BM.letters.df$Condition <- rownames(BM.letters.df) #Create column based on rownames
BM.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Biomass), sd=sd(Biomass)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
BM.letter.df2 <- left_join(BM.letters.df, BM.placement)
#### alter Tukey letters to have control as A --> in excel
write.csv(BM.letter.df2, "Biomass.Tukey.csv")
BM.letter.df2 <- read.csv("Biomass.Tukey.csv") ### use Letter2 column (new Letters)

# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Biomass, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=BM.letter.df2, aes(label=Letter2, x=Condition, y = 0.65, size=6, vjust=1.2, hjust=1)) +
  ylab("Dry Plant Biomass (g)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.14, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~Mass) +
  theme(strip.text = element_text(size=12, face="bold")) + coord_flip() +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)"))
  

ggsave(filename = "Biomass.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

# SHOOT HEIGHT

ggplot(DF, aes(x=factor(Condition, level = Condition.Order), y=Shoot.Height, color=Condition)) + 
  geom_boxplot() + theme_bw() + stat_compare_means(comparisons = my_comps2) + xlab("")  +
  ylab("Shoot Height (cm)") + geom_jitter(alpha=0.4) + 
  scale_color_manual(values = c("ivory3", "red3", "red3", "steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) + 
  theme(axis.text.y = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust=1.5, face = "bold")) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5)

# TUKEY test and letters to graph
SH.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Shoot.Height ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(SH.letters.df)[1] <- "Letter" #Reassign column name
SH.letters.df$Condition <- rownames(SH.letters.df) #Create column based on rownames
SH.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Shoot.Height), sd=sd(Shoot.Height)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
SH.letter.df2 <- left_join(SH.letters.df, SH.placement)

# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Shoot.Height, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=SH.letter.df2, aes(label=Letter, x=Condition, y = 9, vjust=1.5, size=12)) +
  ylab("Shoot Height (cm)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~SH) +
  theme(strip.text = element_text(size=12, face="bold")) + coord_flip() +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) 

ggsave(filename = "Shoot.Height.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

# ROOT LENGTH

ggplot(DF, aes(x=factor(Condition, level = Condition.Order), y=Root.Length, color=Condition)) + 
  geom_boxplot() + theme_bw() + stat_compare_means(comparisons = my_comps2) + xlab("")  +
  ylab("Root Length (cm)") + geom_jitter(alpha=0.4) + 
  scale_color_manual(values = c("ivory3", "red3", "red3", "steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) + 
  theme(axis.text.y = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust=1.5, face = "bold")) +
  geom_hline(yintercept=16, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5)

# TUKEY test and letters to graph
RL.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Root.Length ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(RL.letters.df)[1] <- "Letter" #Reassign column name
RL.letters.df$Condition <- rownames(RL.letters.df) #Create column based on rownames
RL.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Root.Length), sd=sd(Root.Length)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
RL.letter.df2 <- left_join(RL.letters.df, RL.placement)

# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Root.Length, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=RL.letter.df2, aes(label=Letter, x=Condition, y = 30, size=12)) +
  ylab("Root Length (cm)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=16, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~RL) +
  theme(strip.text = element_text(size=12, face="bold")) + coord_flip() +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)"))
  

ggsave(filename = "Root.Length.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

### Root:Shoot ratio ###

DF.1$Root.Shoot <- DF.1$Root.Length / DF.1$Shoot.Height

# TUKEY test and letters to graph
RS.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Root.Shoot ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(RS.letters.df)[1] <- "Letter" #Reassign column name
RS.letters.df$Condition <- rownames(RS.letters.df) #Create column based on rownames
RS.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Root.Shoot), sd=sd(Root.Shoot)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
RS.letter.df2 <- left_join(RS.letters.df, RS.placement)

DF.1$RS.Length <- "Root:Shoot Length Ratio"
# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Root.Shoot, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=RS.letter.df2, aes(label=Letter, x=Condition, y = 7, size=12)) +
  ylab("R:S Ratio") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=3.32, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~RS.Length) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  coord_flip()


ggsave(filename = "Root.Shoot.LengthRatio.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

root.shoot.len.lm <- aov(Root.Shoot ~ Bac.Inoc.Type*EcMF.Inoc.Type, 
                         data=DF.1)

summary(root.shoot.len.lm)

### LINEAR MODELS ###
library(broom)
library(dplyr)

# Biomass
bio.mass.lm <- aov(Biomass ~ Bac.Inoc.Type*EcMF.Inoc.Type, data = DF.1)
summary(bio.mass.lm)
bm.df <- tidy(bio.mass.lm)

tukey.bm <- TukeyHSD(bio.mass.lm)


Export the significant results to a CSV file
write.csv(tidy_tukey, "significant_tukey_BIOMASS.results.csv", row.names = FALSE)

Filter for only significant results (e.g., p.value < 0.05)
significant_results <- tidy_tukey %>%
  filter(adj.p.value < 0.05)
print(significant_results)



# Shoot height
SH.lm <- aov(Shoot.Height ~ Bac.Inoc.Type * EcMF.Inoc.Type, data = DF.1)
summary(SH.lm)
# Shoot height minus heat treated
DF3 <- subset(DF, Condition !="Burkholderia.HT" | Condition != "Burkholderia.HT+Suillus")
SH.lm2 <- aov(Shoot.Height ~ Bac.Inoc.Type * EcMF.Inoc.Type, data = DF3)
summary(SH.lm2)
# Root length
RL.lm <- aov(Root.Length ~ Bac.Inoc.Type*EcMF.Inoc.Type, data = DF.1)
summary(RL.lm)
# Colonization 
colon.lm <- aov(Perc.Colon ~ Bac.Inoc.Type*EcMF.Inoc.Type, data = DF.1)
summary(colon.lm)

### Try colonization model without control and bacteria since these were 0's
colon.adj <- subset(DF.1, Condition != "Burkholderia")
colon.adj <- subset(colon.adj, Condition != "Control")
colon.adj <- subset(colon.adj, Condition != "Burkholderia.HT")

colon.lm2 <- aov(Perc.Colon ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=colon.adj)
summary(colon.lm2) ### Stick with first model. It makes sense that Bac.Inoc.Type would be signficant, 
## because it's absence or presence makes a difference.


### Correlations between biomass and colonization
# data frame without controls and bacteria only conditions
DF.2 <- subset(DF.1, Condition !="Control")
DF.2 <- subset(DF.2, Condition != "Burkholderia")
DF.2 <- subset(DF.2, Condition != "Burkholderia.HT")
ggplot(DF.2, aes(x=Perc.Colon, y=Biomass, color=Condition)) + geom_point(size = 4, alpha = 0.9) +
  theme_bw() + scale_color_manual(values = c("red3", "steelblue", "slategray")) + 
  geom_smooth(aes(x=Perc.Colon,y=Biomass, color=Condition), method = "lm", se = TRUE, size = 1, linetype = "dashed") +
  xlab("Mycorrhizal Colonization %") + ylab("Dry Plant Biomass (g)") + facet_grid(~Condition) + theme(legend.position = "none") +
  theme(strip.text = element_text(size=6, face="bold"))

# Create a new data frame with Condition names and r2

ggsave(filename = "BiomassByPercColon.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 4,
       height = 3,
       units = c("in"),
       dpi = 300)

# determine r2 and model stats for each 
DF2.1 <- subset(DF.2, Condition == "Burkholderia.HT+Suillus") # Burk.HT + Suillus
DF2.2 <- subset(DF.2, Condition == "Burkholderia+Suillus") # Burk + Suillus
DF2.3 <- subset(DF.2, Condition == "Suillus") # Suillus

# Burk.HT + Suillus linear model
DF2.1.lm <- lm(Biomass ~ Perc.Colon, data = DF2.1)
summary(DF2.1.lm)

# Burk + Suillus linear model
DF2.2.lm <- lm(Biomass ~ Perc.Colon, data = DF2.2)
summary(DF2.2.lm)

# Suillus linear model
DF2.3.lm <- lm(Biomass ~ Perc.Colon, data = DF2.3)
summary(DF2.3.lm)


### Plant Shoot Chemistry

psc <- read.csv("P.chem.csv")
Condition.Order <- c("Control", "Burkholderia", "Burkholderia.HT", "Suillus", "Burkholderia+Suillus", "Burkholderia.HT+Suillus")


# NITROGEN
# basic plot
ggplot(psc, aes(x=factor(Condition, level = Condition.Order), y=N, color=Condition)) + 
  geom_boxplot() + theme_bw() + stat_compare_means(comparisons = my_comps2) + xlab("")  +
  ylab("N%") + geom_jitter(alpha=0.4) + 
  scale_color_manual(values = c("ivory3", "red3", "red3", "steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) + 
  theme(axis.text.y = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust=1.5, face = "bold")) +
  geom_hline(yintercept=0.14, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5)

# AOV
N.aov <- aov(N ~ Condition, data = psc)
summary(N.aov)

# TUKEY test and letters to graph
N.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(N ~ Condition, data = psc))$Condition[,4])$Letters)
colnames(N.letters.df)[1] <- "Letter" #Reassign column name
N.letters.df$Condition <- rownames(N.letters.df) #Create column based on rownames
N.placement <- psc %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(N), sd=sd(N)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
N.letter.df2 <- left_join(N.letters.df, N.placement)

# facet label
psc$N.label <- "Percent Nitrogen"

# New plot with Tukey posthoc letters instead of p values
ggplot(psc, aes(x=factor(Condition, level = Condition.Order), y=N, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=N.letter.df2, aes(label=Letter, x=Condition, y = mean, size=12, vjust=-11)) +
  ylab("N%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(angle = 45, face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.69, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~N.label) +
  theme(strip.text = element_text(size=12, face="bold"))

ggsave(filename = "Nitrogen.PLot.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 3,
       height = 6,
       units = c("in"),
       dpi = 300)

### Phosphorus
# TUKEY test and letters to graph
P.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(P ~ Condition, data = psc))$Condition[,4])$Letters)
colnames(P.letters.df)[1] <- "Letter" #Reassign column name
P.letters.df$Condition <- rownames(P.letters.df) #Create column based on rownames
P.placement <- psc %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(P), sd=sd(P)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
P.letter.df2 <- left_join(P.letters.df, P.placement)

# facet label
psc$P.label <- "Percent Phosphorus"

# New plot with Tukey posthoc letters instead of p values
ggplot(psc, aes(x=factor(Condition, level = Condition.Order), y=P, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=P.letter.df2, aes(label=Letter, x=Condition, y = mean, size=12, vjust=-11)) +
  ylab("P%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(angle = 45, face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.12, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~P.label) +
  theme(strip.text = element_text(size=12, face="bold"))

ggsave(filename = "Phosphorus.PLot.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 3,
       height = 6,
       units = c("in"),
       dpi = 300)

### Potassium
# AOV
K.aov <- aov(K ~ Condition, data = psc)
summary(K.aov)
# TUKEY test and letters to graph
K.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(K ~ Condition, data = psc))$Condition[,4])$Letters)
colnames(K.letters.df)[1] <- "Letter" #Reassign column name
K.letters.df$Condition <- rownames(K.letters.df) #Create column based on rownames
K.placement <- psc %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(K), sd=sd(K)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
K.letter.df2 <- left_join(K.letters.df, K.placement)

# facet label
psc$K.label <- "Percent Potassium"

# New plot with Tukey posthoc letters instead of p values
ggplot(psc, aes(x=factor(Condition, level = Condition.Order), y=K, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=K.letter.df2, aes(label=Letter, x=Condition, y = mean, size=12, vjust=-11)) +
  ylab("K%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(angle = 45, face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.81, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~K.label) +
  theme(strip.text = element_text(size=12, face="bold"))

ggsave(filename = "Potassium.PLot.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 3,
       height = 6,
       units = c("in"),
       dpi = 300)

### Calculate shoot biomass from SH:RL and whole plant biomass

## use DF.1
# calc. shoot.height:root length ratio
DF.1$Shoot.Root <- DF.1$Shoot.Height / DF.1$Root.Length
# multiple this ratio by total plant biomass to get shoot biomass
DF.1$Shoot.Biomass <- (DF.1$Biomass * DF.1$Shoot.Root)

### now I need the average shoot biomass for each aggregate sample used for chem.

write.csv(DF.1, "MASTER.DF.8-21.csv")

chem <- read.csv("P.Chem2.csv") # I manually calculated shoot biomass for each aggregate shoot sample

## Multiply shoot biomass by nutrient N, P, K. These will be the new totals

chem$N.tot <- chem$N * chem$Shoot.Bio
chem$P.tot <- chem$P * chem$Shoot.Bio
chem$K.tot <- chem$K * chem$Shoot.Bio

#### NEW PLOTS WITH NEW DATA

# TUKEY test and letters to graph
N.tot.letters <- data.frame(multcompLetters(TukeyHSD(aov(N.tot ~ Treatment, data = chem))$Treatment[,4])$Letters)
colnames(N.tot.letters)[1] <- "Letter" #Reassign column name
N.tot.letters$Treatment <- rownames(N.tot.letters) #Create column based on rownames
N.placement2 <- chem %>% #We want to create a dataframe to assign the letter position.
  group_by(Treatment) %>%
  summarise(mean=mean(N.tot), sd=sd(N.tot)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
N.tot.letters2 <- left_join(N.tot.letters, N.placement2)

# facet label
chem$N.label <- "Percent Nitrogen"
Treatment.Order <- c("Control", "Burkholderia", "Burkholderia_HT", "Suillus", "SuillusxBurkholderia", "SuillusxBurkholderia.HT")


# New plot with Tukey posthoc letters instead of p values
ggplot(chem, aes(x=factor(Treatment, level = Treatment.Order), y=N.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=N.tot.letters2, aes(label=Letter, x=Treatment, y = 0.22, size=12)) +
  ylab("N%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "black","slategray", "steelblue", "red3")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~N.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  theme(strip.background = element_rect(fill="lightblue4"), strip.text = element_text(color="white")) + coord_flip()

ggsave(filename = "Nitrogen.PLot.NEW.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)


#### PHOSPHORUS

# TUKEY test and letters to graph
P.tot.letters <- data.frame(multcompLetters(TukeyHSD(aov(P.tot ~ Treatment, data = chem))$Treatment[,4])$Letters)
colnames(P.tot.letters)[1] <- "Letter" #Reassign column name
P.tot.letters$Treatment <- rownames(P.tot.letters) #Create column based on rownames
P.placement2 <- chem %>% #We want to create a dataframe to assign the letter position.
  group_by(Treatment) %>%
  summarise(mean=mean(P.tot), sd=sd(P.tot)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
P.tot.letters2 <- left_join(P.tot.letters, P.placement2)

# facet label
chem$P.label <- "Percent Phosphorus"
Treatment.Order <- c("Control", "Burkholderia", "Burkholderia_HT", "Suillus", "SuillusxBurkholderia", "SuillusxBurkholderia.HT")


# New plot with Tukey posthoc letters instead of p values
ggplot(chem, aes(x=factor(Treatment, level = Treatment.Order), y=P.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=P.tot.letters2, aes(label=Letter, x=Treatment, y = 0.04, size=12)) +
  ylab("P%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "black","slategray", "steelblue", "red3")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.008, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~P.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  theme(strip.background = element_rect(fill="maroon"), strip.text = element_text(color="white")) + coord_flip()


ggsave(filename = "Phosphorus.PLot.NEW.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)

# TUKEY test and letters to graph
K.tot.letters <- data.frame(multcompLetters(TukeyHSD(aov(K.tot ~ Treatment, data = chem))$Treatment[,4])$Letters)
colnames(K.tot.letters)[1] <- "Letter" #Reassign column name
K.tot.letters$Treatment <- rownames(K.tot.letters) #Create column based on rownames
K.placement2 <- chem %>% #We want to create a dataframe to assign the letter position.
  group_by(Treatment) %>%
  summarise(mean=mean(K.tot), sd=sd(K.tot)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
K.tot.letters2 <- left_join(K.tot.letters, K.placement2)

# facet label
chem$K.label <- "Percent Potassium"
Treatment.Order <- c("Control", "Burkholderia", "Burkholderia_HT", "Suillus", "SuillusxBurkholderia", "SuillusxBurkholderia.HT")


# New plot with Tukey posthoc letters instead of p values
ggplot(chem, aes(x=factor(Treatment, level = Treatment.Order), y=K.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=K.tot.letters2, aes(label=Letter, x=Treatment, y = 0.43, size=12)) +
  ylab("K%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text( face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "black","slategray", "steelblue", "red3")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.055, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~K.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  theme(strip.background = element_rect(fill="lightyellow4"), strip.text = element_text(color="white")) + coord_flip()


ggsave(filename = "Potassium.PLot.NEW.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)

### Linear model (need to add two new columns to test for interaction between bacteria and ECMF)

### First make the dataframe cloumns for the chemistry data

library(dplyr)
d.chemf <- chem %>%
  mutate(
    Bac.Inoc.Type = case_when(
      Treatment == "Burkholderia" ~ "Bacteria.Live",
      Treatment == "Burkholderia_HT" ~ "Bacteria.Dead",
      Treatment == "Suillus" ~ "None",
      Treatment == "SuillusxBurkholderia" ~ "Bacteria.Live",
      Treatment == "SuillusxBurkholderia.HT" ~ "Bacteria.Dead",
      Treatment == "Control" ~ "None",
      TRUE ~ NA_character_
    ),
    EcMF.Inoc.Type = case_when(
      Treatment == "Burkholderia" ~ "None",
      Treatment == "Burkholderia_HT" ~ "None",
      Treatment == "Suillus" ~ "EcMF",
      Treatment == "SuillusxBurkholderia" ~ "EcMF",
      Treatment == "SuillusxBurkholderia.HT" ~ "EcMF",
      Treatment == "Control" ~ "None",
      TRUE ~ NA_character_
    )
  )

#### Run linear models / aovs for N, P, and K
## NITROGEN
shoot.N <- aov(N.tot ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=d.chemf)
summary(shoot.N)
TukeyHSD(shoot.N)

### Phosphorus
shoot.P <- aov(P.tot ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=d.chemf)
summary(shoot.P)
TukeyHSD(shoot.P)

### Potassium
shoot.K <- aov(K.tot ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=d.chemf)
summary(shoot.K)
TukeyHSD(shoot.K)


# I might as well check for root biomass changes now that I have the shoot biomass (biomass * S:R length)

DF.1$Root.Biomass <- DF.1$Biomass - DF.1$Shoot.Biomass

# TUKEY test and letters to graph
RB.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Root.Biomass ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(RB.letters.df)[1] <- "Letter" #Reassign column name
RB.letters.df$Condition <- rownames(RB.letters.df) #Create column based on rownames
RB.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Root.Biomass), sd=sd(Root.Biomass)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
RB.letter.df2 <- left_join(RB.letters.df, RB.placement)

DF.1$RSB <- "Root Biomass"
# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Root.Biomass, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=RB.letter.df2, aes(label=Letter, x=Condition, y = 0.5, size=12)) +
  ylab("Root Biomass (g)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.103, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~RSB) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  coord_flip()

ggsave(filename = "Root.Biomass.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

### Linear Model
root.biomass.lm <- aov(Root.Biomass ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=DF.1)
summary(root.biomass.lm)

#### SHOOT BIOMASS ####

# TUKEY test and letters to graph
SB.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Shoot.Biomass ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(SB.letters.df)[1] <- "Letter" #Reassign column name
SB.letters.df$Condition <- rownames(SB.letters.df) #Create column based on rownames
SB.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(Shoot.Biomass), sd=sd(Shoot.Biomass)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
SB.letter.df2 <- left_join(SB.letters.df, SB.placement)
### Tukey letters seem strange --> change in excel and import back in
write.csv(SB.letter.df2, "SH.Biomass.Tukey.csv")
SB.letter.df2 <- read.csv("SH.Biomass.Tukey.csv") ## added new column with clearer lettering (Letter2)

DF.1$SB <- "Shoot Biomass"
# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=Shoot.Biomass, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=SB.letter.df2, aes(label=Letter2, x=Condition, y = 0.29, size=4, vjust=1.3)) +
  ylab("Shoot Biomass (g)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.035, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~SB) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) + 
  coord_flip()

ggsave(filename = "Shoot.Biomass.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

### Linear Model
shoot.biomass.lm <- aov(Shoot.Biomass ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=DF.1)
summary(shoot.biomass.lm)
#### NOW Root:Shoot BIOMASS RATIO COMPS

DF.1$RS.Biomass <- DF.1$Root.Biomass / DF.1$Shoot.Biomass

# TUKEY test and letters to graph
RSB.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(RS.Biomass ~ Condition, data = DF.1))$Condition[,4])$Letters)
colnames(RSB.letters.df)[1] <- "Letter" #Reassign column name
RSB.letters.df$Condition <- rownames(RSB.letters.df) #Create column based on rownames
RSB.placement <- DF.1 %>% #We want to create a dataframe to assign the letter position.
  group_by(Condition) %>%
  summarise(mean=mean(RS.Biomass), sd=sd(RS.Biomass)) %>%
  arrange(desc(mean))
# merge the letters and placement dataframe for plotting
RSB.letter.df2 <- left_join(RSB.letters.df, RSB.placement)

DF.1$RSB2 <- "Root:Shoot Biomass Ratio"
# New plot with Tukey posthoc letters instead of p values
ggplot(DF.1, aes(x=factor(Condition, level = Condition.Order), y=RS.Biomass, color=Condition)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=SB.letter.df2, aes(label=Letter, x=Condition, y = 4.24, size=12)) +
  ylab("Root:Shoot Biomass (g)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "red3", "red3","steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=2.32, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~RSB2) +
  theme(strip.text = element_text(size=11, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia", "Paraburkholderia (HT)",
                            "Suillus", "Paraburkholderia + Suillus", "Paraburkholderia + Suillus (HT)")) +
  coord_flip()

ggsave(filename = "RS.Biomass.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

### Linear Model
rs.biomass.lm <- aov(RS.Biomass ~ Bac.Inoc.Type*EcMF.Inoc.Type, data=DF.1)
summary(rs.biomass.lm)

##### Three Year Old Bishop Pine Data #####

tyo <- read.csv("Data.Raw.3Y.csv")

### Colonization
# TUKEY test and letters to graph
MC3.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Perc.Colon ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(MC3.letters.df)[1] <- "Letter" #Reassign column name
MC3.letters.df$Treatment <- rownames(MC3.letters.df) #Create column based on rownames
colnames(MC3.letters.df)[1] <- "Letter"
MC3.letters.df$Treatment <- row.names(MC3.letters.df)
tyo$facet1 <- "Mycorrhizal Colonization"
# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=Treatment, y=Myco.Colon, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=MC3.letters.df, aes(label=Letter, x=Treatment, y = 70, size=12)) +
  ylab("Mycorrhizal Colonization (%)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("black", "ivory3", "slategray", "steelblue")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~facet1) +
  theme(strip.text = element_text(size=11, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia","EcMF", "Paraburkholderia + EcMF")) +
  coord_flip()

ggsave(filename = "Colonization.ThreeYear.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)

#### BIOMASS
# TUKEY test and letters to graph
BIO3.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Biomass ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(BIO3.letters.df)[1] <- "Letter" #Reassign column name
BIO3.letters.df$Treatment <- rownames(BIO3.letters.df) #Create column based on rownames
colnames(BIO3.letters.df)[1] <- "Letter"
BIO3.letters.df$Treatment <- row.names(BIO3.letters.df)
tyo$facet2 <- "Biomass (g)"
# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=Treatment, y=Biomass, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=BIO3.letters.df, aes(label=Letter, x=Treatment, y = 5, size=12)) +
  ylab("Biomass (g)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("ivory3", "steelblue", "black", "slategray")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=2.32, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~facet2) +
  theme(strip.text = element_text(size=11, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia","Suillus", "Paraburkholderia + Suillus")) +
  coord_flip()

#### Shoot Height
# TUKEY test and letters to graph
SH3.letters.df <- data.frame(multcompLetters(TukeyHSD(aov(Shoot.Height ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(SH3.letters.df)[1] <- "Letter" #Reassign column name
SH3.letters.df$Treatment <- rownames(BIO3.letters.df) #Create column based on rownames
colnames(SH3.letters.df)[1] <- "Letter"
SH3.letters.df$Treatment <- row.names(SH3.letters.df)
tyo$facet3 <- "Shoot Height (cm)"
# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=Treatment, y=Shoot.Height, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=SH3.letters.df, aes(label=Letter, x=Treatment, y = 15, size=12)) +
  ylab("Shoot Height (cm)") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("black", "ivory3", "slategray", "steelblue")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=1.32, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~facet3) +
  theme(strip.text = element_text(size=11, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia","Suillus", "Paraburkholderia + Suillus")) +
  coord_flip()

ggsave(filename = "ShootHeight.ThreeYear.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 3,
       units = c("in"),
       dpi = 300)


## Multiply shoot biomass by nutrient N, P, K. These will be the new totals

tyo$N.tot <- tyo$Perc.N * tyo$Shoot.Height
tyo$P.tot <- tyo$Perc.P * tyo$Shoot.Height
tyo$K.tot <- tyo$Perc.K * tyo$Shoot.Height

#### NEW PLOTS WITH NEW DATA

# TUKEY test and letters to graph
N.tot.letters3 <- data.frame(multcompLetters(TukeyHSD(aov(N.tot ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(N.tot.letters3)[1] <- "Letter" #Reassign column name
N.tot.letters3$Treatment <- rownames(N.tot.letters3) #Create column based on rownames


# facet label
tyo$N.label <- "Percent Nitrogen"
Treatment.Order <- c("Control", "PB", "Suillus", "Suillus+PB")


# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=factor(Treatment, level = Treatment.Order), y=N.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=N.tot.letters3, aes(label=Letter, x=Treatment, y = 11, size=12)) +
  ylab("N%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("black","ivory3","slategray", "steelblue")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~N.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia",
                            "Suillus", "Paraburkholderia + Suillus")) +
  theme(strip.background = element_rect(fill="lightblue4"), strip.text = element_text(color="white")) + coord_flip()

ggsave(filename = "Nitrogen.PLot.TYO.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)


#### PHOSPHORUS

# TUKEY test and letters to graph
P.tot.letters <- data.frame(multcompLetters(TukeyHSD(aov(P.tot ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(P.tot.letters)[1] <- "Letter" #Reassign column name
P.tot.letters$Treatment <- rownames(P.tot.letters) #Create column based on rownames


# facet label
tyo$P.label <- "Percent Phosphorus"

# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=factor(Treatment, level = Treatment.Order), y=P.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=P.tot.letters, aes(label=Letter, x=Treatment, y = 1.5, size=12)) +
  ylab("P%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text(face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("black", "ivory3", "slategray", "steelblue")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.008, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~P.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia",
                            "Suillus", "Paraburkholderia + Suillus")) +
  theme(strip.background = element_rect(fill="maroon"), strip.text = element_text(color="white")) + coord_flip()


ggsave(filename = "Phosphorus.PLot.TYO.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)

# TUKEY test and letters to graph
K.tot.letters <- data.frame(multcompLetters(TukeyHSD(aov(K.tot ~ Treatment, data = tyo))$Treatment[,4])$Letters)
colnames(K.tot.letters)[1] <- "Letter" #Reassign column name
K.tot.letters$Treatment <- rownames(K.tot.letters) #Create column based on rownames

write.csv(K.tot.letters, "K.ANOVA.Letters.csv")
K.tot.letters <- read.csv("K.ANOVA.Letters.csv")

# facet label
tyo$K.label <- "Percent Potassium"

# New plot with Tukey posthoc letters instead of p values
ggplot(tyo, aes(x=factor(Treatment, level = Treatment.Order), y=K.tot, color=Treatment)) + 
  geom_boxplot() + theme_bw() + xlab("") + geom_text(data=K.tot.letters, aes(label=Letter, x=Treatment, y = 10.5, size=12)) +
  ylab("K%") + geom_jitter(alpha=0.4) +  theme(axis.text.x = element_text( face="bold", size=10, hjust = 1)) +
  scale_color_manual(values = c("black", "ivory3", "slategray", "steelblue")) + 
  theme(legend.position="none") + theme(axis.text.y = element_text(size=12, face="bold")) + 
  theme(axis.text.x = element_text(size = 10)) + theme(axis.title.y = element_text(size = 12, vjust = 2.5)) +
  geom_hline(yintercept=0.055, linetype="dashed", 
             color = "black", size=0.5, alpha=0.5) + facet_grid(~K.label) +
  theme(strip.text = element_text(size=12, face="bold")) +
  scale_x_discrete(labels=c("Control", "Paraburkholderia",
                            "Suillus", "Paraburkholderia + Suillus")) +
  theme(strip.background = element_rect(fill="lightyellow4"), strip.text = element_text(color="white")) + coord_flip()


ggsave(filename = "Potassium.PLot.TYO.png",
       plot = last_plot(),
       device = NULL,
       path = NULL,
       scale = 1,
       width = 5,
       height = 5.5,
       units = c("in"),
       dpi = 300)
