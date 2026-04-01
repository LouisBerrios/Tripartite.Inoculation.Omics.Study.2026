#### Wrangling PATRIC Database Output Information ####



### Comparative Analysis Output ###

### Simple plot to compare number of genes per pathway class among genomes

library(dplyr);library(stringr);library(ggplot2)

#### REVISION #### -- > using data from 16 genomes (instead of 3) Date:10.2025
### Filename: Berrios2025NewPhyt_subsystems.csv


comp <- read.csv("Berrios2025NewPhyt_subsystems.csv")
View(comp)

# Add new column with numeric value for each entery (i.e., each gene) for quantitative plotting

comp$count <- 1

# Aggregate 'count' by genome_name and class
agg.comp <- aggregate(count ~ class + genome_name, FUN = sum, data=comp)

# relativize 

agg.comp_rel <- agg.comp %>%
  group_by(genome_name) %>%
  mutate(rel_abund = count / sum(count)) %>%
  ungroup()

###
ggplot(agg.comp_rel, aes(x=genome_name, y=rel_abund, fill=class)) + 
  geom_bar(stat = "identity", color="black") + theme_bw() +
  scale_fill_manual(values = c(
    "#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e",
    "#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#a6dba0", "#1b7837",
    "#b35806", "#f1a340", "#fee0b6", "#d8daeb", "azure1", "#8073ac",
    "#e08214", "#fdb863", "#fddbc7", "dodgerblue4", "red3", "darkslategray4",
    "darkgoldenrod3", "black", "gray", "#004529"
  )) + coord_flip() + 
   theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) + scale_x_discrete(labels = c(
    "Bacillus 1A" = "Bacillus sp. 1A",
    "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
    "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A", 
    "Pseudomonas I6B" = "Pseudomonas sp. I6B", 
    "Pseudomonas 13A" = "Pseudomonas sp. 13A", 
    "Pseudomonas 12B" = "Pseudomonas sp. 12B", 
    "Burkholderiales A2A" = "Paraburkholderia sp. A2A", 
    "Burkholderiales B3A" = "Paraburkholderia sp. B3A", 
    "Burkholderia C8A.2" = "Paraburkholderia sp. C8A", 
    "Burkholderia C5C" = "Paraburkholderia sp. C5C", 
    "Bacillus 14B2.2" = "Bacillus sp. 14B2",
    "Paraburkholderia C4B" = "Paraburkholderia sp. C4B",
    "Paraburkholderia E1A" = "Paraburkholderia sp. E1A"
  )) + xlab("") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("") + theme(axis.text.y = element_text(face = "bold.italic"))

# map: raw -> pretty
lab_map <- c(
  "Bacillus 1A" = "Bacillus sp. 1A",
  "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
  "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A", 
  "Pseudomonas I6B" = "Pseudomonas sp. I6B", 
  "Pseudomonas 13A" = "Pseudomonas sp. 13A", 
  "Pseudomonas 12B" = "Pseudomonas sp. 12B", 
  "Burkholderiales A2A" = "Paraburkholderia sp. A2A", 
  "Burkholderiales B3A" = "Paraburkholderia sp. B3A", 
  "Burkholderia C8A.2" = "Paraburkholderia sp. C8A", 
  "Burkholderia C5C" = "Paraburkholderia sp. C5C", 
  "Paraburkholderia H4B" = "Paraburkholderia sp. H4B", 
  "Paraburkholderia E9D" = "Paraburkholderia sp. E9D", 
  "Bacillus 14B2.2" = "Bacillus sp. 14B2",
  "Paraburkholderia C4B" = "Paraburkholderia sp. C4B",
  "Paraburkholderia E1A" = "Paraburkholderia sp. E1A",
  "Microbacterium B6AC" = "Microbacterium sp. B6AC"
)

# order raw values by their pretty labels (A→Z)
ord_levels <- names(sort(lab_map, na.last = TRUE))

### Updated plot
library(ggalluvial)
ggplot(agg.comp,
       aes(x = genome_name,
           stratum = class,
           alluvium = class,
           y = count,
           fill = class)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward") +
  geom_stratum(width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("burlywood4", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e",
                               "#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "wheat3", "steelblue",
                               "#b35806", "#f1a340", "#fee0b6", "#d8daeb", "azure1", "#8073ac",
                               "#e08214", "#fdb863", "#fddbc7", "dodgerblue4", "red3", "darkslategray4",
                               "darkgoldenrod3", "black", "gray", "#004529")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Relative Abundance of Pathway Classes", x = NULL, fill = "Gene Class") + 
 coord_flip() +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
      theme(axis.text.y = element_text(face = "bold.italic", size=12)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10, face="bold"),
    axis.title = element_text(size = 11)
  ) + guides(fill = guide_legend(nrow = 7, byrow = TRUE))

ggsave("Class.Comp.Plot.REVISED.short.png",
        plot=last_plot(),
        height= 6,
        width = 5,
        units="in",
        dpi = 300)

#### all on a heatmap

ggplot(agg.comp_rel, aes(x = genome_name, y =reorder(class,count), size = count)) +
  geom_point() +
  scale_fill_manual(values= c("grey", "blue", "red")) +
  theme_bw() + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16, color="white"), 
        strip.background = element_rect(fill="black"))

agg.comp_rel$count_bin <- cut(
  agg.comp_rel$count,
  breaks = c(-Inf, 100, 200, 300, Inf),
  labels = c("<100", "100–200", "200–300", "300+")
)
### plot
ggplot(
  agg.comp_rel,
  aes(
    x = genome_name,
    y = reorder(class, count),
    size = count,
    color = count_bin
  )
) +
  geom_point(alpha=0.7) +
  scale_color_manual(values = c(
    "<100" = "grey",
    "100–200" = "steelblue",
    "200–300" = "darkgoldenrod4",
    "300+" = "red"
  )) +
  scale_size(range = c(1, 4)) +
  guides(size = "none") +   # ✅ removes size legend
  theme_bw() +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11)) +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face = "bold.italic", size = 9)) +
  theme(
    strip.text = element_text(face = "bold", size = 16, color = "white"),
    strip.background = element_rect(fill = "black")
  ) + labs(color = "Gene Product Number")

ggsave("Dot.Raw.Class.png",
       plot = last_plot(),
       height=7.8,
       width=8.2,
       units="in",
       dpi=300)

 #### Secondary Metabolism ####

sm.comp <- subset(comp, class == "Secondary Metabolism")
sm.comp$label <- "Secondary Metabolism"
as.numeric(sm.comp$count)
ggplot(sm.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16, color="white"), 
        strip.background = element_rect(fill="black"))

ggsave("Secondary.Metabolites.REVISED.png",
       plot = last_plot(),
       height=3,
       width=7.2,
       units="in",
       dpi=300)

### Aminos, carbos, cofactors, energy, stress response, nucleotides
comp2 <- comp[comp$class %in% c(
                            "Phosphate Metabolism",
                                "Carbohydrates",
                                "Secondary Metabolism",
                                "Nitrogen Metabolism"), ]


ggplot(comp2, aes(x = genome_name, y = reorder(subsystem_name,class), fill = count, color=class)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=8)) +
  theme(axis.text.y = element_text(size=7)) + theme(legend.position = "none")

ggsave("PCNsm.png",
       plot = last_plot(),
       height=7,
       width=5.2,
       units="in",
       dpi=300)

### plot
ggplot(
  comp2,
  aes(
    x = genome_name,
    y = reorder(role_name, count),
    size = count,
    color = class
  )
) +
  geom_point(alpha=0.7) +
  scale_color_manual(values = c(
    "Phosphate Metabolism" = "purple",
    "Carbohydrates" = "darkgoldenrod3",
    "Secondary Metabolism" = "black",
    "Nitrogen Metabolism" = "blue")) +
  scale_size(range = c(1, 4)) +
  guides(size = "none") + theme_bw() +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11)) +
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face = "bold.italic", size = 9)) +
  theme(
    strip.text = element_text(face = "bold", size = 16, color = "white"),
    strip.background = element_rect(fill = "black")
  ) + labs(color = "Gene Product Number")

ggsave("Dot.Raw.Class.Subset.png",
       plot = last_plot(),
       height=7.8,
       width=8.2,
       units="in",
       dpi=300)

### Phosphate utl
comp.p <- subset(comp, class == "Phosphate Metabolism")
comp.p$label <- "Phosphate Metabolism"
ggplot(comp.p, aes(x = genome_name, y = role_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16), strip.background = element_rect(fill = "#d8b365")) 

ggsave("Carbohydrates.png",
       plot = last_plot(),
       height=4.2,
       width=7,
       units="in",
       dpi=300)

#### Carbohydrates ####

carb.comp <- subset(comp, class == "Carbohydrates")
carb.comp$label <- "Carbohydrates"
write.csv(carb.comp, "Carbohydrate.Genes.csv")


ggplot(carb.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16), strip.background = element_rect(fill = "#d8b365")) 

ggsave("Carbohydrates.png",
       plot = last_plot(),
       height=4.2,
       width=7,
       units="in",
       dpi=300)

### REVISED FOR TALK 

carb.comp2 <- subset(carb.comp, subsystem_name == "Ethanolamine utilization" | 
                       subsystem_name == "D-threonate, L-threonate and D-erythronate utilization" |
                       subsystem_name == "D-arabinitol utilization")
write.csv(carb.comp2, "Carbohydrate.Genes.MODIFIED.csv")
carb.comp2 <- read.csv("Carbohydrate.Genes.MODIFIED.csv")

ggplot(carb.comp2, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16), strip.background = element_rect(fill = "#d8b365")) 

ggsave("Carbohydrates.REVISED.TALK.png",
       plot = last_plot(),
       height=4.2,
       width=7,
       units="in",
       dpi=300)


#### Microbial Communities ####

MC.comp <- subset(comp, class == "Microbial communities")
MC.comp$label <- "Microbial Communities"

ggplot(MC.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c(
    "Bacillus 1A" = "Bacillus sp. 1A",
    "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
    "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A"
  )) + theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=5))

#### Stress ####

stress.comp <- subset(comp, class == "Stress Response, Defense and Virulence")
stress.comp$label <- "Stress Response, Defense and Virulence"

ggplot(stress.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c(
    "Bacillus 1A" = "Bacillus sp. 1A",
    "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
    "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A"
  )) + theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=5))

#### Amino Acids ####

amino.comp <- subset(comp, class == "Amino Acids and Derivatives")
amino.comp$label <- "Amino Acids and Derivatives"

ggplot(amino.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgoldenrod3") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") + scale_x_discrete(labels = c(
    "Bacillus 1A" = "Bacillus sp. 1A",
    "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
    "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A"
  )) + theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=5))

ggsave("x.png",
       plot = last_plot(),
       height=4,
       width=5,
       units="in",
       dpi=300)

#### Nitrogen Metabolism ####

nitro.comp <- subset(comp, class == "Nitrogen Metabolism")
nitro.comp$label <- "Nitrogen Metabolism"

ggplot(nitro.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") +   scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16), strip.background = element_rect(fill="#d8daeb"))

ggsave("NitrogenMet.REVISED.png",
       plot = last_plot(),
       height=3,
       width=7,
       units="in",
       dpi=300)

### Energy and precursor metabolite
energy.comp <- subset(comp, class == "Energy and Precursor Metabolites Generation")
energy.comp$label <- "Energy and Precursor Metabolites Generation"

ggplot(energy.comp, aes(x = genome_name, y = subsystem_name, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none") +   scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16), strip.background = element_rect(fill="#d8daeb"))

ggsave("Energy.REVISED.png",
       plot = last_plot(),
       height=3,
       width=7,
       units="in",
       dpi=300)

### Manually looked for plant-related genes in genome (ARTEMIS)
### REvised for the other 13 genomes
PA.genes <- read.csv("PlantAssoc.csv")
PA.genes$label <- "Interaction-Linked Genes"

ggplot(PA.genes, aes(x = Genome, y = Product, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16, color="white"), strip.background = element_rect(fill="steelblue"))

ggsave("PlantAssoc.Revised.png",
       plot = last_plot(),
       height=3,
       width=7,
       units="in",
       dpi=300)

### Interaction-Linked Genes (ADJUSTED FOR PRESENTATION)
PA.genes2 <- read.csv("PlantAssoc2.csv")
PA.genes2 <- subset(PA.genes2, Product != "Trehalase")
PA.genes2$label <- "Interaction-Linked Genes"


ggplot(PA.genes2, aes(x = Genome, y = Product, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + facet_grid(~label) + theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(legend.position = "none")  +
  theme(axis.text.x = element_text(face="bold.italic", size=10)) +
  theme(strip.text = element_text(face="bold", size=16, color="white"), strip.background = element_rect(fill="steelblue"))

ggsave("PlantAssoc.TALK.PRES.png",
       plot = last_plot(),
       height=3,
       width=7,
       units="in",
       dpi=300)

### Instead of going through each of the 28 classes 1-by-1, I'm going to find
# the unqiue products for Paraburkholderia and plot those. 

library(dplyr)

# Assuming your data frame is named `df`
unique_burk <- comp %>%
  filter(genome_name == "Paraburkholderia D1E") %>%
  select(class, product) %>%
  distinct()

unique_burk$count <- 1
unique_burk$genome_name <- "Paraburkholderia sp. D1E"

# relativize 

burk.agg.comp_rel <- unique_burk %>%
  mutate(rel_abund = count / sum(count)) %>%
  ungroup()

ggplot(burk.agg.comp_rel, aes(x=genome_name, y=rel_abund, fill=class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("burlywood4", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e",
                               "#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "wheat3", "steelblue",
                                "#f1a340", "#fee0b6", "#d8daeb", "azure1", "#8073ac",
                               "#e08214", "#fdb863", "#fddbc7", "dodgerblue4", "red3", "darkslategray4",
                               "darkgoldenrod3", "black", "gray", "#004529")) + coord_flip() + 
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Relative Abundance of Unique Gene Products", x = NULL, fill = "Gene Class") + 
  theme(axis.text.y = element_text(face = "bold.italic", size=12)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10, face="bold"),
    axis.title = element_text(size = 11)
  ) + guides(fill = guide_legend(nrow = 7, byrow = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  


### MAUVE Alignment ORTHOS Output ###

library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)


### Burkholderia and Pseudomonas
# Load file
burk.pseudo.df <- read.csv("BurkD1E.Pseudo32A.ORTHOS.csv")

# Separate the Burkholderia coordinates
burk.pseudo.df2 <- burk.pseudo.df %>%
  separate(Burk.Raw, into = c(NA, "Burk"), sep = "::") %>%
  separate(Burk, into = c("Burk.Start", "Burk.End"), sep = "-")

# Separate the Pseudomonas coordinates
burk.pseudo.df3 <- burk.pseudo.df2 %>%
  separate(Pseudo.Raw, into = c(NA, "Pseudo"), sep = "::") %>%
  separate(Pseudo, into = c("Pseudo.Start", "Pseudo.End"), sep = "-")


write.csv(burk.pseudo.df3, "D1E.32A.ORTHOS.CircaReady.csv")

### Bacillus and Pseudomonas
bac.pseudo.df <- read.csv("Bacillus.Pseudo.ORTHOS2.csv")

# Separate the Bacillus coordinates
bac.pseudo.df2 <- bac.pseudo.df %>%
  separate(Bacillus, into = c(NA, "Bacillus"), sep = "::") %>%
  separate(Bacillus, into = c("Bac.Start", "Bac.End"), sep = "-")

# Separate the Pseudomonas coordinates
bac.pseudo.df3 <- bac.pseudo.df2 %>%
  separate(Pseudomonas, into = c(NA, "Pseudomonas"), sep = "::") %>%
  separate(Pseudomonas, into = c("Pseudo.Start", "Pseudo.End"), sep = "-")


write.csv(bac.pseudo.df3, "Bacillus1a.32A.ORTHOS.CircaReady.csv")


#########################

library(circlize)
library(dplyr)

# Ensure numeric
df <- df.met %>% mutate(across(everything(), as.numeric))

# Define genomes
genome_order <- c("Bacillus", "Burk", "Pseudo")

# Get max range per genome
genome_ranges <- data.frame(
  genome = genome_order,
  max_end = c(
    max(c(df$Bacillus.End, df$Bacillus.Start), na.rm = TRUE),
    max(c(df$Burk.End, df$Burk.Start), na.rm = TRUE),
    max(c(df$Pseudo.End, df$Pseudo.Start), na.rm = TRUE)
  )
)

# --- Circos layout setup ---
circos.clear()
circos.par(
  cell.padding = c(0, 0, 0, 0),
  track.margin = c(0.005, 0.005),
  gap.after = c(5, 5, 10),  # space between genomes
  start.degree = 90
)

# Initialize sectors
circos.initialize(factors = genome_ranges$genome,
                  xlim = matrix(c(rep(0, 3), genome_ranges$max_end), ncol = 2))

# --- 1. Draw axis track with tick marks in Mbp ---
circos.track(ylim = c(0, 1), track.height = 0.01, panel.fun = function(x, y) {
  sector <- CELL_META$sector.index
  xlim <- CELL_META$xlim
  ticks.at <- seq(0, xlim[2], by = 1e6)
  labels <- paste0(ticks.at / 1e6, " Mbp")
  circos.axis(
    major.at = ticks.at,
    labels = labels,
    labels.cex = 0.5,
    direction = "outside"
  )
}, bg.border = NA)

# --- 2. Dummy track to bring links closer to axis ---
circos.track(ylim = c(0, 1), track.height = 0.01, bg.border = NA)

# --- 3. Draw links: 3-way = bold; 2-way = thinner ---
for (i in 1:nrow(df)) {
  row <- df[i, ]
  
  has_pseudo <- !any(is.na(row[c("Pseudo.Start", "Pseudo.End")]))
  has_burk   <- !any(is.na(row[c("Burk.Start", "Burk.End")]))
  has_bac    <- !any(is.na(row[c("Bacillus.Start", "Bacillus.End")]))
  
  # 3-genome link
  if (has_pseudo & has_burk & has_bac) {
    circos.link("Pseudo",  c(row$Pseudo.Start, row$Pseudo.End),
                "Burk",    c(row$Burk.Start,   row$Burk.End),
                col = "black", border = NA, lwd = 100)
    circos.link("Pseudo",  c(row$Pseudo.Start, row$Pseudo.End),
                "Bacillus",c(row$Bacillus.Start, row$Bacillus.End),
                col = "black", border = NA, lwd = 100)
    circos.link("Burk",    c(row$Burk.Start,   row$Burk.End),
                "Bacillus",c(row$Bacillus.Start, row$Bacillus.End),
                col = "black", border = NA, lwd = 100)
  } else {
    # 2-genome links
    if (has_pseudo & has_burk) {
      circos.link("Pseudo", c(row$Pseudo.Start, row$Pseudo.End),
                  "Burk",   c(row$Burk.Start,   row$Burk.End),
                  col = "darkslategray4", border = NA, lwd = 10)
    }
    if (has_pseudo & has_bac) {
      circos.link("Pseudo", c(row$Pseudo.Start, row$Pseudo.End),
                  "Bacillus", c(row$Bacillus.Start, row$Bacillus.End),
                  col = "#d95f02", border = NA, lwd = 1)
    }
    if (has_burk & has_bac) {
      circos.link("Burk", c(row$Burk.Start, row$Burk.End),
                  "Bacillus", c(row$Bacillus.Start, row$Bacillus.End),
                  col = "gray", border = NA, lwd = 1)
    }
  }
}

png("circlize_plot.png", width = 2000, height = 2000, res = 300, plot=last_plot())
circos.clear()
# your plot code
dev.off()


library(circlize)
library(dplyr)

# Prepare data: convert all to numeric if not already
df <- df.met %>% mutate(across(everything(), as.numeric))

# Define genome order
genome_order <- c("Bacillus", "Burk", "Pseudo")

# Compute max genomic range for each genome
genome_ranges <- data.frame(
  genome = genome_order,
  max_end = c(
    max(c(df$Bacillus.End, df$Bacillus.Start), na.rm = TRUE),
    max(c(df$Burk.End, df$Burk.Start), na.rm = TRUE),
    max(c(df$Pseudo.End, df$Pseudo.Start), na.rm = TRUE)
  )
)

# Start PNG device
png("circlize_plot.png", width = 2000, height = 2000, res = 300)

# Circos setup
circos.clear()
circos.par(
  cell.padding = c(0, 0, 0, 0),
  track.margin = c(0.005, 0.005),
  gap.after = c(5, 5, 10),  # space between genomes
  start.degree = 90
)

# Initialize sectors for each genome
circos.initialize(
  factors = genome_ranges$genome,
  xlim = matrix(c(rep(0, 3), genome_ranges$max_end), ncol = 2)
)

# Axis track (tick marks every 1 Mbp)
circos.track(
  ylim = c(0, 1), track.height = 0.01, bg.border = NA,
  panel.fun = function(x, y) {
    sector <- CELL_META$sector.index
    xlim <- CELL_META$xlim
    ticks.at <- seq(0, xlim[2], by = 1e6)
    labels <- paste0(ticks.at / 1e6, " Mbp")
    circos.axis(
      major.at = ticks.at,
      labels = labels,
      labels.cex = 0.5,
      direction = "outside"
    )
  }
)

# Dummy track to place links closer to axis
circos.track(ylim = c(0, 1), track.height = 0.01, bg.border = NA)

# Draw links
for (i in 1:nrow(df)) {
  row <- df[i, ]
  
  has_pseudo <- !any(is.na(row[c("Pseudo.Start", "Pseudo.End")]))
  has_burk   <- !any(is.na(row[c("Burk.Start", "Burk.End")]))
  has_bac    <- !any(is.na(row[c("Bacillus.Start", "Bacillus.End")]))
  
  # 3-genome connection (bold)
  if (has_pseudo & has_burk & has_bac) {
    circos.link("Pseudo",  c(row$Pseudo.Start, row$Pseudo.End),
                "Burk",    c(row$Burk.Start,   row$Burk.End),
                 lwd = 1.5,
                col = adjustcolor("lightgray", alpha.f = 1))
    circos.link("Pseudo",  c(row$Pseudo.Start, row$Pseudo.End),
                "Bacillus",c(row$Bacillus.Start, row$Bacillus.End),
                lwd = 1.5,
                col = adjustcolor("lightgray", alpha.f = 1))
    circos.link("Burk",    c(row$Burk.Start,   row$Burk.End),
                "Bacillus",c(row$Bacillus.Start, row$Bacillus.End),
              lwd = 1.5,
                col = adjustcolor("lightgray", alpha.f = 1))
    
  } else {
    # 2-genome connections (thinner)
    if (has_pseudo & has_burk) {
      circos.link("Pseudo", c(row$Pseudo.Start, row$Pseudo.End),
                  "Burk",   c(row$Burk.Start,   row$Burk.End),
                  lwd = 1,
                  col = adjustcolor("lightgray", alpha.f = 0.2))
    }
    if (has_pseudo & has_bac) {
      circos.link("Pseudo", c(row$Pseudo.Start, row$Pseudo.End),
                  "Bacillus", c(row$Bacillus.Start, row$Bacillus.End),
                   lwd = 1,
                  col = adjustcolor("lightgray", alpha.f = 0.2))
    }
    if (has_burk & has_bac) {
      circos.link("Burk", c(row$Burk.Start, row$Burk.End),
                  "Bacillus", c(row$Bacillus.Start, row$Bacillus.End),
                  lwd = 1,
                  col = adjustcolor("lightgray", alpha.f = 0.2))
    }
  }
}

# Finish and save the PNG
dev.off()

