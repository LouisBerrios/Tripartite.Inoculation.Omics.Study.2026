### CAZyme + Genome Analysis ###
# Berrios et al., 2026, New Phytologist #

# import CAZyme data for each genome #

b.cereusP8 <- read.csv("b.cereusP9.csv")
b.subM11 <- read.csv("b.subtilisM11.csv")
b1A <- read.csv("bacills1A.csv")
b14B2 <- read.csv("bacillus14B2.csv")
brady110 <- read.csv("bradyUSDA110.csv")
brady349 <- read.csv("bradyUSDA349.csv")
brady352 <- read.csv("bradyUSDA352.csv")
burk79b2 <- read.csv("burk79b2.csv")
microB6AC <- read.csv("microB6AC.csv")
microBE180 <- read.csv("microBE180.csv")
microENV77 <- read.csv("microENV77.csv")
microNFIX05 <- read.csv("microNFIX05.csv")
mycoVKM <- read.csv("mycoVKM.Ac-1816-D.csv")
mycoYR708 <- read.csv("mycoYR708.csv")
parabA2A <- read.csv("parabA2A.csv")
parabB3A <- read.csv("parabB3A.csv")
parabC4B <- read.csv("parabC4B.csv")
parabC5C <- read.csv("parabC5C.csv")
parabC8A <- read.csv("parabC8A.csv")
parabD1E <- read.csv("parabD1E.csv")
parabE1A <- read.csv("parabE1A.csv")
parabE9D <- read.csv("parabE9D.csv")
parabHK1456 <- read.csv("parabendoHKI456.csv")
parabH4B <- read.csv("parabH4B.csv")
pseudo12B <- read.csv("pseudo12B.csv")
pseudo13A <- read.csv("pseudo13A.csv")
pseudo32A <- read.csv("pseudo32A.csv")
pseudo317_b1 <- read.csv("pseudo317_b1.csv")
pseudoBBc6R8 <- read.csv("pseudoBBc6R8.csv")
pseudoI6B <- read.csv("pseudoI6B.csv")
strept.505 <- read.csv("strepAcH505.csv")
sphingo.BE319 <- read.csv("sphingomonasBE319.csv")
sphingoDM2R <- read.csv("sphingoDM2-R-LB4.csv")
pginger <- read.csv("pseudo.ginger.NCPPB3146.csv")


### Merge Dfs 

library(dplyr)
library(purrr)

# 1. Place dataframes in a list
df_list <- list(b.cereusP8,
                b.subM11,
                b1A,
                b14B2, 
                brady110,
                brady349,
                brady352, 
                burk79b2,
                microB6AC,
                microBE180, 
                microENV77,
                microNFIX05, 
                mycoVKM, 
                mycoYR708, 
                parabA2A,
                parabB3A, 
                parabC4B, 
                parabC5C, 
                parabC8A,
                parabD1E,
                parabE1A,
                parabE9D,
                parabHK1456,
                parabH4B,
                pseudo12B,
                pseudo13A,
                pseudo32A,
                pseudo317_b1,
                pseudoBBc6R8,
                pseudoI6B,
                strept.505,
                sphingo.BE319,
                sphingoDM2R,
                pginger)

# Merge

# Find columns common to all data frames
common_cols <- Reduce(intersect, lapply(df_list, names))

# Keep only common columns and row-bind
agg_df <- do.call(
  rbind,
  lapply(df_list, function(x) x[, common_cols, drop = FALSE])
)

#### Plotting all CAZy by substrate type instead of family

## Load CAZY family names
fams <- read.csv("fam-substrate-mapping-08262025.csv")
ec <- read.csv("EC-protein-list.csv")

### reconfigure the fams data frame
collapsed_fams <- fams %>%
  group_by(Family) %>%
  summarise(
    Subs = paste(unique(Substrate_curated), collapse = "; "),
    Sub2 = paste(unique(Substrate_high_level), collapse = "; "),
    .groups = "drop"
  )
colnames(collapsed_fams)[colnames(collapsed_fams) == "Family"] <- "Fam"
### save and eedit names to fix duplicate issues
write.csv(collapsed_fams, "CAZY.Families.Collapsed.csv")
collapsed_fams <- read.csv("CAZY.Families.Collapsed.csv")

library(tidyr)
master<- agg_df %>%
  separate(sseqid, into = c("ID", "Family"), sep = "\\|", remove = FALSE)
master$count <- 1

master$Family.Broad <- sub("^([A-Za-z]+).*", "\\1", master$Family)
master$title <- "CAZY Families"

master2 <- master %>%
  separate(Family, into = c("Fam", "Sub.Fam"), sep = "\\_", remove = FALSE)


master.fin <- merge(master2, collapsed_fams, by="Fam", all = TRUE)
master.fin2 <- subset(master.fin, Subs !="NA")
master.fin2 <- subset(master.fin2, Genome !="NA")

write.csv(master.fin2, "Agg.CAZy34.csv")

### Cleaned and polished in excel ###

# Kept Chitin, Cellulose, Chitosan, Alpha-Glucan, Beta-Glucan, 
# Alpha-mannan, Beta-mannan, Abscisic acid, Cutin, Lignin, Trehalose, 
# Pectin, Xylan

### Load polished DF 

df <- read.csv("Master.CAZyme.Final.csv")

# Remove the X, X1 ... columns

df2 <- df %>% select(Fam:Sub)

## Load metadata

meta <- read.csv("Genome.Information.Final.csv")

## Merge dFs 

df.fin <- merge(df2, meta, by="Genome", all=TRUE)
write.csv(df.fin, "Final.DF.Genome34.csv")

### remove Glucomannan
df.fin <- subset(df.fin, Sub !="Glucomannan")
df.fin <- subset(df.fin, Sub!="Starch")
df.fin <- subset(df.fin, Sub !="Plant Lectin")

## colors 
source_cols <- c(
  "Soil"                 = "burlywood4",  # brown
  "Plant Root"           = "#DFC27D",  # tan
  "Populus Roots"        = "#C7E9C0",  # pale green
  "Ectomycorrhiza"       = "ivory4",  # dark gray
  "EcMF Sporocarp"       = "darkolivegreen",  #  green
  "Saprotroph Sporocarp" = "red3",  # red
  "Fungus"               = "#9970AB",  # lavender-purple
  "Fungus Comb"          = "#35978F",  # teal
  "Dairy Farm"           = "#BF812D"   # ochre
)

### Complete Plot ###

library(tidyverse)

cazy_counts <- df.fin %>%
  count(
    Genome,
    Isolation.Source,
    Sub,
    name = "CAZymes"
  )

genome_meta <- df.fin %>%
  distinct(
    Genome,
    Size,
    GC.Content,
    CDS,
    Isolation.Source,
    Genome.ID
  )

total_cazy <- cazy_counts %>%
  group_by(Genome) %>%
  summarise(TotalCAZymes = sum(CAZymes))

genome_order <- genome_meta %>%
  left_join(total_cazy, by = "Genome") %>%
  arrange(
    Isolation.Source,
    desc(TotalCAZymes)
  ) %>%
  pull(Genome)

genome_meta <- genome_meta %>%
  mutate(
    Genome = factor(
      Genome,
      levels = rev(genome_order)
    )
  )

### Add faceting variables 
genome_meta$Genome.Size.Title <- "Genome Size (Mbp)"
genome_meta$GC.Title <- "GC% Content"
genome_meta$CDS.Title <- "No. CDS Regions"

group_breaks <- genome_meta %>%
  arrange(Isolation.Source) %>%
  count(Isolation.Source) %>%
  mutate(
    ypos = cumsum(n) + 0.5
  )

cazy_counts <- cazy_counts %>%
  mutate(
    Genome = factor(
      Genome,
      levels = rev(genome_order)
    )
  )
cazy_counts$CazyTitle <- "CAZyme Substrates"

p_size <- ggplot(
  genome_meta,
  aes(
    x = Size/1e6,
    y = Genome,
    fill = Isolation.Source
  )
) +
  geom_col(color="black") +
  scale_fill_manual(values = source_cols) +
  theme_bw() +
  labs(
    x = "",
    y = NULL,
    fill = NULL
  ) + facet_grid(~Genome.Size.Title) +
  theme(strip.text = element_text(size=10, face="bold"))

p_gc <- ggplot(
  genome_meta,
  aes(
    x = GC.Content,
    y = Genome,
    fill = Isolation.Source
  )
) +
  geom_col(color="black") +
  scale_fill_manual(values = source_cols) +
  theme_bw() +
  labs(
    x = "",
    y = NULL,
    fill = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(~GC.Title) +
  theme(strip.text = element_text(size=10, face="bold"))


p_cds <- ggplot(
  genome_meta,
  aes(
    x = CDS,
    y = Genome,
    fill = Isolation.Source
  )
) +
  geom_col(color="black") +
  scale_fill_manual(values = source_cols) +
  theme_bw() +
  labs(
    x = "",
    y = NULL,
    fill = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(~CDS.Title) +
  scale_x_continuous(breaks = seq(0, 10000, by = 5000)) +
  theme(strip.text = element_text(size=10, face="bold"))


### Genome stats, summary plot

p_cazy <- ggplot(
  cazy_counts,
  aes(
    x = Sub,
    y = Genome,
    fill=CAZymes,
    size = CAZymes
  )
) +
  geom_point(
    shape = 21,
    colour = "black",
    fill="black"
  ) +
  theme_bw() +
  labs(
    x = "",
    y = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) + facet_grid(~CazyTitle) + 
  theme(strip.text = element_text(size=10, face="bold"))



library(patchwork)

sum.plot <- (p_size | p_gc | p_cds | p_cazy) +
  plot_layout(
    widths = c(1.2,1,1.2,4),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

geom_hline(
  yintercept = c(12.5, 21.5),
  linetype = 2
) 

## save
ggsave(
  filename = "cazyme_sum_plot.png",
  plot = sum.plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

### Add Codon tree data to the plot
library(ape)
tree <- ape::read.tree("Final.Tree.34.NewPhyt2026_tree.nwk")

head(tree$tip.label)

lookup <- genome_meta %>%
  distinct(Genome.ID, Genome) %>%
  mutate(Genome.ID = as.character(Genome.ID))

tree_lookup <- data.frame(
  Genome.ID = as.character(tree$tip.label)
) %>%
  left_join(
    lookup,
    by = "Genome.ID"
  )

### Linear regressions ###

model1 <- lm(CAZymes ~ Genome + Isolation.Source + Sub, data=cazy_counts)
summary(model1)

### PERMANOVA

# Q: Does isolation source inform CAZyme potential?
## Groups: fungal-tissue, plant tissue, ectomycorrhiza

fungal_tissue <- c(
  "Fungus",
  "Fungus Comb",
  "Saprotroph Sporocarp"
)

plant_tissue <- c(
  "Plant Root",
  "Populus Roots"
)

ectomycorrhiza <- c(
  "Ectomycorrhiza",
  "EcMF Sporocarp"
)

group_df <- cazy_counts %>%
  mutate(
    SourceGroup = case_when(
      Isolation.Source %in% fungal_tissue ~ "Saprotrophic Fungi",
      Isolation.Source %in% plant_tissue ~ "Plant Tissue",
      Isolation.Source %in% ectomycorrhiza ~ "Ectomycorrhiza",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(SourceGroup))

table(group_df$SourceGroup)

library(tidyverse)

cazy_mat <- group_df %>%
  select(Genome, Sub, CAZymes) %>%
  distinct() %>%
  pivot_wider(
    names_from = Sub,
    values_from = CAZymes,
    values_fill = 0
  )

cazy_analysis <- cazy_mat %>%
  left_join(
    group_df %>%
      select(Genome, SourceGroup) %>%
      distinct(),
    by = "Genome"
  )

library(vegan)

cazy_dist <- vegdist(
  cazy_analysis %>% select(-Genome, -SourceGroup),
  method = "bray"
)

adonis2(
  cazy_dist ~ SourceGroup,
  data = cazy_analysis,
  permutations = 9999
)

pcoa <- cmdscale(cazy_dist, eig = TRUE, k = 2)

plot_df <- data.frame(
  Genome = cazy_analysis$Genome,
  SourceGroup = cazy_analysis$SourceGroup,
  PCoA1 = pcoa$points[,1],
  PCoA2 = pcoa$points[,2]
)

group_cols <- c(
  "Saprotrophic Fungi" = "burlywood4",
  "Plant Tissue" = "darkgreen",
  "Ectomycorrhiza" = "gray"
)

ggplot(plot_df, aes(PCoA1, PCoA2, color = SourceGroup)) +
  geom_point(size = 4, alpha = 0.9) +
  stat_ellipse(level = 0.95, linewidth = 0.8) +
  scale_color_manual(values = group_cols) +
  theme_bw() +
  labs(
    x = "PCoA1 (Bray–Curtis CAZymes)",
    y = "PCoA2 (Bray–Curtis CAZymes)"
  )

### Compute and plot centroids

centroids <- plot_df %>%
  group_by(SourceGroup) %>%
  summarise(
    PCoA1 = mean(PCoA1),
    PCoA2 = mean(PCoA2)
  )

spider_df <- plot_df %>%
  left_join(centroids, by = "SourceGroup")


## Add PERMANOVA results

perm <- adonis2(
  cazy_dist ~ SourceGroup,
  data = cazy_analysis,
  permutations = 9999
)

R2 <- perm$R2[1]
pval <- perm$`Pr(>F)`[1]

label <- paste0(
  "PERMANOVA\nR² = ",
  round(R2, 3),
  ", p = ",
  signif(pval, 2)
)

x_pos <- min(plot_df$PCoA1)
y_pos <- max(plot_df$PCoA2)

x_pos <- min(plot_df$PCoA1) + 0.05 * diff(range(plot_df$PCoA1))
y_pos <- max(plot_df$PCoA2) - 0.05 * diff(range(plot_df$PCoA2))

### Add PCoA variance explained

eig <- pcoa$eig
eig_pos <- eig[eig > 0]
pvar <- eig_pos / sum(eig_pos) * 100
PCoA1_var <- round(pvar[1], 1)
PCoA2_var <- round(pvar[2], 1)

### Add plot faceting variables
plot_df$title <- "CAZyme Composition ~ Isolation Source"

### Final PCoA plot w/ PERMANOVA results

p <- ggplot() +
  
  geom_segment(
    data = spider_df,
    aes(
      x = PCoA1.y, y = PCoA2.y,
      xend = PCoA1.x, yend = PCoA2.x,
      color = SourceGroup
    ),
    alpha = 0.4
  ) + scale_color_manual(values = group_cols) +
  scale_fill_manual(values = group_cols) + 
  
  geom_point(
    data = plot_df,
    aes(PCoA1, PCoA2, color = SourceGroup,
        size = 3, alpha = 0.3)) + scale_color_manual(values = group_cols) +
  geom_point(
    data = centroids,
    aes(PCoA1, PCoA2, fill = SourceGroup),
    shape = 21,
    size = 6,
    color = "black",
    stroke = 1.2, 
  ) + 
  theme_bw() + labs(
    x = paste0("PCoA1 (", PCoA1_var, "%)"),
    y = paste0("PCoA2 (", PCoA2_var, "%)")
  ) +
  guides(
    size = "none",
    alpha = "none",
    color = guide_legend(title = "Isolation Source"),
    fill  = guide_legend(title = "Isolation Source")) +
  annotate(
    "label",
    x = x_pos,
    y = y_pos,
    label = label,
    hjust = 0,
    vjust = 1,
    size = 4,
    fill = "white",
    label.size = 0.5
  ) +
  facet_grid(~title) +
  theme(strip.text = element_text(size=12, face="bold"))

## save
ggsave(
  filename = "cazyme_pcoa_spider_plot.png",
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

### TESTING ParaB + Brady & Sphingo + Myco ###

group_df2 <- group_df %>%
  mutate(
    Genus = word(Genome, 1)
  )

group_df2 <- group_df2 %>%
  mutate(
    GenusGroup = case_when(
      Genus %in% c("Paraburkholderia", "Bradyrhizobium") ~ "Synergist",
      Genus %in% c("Sphingomonas", "Mycobacterium") ~ "Antagonist",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GenusGroup))
table(group_df2$Genus, group_df2$GenusGroup)

# stats
valid_subs <- group_df2 %>%
  group_by(Sub) %>%
  summarise(
    n_groups = n_distinct(GenusGroup),
    .groups = "drop"
  ) %>%
  filter(n_groups == 2) %>%
  pull(Sub)

df_model <- group_df2 %>%
  filter(Sub %in% valid_subs)

library(broom)

lm_results <- df_model %>%
  group_by(Sub) %>%
  do({
    fit <- lm(CAZymes ~ GenusGroup, data = .)
    tidy(fit)
  })

### PLOTTING VOLACANO

volcano_df <- df_model %>%
  group_by(Sub) %>%
  summarise(
    Synergist = mean(CAZymes[GenusGroup == "Synergist"]),
    Antagonist = mean(CAZymes[GenusGroup == "Antagonist"]),
    .groups = "drop"
  ) %>%
  mutate(
    log2FC = log2((Synergist + 1) / (Antagonist + 1))
  )

pvals <- df_model %>%
  group_by(Sub) %>%
  do({
    fit <- lm(CAZymes ~ GenusGroup, data = .)
    data.frame(p = summary(fit)$coefficients[2,4])
  })

volcano_df <- volcano_df %>%
  left_join(pvals, by = "Sub") %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    neglog10p = -log10(p_adj)
  )

### faceting variable
volcano_df$title <- "Synergist vs. Antagonist"
volcano_df <- volcano_df %>%
  mutate(sig = p_adj < 0.05)

library(ggrepel)

## PLOT 
v.plot <- ggplot(volcano_df, aes(x = log2FC, y = neglog10p, label = Sub, color=sig)) +
  
  geom_point(size = 3, alpha=0.3) + scale_color_manual(values=c("black", "red3")) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_hline(yintercept = -log10(0.05), linetype = "dotted") +
  
  geom_text_repel(
    aes(label = Sub),
    size = 3,
    box.padding = 0.3,
    point.padding = 0.3,
    max.overlaps = Inf
  ) + 
  theme_bw() + facet_grid(~title) +
  theme(strip.text = element_text(size=12, face="bold")) +
  labs(
    x = "log2 Fold Change",
    y = "-log10 Adjusted p-value") + 
  guides(color = "none") 

ggsave(
  filename = "cazyme_volcano_plot.SYNvsANT.png",
  plot = v.plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

### Metabolic potential for Suillus-enriched sugars / sugar alcohols

met.df <- read.csv("BVBRC_subsystem34.csv")

### subset and build new df

arab <- subset(met.df, subsystem_name == "D-arabinitol utilization")
arab$Substrate <- "Arabinitol"
galc <- subset(met.df, subsystem_name == "Galactose utilization")
galc$Substrate <- "Galactose"
sucr <- subset(met.df, subsystem_name == "Sucrose to levan conversions")
sucr$Substrate <- "Sucrose"
inos <- subset(met.df, subsystem_name == "Inositol catabolism")
inos$Substrate <- "Inositol"
mann <- subset(met.df, product == "Mannose-6-phosphate isomerase")
mann$Substrate <- "Mannose"
met.list.df <- list(arab, galc, sucr, inos, mann)
agg.met.df <- bind_rows(met.list.df)

write.csv(agg.met.df, "Met.Agg.DF.csv")

library(stringr)

agg.met.df2 <- agg.met.df %>%
  mutate(
    Genus = case_when(
      str_detect(genome_name, regex("burk", ignore_case = TRUE)) ~ "Paraburkholderia",
      str_detect(genome_name, regex("bacillus", ignore_case = TRUE)) ~ "Bacillus",
      str_detect(genome_name, regex("pseudomonas", ignore_case = TRUE)) ~ "Pseudomonas",
      str_detect(genome_name, regex("bradyrhizobium", ignore_case = TRUE)) ~ "Bradyrhizobium",
      str_detect(genome_name, regex("sphingomonas", ignore_case = TRUE)) ~ "Sphingomonas",
      str_detect(genome_name, regex("mycobacterium", ignore_case = TRUE)) ~ "Mycobacterium",
      str_detect(genome_name, regex("microbacterium", ignore_case = TRUE)) ~ "Microbacterium",
      TRUE ~ NA_character_
    )
  )
agg.met.df2$count <- 1

agg.met3 <- aggregate(count ~ genome_name + Substrate + Genus, FUN = sum, data=agg.met.df2)

### Add other Genomes that had zero genes for these 
write.csv(agg.met3, "Metabolite.DF.Agg.csv")
agg.met3 <- read.csv("Metabolite.DF.Agg1.csv")

subsystem_summary <- agg.met3 %>%
  group_by(Genus, Substrate) %>%
  summarise(
    MeanGenes = mean(count),
    SD = sd(count),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

subsystem_summary$title <- "Suillus-Enriched Metabolites"

label_df <- subsystem_summary %>%
  group_by(Genus) %>%
  summarise(
    TotalMean = sum(MeanGenes),
    N = first(N),
    .groups = "drop"
  )

genus_levels <- subsystem_summary %>%
  group_by(Genus) %>%
  summarise(Total = sum(MeanGenes)) %>%
  arrange(Total) %>%
  pull(Genus)

subsystem_summary$Genus <- factor(subsystem_summary$Genus, levels = genus_levels)
label_df$Genus <- factor(label_df$Genus, levels = genus_levels)

### stats 

library(dplyr)
library(broom)

stats_results.met <- subsystem_summary %>%
  group_by(Substrate) %>%
  do({
    fit <- lm(MeanGenes ~ Genus, data = .)
    tidy(fit)
  }) %>%
  ungroup()

stats_p <- agg.met3 %>%
  group_by(Substrate) %>%
  do({
    fit <- lm(count ~ Genus, data = .)
    anova_res <- anova(fit)
    data.frame(p = anova_res$`Pr(>F)`[1])
  }) %>%
  mutate(p_adj = p.adjust(p, method = "BH"))

stats_labels.met <- stats_p %>%
  mutate(label = paste0("p=", signif(p_adj, 2)))

#plot
met.plot <- ggplot(subsystem_summary,
       aes(x = reorder(Genus,MeanGenes),
           y = MeanGenes,
           fill = Substrate)) +
  scale_fill_manual(values = c("red3", "burlywood4", "darkslategray4", "gray", "black")) +
  geom_col(color="black") +
  theme_bw() + coord_flip() +
  labs(y="Avg. Gene Count",
       x="") + facet_grid(~title) +
  theme(strip.text = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face = "italic")) +
  theme(
    legend.position = c(0.9, 0.2),   # inside plot
    legend.background = element_rect(
      fill = "white",
      color = "black"
    )
  )

# save 
ggsave(
  filename = "met.plot.SP.Enriched.png",
  plot = met.plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

#### PATRIC Pathway Mapping

library(dplyr);library(stringr);library(ggplot2)

#### REVISION #### -- > using data from 16 genomes (instead of 3) Date:10.2025
### Filename: Berrios2025NewPhyt_subsystems.csv


comp <- read.csv("BVBRC_subsystem34.csv")
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
    "darkgoldenrod3", "black", "gray", "#004529", "white"
  )) + coord_flip() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +  scale_x_discrete(limits = ord_levels, labels = lab_map) +
 xlab("") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("") + theme(axis.text.y = element_text(face = "bold.italic"))

ggsave("Dot.RelAbund.Class.34.png",
       plot = last_plot(),
       height=9,
       width=20,
       units="in",
       dpi=300)

# map: raw -> pretty
lab_map <- c(
  "Bacillus 1A" = "Bacillus sp. 1A",
  "Paraburkholderia D1E" = "Paraburkholderia sp. D1E",
  "Pseudomonas Pseudomonas.32A" = "Pseudomonas sp. 32A", 
  "Pseudomonas I6B" = "Pseudomonas sp. I6B", 
  "Pseudomonas 13A" = "Pseudomonas sp. 13A", 
  "Pseudomonas 12B" = "Pseudomonas sp. 12B", 
  "Pseudomonas 317_B1" = "Pseudomonas 317_B1",
  "Burkholderiales A2A" = "Paraburkholderia sp. A2A", 
  "Burkholderiales B3A" = "Paraburkholderia sp. B3A", 
  "Burkholderia C8A.2" = "Paraburkholderia sp. C8A", 
  "Burkholderia C5C" = "Paraburkholderia sp. C5C", 
  "Paraburkholderia H4B" = "Paraburkholderia sp. H4B", 
  "Paraburkholderia E9D" = "Paraburkholderia sp. E9D", 
  "Bacillus 14B2.2" = "Bacillus sp. 14B2",
  "Paraburkholderia C4B" = "Paraburkholderia sp. C4B",
  "Paraburkholderia E1A" = "Paraburkholderia sp. E1A",
  "Microbacterium B6AC" = "Microbacterium sp. B6AC", 
  "Bacillus cereus P9" = "Bacillus cereus P9",
  "Bacillus subtilis subsp. subtilis str. MP11" = "Bacillus subtilis MP11",
  "Bradyrhizobium diazoefficiens USDA 110" = "Bradyrhizobium diazoefficiens USDA 110",
  "Bradyrhizobium elkanii USDA 352" = "Bradyrhizobium elkanii USDA 352",
  "Bradyrhizobium japonicum USDA 349" = "Bradyrhizobium diazoefficiens USDA 349",
  "Burkholderia 79_B2" = "Burkholderia sp. 79_B2",
  "Microbacterium sp. NFIX05" = "Microbacterium sp. NFIX05",
  "Microbacterium trichothecenolyticum BE180" = "Microbacterium trichothecenolyticum BE180",
  "Mycobacterium avium subsp. avium Env 77" = "Mycobacterium avium ENV77",
  "Mycobacterium sp. VKM Ac-1816D" = "Mycobacterium sp. VKM Ac-1816D",
  "Mycobacterium sp. YR708" = "Mycobacterium sp. YR708",
  "Paraburkholderia endofungorum strain HKI456" = "Mycohabitans endofungorum HKI456",
  "Pseudomonas fluorescens BBc6R8" = "Pseudomonas fluorescens BBc6R8",
  "Pseudomonas gingeri NCPPB 3146" = "Pseudomonas gingeri NCPPB 3146",
  "Sphingomonas echinoides strain BE319" = "Sphingomonas echinoides BE319",
  "Sphingomonas sp. DM2-R-LB4" = "Sphingomonas sp. DM2-R-LB4",
  "Streptomyces sp. AcH 505" = "Streptomyces sp. AcH 505")

ord_levels <- names(sort(lab_map, na.last = TRUE))


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
  scale_x_discrete(limits = ord_levels, labels = lab_map) +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(face = "bold.italic", size = 9)) +
  theme(
    strip.text = element_text(face = "bold", size = 16, color = "white"),
    strip.background = element_rect(fill = "black")
  ) + labs(color = "Gene Product Number")

ggsave("Dot.Raw.Class.34.png",
       plot = last_plot(),
       height=10,
       width=12,
       units="in",
       dpi=300)
