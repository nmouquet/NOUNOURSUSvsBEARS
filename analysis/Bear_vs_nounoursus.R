###################################################################################################
#' Bear and Teddy bear Morphometry & Colorimetric features
#'
#'This script get the morphometric and colorimetric features from the analysis of images for
#'    - Teddy bears
#'    - Real bears
#'
#'Produces dataframe
#'    - results/bear_all
#'    - outputs/fig1_sup.tiff
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'
#'
#' @date 2023/01/30 first created, 2025/04/30 major update
##################################################################################################

rm(list = ls(all = TRUE))

#Functions----

norm01 <- function(dat) {
  min_dat <- min(dat, na.rm = TRUE)
  max_dat <- max(dat, na.rm = TRUE)
  dat <- dat + abs(min_dat)
  min_dat <- min(dat, na.rm = TRUE)
  max_dat <- max(dat, na.rm = TRUE)
  dat <- (dat - min_dat) / (max_dat - min_dat)
  return(dat)
}
#end----

#Prepare the data----

real_bears_morpho <- read.csv2(here::here('results', 'real_bears_morpho.csv'))
colnames(real_bears_morpho)[colnames(real_bears_morpho) == "species"] <- "ID"
nounoursus_morpho <- read.csv2(here::here('results', 'nounoursus_morpho.csv'))
colnames(nounoursus_morpho)[colnames(nounoursus_morpho) == "Image_id"] <- "ID"

#keep the same variables as in Tribot et al. 2025

real_bears_morpho <- real_bears_morpho[, c(
  "ID",
  "ChestHigh",
  "ChestLen",
  "EarLen",
  "EyeDia",
  "FrontLeg",
  "HeadBodyLen",
  "HeadFron",
  "HeadHei",
  "HeadLat",
  "HindLeg",
  "SnoutLen",
  "TruffleHei",
  "TruffleWid",
  "SnoutHei",
  "SnoutWid"
)]
real_bears_morpho$Type <- "bears"
nounoursus_morpho <- nounoursus_morpho[, c(
  "ID",
  "ChestHigh",
  "ChestLen",
  "EarLen",
  "EyeDia",
  "FrontLeg",
  "HeadBodyLen",
  "HeadFron",
  "HeadHei",
  "HeadLat",
  "HindLeg",
  "SnoutLen",
  "TruffleHei",
  "TruffleWid",
  "SnoutHei",
  "SnoutWid"
)]
nounoursus_morpho$Type <- "teddy"

bear_all <- rbind(real_bears_morpho, nounoursus_morpho)


#divide by HeadBodyLen (to make the data comparable with the teddy bear data set)
bear_all[, c(
  "ChestHigh",
  "ChestLen",
  "EarLen",
  "EyeDia",
  "FrontLeg",
  "HeadBodyLen",
  "HeadFron",
  "HeadHei",
  "HeadLat",
  "HindLeg",
  "SnoutLen",
  "TruffleHei",
  "TruffleWid",
  "SnoutHei",
  "SnoutWid"
)] <- bear_all[, c(
  "ChestHigh",
  "ChestLen",
  "EarLen",
  "EyeDia",
  "FrontLeg",
  "HeadBodyLen",
  "HeadFron",
  "HeadHei",
  "HeadLat",
  "HindLeg",
  "SnoutLen",
  "TruffleHei",
  "TruffleWid",
  "SnoutHei",
  "SnoutWid"
)] /
  bear_all$HeadBodyLen

#create some combined data
bear_all$Juv_eyes = bear_all$EyeDia / bear_all$HeadLat
bear_all$Juv_muzzle = bear_all$SnoutLen / bear_all$HeadHei
bear_all$Nose = bear_all$TruffleWid * bear_all$TruffleHei * pi
bear_all$Muzzle_vol = (pi / 6) * bear_all$SnoutWid^2 * bear_all$SnoutHei


#log10 some variables (if needed; to be coherent with the teddy bear data set)
bear_all$Nose = log10(bear_all$Nose)
bear_all$Muzzle_vol = log10(bear_all$Muzzle_vol)

#Rename chestlen

colnames(bear_all)[colnames(bear_all) == "ChestLen"] <- "Chest_width"


#remove HeadBodyLen from the database + the variable used to compute the "combined data"
bear_all <- bear_all[, -which(names(bear_all) %in% "HeadBodyLen")]
bear_all <- bear_all[, -which(names(bear_all) %in% "EyeDia")]
bear_all <- bear_all[, -which(names(bear_all) %in% "HeadLat")]
bear_all <- bear_all[, -which(names(bear_all) %in% "SnoutHei")]
bear_all <- bear_all[, -which(names(bear_all) %in% "HeadHei")]
bear_all <- bear_all[, -which(names(bear_all) %in% "TruffleWid")]
bear_all <- bear_all[, -which(names(bear_all) %in% "TruffleHei")]
bear_all <- bear_all[, -which(names(bear_all) %in% "SnoutWid")]
bear_all <- bear_all[, -which(names(bear_all) %in% "SnoutLen")]

#add the color features (select only the features used in Tribot et al. 2025)

cluster_real <- read.csv2(here::here("results", "features_real", "cluster.csv"))
lumsat_real <- read.csv2(here::here("results", "features_real", "lumsat.csv"))
color_real <- merge(cluster_real, lumsat_real)

cluster_teddy <- read.csv(here::here("data", "features_teddy", "cluster.csv"))
cluster_teddy <- cluster_teddy[, -1]
colnames(cluster_teddy)[colnames(cluster_teddy) == "name"] <- "Image_id"
lumsat_teddy <- read.csv(here::here("data", "features_teddy", "lumsat.csv"))
lumsat_teddy <- lumsat_teddy[, -1]
colnames(lumsat_teddy)[colnames(lumsat_teddy) == "name"] <- "Image_id"
color_teddy <- merge(cluster_teddy, lumsat_teddy)

color_all <- rbind(color_real, color_teddy)
color_all <- color_all[, c(
  "Image_id",
  "CL_cie_d_mean",
  "LS_mean_satu",
  "LS_mean_light",
  "LS_sd_light"
)]
colnames(color_all) <- c(
  "ID",
  "Color_hetero",
  "Saturation",
  "Brigthness",
  "Constrast"
)

bear_all <- merge(bear_all, color_all)


#add the Hair_length (FurThick in real bear dataset)
bear_all$Hair_length <- NA
real_bears_morpho <- read.csv2(here::here('results', 'real_bears_morpho.csv'))

for (i in 1:nrow(real_bears_morpho)) {
  bear_all$Hair_length[which(
    bear_all$ID %in% real_bears_morpho$species[i]
  )] = real_bears_morpho$FurThick[i]
}

nounoursus_feat <- read.csv2(here::here("data", "nounoursus_feat.csv"))
for (i in 1:nrow(nounoursus_feat)) {
  bear_all$Hair_length[which(
    bear_all$ID %in% nounoursus_feat$Image_id[i]
  )] = nounoursus_feat$hair_length[i]
}

#normalize
for (i in 1:ncol(bear_all)) {
  if (is.numeric(bear_all[, i])) {
    bear_all[, i] <- norm01(bear_all[, i])
  }
}

#add the Cutteness

teddy_bear <- read.csv2(here::here("data", "final_nounoursus_all.csv"))

load(here::here("data", "Elo_Age_class_all.RData"))

names(Elo_class$scores_class)
colnames(Elo_class$scores_class) <- c(
  "Image_id",
  "Cutteness_under_10",
  "Elo_sd_all_3_10",
  "Cutteness_11_18",
  "Elo_sd_all_11_18",
  "Cutteness_19_30",
  "Elo_sd_all_19_30",
  "Cutteness_30_59",
  "Elo_sd_all_30_59",
  "Cutteness_over_60",
  "Elo_sd_all_60_100"
)

bear_all$Cutteness <- NA
bear_all$Cutteness_children <- NA
bear_all$Cutteness_adults <- NA #redo with the real adult (like between 30 and 45 years old)
for (i in 1:nrow(teddy_bear)) {
  bear_all$Cutteness[which(
    bear_all$ID %in% teddy_bear$Image_id[i]
  )] = teddy_bear$Cutteness[i]
  #bear_all$Cutteness_children[which(bear_all$ID%in%Elo_class$Image_id[i])]=Elo_class$Cutteness_under_10[i]
  # bear_all$Cutteness_adults[which(bear_all$ID%in%Elo_class$Image_id[i])]=Elo_class$Cutteness_over_60[i]
}

for (i in 1:nrow(Elo_class$scores_class)) {
  bear_all$Cutteness_children[which(
    bear_all$ID %in% Elo_class$scores_class$Image_id[i]
  )] = Elo_class$scores_class$Cutteness_under_10[i]
  bear_all$Cutteness_adults[which(
    bear_all$ID %in% Elo_class$scores_class$Image_id[i]
  )] = Elo_class$scores_class$Cutteness_over_60[i]
}

#Change some variable names

names(bear_all)[names(bear_all) == "Chest_width"] <- "Chest width"
names(bear_all)[names(bear_all) == "FrontLeg"] <- "Front leg lenght"
names(bear_all)[names(bear_all) == "Juv_eyes"] <- "Juvenil eyes"
names(bear_all)[names(bear_all) == "Muzzle_vol"] <- "Muzzle volume"
names(bear_all)[names(bear_all) == "Color_hetero"] <- "Color heterogeneity"
names(bear_all)[names(bear_all) == "Hair_length"] <- "Fur thickness"
names(bear_all)[names(bear_all) == "Juv_muzzle"] <- "Juvenil muzzle"

#Save

write.csv2(bear_all, here::here("results", "bear_all.csv"), row.names = F)
#end----

#PCA----

traits_all <- bear_all
rownames(traits_all) <- traits_all$ID
traits_all <- traits_all[,
  !(names(traits_all) %in% c("ID", "Type", "Cutteness"))
]
traits_all <- traits_all[,
  (names(traits_all) %in%
    c(
      "Fur thickness",
      "Juvenil eyes",
      "Juvenil muzzle",
      "Front leg lenght",
      "Color heterogeneity",
      "Muzzle volume",
      "Saturation",
      "Chest width"
    ))
]

pca_all <- ade4::dudi.pca(traits_all, scannf = FALSE, nf = 5)
sum((100 * pca_all$eig / sum(pca_all$eig))[1:3])

res.ind <- factoextra::get_pca_ind(pca_all)


pca_eigen <- factoextra::fviz_eig(pca_all, main = "Eigenvalues") #the three first axis takes over almost 60% of the variance

df <- data.frame(
  pc1 = res.ind$coord[, 'Dim.1'],
  pc2 = res.ind$coord[, 'Dim.2'],
  pc3 = res.ind$coord[, 'Dim.3'],
  Cutteness = bear_all$Cutteness,
  Cutteness_children = bear_all$Cutteness_children,
  Cutteness_adults = bear_all$Cutteness_adults,
  names = rownames(res.ind$coord)
)

GGally::ggpairs(df[, 1:4]) #PC1 and 3 are clearly linked to the cuteness score

#Figure sup Fig1b,c,d
library(ggplot2)
library(gridExtra)
# Calculate Pearson correlations
cor1 <- cor(df$pc1, df$Cutteness, use = "complete.obs")
cor2 <- cor(df$pc2, df$Cutteness, use = "complete.obs")
cor3 <- cor(df$pc3, df$Cutteness, use = "complete.obs")

# Panel 1: PC1 vs Cutteness
p1 <- ggplot(df, aes(x = pc1, y = Cutteness)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  #ggtitle("PC1 vs Cutteness") +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("r = ", round(cor1, 2)),
    hjust = 1.1,
    vjust = 1.5,
    size = 4,
    color = "black"
  ) +
  theme_bw()

# Panel 2: PC2 vs Cutteness
p2 <- ggplot(df, aes(x = pc2, y = Cutteness)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  #ggtitle("PC2 vs Cutteness") +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("r = ", round(cor2, 2)),
    hjust = 1.1,
    vjust = 1.5,
    size = 4,
    color = "black"
  ) +
  theme_bw()

# Panel 3: PC3 vs Cutteness
p3 <- ggplot(df, aes(x = pc3, y = Cutteness)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  #ggtitle("PC3 vs Cutteness") +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("r = ", round(cor3, 2)),
    hjust = 1.1,
    vjust = 1.5,
    size = 4,
    color = "black"
  ) +
  theme_bw()

# Combine the plots and save
combined_plot <- arrangeGrob(pca_eigen, p1, p2, p3, ncol = 2)
ggsave(
  here::here("outputs", "fig1_sup.tiff"),
  combined_plot,
  width = 10,
  height = 7,
  dpi = 300,
  units = "in",
  device = "tiff"
)

#Interpret PC1 and PC2
factoextra::fviz_pca_var(
  pca_all,
  axes = c(1, 3),
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) #PC1 and 3 are easy to interpret


#look at the position of the species in the PCA #1#3

rb <- c(
  "Ailuropoda_melanoleuca",
  "Helarctos_malayanus",
  "Melursus_ursinus",
  "Thalarctos_maritimus",
  "Tremarctos_ornatus",
  "Ursus_americanus",
  "Ursus_arctos_arctos",
  "Ursus_arctos_horribilis",
  "Ursus_arctos_isabellinus",
  "Ursus_arctos_middendorffi",
  "Ursus_thibetanus"
)
pca_realbr <- pca_all$li[rownames(pca_all$li) %in% rb, ]
rownames(pca_realbr) <- c(
  "Panda",
  "Sun",
  "Sloth",
  "Polar",
  "Spectacled",
  "American",
  "Eurasian",
  "Grizzli",
  "Himalayan",
  "Kodiak",
  "Asian"
)

colors_grad <- rev(RColorBrewer::brewer.pal(8, "Spectral"))

library(ggplot2)

pca_realbr$hjust <- -0.15
pca_realbr$vjust <- 0.5
pca_realbr$hjust[rownames(pca_realbr) %in% "Grizzli"] = -0.2
pca_realbr$hjust[rownames(pca_realbr) %in% "Kodiak"] = -0.2
pca_realbr$hjust[rownames(pca_realbr) %in% "American"] = -0.1
pca_realbr$vjust[rownames(pca_realbr) %in% "Panda"] = 0.45
pca_realbr$hjust[rownames(pca_realbr) %in% "Panda"] = -0.2
pca_realbr$hjust[rownames(pca_realbr) %in% "Sloth"] = 0.5
pca_realbr$vjust[rownames(pca_realbr) %in% "Sloth"] = -1
pca_realbr$hjust[rownames(pca_realbr) %in% "Himalayan"] = 1.1
pca_realbr$vjust[rownames(pca_realbr) %in% "Himalayan"] = 0.4
pca_realbr$hjust[rownames(pca_realbr) %in% "Eurasian"] = -0.1
pca_realbr$hjust[rownames(pca_realbr) %in% "Spectacled"] = 0.5
pca_realbr$vjust[rownames(pca_realbr) %in% "Spectacled"] = -0.8
pca_realbr$hjust[rownames(pca_realbr) %in% "Polar"] = 0.5
pca_realbr$vjust[rownames(pca_realbr) %in% "Polar"] = -1
pca_realbr$hjust[rownames(pca_realbr) %in% "Sun"] = 0.5
pca_realbr$vjust[rownames(pca_realbr) %in% "Sun"] = 1.9
pca_realbr$hjust[rownames(pca_realbr) %in% "Asian"] = 0.5
pca_realbr$vjust[rownames(pca_realbr) %in% "Asian"] = 1.9


fig_pca <- factoextra::fviz_pca_biplot(
  pca_all,
  axes = c(1, 3),
  col.ind = bear_all$Cutteness,
  geom = "point",
  gradient.cols = colors_grad,
  repel = TRUE,
  col.var = "#575656",
  geom.var = c("arrow"),
  alpha.ind = 1,
  ggtheme = theme_bw(),
  pointsize = 2.5
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(col = "Range", title = " ") +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  geom_point(
    data = pca_realbr,
    aes(x = Axis1, y = Axis3),
    shape = 1,
    color = "black",
    size = 2.5
  ) +
  stat_ellipse(
    data = pca_realbr,
    aes(x = Axis1, y = Axis3),
    type = "t",
    level = 0.95,
    linetype = "dashed",
    color = "#BDBDBD",
    size = 0.7
  ) +
  geom_text(
    data = pca_realbr,
    aes(
      x = Axis1,
      y = Axis3,
      label = rownames(pca_realbr),
      hjust = hjust,
      vjust = vjust
    ),
    color = "black",
    size = 3.5
  ) +
  coord_cartesian(xlim = c(-7.5, 8.5), ylim = c(-7.5, 7.5))

ggsave(
  filename = here::here("outputs", "fig_pca.tiff"),
  plot = fig_pca,
  width = 8,
  height = 8,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

#find examples

#plot(pca_all$li$Axis1, pca_all$li$Axis3, pch = 19, col = "blue", main = "Click on a point to get its ID")
#clicked_points <- identify(pca_all$li$Axis1, pca_all$li$Axis3, labels = rownames(pca_all$li), plot = TRUE)
#print(paste("You clicked on ID(s):", rownames(pca_all$li)[clicked_points]))

#end----
