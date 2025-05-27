###################################################################################################
#' Bear Morphometry
#'
#'This script get the morphometric measures from the analysis of bears images
#'
#'The annotated files for real bears with ImageJ are in the folder data/images_real
#'
#'Produces dataframe
#'    - results/nounoursus_morpho.csv
#'    - results/real_bears_morpho.csv
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'
#'
#' @date 2023/01/30 first created, 2025/04/28 major update
##################################################################################################

rm(list = ls(all = TRUE))

#MORPHO EXTRACTION FOR THE NOUNOURSUS IMAGES----

nounoursus_feat <- read.csv2(here::here("data", "nounoursus_feat.csv"))

var_morpho <- c(
  "Lenght",
  "HeadHei",
  "HeadLat",
  "HeadFron",
  "SnoutLen",
  "SnoutHei",
  "SnoutWid",
  "EarLen",
  "TruffleWid",
  "TruffleHei",
  "EyeDia",
  "HeadBodyLen",
  "ChestLen",
  "ChestHigh",
  "FrontLeg",
  "HindLeg"
)

# 1-2 lenght (Total length) (from the toes to the top of the head)
# 3-4 HeadHei (Head height) (from the chin to the top of the head)
# 5-6 HeadLat (Head large) (from the base of one ear to the other)
# 7-8 HeadFron (Head Front) (from the back of the head to the truffle)
# 9-10 SnoutLen (Snout length) (from the base of the snout to the truffle)
# 11-12 SnoutHei (Snout height)
# 13-14 SnoutWid (Snout width)
# 15-16 EarLen (Ear length)
# 17-18 TruffleWid (Truffle width)
# 19-20 TruffleHei (Truffle height)
# 21-22 EyeDia (Eye diameter)
# 23-24 HeadBodyLen (Head Body length) distance from anus to muzzle
# 25-26 ChestLen (Chest width) (from the base of one arm to the other)
# 27-28 ChestHigh (Chest depth) from belly to back
# 29-30 FrontLeg (Front Leg)
# 31-32 HindLeg (Hind Leg)

#Function to extract the morpho values from the images annotated with ImageJ
#note that the length of the teddy bear is estimated for the real measures (nounoursus_feat) and used to
#transform the distance measure with Image J into centimeters

morpho_nunursus <- function(id_bear, var_morpho) {
  #id_bear=22
  #var_morpho <- "Lenght"

  lenght = as.numeric(nounoursus_feat$length[
    nounoursus_feat$Image_id == id_bear
  ])

  data_m <- read.table(here::here(
    "data",
    "nounoursus",
    "jpg_morpho",
    paste0(id_bear, ".txt")
  ))
  colnames(data_m) <- c("X", "Y")

  xlst <- c("Image_id", var_morpho)
  data_bear <- data.frame(matrix(ncol = length(xlst), nrow = 1))
  colnames(data_bear) <- xlst
  data_bear$Image_id = id_bear
  data_bear$Lenght = lenght

  lenght_xy <- as.numeric(spatstat.geom::crossdist(
    data_m$X[1],
    data_m$Y[1],
    data_m$X[2],
    data_m$Y[2]
  ))

  pos = 3

  for (id_var in var_morpho[2:length(var_morpho)]) {
    #id_var=var_morpho[2]
    xy <- as.numeric(spatstat.geom::crossdist(
      data_m$X[pos],
      data_m$Y[pos],
      data_m$X[pos + 1],
      data_m$Y[pos + 1]
    ))
    val <- round((xy * lenght) / lenght_xy, digits = 2)
    data_bear[1, id_var] <- val
    pos = pos + 2
  }
  return(data_bear)
}

#check that all the txt file have 32 points data
do.call(
  rbind,
  lapply(nounoursus_feat$Image_id, function(id_bear) {
    data_m <- read.table(here::here(
      "data",
      "nounoursus",
      "jpg_morpho",
      paste0(id_bear, ".txt")
    ))
    if (dim(data_m)[1] < 32) {
      id_bear
    }
  })
)

#compute morpho extraction
nounoursus_morpho <- do.call(
  rbind,
  lapply(nounoursus_feat$Image_id, function(id_bear) {
    morpho_nunursus(id_bear = id_bear, var_morpho = var_morpho)
  })
)

write.csv2(
  nounoursus_morpho,
  here::here("results", "nounoursus_morpho.csv"),
  row.names = F
)

#----

#MORPHO EXTRACTION FOR THE REAL BEARS----

#Get the morpho from the images
#this function was used to construct the file real_bears.csv for each variable it gives the value the sex
#and the code to the associated ref found in the file real_bear_ref.xls
#Some of the values have been filled by hand and some other automatically
#They always show this pattern : X_Y_Z with X the value, Y the sex (M, F or ?) and,
# Z the ref code where the data have been found (either ref of from the images)

#FRONT
#1-2 HeadBodyLen
#3-4 ChestLen
#5-6 HeadHei
#7-8 HeadLat
#9-10 EarLen
#11-12 EyeDia
#13-14 TruffleWid
#15-16 TruffleHei
#17-18 SnoutHei
#19-20 SnoutWid

#PROFIL
#1-2 HeadBodyLen
#3-4 ChestHigh
#5-6 FrontLeg
#7-8 HindLeg
#9-10 HeadFron
#11-12 SnoutLen

#function morpho_real
morpho_real <- function(data, lenght_m, sex, ref, var) {
  # data=data_m
  # lenght_m=156
  # sex="M"
  # ref=66
  # var=var_front

  cat(paste0(var[1], " ", lenght_m, "_", sex, "_", ref, "\n"))

  lenght_xy <- spatstat.geom::crossdist(
    data_m$X[1],
    data_m$Y[1],
    data_m$X[2],
    data_m$Y[2]
  )

  pos = 3
  for (var_m in var[2:length(var)]) {
    #var_m=var[2]
    xy <- spatstat.geom::crossdist(
      data_m$X[pos],
      data_m$Y[pos],
      data_m$X[pos + 1],
      data_m$Y[pos + 1]
    )
    val <- round((xy * lenght_m) / lenght_xy, digits = 1)
    cat(paste0(var_m, " ", val, "_", sex, "_", ref, "\n"))
    pos = pos + 2
  }
}

#example
real_bears_refs <- readxl::read_xlsx(here::here("data", "real_bears_refs.xlsx"))
real_bears_refs <- as.data.frame(real_bears_refs)

data_m <- read.table(here::here(
  "data",
  "images_real",
  "Ursus_thibetanus_face_2.txt"
))
colnames(data_m) <- c("X", "Y")

var_front <- c(
  "HeadBodyLen",
  "ChestLen",
  "HeadHei",
  "HeadLat",
  "EarLen",
  "EyeDia",
  "TruffleWid",
  "TruffleHei",
  "SnoutHei",
  "SnoutWid"
)

var_profile <- c(
  "HeadBodyLen",
  "ChestHigh",
  "FrontLeg",
  "HindLeg",
  "HeadFron",
  "SnoutLen"
)

morpho_real(data = data_m, lenght_m = 156, sex = "M", ref = 65, var = var_front)
morpho_real(
  data = data_m,
  lenght_m = 145,
  sex = "M",
  ref = 6,
  var = var_profile
)

#The real_bears.csv file have been produced with the above function

#use the real_bears.csv to produce the tiddy_real_bear table that will be used afterward

real_bears_refs <- readxl::read_xlsx(here::here("data", "real_bears_refs.xlsx"))
real_bears_refs <- as.data.frame(real_bears_refs)
real_bears <- read.csv2(
  here::here("data", "real_bears.csv"),
  stringsAsFactors = FALSE
)
rownames(real_bears) <- real_bears$Latin

var_list <- c(
  "HeadBodyLen",
  "ShoulderHeight",
  "Weight",
  "FurThick",
  "ChestLen",
  "HeadHei",
  "HeadLat",
  "EarLen",
  "EyeDia",
  "TruffleWid",
  "TruffleHei",
  "ChestHigh",
  "FrontLeg",
  "HindLeg",
  "HeadFron",
  "SnoutLen",
  "SnoutHei",
  "SnoutWid"
)

tiddy_real_bears <- do.call(
  rbind,
  lapply(var_list, function(id_var) {
    #id_var <- var_list[1]

    if (id_var == "Weight") unit = "kg" else {
      if (id_var == "FurThick") unit = "NA" else unit = "cm"
    }
    do.call(
      rbind,
      lapply(rownames(real_bears), function(id) {
        #id <- rownames(real_bears)[1]

        dat <- unlist(strsplit(real_bears[id, id_var], " "))
        do.call(
          rbind,
          lapply(dat, function(id_dat) {
            # id_dat <- dat[1]

            val <- unlist(strsplit(id_dat, "_"))
            scientific_name <- id
            english <- real_bears[id, "English_1"]
            french <- real_bears[id, "French"]
            trait <- id_var
            sex <- val[2]
            value <- val[1]
            ref_id <- val[3]
            ref_type <- real_bears_refs$type[real_bears_refs$code == ref_id]
            ref_ref <- real_bears_refs$ref[real_bears_refs$code == ref_id]

            cbind.data.frame(
              scientific_name = scientific_name,
              english = english,
              french = french,
              trait = trait,
              sex = sex,
              value = value,
              unit = unit,
              ref_id = ref_id,
              ref_type = ref_type,
              ref_ref = ref_ref
            )
          })
        )
      })
    )
  })
)

tiddy_real_bears <- tiddy_real_bears[order(tiddy_real_bears$scientific_name), ]

write.csv2(
  tiddy_real_bears,
  here::here('results', 'tiddy_real_bears.csv'),
  row.names = FALSE
)

#produce the bear_morpho data set

tiddy_real_bears$value <- as.numeric(as.character(tiddy_real_bears$value))

sub_tiddy_real_bears <- tiddy_real_bears[tiddy_real_bears$sex != "F", ]
avg_bears <- aggregate(
  value ~ scientific_name + trait,
  data = sub_tiddy_real_bears,
  mean
)

traits <- unique(avg_bears$trait)
bears <- unique(avg_bears$scientific_name)

real_bears_morpho <- data.frame(species = bears)

for (id_traits in traits) {
  for (id_bears in bears) {
    sub1 <- avg_bears[avg_bears$scientific_name == id_bears, ]
    real_bears_morpho[
      real_bears_morpho$species == id_bears,
      id_traits
    ] = sub1$value[sub1$trait == id_traits]
  }
}

real_bears_morpho$species <- gsub(" ", "_", real_bears_morpho$species)

write.csv2(
  real_bears_morpho,
  here::here("results", "real_bears_morpho.csv"),
  row.names = F
)

#----
