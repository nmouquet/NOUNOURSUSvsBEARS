###################################################################################################
#' Cludy toys
#'
#'This script analyses the answers of the online survey to the questions
#'    - "Had you a cuddly toy ?"
#'    - "What was your cuddly toy ? "
#'
#'Produces
#'.   - outputs/cuddly.tiff
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}, Nicolas Casajus, \email{nicolas.casajus@fondationbiodiversite.fr}
#'
#' @date 2025/05/01 first created
##################################################################################################

rm(list = ls(all = TRUE))

library(tidyverse)
library(ggplot2)

# Load data #11188 answers
data <- get(load("data/data_judges.RData"))
#data <- data[data$Age>50,] #if ones wants to look at the age class ...
tot_resp <- nrow(data)

# Compute the % for the question had you a cuddly toy
per_cludy <- round(100 * table(data$Cludly_toy) / tot_resp, 1)

#WORD COUNT----

data <- data$Cludly_other

# Add "Ours" for the people who did not answer this question (as they had answered
#yes to the question if there cuddly toy was a teddy bear)
data[data == ""] <- "Ours"

#Clean the list of cludy toys

# Clean text: lowercase, remove accents and punctuation, trim spaces
clean_responses <- data %>%
  str_to_lower() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_squish()

# Prepare for word frequency analysis
text_df <- tibble(text = clean_responses)

# Breaking down the text into individual words and remove French stopwords
data_tokens <- text_df %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(tidytext::get_stopwords(language = "fr"), by = "word") %>%
  count(word, sort = TRUE)

# Select most frequent words (count>1)
top_words <- data_tokens %>%
  filter(n > 1) %>%
  pull(word)

# Final cuddly list
common_words <- c(
  "ours",
  "lapin",
  "lion",
  "chat",
  "chien",
  "elephant",
  "girafe",
  "renard",
  "singe",
  "tigre",
  "zebre",
  "koala",
  "panda",
  "dauphin",
  "vache",
  "canard",
  "souris",
  "mouton",
  "chevre",
  "grenouille",
  "hippopotame",
  "loup",
  "poule",
  "tissu",
  "poupee",
  "monstre",
  "marionette",
  "lange",
  "poussin",
  "marsupilami",
  "ane",
  "coussin"
)

cuddly <- unique(c(top_words, common_words))
cuddly <- cuddly[!is.na(cuddly)]
cuddly <- cuddly[nchar(cuddly) >= 3]
cuddly <- cuddly[!grepl("[0-9]", cuddly)]

# Fuzzy matching function
extract_cuddly_name <- function(text, reference_words, max_dist = 2) {
  words <- unlist(str_split(text, " "))
  dist_mat <- stringdist::stringdistmatrix(
    words,
    reference_words,
    method = "osa"
  )
  match_indices <- which(dist_mat == min(dist_mat), arr.ind = TRUE)
  if (nrow(match_indices) > 0 && min(dist_mat) <= max_dist) {
    return(reference_words[match_indices[1, "col"]])
  } else {
    return(NA_character_)
  }
}

# Parallel processing
standardized_cuddly <- unlist(
  pbmcapply::pbmclapply(
    clean_responses,
    function(x) {
      tryCatch(
        extract_cuddly_name(x, cuddly),
        error = function(e) NA_character_
      )
    },
    mc.cores = parallel::detectCores() - 1
  )
)

# Drop NAs
standardized_cuddly <- standardized_cuddly[!is.na(standardized_cuddly)]

# Merge some words

standardized_cuddly <- ifelse(
  standardized_cuddly %in% c("ours", "panda", "nounours"),
  "Teddy Bear",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "tisdu",
      "ruban",
      "tissu",
      "taie",
      "sweat",
      "pyjama",
      "polochon",
      "polaire",
      "mouchoirs",
      "laine",
      "gilet",
      "gant",
      "fourrure",
      "foulards",
      "etiquette",
      "eponge",
      "edredon",
      "echarpe",
      "duvet",
      "draps",
      "couvertures",
      "couette",
      "couche",
      "chiffons",
      "chemise",
      "chaussette",
      "carre",
      "bandana",
      "bavoir",
      "vetement",
      "torchon",
      "serviette",
      "pull",
      "drap",
      "tissus",
      "shirt",
      "mouchoir",
      "foulard",
      "chiffon",
      "couverture",
      "velours",
      "tricot",
      "soie",
      "satin",
      "pareo",
      "alpaga"
    ),
  "Fabric",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "snoopy",
      "pluto",
      "personnages",
      "personnage",
      "minnie",
      "mini",
      "hello",
      "franklin",
      "disney",
      "donald",
      "babar",
      "arlequin",
      "alf",
      "tigrou",
      "stitch",
      "schtroumpf",
      "pokemon",
      "pikachu",
      "mickey",
      "marsupilami",
      "kiki",
      "winnie",
      "totoro",
      "teletubbies",
      "spiderman",
      "pierrot",
      "bambi",
      "popples",
      "popi",
      "bisounours"
    ),
  "Character",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "lapin",
      "lievre",
      "lapine",
      "lapain",
      "lapins",
      "lapinou"
    ),
  "Rabbit",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "dragon",
      "licorne",
      "lutin",
      "monstre",
      "fantome",
      "creature",
      "fee"
    ),
  "Fantasy",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "oiseau",
      "poule",
      "poulet",
      "paon",
      "oie",
      "manchot",
      "hiboux",
      "hibou",
      "chouette",
      "pingouin",
      "canard",
      "perroquet",
      "pinguin",
      "pinguoin",
      "toucan",
      "poussin"
    ),
  "Bird",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "singe",
      "ouistiti",
      "orang",
      "chimpanze",
      "paresseux",
      "lemurien",
      "gorille",
      "maki"
    ),
  "Primate",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c("papillon", "luciole", "libellule", "chenille", "coccinelle", "abeille"),
  "Insect",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in% c("oreiller", "coussin"),
  "Pillow",
  standardized_cuddly
)


standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "chien",
      "labrador",
      "dalmatien",
      "chiens",
      "chienne",
      "boulldug",
      "husky",
      "chiot",
      "caniche"
    ),
  "Dog",
  standardized_cuddly
)


standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "fleur",
      "fraise",
      "corolle",
      "banane",
      "aubergine",
      "carotte",
      "tomate",
      'rose'
    ),
  "Plant",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in% c("champignon"),
  "Fungus",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in% c("poupe", "poupée", "poupee"),
  "Doll",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "tigre",
      "lion",
      "panthere",
      "lionceau",
      "leopard",
      "lionne",
      "pantere",
      "lynx",
      "felins",
      "felin",
      "jaguar",
      "guepard",
      "chat",
      "chaton",
      "chatton"
    ),
  "Felid",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "mouton",
      "ane",
      "vache",
      "cochon",
      "agneau",
      "bourriquet",
      "poney",
      "anes",
      "belier",
      "brebis",
      "chevre",
      "cheval",
      "veau",
      "meu"
    ),
  "Farm animal",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "elephant",
      "girafe",
      "hippopotame",
      "crocodile",
      "giraffe",
      "zebre",
      "hipopotame",
      "rhinoceros",
      "girafes",
      "giraphe",
      "hyppopotame"
    ),
  "Savanna animal",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "renard",
      "herisson",
      "loup",
      "ecureuil",
      "biche",
      "blaireau",
      "ecureil",
      "faon",
      "elan",
      "renne",
      "castor",
      "caribou",
      "roussette",
      "rousette"
    ),
  "Forest animal",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "poupon",
      "bebe",
      "baigneur",
      "laveur"
    ),
  "Baby doll",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "tetine",
      "biberon",
      "tetines",
      "sucette",
      "turbulette",
      "gigoteuse",
      "lange"
    ),
  "Baby accessory",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in% c("souris", "sourie", "rat"),
  "Mouse",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "dauphin",
      "phoque",
      "tortue",
      "otarie",
      "orque",
      "baleine",
      "poulpe",
      "poisson",
      "requin",
      "pieuvre",
      "raie",
      "morse",
      "crabe",
      "beluga"
    ),
  "Marine animal",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly == "dinosaure",
  "Dinosaure",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "marmotte",
      "koala",
      "kangourou",
      "taupe",
      "ornithorynque",
      "mammouth",
      "loutre",
      "lama",
      "coyotte",
      "chamois",
      "koalas",
      "grenouille",
      "dromadaire",
      "fennec",
      "chameau"
    ),
  "Other animal",
  standardized_cuddly
)

standardized_cuddly <- ifelse(
  standardized_cuddly %in%
    c(
      "marionnette",
      "marionette",
      "figurine",
      "robot",
      "jouet",
      "camion",
      "ballerine",
      "homme",
      "bonhomme",
      "clown"
    ),
  "Toy",
  standardized_cuddly
)

# Remove some words
words_to_remove <- c(
  "petit",
  "coeur",
  "corps",
  "cou",
  "tout",
  "couleurs",
  "dame",
  "doux",
  "deux",
  "laveur",
  "dors",
  "doudous",
  "doudou",
  "animal",
  "blanc",
  "mere",
  "petite",
  "plus",
  "plusieurs",
  "sais",
  "1",
  "2",
  "20cm",
  "3",
  "animaux",
  "aucun",
  "aucune",
  "avoir",
  "blanche",
  "bleue",
  "bout",
  "ca",
  "tete",
  "puis",
  "morceau",
  "maman",
  "etait",
  "rien",
  "a",
  "rouge",
  "rappelle",
  "plein",
  "oui",
  "etoile",
  "bleu",
  "pouce",
  "plush",
  "vert",
  "tricotee",
  "souviens",
  "souvenir",
  "non",
  "nain",
  "meme",
  "carton",
  "jaune",
  "belle",
  "peluche",
  "yeux",
  "truc",
  "toute",
  "toujours",
  "sorte",
  "queue",
  "soleil",
  "porte",
  "pleins",
  "plat",
  "plat",
  "petits",
  "pere",
  "peluches",
  "peau",
  "pattes",
  "nid",
  "marin",
  "man",
  "livre",
  "jar",
  "jamais",
  "importe",
  "hamster",
  "gros",
  "grande",
  "forme",
  "fine",
  "fait",
  "facon",
  "etc",
  "espece",
  "environ",
  "env",
  "entre",
  "enfant",
  "chose",
  "bras",
  "tous",
  "indetermine",
  "differentes",
  "comme",
  "aussi",
  "cheveux",
  "peu",
  "ressemble"
)
standardized_cuddly <- standardized_cuddly[
  !standardized_cuddly %in% words_to_remove
]

final_cludy <- length(standardized_cuddly)

#Percentage for all
cuddly_counts <- as.data.frame(table(standardized_cuddly)) %>%
  rename(Animal = standardized_cuddly, Count = Freq) %>%
  mutate(Percent = round(100 * Count / sum(Count), 3)) %>%
  arrange(desc(Percent))

non_living <- round(
  sum(cuddly_counts[
    cuddly_counts$Animal %in%
      c(
        "Fabric",
        "Doll",
        "Character",
        "Pillow",
        "Baby doll",
        "Fantasy",
        "Toy",
        "Baby accessory"
      ),
    "Percent"
  ]),
  2
)
plants <- round(
  sum(cuddly_counts[cuddly_counts$Animal %in% c("Plant"), "Percent"]),
  2
)
fungus <- round(
  sum(cuddly_counts[cuddly_counts$Animal %in% c("Fungus"), "Percent"]),
  5
)
animals <- round(100 - (non_living + plants + fungus), 2)

#non_living=16.2%
#plants=0.18%
#fungus=0.01%
#animals = 83.6%

#Final table
cuddly_counts <- cuddly_counts[cuddly_counts$Percent >= 1, ]
cuddly_counts$Animal <- str_to_upper(cuddly_counts$Animal)
cuddly_counts$Animal <- factor(
  cuddly_counts$Animal,
  levels = cuddly_counts$Animal[order(cuddly_counts$Percent)]
)
cuddly_counts <- cuddly_counts %>%
  arrange(desc(Percent)) %>%
  mutate(rank = row_number())

#----

#PLOTS----

#Cuddly plot
plot_cuddly <- ggplot(
  cuddly_counts,
  aes(x = Percent, y = fct_reorder(Animal, Percent))
) +
  geom_col(fill = "#1B8DD4") +
  geom_text(
    data = filter(cuddly_counts, rank <= 4),
    aes(label = Animal),
    x = 5,
    hjust = 0,
    color = "white",
    size = 3,
    fontface = "bold"
  ) +
  geom_text(
    data = filter(cuddly_counts, rank > 4),
    aes(label = Animal),
    x = 5,
    hjust = 0,
    color = "black",
    size = 3
  ) +
  scale_x_continuous(
    position = "top",
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "What cuddly toy ?") +
  theme_bw(base_size = 12) +
  theme(
    #panel.grid = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#Respondent plot
cuddly_data <- tibble(
  category = c("?", "No", "Yes"),
  percent = c(per_cludy[1], per_cludy[2], per_cludy[3])
)

plot_respond <- ggplot(
  cuddly_data,
  aes(x = reorder(category, percent), y = percent)
) +
  geom_col(fill = "#f2c45f") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Had a Cuddly Toy ?", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 12, vjust = -4, hjust = 0.1), # Move title downward into plot
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Combine both plots and save
final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(plot_cuddly) +
  cowplot::draw_plot(
    plot_respond,
    x = 0.45,
    y = 0.12,
    width = 0.525,
    height = 0.65
  )

ggsave(
  filename = here::here("outputs", "cuddly.tiff"),
  plot = final_plot,
  width = 8,
  height = 4,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

#----
