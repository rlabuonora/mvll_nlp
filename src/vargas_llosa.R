library(purrr)
library(tibble)
library(dplyr)
library(zoo)
library(stringr)


# project_dir <- "C:/Users/rlabuonora/Desktop/ubuntu/mvll"
txt_folder <- "./txt"
#setwd(project_dir)
# Carga los archivos de texto

read_txt <- function(file, title) {
  txt <- readLines(file, encoding = "UTF-8")
  tibble(text = txt, title = title) %>%
    mutate(line = row_number())
}

# Nombres de los archivos y los libros
libros <- tribble(
  ~file,                                                 ~title,                             ~year,
  "time_of_the_hero.txt",                         "Time of the Hero",                         1963,
  "conversation_in_the_cathedral.txt",            "Conversation in the Cathedral",            1969,
  "captain_pantoja_and_the_special_service.txt",  "Captain Pantoja and the Special Service",  1973,
  "aunt_julia_and_the_scriptwriter.txt",          "Aunt Julia and the Scriptwriter",          1977,
  "war_of_the_end_of_the_world.txt",              "War of the End of the World",              1981,
  "real_life_of_alejandro_mayta.txt",             "Real Life of Alejandro Mayta",             1984 ,
  "storyteller.txt",                              "The Storyteller",                          1987 ,
  "a_fish_in_the_water.txt",                      "A Fish in the Water",                      1993,
  "death_in_the_andes.txt" ,                      "Death in the Andes",                       1994,
  "notebooks_of_don_rigoberto.txt",               "Notebooks of Don Rigoberto",               1997,
  "feast_of_the_goat.txt",                        "Feast of the Goat",                        2000,
  "way_to_paradise.txt",                          "Way to Paradise",                          2003,
  "bad_girl.txt",                                 "Bad Girl",                                 2006,
  "dream_of_the_celt.txt",                        "Dream of the Celt",                        2010,
  "discrete_hero.txt",                            "Discrete Hero",                            2013
)

bind_chapter_headers <- function(df) {
  pat_1 <- regex("
             ^((one|two|three
               |four|five|six|seven
               |eight|nine|ten|eleven
               |twelve|thirteen|fourteen
               |\\[?([IVXLC]+)\\]?
               |\\d{1,2}).?)$
               ",
                 ignore_case = TRUE,
                 comments = TRUE)
  
  pat_2 <- regex("^[A-Z]+$")
  
  df %>%
    mutate(chapter_header = str_detect(text, pat_1)) %>%
    group_by(title) %>%
    mutate(chapter = cumsum(chapter_header)) %>%
    ungroup %>%
    select(-chapter_header)
}

vargas_llosa <- function() {
  map2_df(file.path(txt_folder, libros$file), 
          libros$title, read_txt) %>%
    mutate(title = factor(title, levels=libros$title)) %>%
    bind_chapter_headers
}


