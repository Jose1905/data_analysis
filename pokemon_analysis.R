#import required packages
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)


#Store the dataset into a variable
pokemonList <- read.csv("pokemon.csv")

#Change the Legendary column to the correct datatype
str(pokemonList)
pokemonList$Legendary <- as.logical(pokemonList$Legendary)


#Question #1: Who are the strongest non-legendary and non-mega pokemon in terms of base stats? ----------------------
strongest_pokemon <- pokemonList %>% 
  filter(Legendary == FALSE
         & grepl("Mega ", Name) == FALSE) %>%  #Included the string "Mega " With the white space as there are pokemon that include "Mega" in their name, such as Meganium
  select(Name, Total) %>% 
  arrange(desc(Total)) %>%
  slice(1:20)


#Question #2: What generation introduced the Pokemons with the highest average base stats? --------------------------
generation_avg <- pokemonList %>%
  group_by(Generation) %>% 
  summarise(Average_Total = mean(Total)) %>% 
  arrange(desc(Average_Total)) #%>% 
  #slice(1)
  

#Draw a graph showing some of the results. --------------------------------------------------------------------------

#Graph question #1
ggplot(strongest_pokemon, aes(x = Name, y = Total)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Strongest non-legendary Pokemon",
       x = "Pokemon Name",
       y = "Total base stats") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Graph question #2
ggplot(generation_avg, aes(x = Generation, y = Average_Total, fill = Generation)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Total Stats by Generation",
       x = "Generation",
       y = "Average Total Stats") +
  theme_minimal()


