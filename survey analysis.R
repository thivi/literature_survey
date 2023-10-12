library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tools)
library("ggvenn")

survey_data <- read.csv("../Desktop/Book1.csv")

data_store <- "C:/Users/thevi/OneDrive/Academic Work/Studies/Ph.D. University of Glasgow/Research/First Year/Data/"

# ------------------------------------- #
#           Utility Functions           #
# ------------------------------------- #
sanitize_data <- function(data) {
  data %>% 
    separate_rows("Algorithm", sep = ",") %>% 
    mutate("Algorithm"=tolower(trimws(Algorithm, which ="left"))) %>%
    mutate("Algorithm"=trimws(Algorithm, which = "right")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "integer quadratically constrained program", "exact")) %>% 
    mutate("Algorithm"=str_replace(Algorithm, "approximation algorithm", "heuristic")) %>% 
    mutate("Algorithm"=str_replace(Algorithm, "bee colony", "swarm intelligence")) %>% 
    mutate("Algorithm"=str_replace(Algorithm, "column generation", "heuristic")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "dynamic programming", "exact")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "finite automaton", "exact")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "grey wolf", "swarm intelligence")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "hungarian", "exact")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "resource constrained project scheduling problem", "heuristic")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "stable matching", "heuristic")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "tabu search", "heuristic")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "viterbi", "heuristic")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "ga\\b", "Genetic Algorithm")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "ml", "Machine Learning")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "lp", "Linear Programming")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "rl", "Reinforcement Learning")) %>%
    mutate("Algorithm"=str_replace(Algorithm, "meteor shower optimization", "heuristic")) %>%
    filter(Algorithm != "blockchain") %>% 
    filter(Algorithm != "n/a") %>%
    filter(Algorithm != "rfpa") %>% 
    filter(Algorithm != "flexible job-shop problem") %>% 
    mutate("Algorithm"=toTitleCase(Algorithm))
}

# ------------------------------------- #
#   Algorithms used and their count     #
# ------------------------------------- #
algo <- data.frame("Algorithm" = survey_data$Algorithm) %>% 
  sanitize_data() %>% 
  group_by(Algorithm) %>% 
  summarise(Count=n()) %>% 
  write.csv(file=paste(data_store, "algorithm_count.csv", sep = ""))

# ------------------------------------- #
#           Algorithm Trend             #
# ------------------------------------- #
trend <- data.frame("Algorithm" = survey_data$Algorithm, "Year"= survey_data$Year) %>%
  sanitize_data() %>% 
  group_by(Year, Algorithm) %>% 
  summarise(Count=n()) %>% 
  pivot_wider(names_from = "Algorithm", values_from="Count") %>% 
  write.csv(file=paste(data_store, "trend.csv", sep = ""))

# ------------------------------------- #
#           Scalability                 #
# ------------------------------------- #
scalability <- data.frame("Scalability" = survey_data$Scalable) %>% 
  group_by(Scalability) %>% 
  summarise(Count=n()) %>% 
  write.csv(file=paste(data_store,"scalability.csv", sep = ""))

# ------------------------------------- #
#           Self-adaptive               #
# ------------------------------------- #
adaptive <- data.frame("Selfadaptive" = survey_data$Shows.adaptiveness) %>% 
  group_by(Selfadaptive) %>% 
  summarise(Count = n()) %>% 
  write.csv(file=paste(data_store,"adaptive.csv", sep = ""))

# ------------------------------------- #
#           Implementation              #
# ------------------------------------- #
impl <- data.frame("Implementation" = survey_data$Implementation) %>%
  separate_rows("Implementation", sep=",") %>% 
  mutate("Implementation" = trimws(Implementation, which ="left")) %>%
  mutate("Implementation" = trimws(Implementation, which ="right")) %>%
  mutate("Implementation" = str_replace(Implementation, "OMNET", "OMNet")) %>%  
  group_by(Implementation) %>% 
  summarise(Count = n()) %>% 
  mutate(Implementation=ifelse(Count == 1, "Others", Implementation)) %>% 
  aggregate(Count ~ Implementation, sum)

implyesno <- impl %>% 
  mutate(Implementation=ifelse(Implementation != "No", "Yes", "No")) %>% 
  aggregate(Count ~ Implementation, sum) %>% 
  write.csv(file=paste(data_store, "implementations.csv", sep = ""))

sims <- impl %>% 
  filter(Implementation != "No") %>% 
  write.csv(file=paste(data_store, "simulators.csv", sep = ""))
  
# ------------------------------------- #
#       Optimization Objectives         #
# ------------------------------------- #
opt <-  data.frame("Objective" = survey_data$Optimization.factors) %>% 
  separate_rows("Objective", sep = ",") %>% 
  mutate("Objective"=tolower(trimws(Objective, which ="left"))) %>%
  mutate("Objective"=trimws(Objective, which = "right")) %>%
  mutate(Objective=str_replace(Objective, "reconfiguration cost", "cost")) %>%
  mutate(Objective=str_replace(Objective, "scaling cost", "cost")) %>%
  group_by(Objective) %>% 
  summarise(Count = n()) %>% 
  filter(Objective != "n/a") %>% 
  mutate("Objective"=toTitleCase(Objective)) %>% 
  mutate(Objective=str_replace(Objective, "Vnf", "VNF")) %>% 
  write.csv(file=paste(data_store, "objectives.csv", sep = ""))

# ------------------------------------- #
#             Venn Diagram              #
# ------------------------------------- #
venn_data <- data.frame("FGE"=survey_data$VNF.FGE, "CC"=survey_data$VNF.CC, "SCH"=survey_data$VNF.SCH, "Coordinated"=survey_data$Coordinated)

FGE <- which(venn_data$FGE == "Yes")
CC <- which(venn_data$CC == "Yes")
SCH <- which(venn_data$SCH == "Yes")
coordinated <- which(venn_data$Coordinated == "Yes")

venn <- list(FGE=FGE, CC=CC, SCH=SCH, Coordinated=coordinated)
ggvenn(venn, 
       stroke_size = 0.1, 
       set_name_size = 5, 
       fill_color = c("#00a3dd", "#009977", "#aa004f", "#e23d28"), 
       fill_alpha = 0.5, 
       stroke_alpha =0.2,
       stroke_linetype = 0)
