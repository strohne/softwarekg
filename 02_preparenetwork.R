#
# Prepare data ----
#



#
# Packages -----
#

library(tidyverse)
library(widyr)

library(igraph)
library(tidygraph)
library(ggraph)

library(writexl)
library(npmi)


#
# Read data ----
#

source("settings.R")

kg_concepts <- read_csv2(paste0(path.data,"SoMeSci_concepts.csv")) 
kg_mentions <- read_csv2(paste0(path.data,"SoMeSci_mentions.csv")) 
kg_software <- read_csv2(paste0(path.data,"SoMeSci_software.csv")) 

#
# To network ----
#

kg_software <- kg_software %>% 
  mutate(
    softwaretype = str_remove(softwaretype,"<http://data.gesis.org/softwarekg/vocab/SoftwareType_"),
    softwaretype = str_remove(softwaretype,">$")
  ) %>% 
  group_by(sw) %>% 
  slice_sample(n=1) %>% 
  ungroup()

# Mentions
kg_mentions <- kg_mentions %>% 
  filter(!is.na(software), !is.na(article)) %>% 
  mutate(
    mentionType = str_remove(mentionType,"<http://data.gesis.org/softwarekg/vocab/MentionType_"),
    mentionType = str_remove(mentionType,">$")
  ) %>% 
  group_by(article,year,journal,software) %>% 
  summarize(mentionType = paste0(mentionType, collapse=";")) %>% 
  ungroup() 
  

# Journal topics
kg_concepts <- kg_concepts %>% 
  group_by(journal) %>% 
  summarize(topconcepts = paste0(unique(topconcept),collapse = "; ")) %>% 
  ungroup()

kg_mentions <- kg_mentions %>% 
  left_join(kg_concepts,by="journal") 


# Edges-List: Co-mentions
com_edges <- kg_mentions %>% 
  select(item=article, feature=software) %>% 
  get_cooccurrence() %>% 
  filter(n > 0, source != target) %>% 
  mutate(weight=p_cond)


# Nodes-List
com_nodes <- kg_mentions %>% 
  distinct(software, .keep_all=T) %>% 
  left_join(distinct(kg_software),by=c("software"="sw")) %>% 
  select(id=software, label = name,softwaretype) %>% 
  
  inner_join(
    com_edges %>% 
      pivot_longer(cols=c(source, target)) %>% 
      distinct(value),
    by=c("id"="value")
  ) 




#
# Network ----
#


# Create network
gr_com <- tbl_graph(com_nodes, com_edges, directed = T)


# Group detection alternatives for undirected graphs
# group_edge_betweenness
# infomap.community()
# leiden

#https://www.r-bloggers.com/2012/06/summary-of-community-detection-algorithms-in-igraph-0-6/


gr_com <- gr_com %>% 
  mutate(
    community_no = as.factor(group_infomap(weights=p_cond)),
    graph_degree_in = centrality_degree(mode="in", weights=p_cond),
    graph_degree_out = centrality_degree(mode="out", weights=p_cond),
    graph_betweenness = centrality_betweenness(directed=T , weights=p_cond)
  )
  

# Communities 
# - get nodes by community
# - compute measures
# - combine data
com_communities <- gr_com %>% 
  as_tibble() 

# TODO: count software types, pivot wider

communities_data <-  gr_com %>%  
  morph(to_split, community_no) %>% 
  crystallise() %>%  
  mutate(
    density = map(graph,graph.density), 
    #clique_no = map(graph, count_max_cliques),
    n_edges = map(graph, gsize), 
    n_nodes = map(graph, gorder),
    #recpro = map(graph, reciprocity),
    community_no = as.factor(row_number()),
    #community_degree = map(graph, degree),
    #community_betweenness = map(graph, betweenness)
  )

com_nodes <-com_communities %>% 
  left_join(select(communities_data, -graph), by="community_no") 

rm(com_communities, communities_data)

# Abspeichern 
write_xlsx(com_nodes, "network/com_nodes.xlsx")
write_xlsx(com_edges, "network/com_edges.xlsx")



