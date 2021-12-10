#
# Prepare data ----
#



#
# Packages -----
#

#install.packages('devtools')
#devtools::install_github('kharchenkolab/leidenAlg', build_vignettes = TRUE)


library(tidyverse)

library(igraph)
library(tidygraph)
library(leidenAlg)

library(writexl)
library(npmi)

theme_set(theme_bw(base_size=12))

#
# Read data ----
#

source("settings.R")

kg_concepts <- read_csv2(paste0(path.data,"SoMeSci_concepts.csv")) 
kg_mentions <- read_csv2(paste0(path.data,"SoMeSci_mentions.csv")) 
kg_software <- read_csv2(paste0(path.data,"SoMeSci_software.csv")) 

#
# Prepare data ----
#

kg_software <- kg_software %>% 
  mutate(
    softwaretype = str_remove(softwaretype,"<http://data.gesis.org/softwarekg/vocab/SoftwareType_"),
    softwaretype = str_remove(softwaretype,">$")
  ) %>% 
  rename(software = sw) %>% 
  group_by(software) %>% 
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

#
# Prepare network ----
#

# Edges-List: Co-mentions
com_edges <- kg_mentions %>% 
  select(item=article, feature=software) %>% 
  get_cooccurrence() %>% 
  filter(n > 0, source != target) %>% 
  mutate(weight = p_cond)


# Nodes-List
com_nodes <- kg_mentions %>% 
  distinct(software, .keep_all=T) %>% 
  left_join(distinct(kg_software),by="software") %>% 
  select(id=software, label = name,softwaretype) %>% 
  
  inner_join(
    com_edges %>% 
      pivot_longer(cols=c(source, target)) %>% 
      distinct(value),
    by=c("id"="value")
  ) 

# Create network
gr <- tbl_graph(com_nodes, com_edges, directed = T)


#
# Network analytics ----
#



# Community detection and basic node properties
set.seed(123)
gr <- gr %>% 
  mutate(
    #community_no = as.factor(group_infomap(weights=p_cond)),
    community_no = leiden.community(.,  n.iterations = 100)$membership,
    degree_in = centrality_degree(mode="in", weights=p_cond),
    degree_out = centrality_degree(mode="out", weights=p_cond),
    betweenness = centrality_betweenness(directed=T , weights=p_cond)
  )


# Helper function for calculating mean transitivity in the communities
# Local transitivity = clustering coefficient 
# = how connected are the triads in the network
graph.transitivity <- function(gr) {
  gr <- gr %>%
    activate(nodes) %>%
    mutate(tr = local_transitivity() ) %>%
    as_tibble()
  
  mean(gr$tr)
}


# Structural community properties
gr_communities_netstats <-  gr %>%  
  morph(to_split, community_no) %>% 
  crystallise() %>%  
  mutate(
    community_no = as.factor(row_number()-1),
    
    n_nodes = map_int(graph, gorder),
    n_edges = map_dbl(graph, gsize), 
    density = map_dbl(graph,graph.density), 
    transitivity = map_dbl(graph,graph.transitivity),
    distance = map_dbl(graph,mean_distance),
  ) %>% 
  select(-graph)


# Node based community properties
com_nodes_in <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-degree_in) %>% 
  slice_max(degree_in, n =3) %>% 
  summarize(
    top_degree_in = paste0(label, collapse=";")
  ) %>% 
  ungroup()

com_nodes_out <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-degree_out) %>% 
  slice_max(degree_out, n =3) %>% 
  summarize(
    top_degree_out = paste0(label, collapse=";")
  ) %>% 
  ungroup()

com_nodes_between <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-betweenness) %>% 
  slice_max(betweenness, n =3) %>% 
  summarize(
    top_betweenness = paste0(label, collapse=";")
  ) %>% 
  ungroup()

com_nodes_softwares <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  count(softwaretype) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = softwaretype,
    values_from=c("n","p"),
    values_fill = 0
  )
  

gr_communities  <- gr_communities_netstats %>% 
  left_join(com_nodes_in) %>%
  left_join(com_nodes_out) %>% 
  left_join(com_nodes_between) %>% 
  left_join(com_nodes_softwares)


rm(gr_communities_netstats,com_nodes_in,com_nodes_out,com_nodes_between,com_nodes_softwares)


# Nodes-Liste mit erweiterten Informationen abspeichern 
 com_nodes <- gr %>% 
   as_tibble() 
 
# Plots
 com_nodes %>%  
   group_by(community_no) %>% 
   count(softwaretype) %>% 
   ggplot(aes(x=softwaretype, y=n, fill=softwaretype)) +
   geom_col(stat="identity") +
   facet_wrap(~community_no) +
   coord_flip()

 
# Plot erzeugen: In- und Out-Degree je Community-No je Software-Typ
com_nodes %>%  
  filter(as.numeric(community_no) < 10) %>% 
  pivot_longer(cols=starts_with("degree"), names_to="degree") %>% 
  ggplot(aes(x=softwaretype, y=value, fill=degree)) +
  #ggplot(aes(x=degree, y=value, fill=softwaretype, group = softwaretype)) +
  geom_boxplot() +
  facet_wrap(~community_no) +
  scale_y_log10() +
  coord_flip()
ggsave(paste0(path.data, "../network/degree_by_softwaretype.png"), width=5, height=5, units="cm")

# 
com_nodes %>%  
  filter(as.numeric(community_no) < 10) %>%
  group_by(community_no, softwaretype) %>% 
  slice_sample(n=10) %>% 
  ungroup() %>% 
  pivot_longer(cols=starts_with("degree"), names_to="degree") %>% 
  ggplot(aes(x=softwaretype, y=value, fill=degree)) +
  geom_boxplot() +
  geom_text(aes(label=label)) + 
  facet_wrap(~community_no) +
  scale_y_log10() 


# Abspeichern 
write_csv(com_nodes, paste0(path.data, "../network/com_nodes.csv"))
write_csv(com_edges, paste0(path.data, "../network/com_edges.csv"))



