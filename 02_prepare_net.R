#
# Prepare co-mention network ----
#

# Results in three csv files:
# com_nodes contains the node list
# com_edges contains the edge list
# com_communities contains communities and their properties

#
# Packages -----
#

# install.packages('devtools')

# devtools::install_github("strohne/npmi", build_vignettes = TRUE)
# devtools::install_github('kharchenkolab/leidenAlg', build_vignettes = TRUE)


library(tidyverse)

library(igraph)
library(tidygraph)
library(leidenAlg)

library(npmi)

#
# Read data ----
#


# We kept our data in a synced folder outside the repo.
# Since paths depend on user & machine, we define them in settings.R.
# Example content of settings.R:
# path.root <- "C:/Users/Jakob/sciebo/Projekte/SoftwareKG/"
# path.data <- paste0(path.root,"data/")
# path.network <- paste0(path.root,"network/")
source("settings.R")

kg_concepts <- read_csv2(paste0(path.data,"SoMeSci_concepts.csv")) 
kg_mentions <- read_csv2(paste0(path.data,"SoMeSci_mentions.csv")) 
kg_software <- read_csv2(paste0(path.data,"SoMeSci_software.csv")) 

#
# Prepare data ----
#


# Mentions
kg_mentions <- kg_mentions %>% 
  filter(!is.na(software), !is.na(article)) %>% 
  rename(mentiontype = mentionType) %>% 
  mutate(
    mentiontype = str_remove(mentiontype,"<http://data.gesis.org/softwarekg/vocab/MentionType_"),
    mentiontype = str_remove(mentiontype,">$"),
    mentiontype = str_replace_all(str_to_lower(mentiontype),"[^a-z]+","_")
  ) %>% 
  group_by(article,year,journal,software) %>% 
  summarize(mentiontypes = paste0(unique(mentiontype), collapse=";")) %>% 
  ungroup() 


# Journal topics
kg_concepts <- kg_concepts %>% 
  group_by(journal) %>% 
  mutate(topconcept = str_replace_all(str_to_lower(topconcept), "[^a-z]+", "_")) %>%
  summarize(topconcepts = paste0(unique(topconcept),collapse = ";")) %>% 
  ungroup()

# Software
kg_software <- kg_software %>% 
  mutate(
    softwaretype = str_remove(softwaretype,"<http://data.gesis.org/softwarekg/vocab/SoftwareType_"),
    softwaretype = str_remove(softwaretype,">$")
  ) %>% 
  rename(software = sw) %>% 
  group_by(software) %>% 
  slice_sample(n=1) %>% 
  ungroup()


# Mentions: join concepts
kg_mentions <- kg_mentions %>% 
  left_join(kg_concepts,by="journal") 


# Softwares: join topconcepts
concepts_wide <- kg_mentions %>% 
  separate_rows(topconcepts,sep=";") %>% 
  count(software,topconcepts, sort=T) %>% 
  pivot_wider(
    names_from=topconcepts, 
    values_from = n, 
    values_fill=0,
    names_prefix = "con_"
  )

kg_software <- kg_software %>% 
  left_join(concepts_wide, by="software")

rm(concepts_wide)

# Softwares: join mentiontypes
mentiontypes_wide <- kg_mentions %>% 
  separate_rows(mentiontypes,sep=";") %>% 
  count(software,mentiontypes, sort=T) %>% 
  pivot_wider(
    names_from=mentiontypes, 
    values_from = n, 
    values_fill=0,
    names_prefix = "mt_"
  )

kg_software <- kg_software %>% 
  left_join(mentiontypes_wide, by="software")

rm(mentiontypes_wide)


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
com_nodes <- com_edges %>% 
  pivot_longer(cols=c(source, target), values_to="id") %>% 
  distinct(id) %>% 
  left_join(kg_software, by=c("id"="software")) %>% 
  select(id, label=name, everything())


#
# Network analytics ----
#


# Create network
gr <- tbl_graph(com_nodes, com_edges, directed = T)

# Community detection and basic node properties
set.seed(1852)
gr <- gr %>% 
  mutate(
    #community_no = as.factor(group_infomap(weights=p_cond)),
    community_no = as.numeric(leiden.community(.,  n.iterations = 100)$membership),
    degree_in = centrality_degree(mode="in", weights=p_cond),
    degree_out = centrality_degree(mode="out", weights=p_cond),
    betweenness = centrality_betweenness(directed=T , weights=p_cond)
  )

# Update node list
com_nodes <- gr %>% 
  as_tibble() 

#
# Community properties ----
#

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
com_communities_netstats <-  gr %>%  
  morph(to_split, community_no) %>% 
  crystallise() %>%  
  mutate(
    community_no = row_number(),
    
    n_nodes = map_int(graph, gorder),
    n_edges = map_dbl(graph, gsize), 
    density = map_dbl(graph, graph.density), 
    transitivity = map_dbl(graph, graph.transitivity),
    distance = map_dbl(graph, mean_distance),
  ) %>% 
  select(-graph)


# Node based community properties
# - top 3 nodes by indegree
com_nodes_in <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-degree_in) %>% 
  slice_max(degree_in, n =3) %>% 
  summarize(top_degree_in = paste0(label, collapse=";")) %>% 
  ungroup()

# - top 3 nodes by outdegree
com_nodes_out <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-degree_out) %>% 
  slice_max(degree_out, n =3) %>% 
  summarize(top_degree_out = paste0(label, collapse=";")) %>% 
  ungroup()

# - top 3 nodes by betweenness
com_nodes_between <- gr %>% 
  as_tibble() %>% 
  group_by(community_no) %>% 
  arrange(-betweenness) %>% 
  slice_max(betweenness, n =3) %>% 
  summarize(top_betweenness = paste0(label, collapse=";")) %>% 
  ungroup()

# - share of software types in community
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
  
# - share of journal topics in community
com_nodes_concepts <- gr %>% 
  as_tibble() %>% 
  pivot_longer(
    cols=starts_with("con_"),
    names_to = "concepts_name",
    names_prefix="con_",
    values_to = "concepts_n"
  ) %>% 
  group_by(community_no) %>% 
  mutate(concepts_p = concepts_n / sum(concepts_n)) %>% 
  slice_max(concepts_n,n=3) %>% 
  summarize(
    topconcepts_names = paste0(concepts_name, collapse=";"),
    topconcepts_p = paste0(round(concepts_p,2), collapse=";")
  ) %>% 
  ungroup()


com_communities  <- com_communities_netstats %>% 
  left_join(com_nodes_in) %>%
  left_join(com_nodes_out) %>% 
  left_join(com_nodes_between) %>% 
  left_join(com_nodes_softwares) %>% 
  left_join(com_nodes_concepts)

rm(com_communities_netstats,com_nodes_in,com_nodes_out,com_nodes_between,com_nodes_softwares, com_nodes_concepts)


#
# Save ----
#

write_csv(com_communities, paste0(path.network, "com_communities.csv"))
write_csv(com_nodes, paste0(path.network, "com_nodes.csv"))
write_csv(com_edges, paste0(path.network, "com_edges.csv"))



