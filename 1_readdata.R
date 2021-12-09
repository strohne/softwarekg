#
# Extract KnowledgeKG data from JSONLD
#


# See https://data.gesis.org/somesci/
# See https://arxiv.org/abs/2108.09070
# See https://github.com/f-krueger/SoftwareKG-PMC-Analysis/blob/main/SoftwareKG_PMC_Analyses.ipynb



#
# Packages -----
#

library(tidyverse)
library(rdflib)
#library(jsonld)

library(widyr)

library(igraph)
library(tidygraph)
library(ggraph)

library(readxl)
library(writexl)

#
# Read data ----
#

rdf <- rdf_parse("data/SoMeSci_with_SoftwareKG_datamodel.jsonld",format = "jsonld")
rdf


query_context <-  "
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX schema: <http://schema.org/>
  PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
  PREFIX skg: <http://data.gesis.org/softwarekg/PMC/>
  PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX bibo: <http://purl.org/ontology/bibo/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
"

kg_mentions <- rdf_query(
  rdf,
  paste0(
    query_context,
    "SELECT ?journal ?article ?text ?sw ?sw_type  ?mentiontype ?mention
     FROM <http://data.gesis.org/softwarekg2> 
                         
     WHERE{
      ?article schema:mentions ?mention .
      ?mention skgv:mentionType ?mentiontype .
      ?mention nif:isString ?text .
      ?mention skgv:software ?sw .
      ?mention skgv:softwareType ?sw_type .
      ?article dct:isPartOf ?journal .
    }"
  )
)


# Get software ids and labels
kg_softwares <- rdf_query(
  rdf,
  paste0(
    query_context,
    "SELECT ?sw ?label ?confidence
    FROM <http://data.gesis.org/softwarekg2>
    WHERE
    {
        ?name rdf:subject ?sw .
        ?name rdf:object ?label .
        ?name skgv:confidence ?confidence .
    }
    "
    )
  ) %>% 
  group_by(sw) %>% 
  arrange(-confidence) %>% 
  slice_head(n=1) %>% 
  ungroup()


kg_labels <- rdf_query(
  rdf,
  paste0(
    query_context,
    "SELECT ?id ?label
    FROM <http://data.gesis.org/softwarekg2>
    WHERE
    {
        ?id schema:name  ?label .
    }
    ")
) 

kg_domains <- rdf_query(
  rdf,
  paste0(
    query_context,
    "SELECT ?journal ?subject
    FROM <http://data.gesis.org/softwarekg2>
    WHERE
    {
      ?journal dct:subject ?subject .

    }
    ")
)


#
# Prepare data ----
#

# Edges-List: Co-mentions
com_edges <- kg_mentions %>% 
  pairwise_count(sw,article,upper=F) %>% 
  select(source=item1,target=item2,weight=n)

# Nodes-List
com_nodes <- kg_mentions %>% 
  select(id=sw, label=text, sw_type) %>% 
  distinct(id, .keep_all=T) %>% 
  left_join(select(kg_softwares,id=sw,label_desambiguated=label),by="id") %>% 
  
  
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
gr_com <- tbl_graph(com_nodes, com_edges, directed = FALSE)
gr_com


# Add measures to graph
# - detect communities (Louvain-Algorithm)
# - compute centrality measures
gr_com <- gr_com %>% 
  mutate(
    community_no = as.factor(group_louvain()),
    graph_degree = centrality_degree(),
    graph_betweenness = centrality_betweenness()
  )

# Communities 
# - get nodes by community
# - compute measures
# - combine data
com_communities <- gr_com %>% 
  as_tibble() 

communities_data <-  gr_com %>%  
  morph(to_split, community_no) %>% 
  crystallise() %>%  
  mutate(
    density = map(graph,graph.density), 
    clique_no = map(graph, count_max_cliques),
    n_edges = map(graph, gsize), 
    n_nodes = map(graph, gorder),
    recpro = map(graph, reciprocity),
    community_no = as.factor(row_number()),
    community_degree = map(graph, degree),
    community_betweenness = map(graph, betweenness)
  )

com_nodes <-com_communities %>% 
  left_join(select(communities_data, -graph), by="community_no") 

rm(com_communities, communities_data)

# Abspeichern 
write_xlsx(com_nodes, "network/com_nodes.xlsx")
write_xlsx(com_edges, "network/com_edges.xlsx")


#
# Snippets ----
#


query_predicates <- paste0(
  query_context,
  "SELECT DISTINCT ?Concept FROM <http://data.gesis.org/softwarekg2> 
  WHERE 
  { 
    ?a ?Concept ?b 
  }")


predicates <- rdf_query(rdf, query_predicates)


#                      
## Var2 
#tmp <-  gr_comentions %>%  
#  morph(to_split, community_no) 
#  
#tmp <- map_df(tmp$graph, function(subgraph) {
#  tibble(
#    dens = graph.density(subgraph),
#  )
#} )
#
#
## Var3
#tmp <- lapply(unique(com$community_no), 
#              function(no) {
#                
#                subgraph <- gr_comentions %>% 
#                  activate("nodes") %>% 
#                  filter(community_no == no)
#
#                tibble(
#                  community_no = no,
#                  dens = graph.density(subgraph),
#                  
#                )
#                
#              }
#            )
#
#morph()
#tmp <- do.call(rbind,tmp)
#


#gr_comentions %>% 
#  filter(community_no == 2) %>% 
#
#  ggraph() + 
#  geom_edge_link() + 
#  geom_node_label(aes(label = value)) + 
#  coord_fixed() +
#  theme_void() 
#
