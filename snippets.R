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