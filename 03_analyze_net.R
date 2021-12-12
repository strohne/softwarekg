#
# Basic description of the co-mention software communities
#

#
# Packages ----
#


library(tidyverse)
library(ggrepel)

theme_set(theme_bw(base_size=12))


# We originally kept our data in a synced folder outside the repo.
# Since paths depend on user & machine, we define them in settings.R.
# Example content of settings.R:
# path.root <- "C:/Users/Jakob/sciebo/Projekte/SoftwareKG/"
# path.data <- paste0(path.root,"data/")
# path.network <- paste0(path.root,"network/")
# path.results <- paste0(path.root,"results/")
# source("settings.R")

path.data <- "data/"
path.network <- "network/"
path.results <- "results/"


colours = c("#F8766D", "#619CFF", "#48d993", "#C77CFF",
            "#999999", "#f2b602", "#0072B2", "#D55E00", 
            "#9abd8c", "#CF2355", "#56B4E9", "#61295c",
            "#009E73","#61295c", "#56B4E9", "#CF2355", 
            "#996953", "#8787e6", "#2E27E6","#F8766D",
            "#619CFF")


#
# Data ----
#

com_communities <- read_csv(paste0(path.network, "com_communities.csv"))
com_nodes <- read_csv(paste0(path.network, "com_nodes.csv"))

#
# Community sizes ----
#

# Using the Leiden algorithm on conditional probabilities of cooccurrences, we found 34 communities:
#   
# - 11 pairs, i.e. communities consisting of2 two nodes each. 
# - 9 small communities constituted by 3 to 20 nodes.
# - 8 mid-sized communities with sizes beetween 21 and 50.
# - 6 large communities containing more than 50 nodes.
com_communities %>% 
  count(n_nodes)

com_communities %>% 
  mutate(group = cut(n_nodes,breaks=c(0,2,20,50,Inf))) %>% 
  count(group)


#
# Density ----
#

# On the basis of community densities (density)
# and local clustering of the nodes (transitivity)
# we see three basic types of communities:
# - The large communities (community number <= 10)
#   have the lowest densities (below 0.25) and transitivity (mostly below 0.7),
#   they integrate diverse softwares.
# - The mid-sized communities have a density around 0.5 and 
#   comparatively high local densities around 0.8.
# - The small communities usually are fully connected with regard to 
#   nodes inside the community (density 1.0). Nevertheless, for
#   some communities we find transitivity scores below 0.8
#   indicating they occasionally are comentioned with different 
#   softwares outside the community.
#
# Against this backdrop, three communities stand out:
#
# No  7: Large community with low transitivity 
#        (STATA, SAS, ArcGIS, see below, generalist softwares).
# No  4: Large community with high density 
#        (BLAST, MEGA, GENEPOP, see below, DNA sequence analysis).
# No 12: mid-sized community with large density and transitivity 
#        (samtools, PolyPhen, edgeR, specialist DNA sequence analysis?)

com_communities %>%
  filter(n_nodes > 2) %>% 
  ggplot(aes(x=density,y=transitivity, fill=n_nodes)) +
  geom_label_repel(aes(label=community_no), color="#FFFFFF") +
  scale_fill_gradient2(low = "#000000", mid="blue", high = "#FF0000")

ggsave(paste0(path.results,"communities_density.png"), width=15, height=10, units="cm")

#
# Degree ----
#

# Since the edge weights were constructed from conditional probabilities,
# comentions are asymmetric, thus, the network is directed. 
#
# A high indegree of a node indicates the software is a dependency of many other softwares.
# A high outdegree of a node indicates the software is dependent on many other softwares.
# In each community, we extracted the top three nodes by indegree, outdegree and betweenness,
# see com_communities.csv.
#
# - The large communities depict different traditions of statistical analysis.
#   The largest community emerges around R, other large communities have
#   Matlab,  Prism, SPSS, Stata or Excel in their center.
#   Some large communities mirror the specific topic of the sample (No 4, BLAST) 
#   or software dependencies (No 80, Windows/Linux/Mac?) 
#   and development setups or workflows (No 8: Python/Virtualbox; No 9: Java/GitHub)
#
# The network plot (see Gephi output) indicates a basic axis in the field of software mentions
# is constituted by a focus on programming on one side (e.g. around Python, R and development tools) vs.
# a focus on using ready made applications (e.g. in the field of DNA analysis) on the other.

com_communities %>% 
  mutate(top_degree_in = str_trunc(top_degree_in,30)) %>%
  mutate(top_degree_out = str_trunc(top_degree_out,30)) %>% 
  select(no=community_no,size=n_nodes, top_degree_in, top_degree_out) %>% 
  print(n=40)


# Example nodes by degree
com_nodes %>%  
  select(community_no,label,softwaretype,starts_with("degree")) %>% 
  pivot_longer(cols=starts_with("degree"), names_to="degree") %>% 
  
  # Determine top & bottom examples
  mutate(label = str_trunc(label,10,ellipsis = "")) %>% 
  
  group_by(community_no, degree) %>%
  slice_sample(prop=1) %>%
  arrange(value) %>% 
  mutate(
    top = (row_number() == 1),
    bottom = (row_number() == n())
  ) %>% 
  ungroup() %>% 
  mutate(label = ifelse(top | bottom, label,NA_character_))  %>% 
  
  
  ggplot(aes(x=degree, y=value, color=softwaretype)) +
  
  geom_boxplot(fill="grey", color="grey", width=0.25) +
  geom_text_repel(aes(label=label),na.rm=T,size=3) + 
  
  scale_y_log10() +
  scale_color_manual(values=colours) +
  
  facet_wrap(~community_no) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    axis.title.x = element_blank()
  )

ggsave(paste0(path.results,"communities_degree.png"), width=30, height=30, units="cm")


#
# Software types ----
#

# Again, no 4 stands out, containing few plugins.
# In No 3 we see a higher share of operating systems and programming environments. 
# No 8 is characterized by a high share of plugins (in the context of Python)
com_nodes %>%  
  count(community_no, softwaretype) %>% 
  group_by(community_no) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup() %>% 
  
  ggplot(aes(y=n,x=community_no,fill=softwaretype)) +
  geom_col()

ggsave(paste0(path.results,"communities_softwaretypes.png"), width=15, height=10, units="cm")

#
# Concepts ----
#


# The sample is dominated by biochemistry, genetics and molecular biology 
# as well as medicine.
#
# Communities No 3, 8 and 9 are more diverse, bridging
# life sciences and computer science.
# Some communities emerge around chemistriy (No 11,13,17, 23).
# Overall, it looks like some disciplines stick to typical software sets.
com_nodes %>% 
  
  # Calculate 
  pivot_longer(
    cols=starts_with("con_"),
    names_to = "concepts_name",
    names_prefix="con_",
    values_to = "concepts_n"
  ) %>% 
  
  group_by(community_no, concepts_name) %>%
  summarize(concepts_n = sum(concepts_n)) %>% 
  ungroup() %>% 
  
  group_by(community_no) %>% 
  mutate(concepts_p= concepts_n / sum(concepts_n)) %>% 
  ungroup() %>% 
  
  # Labels and order
  mutate(concepts_name = fct_reorder(concepts_name,-concepts_p,mean)) %>%
  mutate(concepts_no = as.numeric(concepts_name)) %>% 
  mutate(concepts_name = paste0(concepts_no," ",concepts_name)) %>%
  mutate(concepts_name = fct_reorder(concepts_name,concepts_no,mean)) %>%
  mutate(concepts_label = ifelse(concepts_p > 0.05, concepts_no,"")) %>% 
  
  # Plot
  ggplot(aes(y=concepts_p,x=community_no,fill=concepts_name)) +
  geom_col() +
  geom_text(
    aes(label=concepts_label),
    position = position_stack(vjust = .5),
    size=3, color="white"
  ) +

  scale_fill_manual(values=colours) +
  theme(legend.position = "bottom",legend.title = element_blank())


ggsave(paste0(path.results,"communities_concepts.png"), width=35, height=15, units="cm")

#
# Degrees by software types ----
#


com_nodes %>%  
  filter(as.numeric(community_no) <= 15) %>% 
  pivot_longer(cols=starts_with("degree"), names_to="degree") %>% 
  ggplot(aes(x=softwaretype, y=value, fill=degree)) +

  geom_boxplot() +
  facet_wrap(~community_no, ncol=5) +
#  scale_y_log10() +
  coord_flip()

ggsave(paste0(path.results,"communities_degree.png"), width=30, height=15, units="cm")

