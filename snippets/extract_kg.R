#
# Extract KnowledgeKG data from JSONLD
#

# Alternative for extracting data from JSONLD using rdflib.
# Missing OPTIONAL statements in queries
# -> DO NOT USE, see 01_create_csv.R


# See https://data.gesis.org/somesci/
# See https://arxiv.org/abs/2108.09070
# See https://github.com/f-krueger/SoftwareKG-PMC-Analysis/blob/main/SoftwareKG_PMC_Analyses.ipynb



#
# Packages -----
#

library(tidyverse)
library(rdflib)

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
