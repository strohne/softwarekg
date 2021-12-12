library(tidyverse)
library(SPARQL)
library(digest)
library(xtable)
library(gridExtra)
library(grid)
library(ggplot2)
library(magrittr)

server = 'http://localhost:3030/somesci/sparql' # Apache Jena Fuseki instance

context = "
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX schema: <http://schema.org/>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
PREFIX skg: <http://data.gesis.org/softwarekg/PMC/>
PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
"

# base query to get every article with software and the software's id
# nested queries to gather the most used software names. 
#
# FILTER lines are important -> the dataset is split into 4 parts, creation sentences and plos sentences are specifically 
# sampled for software mentions so they introduce an bias is some analyses.
software_per_article_query = "
SELECT
    ?article
    ?text
    ?sw
    ?name
WHERE{
    ?article schema:mentions ?mention .
    FILTER NOT EXISTS {?article schema:isPartOf <http://data.gesis.org/somesci/Creation_sentences> }
    FILTER NOT EXISTS {?article schema:isPartOf <http://data.gesis.org/somesci/PLoS_sentences>  }
    ?mention nif:isString ?text .
    ?mention skgv:software ?sw .
    {
        SELECT 
            ?sw 
            ?name 
            ?max_ratio
        WHERE
        {
            {
                SELECT 
                ?sw 
                (MAX(?ratio) AS ?max_ratio)
                WHERE
                {
                    ?stmt a rdf:Statement ;
                    rdf:subject ?sw ;
                    rdf:predicate schema:name ;
                    skgv:confidence ?ratio .
                }
                GROUP BY ?sw
            }
            ?stmt a rdf:Statement ;
            rdf:subject ?sw ;
            rdf:predicate schema:name ;
            rdf:object ?name ;
            skgv:confidence ?max_ratio .
    }
}
}"

query = paste0(context, software_per_article_query)

# executing the query
res = SPARQL(url = server, query)$results

# there is more than one name for a software if they are used equally 
# e.g. software SAMtools is present two times: samtools and SAMtools 
# -> both have a weight of 50%, so two values are returned
# here we order the values and only keep the first one. 
res %>% 
    group_by(article, text, sw) %>%
    arrange(name, .by_group=T) %>%
    summarize(name=first(name)) %>%
    ungroup() ->
    res

# get the average number of distinct software used per article.  
res %>%
    group_by(article) %>%
    summarize(sw_count=n_distinct(sw)) %>%
    summarize(mean=mean(sw_count)) -> 
    n_software_per_paper
n_software_per_paper
