library(SPARQL)
library(tidyverse)
library(widyr)

server <- "http://127.0.0.1:3030/SoMeSci/sparql"

query_mentions <- "
PREFIX schema: <http://schema.org/>
PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>

SELECT ?article (year(?date) as ?year) ?journal ?mentiontext ?software ?mentionType

FROM <http://data.gesis.org/softwarekg2>

WHERE{
  ?article a schema:ScholarlyArticle.
   		   
  OPTIONAL {?article dct:isPartOf ?journal;
                     schema:datePublished ?date} .
  OPTIONAL {
    ?article schema:mentions ?mention.
    ?mention nif:isString ?mentiontext;
			 skgv:software ?software.
    OPTIONAL { ?mention skgv:mentionType ?mentionType.}
  }
}"

df_mentions <- SPARQL(url=server, query=query_mentions)$results


query_concept <- "
PREFIX schema: <http://schema.org/>
PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT ?journal ?concept ?topconcept

FROM <http://data.gesis.org/softwarekg2>

WHERE{
  ?journal a bibo:Journal;
           dct:subject/schema:name ?concept;
           dct:subject/skos:hasTopConcept/schema:name ?topconcept.
  
}"

df_concept <- SPARQL(url=server, query=query_concept)$results


query_softwarename <- "
PREFIX schema: <http://schema.org/>
PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

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
ORDER BY ?sw
"
df_softwarename <- SPARQL(url=server, query=query_softwarename)$results


query_softwaretype <- "
PREFIX schema: <http://schema.org/>
PREFIX skgv: <http://data.gesis.org/softwarekg/vocab/>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

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
                  rdf:predicate skgv:softwareType ;
                  skgv:confidence ?ratio .
        }
        GROUP BY ?sw
    }

    ?stmt a rdf:Statement ;
          rdf:subject ?sw ;
          rdf:predicate skgv:softwareType ;
          rdf:object ?name ;
          skgv:confidence ?max_ratio .
}
ORDER BY ?sw"

df_softwaretype <- SPARQL(url=server, query=query_softwaretype)$results


write_csv2(df_mentions, file = "SoMeSci_mentions.csv")
write_csv2(df_concept, file = "SoMeSci_concepts.csv")

df_softwarename %>%
  left_join(df_softwaretype, by = 'sw') %>%
  select(sw=sw, name=name.x, softwaretype=name.y) %>%
  write_csv2(., file = "SoMeSci_software.csv")


