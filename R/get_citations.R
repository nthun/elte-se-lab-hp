library(scholar)
library(tidyverse)
library(gt)
library(RefManageR)
library(lubridate)
library(stringi)

tamas_scholar <- "Lec9WuYAAAAJ"
levente_scholar <- "Uw7vdrQAAAAJ"
start_year <- 2023

tamas_pub <- get_publications(tamas_scholar) |> filter(year >= start_year)
levente_pub <- get_publications(levente_scholar) |> filter(year >= start_year)

get_alldata <- function(sid = NULL, start_year = 2023){
    get_publications(id = sid) |> 
        filter(year >= start_year) |> 
        mutate(link = map_chr(pubid, ~get_publication_url(id = sid, pub_id = .x)),
               alldata = map(pubid, ~get_publication_data_extended(id = sid, pub_id = .x))) 
    
}

tamas_pub <- get_alldata(tamas_scholar, start_year = 2023)
levente_pub <- get_alldata(levente_scholar, start_year = 2023)

pubs <- 
    bind_rows(tamas_pub, levente_pub) |> 
    unnest(alldata) |>    
    arrange(-year) |> 
    force()

str_count(pubs$Authors, ",")

paste0(str_extract(pubs$Authors, "^([^,]*,){0,7}[^,]*"), 
      str_extract(pubs$Authors, "..., [^,]*$"))


# Create a gt table with the publication data
pubs |> 
    rowwise() |> 
    mutate(Authors = if_else(str_count(Authors, ",") >= 8, 
                             true = paste0(str_extract(Authors, "^([^,]*,){0,7}[^,]*"), 
                                           ", ..., ",
                                           str_extract(Authors, "[^,]*$")), 
                             false = Authors)) |> 
    transmute(Citation = str_glue("{Authors} ({year}). **{title}.** _{str_to_title(journal)}_, {number}"),
              Abstract = Description,
              Link = link,
              year) |> 
    group_by(year) |> 
    gt() |> 
    fmt_passthrough(Abstract) |> 
    fmt_markdown(Citation) |> 
    fmt_url(Link, as_button = TRUE, label = "Link") |> 
    cols_width(Citation ~ px(300),
               Abstract ~ px(700),
               Link ~ px(70)) |> 
    # gtsave("pubs.html") |> 
    force()



# Create a bibtex file that can be used for the publications

x <-
    pubs |> 
        transmute(bibtype = "Article",
                  # Create a key using the first author, year, and rownumber
                  key = str_match(author, "\\s(.*?)\\,")[,2] |> 
                   str_to_lower() |> 
                   stri_trans_general(id = "Latin-ASCII") |> 
                   paste0(year, "_", row_number()),
               author,
               year,
               title,
               journaltitle = journal,
               number,
               abstract = Description,
               publisher = Publisher,
               url = link
               ) |> 
    # Manually change the name of the journal for two entries (should be fixed on scholar)
        mutate(journaltitle = case_when(journaltitle == "" & str_detect(author, "Oshiro") ~ "OSF Preprints",
                                        journaltitle == "" & str_detect(author, "Terry") ~ "Journal of Open Psychology Data",
                                        TRUE ~ journaltitle)) |> 
    column_to_rownames("key")



WriteBib(as.BibEntry(x), "temp.bib")


