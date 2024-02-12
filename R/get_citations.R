library(scholar)
library(dplyr)
library(purrr)
library(tidyr)
library(gt)
library(glue)

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

pubs |> 
    rowwise() |> 
    transmute(Citation = glue("{Authors} ({year}). {title}, {journal}, {number}"),
              Abstract = Description,
              Link = link,
              year) |> 
    group_by(year) |> 
    gt() |> 
    fmt_passthrough(Abstract) |> 
    fmt_url(Link, as_button = TRUE, label = "Link") |> 
    cols_width(Citation ~ px(300),
               Abstract ~ px(700),
               Link ~ px(70)) |> 
    gtsave("pubs.html")

