url <- "https://www.gov.uk/government/publications/ges-technical-framework-2022/1-application-of-knowledge"

req <- httr::GET(url)

content <- httr::content(req) |>
  xml2::xml_find_all("//div[@class = 'govspeak']//ul") |>
  purrr::map(~.x |> xml2::xml_children() |> xml2::xml_text())

heading <- httr::content(req) |>
  xml2::xml_find_all("//div[@class = 'govspeak']//h2") |>
  xml2::xml_text() |>
  stringr::str_remove_all("\n")

loc <- stringr::str_locate(heading, "\\.\\s")[,"start"] + 2
heading <- stringr::str_sub(heading, loc, nchar(heading))


level <- c("Level 1 - Awareness - Apprentice/Placement", 
           "Level 2 – Practitioner: HEO", 
           "Level 2 – Practitioner: SEO", 
           "Level 3 – Expert: G7",
           "Level 3 – Expert: G6",
           "SCS")

heading_level <- purrr::map_df(seq_along(heading), ~{
  dplyr::tibble(heading = heading[.x], heading_index = .x, level, level_index = 1:length(level))
})

tbl <- 1:nrow(heading_level) |> 
  purrr::map_df(~{
    
    content_index <- 1:length(content[[.x]])
    heading_index <- heading_level$heading_index[.x]
    level_index <- heading_level$level_index[.x]
    index <- paste(heading_index, level_index, content_index, sep = ".")
    
    dplyr::tibble(
      content = content[[.x]]
    ) |>
      dplyr::mutate(heading = heading_level$heading[.x]) |>
      dplyr::mutate(level = heading_level$level[.x]) |>
      dplyr::relocate(content, .after= level) |>
      dplyr::mutate(index = index) |>
      dplyr::relocate(index)
    
  })

write.csv(tbl, "posts/ges-competencies/application-of-knowledge.csv", row.names = FALSE)
