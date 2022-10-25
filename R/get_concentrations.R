#' Get Table of Normal Concentrations
#'
#' Retrieve the `Normal Concentrations` table for an accession entry from the Human Metabolome Database (HMDB)
#'
#' @param x a character value of the HMDB accession ID (ie, 000001)
#' @return a `tbl_df` of Normal Concentration information
#' @export
#' @importFrom magrittr %>%

get_concentrations <- function(x)
  {

  xml_address <-
    glue::glue('https://hmdb.ca/metabolites/HMDB', {
      x
    }, '.xml')

  xml_open <- xml2::read_xml(xml_address)

  xml_name <- xml2::xml_find_all(xml_open, 'name') %>% xml2::xml_text()

  xml_find_concentrations <-
    xml2::xml_find_all(xml_open, '//normal_concentrations') %>%
    xml2::xml_children() %>%
    xml2::as_list()


  biospecimen <- purrr::map_chr(xml_find_concentrations,
                                ~ {
                                  .$biospecimen[[1]]
                                })

  concentration_value <- purrr::map(xml_find_concentrations,
                                        ~ {
                                          .$concentration_value
                                        })

  concentration_length <- purrr::map_dbl(concentration_value, length)


  concentration_value[concentration_length == 0] <- NA
  concentration_value <- unlist(concentration_value)


  concentration_units <- purrr::map(xml_find_concentrations,
                                    ~ {
                                      .$concentration_units
                                    })


  concentration_units[concentration_length == 0] <- NA
  concentration_units <- unlist(concentration_units)

  subject_age <- purrr::map(xml_find_concentrations,
                                ~ {
                                  .$subject_age
                                })

  age_length <- purrr::map_dbl(subject_age, length)


  subject_age[age_length == 0] <- NA
  subject_age <- unlist(subject_age)


  subject_sex <- purrr::map(xml_find_concentrations,
                                ~ {
                                  .$subject_sex
                                })



  sex_length <- purrr::map_dbl(subject_sex, length)


  subject_sex[sex_length == 0] <- NA
  subject_sex <- unlist(subject_sex)


  subject_condition <- purrr::map_chr(xml_find_concentrations,
                                ~ {
                                  .$subject_condition[[1]]
                                })


  pubmed <- purrr::map(xml_find_concentrations,
                           ~ {
                             .$references$reference$pubmed_id
                           })

  pubmed_length <- purrr::map_dbl(pubmed, length)

  pubmed[pubmed_length == 0] <- NA
  pubmed <- unlist(pubmed)



  concentration_tibble <-
    tibble::tibble(
      name = xml_name,
      accession = glue::glue('HMDB', {
        x
      }),
      biospecimen = biospecimen,
      concentration_value = concentration_value,
      concentration_units = concentration_units,
      subject_age = subject_age,
      subject_sex = subject_sex,
      subject_condition = subject_condition,
      pubmed = pubmed
    )



  return(concentration_tibble)


}
