clean <- function(x) {
  x %>%
    str_remove_all("\\*.*") %>% #  asterisks
    str_remove("^(about|additional|accompaniments?:)\\s*") %>% # annotations
    str_remove("^an?\\s+") %>% #  leading indefinite article
    str_replace_all("[-—–‑\"\':%;]", " ") %>% # remove various stuff
    str_remove_all("\\(.*?\\)\\s*") %>% # remove parentheses
    str_remove_all("\\[.*?\\]\\s*") %>% # remove brackets
    str_remove_all("\\d+(/\\d+)?(\\.\\d+)?\\s*(or|to)\\s*\\d+(/\\d+)?(\\.\\d+)?\\s?") %>% # number to|or number
    str_remove_all("\\d+(/\\d+)?(\\.\\d+)?\\s*") %>% # number-number
    str_remove_all("^/") %>%
    str_remove("^\\s*(cup|tablespoon|teaspoon|gram|ounce|pound|large|recipe|medium|small|stick|inch|quart|pint|can|gallon|bag|oz|qt|lb|g|tbsp|tsp)s?\\.? ") %>% # measurements
    str_remove("^(,\\s*)+") %>% # left over commas
    str_trim()
}

prep <- function(ingredients, min_num = 1, to_mat = TRUE) {
  # replace nas with regex cleaned version
  nas <- is.na(ingredients$base)
  ingredients$marker <- nas

  ingredients[nas, "base"] <- ingredients$ingredients[nas] %>%
    clean()

  ingredients <- ingredients %>%
    mutate(base = base %>%
      stringi::stri_trans_general(id = "latin-ascii") %>%
      clean()) %>%
    filter(str_length(base) > 0)

  # remove doubles
  dups <- ingredients %>%
    with(!is.na(base) & str_detect(base, "^(.+) \\1$"))

  ingredients[dups, "base"] <- ingredients[dups, "base"] %>%
    mutate(base = str_match(base, "^(.+) \\1$")[, 2]) %>%
    pull(base)

  # combine plurals and singulars
  plurals <- ingredients %>%
    distinct(base) %>%
    filter(str_detect(base, "s$")) %>%
    mutate(s = str_match(base, "^(.*)s$")[, 2]) %>%
    filter(s %in% ingredients$base)

  plu_list <- plurals$s
  names(plu_list) <- plurals$base

  ingredients <- ingredients %>%
    mutate(base = if_else(base %in% names(plu_list), plu_list[.$base], base))

  if (!to_mat) {
    ingredients
  } else {
    ingredients %>%
      distinct(id, base) %>%
      add_count(base) %>%
      filter(n >= min_num) %>%
      select(-n) %>%
      mutate(n = 1) %>%
      pivot_wider(names_from = "base", values_from = n, values_fill = 0) %>%
      select(-id) %>%
      as.matrix()
  }
}
