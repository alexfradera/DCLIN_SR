# helper script to understand what led to exclusions at full-text.
# not part of wider data flow.

# exclusions
exclu <- tibble::tribble(
                      ~reason,
            "no chronic pain",
       "no effective control",
                   "confound",
                     "repeat",
                   "confound",
       "no effective control",
       "no effective control",
       "no effective control",
                     "repeat",
            "incomplete data",
            "incomplete data",
            "no chronic pain",
            "incomplete data",
                     "repeat",
            "no chronic pain",
            "no chronic pain",
            "no chronic pain",
  "screen for screening only",
            "no chronic pain",
         "ill-defined groups",
         "ill-defined groups",
            "incomplete data",
            "no chronic pain",
            "no chronic pain",
            "no chronic pain",
            "incomplete data",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
         "ill-defined groups",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
  "screen for screening only",
         "ill-defined groups",
         "no relevant screen",
         "no relevant screen",
         "no relevant screen",
       "no effective control",
       "no effective control",
       "no effective control",
       "no effective control",
         "ill-defined groups",
         "no relevant screen",
         "ill-defined groups",
         "no relevant screen",
       "no effective control",
         "ill-defined groups",
            "no chronic pain",
            "no chronic pain",
            "no chronic pain",
         "no relevant screen",
            "incomplete data",
       "no effective control",
       "no effective control",
            "no chronic pain",
            "no chronic pain",
                   "confound",
                   "confound",
                   "confound",
         "ill-defined groups",
            "no chronic pain",
            "no chronic pain",
       "no effective control",
       "no effective control",
       "no effective control",
       "no effective control",
                   "confound",
       "no effective control",
                     "repeat",
            "incomplete data",
                     "repeat",
                     "repeat",
                   "confound",
         "no relevant screen",
         "no relevant screen",
         "no relevant screen",
         "no relevant screen",
            "no chronic pain"
  )
exclu %>%
  group_by(reason) %>%
  summarise(
    count = n()
  )


library(flextable)
library(officer)
library(magrittr)
my_df = data.frame(col_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                   col_b = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'))
my_tb = regulartable(my_df)
my_doc =
  read_docx() %>%
  body_add_par('Two Column Table', style = "centered") %>%
  body_end_section_continuous() %>%
  body_add_flextable(my_tb, align = "center") %>%
  body_end_section_columns(widths = rep(2.9, 2), space = .5)
print(my_doc, target = 'test.docx') %>% browseURL()


my_doc =
  read_docx() %>%
  body_add_par('One Column Table', style = "centered") %>%
  body_add_flextable(my_tb, align = "center") %>%
  body_add_par('') %>%
  body_add_par('') %>%
  body_add_par('Two Column Table', style = "centered") %>%
  body_end_section_continuous() %>%
  body_add_flextable(my_tb, align = "center") %>%
  body_end_section_columns(widths = rep(1.5, 2),
                           space = .5)
print(my_doc, target = 'test.docx')
