# This is a run file for generating the outputs for:
# 
# The growing effect of job demands on teacher mental health: results from a 
# longitudinal national household panel survey

# the current directory of this file
home <- dirname(rstudioapi::getSourceEditorContext()$path)

# JOEM submission ####
# 
#### Render main docx
rmarkdown::render(
  paste0(home, "/src/Word JOEM.Rmd"),
  output_file = "MORRIS.docx",
  output_dir = paste0(home, "/JOEM/")
)

#### Render figures.pdf
rmarkdown::render(
  paste0(home, "/src/Figures JOEM.Rmd"),
  output_file = "Figures.pdf",
  output_dir = paste0(home, "/JOEM/")
)

#### Render appendix.pdf
rmarkdown::render(
  paste0(home, "/src/Supplemental JOEM.Rmd"),
  output_file = "Supplemental MORRIS.pdf",
  output_dir = paste0(home, "/JOEM/")
)

# LCC working paper series ####
# 
#### Render main pdf for LCC Working paper series
rmarkdown::render(
  paste0(home, "/src/Word JOEM.Rmd"),
  output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
  output_file = "Modern teachers.pdf",
  output_dir = paste0(home, "/LCC working paper/")
)


# Github-pages ####
occupation_key = c(
  # `2310` = "College",
  `2320` = "Secondary",
  `2331` = "Primary",
  # `2332` = "Pre-primary",
  # `2340` = "Special ed",
  # `2359` = "Other teaching n.e.c."
  .default = NA_character_
)

occupation_key = c(
  `2142` = "Engineer",  #"Civil",
  `2143` = "Engineer",  #"Electrical",
  `2144` = "Engineer",  #"Electronics and telecommunications",
  `2147` = "Engineer",  #"Mining, metallurgists and related",
  `2149` = "Engineer",  #"Architects, engineers and related",
  `3110` = "Technicians",  #"Physical and engineering science technicians",
  `3114` = "Technicians",  #"Electronics and telecommunications technicians",
  `3119` = "Technicians",  #"Technicians n.e.c.",
  .default = NA_character_
)

occupation_key = c(
  `2320` = "Teachers",
  `2331` = "Teachers",
  `2230` = "Nurses",
  `2411` = "Accountants",
  # `2000` = "Professionals",
  .default = NA_character_
)


rmarkdown::render(
  paste0(home, "/src/Report.Rmd"),
  params = list(key = occupation_key),
  output_file = "index.html",
  output_dir = paste0(home, "/docs"))



