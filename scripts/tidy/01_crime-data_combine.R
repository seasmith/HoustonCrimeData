
# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(tools)
library(lubridate)

not_one_of <- compose(`-`, one_of)



# NOTES -------------------------------------------------------------------

# THE FOLLOWING FILES MUST BE RE-FORMATTED IN EXCEL
# IN ORDER FOR THIS SCRIPT TO PROPERLY WORK. THIS
# IS A MANUAL PROCESS.
#
# THESE FILES MUST HAVE THEIR FILL COLOR SET TO
# 'NO COLOR':
# 
# * jul09
# * nov09
# * sep09
# * sep10
# * oct17
# * sep17



# SCRIPT ------------------------------------------------------------------

# -- 2009-2017
dest <- "data/hou_crime_data"

# Handle expected:1 got: 0 errors from fread()
all_files <- dest %>%
  list.files(full.names = TRUE)

file_cy <- all_files[str_detect(all_files, "17\\.xls$")]
files_pre_cy <- all_files[!str_detect(all_files, "17\\.xls$")]

hou_orig_list <- files_pre_cy %>%
  map(function(x) {
    print(x)
    read_excel(x)
    # if (grepl("jul09\\.xls$", x)) {
    #   print(x)
    #   read_xls(x, range = "A1:J12922")
    # } else if (grepl("nov09\\.xls$", x)) {
    #   print(x)
    #   read_xls(x, range = "A1:J11903")
    # } else if (grepl("sep09\\.xls$", x)) {
    #   print(x)
    #   read_xls(x, range = "A1:J12422")
    # } else if (grepl("sep10\\.xls$", x)) {
    #   print(x)
    #   read_xls(x, range = "A1:J10921")
    # } else {
    #   print(x)
    #   read_xls(x)
    # }
  })

names(hou_orig_list) <- file_path_sans_ext(basename(files_pre_cy))

# -- Clean up tibbles with 10 columns

# Uniqueness of first column name
reorg <- function(dat) {
  map(unique(lens <- lengths(dat)), function(i) {
    dat[lens == i]
  })
}

hou_orig_list <- reorg(hou_orig_list)

# Find unique row-name sets
sh_nms <- hou_orig_list %>%
  modify_depth(2, colnames) %>%
  modify_depth(2, as.matrix) %>%
  map(function(x) t(reduce(x, cbind))) %>%
  map(as.data.frame) %>%
  map(as_tibble) %>%
  map(distinct)

# Get "proper" names
nms_10 <- sh_nms[[1]][1, ] %>% as_vector() %>% as.character()
nms_11 <- sh_nms[[2]] %>% as_vector() %>% as.character()
nms_9  <- sh_nms[[3]][1,] %>% as_vector() %>% as.character()
nms_9[[9]] <- nms_10[[10]]

# Set "proper" names
hou_orig_list[1] <- hou_orig_list[[1]] %>% map(function(x) setNames(x, nms_10)) %>% list()
hou_orig_list[3] <- hou_orig_list[[3]] %>% map(function(x) setNames(x, nms_9)) %>% list()

# Remove "unkown" column X__1
hou_orig_list[2] <- hou_orig_list[[2]] %>% map(function(x) select(x, not_one_of("X__1", "Field11"))) %>% list()

# Add column Premise to tibbles with 9 columns
hou_orig_list[3] <- hou_orig_list[[3]] %>%
  map(function(x) add_column(x, Premise = NA_character_)) %>%
  list()

# Re-assign hou_orig as hou_orig_list
# hou_orig <- hou_orig_list


# hou_orig_list %>%
#   map(function(x) reduce(x, function(...) union_all(...))) %>%
#   reduce(function(...) union_all(...))

# Unionize all tibbles in hou_orig
hou_orig <- hou_orig_list %>%
  modify_depth(1, ~bind_rows(.x, .id = "sheet")) %>%
  bind_rows()
  
  

# -- 2017
# cols <- c("date", rep("text", 8), "numeric")
nms <- names(hou_orig)

hou_orig_cy <- file_cy %>%
  map(~{
    print(.x)
    read_excel(.x, col_names = nms[-1L], skip = 1L)
  })

names(hou_orig_cy) <- file_path_sans_ext(basename(file_cy))

hou_orig_cy <- hou_orig_cy %>%
  map(~mutate_if(.x, is.POSIXct, as.Date)) %>%
  map(~mutate_at(.x, vars(Date), as.Date, format = "%m/%d/%Y")) %>%
  bind_rows(.id = "sheet")

hou_orig_cy <- hou_orig_cy %>%
  mutate(`Offense Type` = if_else(`Offense Type` == "AutoTheft", "Auto Theft", `Offense Type`))

hou_orig <- hou_orig %>%
  mutate_at(vars(Date), as.Date) %>%
  bind_rows(hou_orig_cy)

# -- Remove unecessary objects
rm(list = c("hou_orig_list", "hou_orig_cy"))

save(hou_orig, file = "data/hou_orig.RData")
