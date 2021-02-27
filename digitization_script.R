library(tidyverse)
library(readxl)

# 1. DATA IMPORT ----------------------------------------------------------
DL_new <- readxl::read_xls("./Data_digitization/DL_new.xls")
DL_repl <- readxl::read_xls("./Data_digitization/DL_Replacement.xls")
RC_new <- readxl::read_xls("./Data_digitization/RC_New.xls")
RC_repl <- readxl::read_xls("./Data_digitization/RC_Replacement.xls")

# 2.  EDA -----------------------------------------------------------------


