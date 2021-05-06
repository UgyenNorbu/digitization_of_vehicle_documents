library(tidyverse)
library(readxl)
library(lubridate)
library(directlabels)
library(ggrepel)

# 1. DATA IMPORT ----------------------------------------------------------

DL_new <- readxl::read_xls("./Data_digitization/DL_new.xls")
DL_repl <- readxl::read_xls("./Data_digitization/DL_Replacement.xls")
RC_new <- readxl::read_xls("./Data_digitization/RC_New.xls")
RC_repl <- readxl::read_xls("./Data_digitization/RC_Replacement.xls")
budget <- readxl::read_xlsx("./Data_digitization/Budget.xlsx")

# 2.  Budget for printing -----------------------------------------------------------------

str(budget)

budget %>% 
  ggplot(aes(x = Year, y = Budget)) +
  geom_point(size = 3, color = "#104E8B") +
  geom_line(aes(group = 1), color = "#1874CD") +
  ylim(0, 15) +
  labs(y = "Alloated budget (in million)",
       x = "") +
  theme_minimal() +
  theme(axis.text = element_text(family = "serif", size = 10, hjust = 0, vjust = 2),
        axis.text.x = element_text(angle = 15),
        axis.title = element_text(family = "serif", size = 12)) 
ggsave("budget.jpg", width = 20, height = 15, units = "cm")  
  
# DL new summary ----------------------------------------------------------
str(DL_new$Issue_Date)
sum_DL_new <- DL_new %>% 
  group_by(month=floor_date(Issue_Date, "month")) %>%
  summarise(total = n())

writexl::write_xlsx(sum_DL_new, "sum_DL_new.xlsx")

# DL replacement summary --------------------------------------------------
str(DL_repl$Duplication_Date)

sum_DL_repl <- DL_repl %>% 
  group_by(month=floor_date(Duplication_Date, "month")) %>%
  summarise(total = n())

writexl::write_xlsx(sum_DL_repl, "sum_DL_rep.xlsx")

# RC new summary ----------------------------------------------------------
str(RC_new$Receipt_Date)

sum_RC_new <- RC_new %>% 
  group_by(month=floor_date(Receipt_Date, "month")) %>%
  summarise(total = n())

writexl::write_xlsx(sum_RC_new, "sum_RC_new.xlsx")

# RC replacement summary --------------------------------------------------

sum_RC_repl <- RC_repl %>% 
  group_by(month=floor_date(Duplicate_Issue_Date, "month")) %>%
  summarise(total = n())

writexl::write_xlsx(sum_RC_repl, "sum_RC_rep.xlsx")

# Merge all ---------------------------------------------------------------

final_summary <-full_join(sum_DL_new, sum_DL_repl, by= "month") %>% 
  full_join(.,  sum_RC_new, by= "month") %>% 
  full_join(., sum_RC_repl, by = "month")

colnames(final_summary) <- c("month", "DL_new", "DL_repl", "RC_new", "RC_repl")

writexl::write_xlsx(final_summary, "final_summary.xlsx")

# Plotting transaction ----------------------------------------------------------------
transaction <- readxl::read_xlsx("./final_summary.xlsx")
transaction$month <- as.Date(transaction$month)

transaction %>% 
  pivot_longer(-month, names_to = "type", values_to = "number") %>%
  ggplot(aes(x = month, y = number, color = type)) +
  geom_line() +
  ylim(0, 1700) +
  labs(x = "", 
       y = "No. of transactions") +
  geom_dl(aes(label = type),
          method = list(dl.combine("last.points"))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15)) +
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%Y %m", 
               limits = c(as.Date("2019-07-01"), 
                          as.Date("2021-03-01"))) +
  geom_vline(xintercept = as.Date("2020-8-11"), linetype="dashed", 
             color = "#999696", size= 0.7) +
  geom_vline(xintercept = as.Date("2020-9-5"), linetype="dashed", 
             color = "#999696", size= 0.7) +
  geom_vline(xintercept = as.Date("2020-12-20"), linetype="dashed", 
             color = "#999696", size= 0.7) +
  geom_vline(xintercept = as.Date("2021-1-29"), linetype="dashed", 
             color = "#999696", size= 0.7) +
  annotate(geom="text", x= as.Date("2020-8-25"), y= 1500, label="Lockdown 1.0",
           color="#474747", size = 4) +
  annotate(geom="text", x= as.Date("2021-1-10"), y= 1500, label="Lockdown 2.0",
           color="#474747", size = 4)

ggsave("transaction.jpg", width = 23, height = 13, units = "cm")  

