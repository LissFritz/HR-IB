
```{r benchmark_extract}
#| eval: FALSE 
# Alle Excel-Dateien im Ordner auflisten
excel_files <- list.files("C:/Users/Uwu14/Desktop/R-Project/CHRB-2017", pattern = "\\.xls$", full.names = TRUE)

# Funktion zum Extrahieren der relevanten Daten aus jeder Excel-Datei
extract_data <- function(file_path) {
  # Excel-Datei laden
  sheet_data <- read_excel(file_path, range = "B11:B13", col_names = FALSE)
  
  # Extrahieren des Unternehmensnamens und des Scores
  company_name <- sheet_data[1, 1]  # B11 (Unternehmensname)
  score <- sheet_data[3, 1]  # B13 (Score)
  
  # Rückgabe als DataFrame
  data.frame(company = company_name, overall_score = score, stringsAsFactors = FALSE)
}

# Wenden der Funktion auf alle Excel-Dateien an
score_data_list <- map(excel_files, extract_data)

# Kombinieren der extrahierten Daten in einem einzelnen DataFrame
final_data <- bind_rows(score_data_list)

# Speichern der extrahierten Daten in einer neuen Excel-Datei
write_xlsx(final_data, "extrahierte_scorecards_2017.xlsx")

```

```{r benchmark_read}

#| eval: FALSE 
data2017 <- read_excel("extrahierte_scorecards_2017.xlsx", sheet = 1, col_names = FALSE)

# Entfernen der ersten Zeile, die die Spaltennamen enthält
data2017 <- data2017[-1, ]

# Manuell Spaltennamen setzen
colnames(data2017) <- c("company", "score17")


data2018 <- read_excel("WBA-CHRB-2018-results-data.xlsx", sheet = 2) %>% 
  select(CompanyName, Sector, `Total Rounded Score`) %>%
  rename(
    company = CompanyName, sector =  Sector, score18 = `Total Rounded Score`
  )

data2018 <- head(data2018, -4) # notes am ende entfernen

data2019 <- read_excel("CHRB2019Data.xlsx", sheet = 2) %>% 
  select(`Company Name`, `Company Sector`, `2019 Total Rounded Score`, `Headquarter country`) %>%
  rename(
    company = `Company Name`, sector =  `Company Sector`, score19 = `2019 Total Rounded Score`, hq = `Headquarter country`
  )

data2019 <- head(data2019, -4) # notes am ende entfernen


data2020 <- read_excel("WBA-CHRB-2020-Results-Data-.xlsx", sheet = 3) %>% 
  select(`Company Name`, `Company Sector`, `Total Rounded Score`) %>%
  rename(
    company = `Company Name`, sector =  `Company Sector`, score20 = `Total Rounded Score`
  )


data2022 <- read_excel("CHRB-2022-Dataset_updated19July2023.xlsx", sheet = 2, range =  "A3:O130") %>% 
  select(`Company Name`, `Company Sector`, `Rounded Total`) %>%
  rename(
    company = `Company Name`, sector =  `Company Sector`, score22 = `Rounded Total`
  )


data2023 <- read_excel("CHRB_2023_final_dataset_13Mar2024.xlsx", sheet = 2) %>%
  select(Company, Sector, `Rounded Total`, `HQ country`) %>%
  rename(
    company = Company, sector =  Sector, score23 = `Rounded Total`, hq = `HQ country`
  )

data2023 <- head(data2023, -2) # notes am ende entfernen
```

```{r benchmark_combination}
#| eval: FALSE 
combined_data <- data2018 %>%
  full_join(data2019, by = c("company", "sector")) %>%
  full_join(data2020, by = c("company", "sector")) %>%
  full_join(data2022, by = c("company", "sector")) %>%
  full_join(data2023, by = c("company", "sector"))  %>%
  full_join(data2017, by = c("company")) %>%
  mutate(hq = coalesce(hq.x, hq.y)) %>% 
  select(company, sector, hq, score17, score18, score19, score20, score22, score23) %>% 
  mutate(hq = recode(hq, 
                     "United States of America" = "USA", 
                     "United Kingdom" = "UK"))

combined_data$score18 <- round(combined_data$score18, 1)


#write_xlsx(combined_data, "combined_data.xlsx")

# manuelle Manipulation von doppels: 

combined_manipulated <- read_excel("combined_data (1).xlsx")

```

```{r benchmark_visualization}

combined_manipulated <- read_excel("combined_data (1).xlsx")

combined_data_heatmap <- combined_manipulated %>%
  mutate(across(starts_with("score"), ~ ifelse(!is.na(.), 1, 0)))
# Umwandeln der Daten in Long-Format für ggplot
combined_data_heatmap_long <- combined_data_heatmap %>%
  pivot_longer(cols = starts_with("score"), names_to = "year", values_to = "data_present") %>%
  mutate(year = gsub("score", "", year))  # Entfernen des Präfixes "score"

companies_with_data_2023 <- combined_data_heatmap_long %>%
  filter(year == "23" & data_present == 1) %>%
  pull(company)

filter23 <- combined_data_heatmap_long %>%
  filter(company %in% companies_with_data_2023)

ggplot(filter23, aes(x = year, y = company, fill = factor(data_present))) +
  geom_tile() + 
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +  # Weiß für keine Daten, Blau für Daten
  labs(title = "Heatmap: Datenverfügbarkeit pro Jahr für jedes Unternehmen",
       x = "Jahr",
       y = "Unternehmen",
       fill = "Daten vorhanden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r benchmark_final}
# Auswählen von den beiden überprüfungsjahren
comparison_data <- combined_manipulated %>% 
  filter(!is.na(score19) & !is.na(score23)) %>% 
  select(c(company, sector, hq, score19, score23))

#View(comparison_data)
```
