library(shiny)
library(shinyjs)
library(tidyverse)
library(readxl)
library(openxlsx)

ui <- fluidPage(
  includeCSS("www/css_mIBI_calc.css"),
  htmlTemplate("www/html_mIBI_calc.html"),
  useShinyjs()
)

MASTER7 <- read_excel("\\\\Illinois.gov\\EPA\\DesPlainesShared\\BOW\\Bugs\\MIBI_Calc\\mIBI R Shiny app\\www\\master7_copy.xlsx",
                      col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

#select necessary columns from Master7 to create Master7_subset
Master7_subset <- MASTER7 %>%
  select(ID, BIOSNAME, TOLERANCE, FFG, ORDER, FAMILY, GENUS, GROUP, TRIBE_ETC, CLASS, PHYLUM)

server <- function(input, output, session) {
  mIBI_thing <- reactive({
    
    AWQMS_data <- read_excel(input$upload$datapath, skip = 1)
    
    #SMU data has an asterisk next to the taxon name to denote rare taxa, so that needs to be factored in
    
    AWQMS_for_mIBI <- AWQMS_data %>%
      filter(`Tolerance Value` != 99.9) %>% 
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, `Equipment ID`, `Characteristic Name`,
             `Result Value`, `Result Unit`, `Taxonomic Name`, `Biological Individual ID`) %>%
      rename(ID = `Biological Individual ID`) %>%
      mutate(ID = as.numeric(ID)) %>% 
      #join bug data and Master7_subset by ID column
      left_join(Master7_subset, by = c("ID" = "ID")) %>%
      #select necessary columns
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, `Equipment ID`, ID, BIOSNAME, `Result Value`,
             TOLERANCE, FFG, ORDER, FAMILY, GENUS, TRIBE_ETC, CLASS, PHYLUM) %>%
      #all data filters
      filter(is.numeric(`Result Value`),
             `Result Value` > -1,
             !is.na(`Monitoring Location ID`), !is.na( `Activity Start Date`), !is.na(`Activity Start Time`),
             !is.na(`Equipment ID`), `Equipment ID` == "20-jab") %>%  
      #standardize taxa/mIBITUs
      #specify class/family for relevant taxa
      mutate(StandardizedTaxa = case_when(CLASS %in% c("TURBELLARIA", "Turbellaria") ~ "TURBELLARIA",
                                          CLASS %in% c("OLIGOCHAETA", "Oligochaeta") ~  "OLIGOCHAETA",
                                          CLASS %in% c("HIRUDINEA", "Hirudinea") ~ "HIRUDINEA",
                                          CLASS %in% c("PISIDIIDAE", "Pisidiidae") ~ "SPHAERIIDAE", 
                                          FAMILY %in% c ("SPHAERIIDAE", "Sphaeriidae") ~ "SPHAERIIDAE",
                                          FAMILY %in% c("UNIONIDAE", "Unionidae") ~ "UNIONIDAE",
                                          FAMILY %in% c("CAMBARIDAE", "Cambaridae") ~ "CAMBARIDAE")) %>%
      #assign all remaining taxa to be standardized by genus
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), GENUS,  StandardizedTaxa)) %>% 
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), TRIBE_ETC, StandardizedTaxa)) %>%
      #if genus not available, standardize by family
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), FAMILY, StandardizedTaxa)) %>%
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), ORDER, StandardizedTaxa)) %>%
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), CLASS, StandardizedTaxa)) %>%
      mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), PHYLUM, StandardizedTaxa)) %>%
      select(-BIOSNAME) %>% 
      rename(BIOSNAME = StandardizedTaxa) %>% 
      mutate(BIOSNAME = as.character(BIOSNAME)) %>% 
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, BIOSNAME, `Result Value`) %>%
      left_join(Master7_subset)
    
    #Checking for atypical taxonomic resolution exceptions where genus isn't specified but family is
    df_tribe <- AWQMS_for_mIBI %>%
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #keep rows where GENUS is listed as NA
      filter(is.na(GENUS)) %>%
      #keep rows where FAMILY is NOT listed as NA
      filter(!is.na(GROUP)) %>% 
      filter(!is.na(TRIBE_ETC))
    
    #New df where family is a unique occurrence for the site
    df_tribe_unique <- df_tribe %>% 
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      distinct(TRIBE_ETC, .keep_all = T)
    
    #Keep all rows that could cause a double count
    df_tribe_doubles <- anti_join(df_tribe, df_tribe_unique)
    
    #Checking for atypical taxonomic resolution exceptions where genus isn't specified but family is
    df_family <- AWQMS_for_mIBI %>%
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #keep rows where GENUS is listed as NA
      filter(is.na(GENUS)) %>%
      #keep rows where FAMILY is NOT listed as NA
      filter(!is.na(FAMILY))
    
    #New df where family is a unique occurrence for the site
    df_family_unique <- df_family %>% 
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      distinct(FAMILY, .keep_all = T)
    
    #Keep all rows that could cause a double count
    df_family_doubles <- anti_join(df_family, df_family_unique)
    df_family_doubles1 <- anti_join(df_family_doubles, df_tribe_unique)
    
    df_order <- AWQMS_for_mIBI %>% 
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      filter(is.na(FAMILY)) %>% 
      distinct(BIOSNAME, .keep_all = T) %>% 
      filter(!is.na(ORDER))
    
    df_order_unique <- df_order %>% 
      distinct(ORDER, .keep_all = T)
    
    df_order_doubles <- anti_join(df_order, df_order_unique)
    
    df_class <- AWQMS_for_mIBI %>% 
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      filter(is.na(ORDER)) %>% 
      distinct(BIOSNAME, .keep_all = T) %>% 
      filter(!is.na(CLASS))
    
    df_class_unique <- df_class %>% 
      distinct(CLASS, .keep_all = T)
    
    df_class_doubles <- anti_join(df_class, df_class_unique)
    
    df_phylum <- AWQMS_for_mIBI %>% 
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      filter(is.na(CLASS)) %>% 
      distinct(BIOSNAME, .keep_all = T) %>% 
      filter(!is.na(PHYLUM))
    
    df_phylum_unique <- df_phylum %>% 
      distinct(PHYLUM, .keep_all = T)
    
    df_phylum_doubles <- anti_join(df_phylum, df_phylum_unique)
    
    mIBI_calc <- AWQMS_for_mIBI %>% 
      anti_join(df_tribe_doubles) %>% 
      anti_join(df_family_doubles1) %>% 
      anti_join(df_order_doubles) %>% 
      anti_join(df_class_doubles) %>% 
      anti_join(df_phylum_doubles)
    
    #compute metric values
    #TotalTaxa_Value - sum of MIBITUs (StandardizedTaxa) in sample
    CalculateTotalTaxa_Value <- mIBI_calc %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #remove semi aquatic
      #filter(TOLERANCE != 99.9) %>%
      #create TotalTaxa_value column by counting the number of unique StandardizedTaxa for each sample id (station code/date)
      mutate(TotalTaxa_Value = length(unique(ID))) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, TotalTaxa_Value) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #ColeopteraTaxa_Value - sum of unique coleoptera MIBITUs in sample
    # do not include semi-aquatic taxa (TV = 99.9)
    CalculateColeopteraTaxa_Value <- mIBI_calc %>%
      #filter to remove semi-aquatic taxa and retain only order Coleoptera
      filter(TOLERANCE != 99.9, ORDER %in% c("Coleoptera", "COLEOPTERA")) %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create ColeopteraTaxa_Value column by counting the number of unique Standardized Coleoptera taxa for each sample id (station code/date)
      mutate(ColeopteraTaxa_Value = length(unique(ID))) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, ColeopteraTaxa_Value) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #EphemeropteraTaxa_Value - sum of unique Ephemeroptera MIBITUs in sample, susceptible to double counting
    CalculateEphemeropteraTaxa_Value <- mIBI_calc %>%
      #filter to retain only order Ephemeroptera
      filter(ORDER %in% c("Ephemeroptera", "EPHEMEROPTERA")) %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create EphemeropteraTaxa_value column by counting the number of unique Standardized Coleoptera taxa for each sample id (station code/date)
      mutate(EphemeropteraTaxa_Value = length(unique(ID))) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, EphemeropteraTaxa_Value) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #IntolerantTaxa_Value = sum of intolerant MIBITUs in sample, provides measure of the number of unique
    #intolerant MIBITUs in sample (TV <= 3.0)
    CalculateIntolerantTaxa_Value <- mIBI_calc %>%
      #filter to retain only Tolerance less than or equal to 3
      filter(TOLERANCE <= 3.0) %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create IntelorantTaxa_Value column by counting the number of unique Standardized intolerant taxa for each sample id (station code/date)
      mutate(IntolerantTaxa_Value = length(unique(ID))) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, IntolerantTaxa_Value) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #PercentScrapers_Value = percent of scrapers (assigned FFG) compared to total n of sample
    CalculatePercentScrapers_value <- mIBI_calc %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create Total abundance column by finding the sum of abundance for each sample id (station code/date)
      mutate(Total_ABUNDANCE = sum(`Result Value`)) %>%
      #filter to retain only Scrapers
      filter(FFG == "SC") %>%
      #create N_scrapers column to sum abundance of Scrapers
      mutate(N_Scrapers = sum(`Result Value`)) %>%
      #create PercentScrapers column by dividng N_Scrapers by total abundance
      mutate(PercentScrapers = N_Scrapers/Total_ABUNDANCE * 100) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, PercentScrapers) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #PercentEPT_Value = percent of Ephemeroptera/Plecoptera/Trichoptera compared to total n of sample
    CalculatePercentEPT_Value <- mIBI_calc %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create total abundance column by finding the sum of abundance for each sample id (station code/date)
      mutate(Total_ABUNDANCE = sum(`Result Value`)) %>%
      #filter to retain only orders Emphemeroptera, Plecoptera, Trichophtera
      filter(ORDER %in% c("Ephemeroptera", "EPHEMEROPTERA", "Plecoptera", "PLECOPTERA",
                          "Trichoptera", "TRICHOPTERA")) %>%
      #create N_EPT column by summing all EPT taxa for each sample id (station code/date)
      mutate(N_EPT = sum(`Result Value`)) %>%
      #create Percent_EPT column by diving N_EPT by Total_Abundance
      mutate(Percent_EPT = N_EPT/Total_ABUNDANCE * 100) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, Percent_EPT) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #MacroinvertebrateBioticIndex =  S (ni*ti)/N
    #Where, ni is the number of individuals in each MIBITU, 
    #ti is the tolerance value assigned to the given MIBITU 
    #and N is the total number of individuals in the sample
    CalculateMBI <- mIBI_calc %>%
      #calculate abundance * tolerance for each row
      mutate(ni_by_ti = `Result Value` * TOLERANCE) %>%
      #group by sample id (station code/date)
      group_by(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`) %>%
      #create column for total abundance for each sample id (station code/date)
      mutate(Total_ABUNDANCE = sum(`Result Value`)) %>%
      #create column for sum of abundance * tolerance for each sample id (station code/date)
      mutate(sum_ni_by_ti = sum(ni_by_ti)) %>%
      #create column for MBI by dividing sum of abundance * tolerance by total abundance for each sample id (station code/date)
      mutate(MBI = sum_ni_by_ti/Total_ABUNDANCE) %>%
      #select necessary columns for future joins
      select(`Monitoring Location ID`, `Activity Start Date`, `Activity Start Time`, MBI) %>%
      #keep only unique values/one per sample id (station code/date)
      unique()
    
    #assemble table for computing metric scores
    #start wtih total taxa value
    MetricScores_dataprepr <- CalculateTotalTaxa_Value %>%
      #join ColepteraTaxa_Value
      full_join(CalculateColeopteraTaxa_Value) %>%
      #join EphemeropteraTaxa_value
      full_join(CalculateEphemeropteraTaxa_Value) %>%
      #join IntolerantTaxa_Value
      full_join(CalculateIntolerantTaxa_Value) %>%
      #join MBI scores
      full_join(CalculateMBI) %>%
      #join Percent Scrapers
      full_join(CalculatePercentScrapers_value) %>%
      #join Percent EPT 
      full_join(CalculatePercentEPT_Value) %>%
      #replace all NAs with 0
      replace(is.na(.), 0)
    
    #compute metric scores by x/best value * 100 for all except MBI
    MetricScores_calculation <- MetricScores_dataprepr %>%
      #ColeopteraTaxa_Score - best value = 5
      mutate(ColeopteraTaxa_Score = ColeopteraTaxa_Value/5 * 100) %>%
      #EphemeropteraTaxa_Score - best value = 10.2
      mutate(EphemeropteraTaxa_Score = EphemeropteraTaxa_Value/10.2 * 100) %>%
      #TotalTaxa_Score - best value = 46
      mutate(TotalTaxa_Score = TotalTaxa_Value/46 * 100) %>%
      #IntolerantTaxa_Score - best value = 9
      mutate(IntolerantTaxa_Score = IntolerantTaxa_Value/9 * 100) %>%
      #MBI - best value = 4.9, ((11-MBI)/(11 - best value)) * 100
      mutate(MBI_score = ((11-MBI)/(11-4.9) * 100)) %>%
      #PercentScraper_Score - best value = 29.6
      mutate(PercentScraper_Score = PercentScrapers/29.6 * 100) %>%
      #PercentEPT_Score - best value = 74
      mutate(Percent_EPT = Percent_EPT) %>%
      #compute mIBI (average of all metric scores)
      mutate(mIBI = ((ColeopteraTaxa_Score + EphemeropteraTaxa_Score + TotalTaxa_Score +
                        IntolerantTaxa_Score + MBI_score + PercentScraper_Score +
                        Percent_EPT)/7)) %>%
      mutate(mIBI = ifelse(mIBI > 100, 100, mIBI)) %>%
      #interpret mIBI, assign narrative descriptions
      #73 - 100 = >75th percentile, "Exceptional"
      #41.8 - 72.9 = >10th percentile, "Good"
      #20.9 - 41.8 = bisect 10th percentile (upper), "Fair"
      #0.0 - 20.8 = bisect 10th percentile (lower), "Poor"
      mutate(Comparison_to_Reference = case_when(mIBI >= 73 & mIBI <= 100 ~ ">75th percentile",
                                                 mIBI >= 41.8 & mIBI <= 72.9 ~ "> 10th percentile",
                                                 mIBI >= 20.9 & mIBI <= 41.8 ~ "bisect 10th percentile (upper)",
                                                 mIBI >= 0 & mIBI <= 20.8 ~ "bisect 10th percentile (lower)")) %>%
      mutate(Narrative_Description = case_when(mIBI >= 73 & mIBI <= 100 ~ "Exceptional",
                                               mIBI >= 41.8 & mIBI <= 72.9 ~ "Good",
                                               mIBI >= 20.9 & mIBI <= 41.8 ~ "Fair",
                                               mIBI >= 0 & mIBI <= 20.8 ~ "Poor"))
    
    MetricScores_calculation
  })
  
  observeEvent(input$upload, {
        req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           xlsx = readxl::read_excel(input$upload$datapath, skip = 1),
           validate(NULL, F)
    )
    enable("mIBI_scores_file")
  })
  
  output$file_type_warning <- renderTable({
    req(input$upload)
    ext1 <- tools::file_ext(input$upload$name)
    switch(ext1,
           xlsx = NULL,
           validate("Invalid file type, please upload an .xlsx file")
    )
  })
  
  output$mIBI_scores_file <- downloadHandler(
    filename = function() {
      paste0("mIBI_scores", ".xlsx")
    },
    content = function(file) {
      write.xlsx(mIBI_thing(), file)
    }
  )
}

shinyApp(ui, server)