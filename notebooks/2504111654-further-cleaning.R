# Clean and visualize jobs data

## Created on 11 April 2025
library(pacman)
p_load(readxl, patchwork, janitor, tidyverse,
        writexl, DT, here,
       ggsci
       )

ggthemr("pale")

# IMPORT ----
path_in <- 
  here("data", "food-sci-tech-jobs-scrapped-2504111249.xlsx")

path_out <- 
  here("data", "food-sci-tech-jobs-cleaned-2504130848.xlsx")


df_in <- 
  read_xlsx(path_in) %>% 
  mutate(date_posted = ymd(date_posted)) %>% 
  arrange(company, title, date_posted) %>% 
  mutate(company = str_to_lower(company))

df_in %>% head()
df_in %>% tail()

# CLEAN ----

## Standardise company names ----

## Check number of distinct companies (before cleaning): 759

companies <- 
  df_in %>% 
  select(company, title) %>% 
  distinct()

companies

## function to clean duplicated names for companies
clean_company_text <- function(df, pattern){
  {{df}} %>% 
    mutate(company = ifelse(
      str_detect(company, pattern),
      str_extract(company, pattern),
      company
    ))
}

rename_company_text <- function(df, pattern, replacement){
  {{df}} %>% 
    mutate(company = str_replace_all(company, {{pattern}}, {{replacement}}))
}

remove_rows_with_company_text <- function(df, pattern){
  df %>% 
    filter(!str_detect(company, pattern))
}

## Remove roles in titles that are irrelevant-----
# check
df_in %>% 
  filter(str_detect(title, "ACCOUNTING"))

df_in %>% 
  filter(str_detect(title, "Intern"))

df_in %>% 
  filter(str_detect(title, "Sous Chef"))

# remove
df_cleaned_roles <- df_in %>% 
  filter(!str_detect(title, "Intern")) %>% 
  filter(!str_detect(title, "Temp")) %>% 
  filter(!str_detect(title, "Maintenance Technician")) %>%
  filter(!str_detect(title, "Vulnerability Management")) %>%
  filter(!str_detect(title, "Part – Time")) %>%
  filter(!str_detect(title, "ACCOUINTING")) %>% 
  filter(!str_detect(title, "Sous Chef")) %>% 
  filter(!str_detect(title, "Urban Solutions and Sustainability")) %>% 
  filter(!str_detect(title, "Commis")) %>% 
  filter(!str_detect(title, "Tea Lady")) %>%
  filter(!str_detect(title, "UX")) %>% 
  filter(!str_detect(title, "Tuition")) %>% 
  filter(!str_detect(title, "Temp")) %>% 
  filter(!str_detect(title, "AGV")) %>% 
  filter(!str_detect(title, "Air Hub")) %>% 
  filter(!str_detect(title, "Anaesthetic")) %>% 
  filter(!str_detect(title, "Area Manager")) %>% 
  filter(!str_detect(title, "Building")) %>% 
  filter(!str_detect(title, "Infrastructure")) %>% 
  filter(!str_detect(title, "Vehicle")) %>% 
  filter(!str_detect(title, "HRIS")) %>% 
  filter(!str_detect(title, "Tenancy")) %>% 
  filter(!str_detect(title, "In-Room Dining")) %>% 
  filter(!str_detect(title, "Operational Technology")) %>% 
  filter(!str_detect(title, "Technology Business Partner")) %>% 
  filter(!str_detect(title, "Automation Lead")) %>% 
  filter(!str_detect(title, "Baggage Lead")) %>% 
  filter(!str_detect(title, "Baker")) %>% 
  filter(!str_detect(title, "Barista")) %>% 
  filter(!str_detect(title, "Bellman")) %>% 
  filter(!str_detect(title, "Biotechnologist")) %>% 
  filter(!str_detect(title, "Bioprocess")) %>% 
  filter(!str_detect(title, "Estate")) %>% 
  filter(!str_detect(title, "Cabin Assistant")) %>% 
  filter(!str_detect(title, "Personal Care")) %>% 
  filter(!str_detect(title, "Cell")) %>% 
  filter(!str_detect(title, "Catering")) %>% 
  filter(!str_detect(title, "Cleanroom")) %>% 
  filter(!str_detect(title, "CLIC")) %>% 
  filter(!str_detect(title, "Clinical Sales")) %>% 
  filter(!str_detect(title, "Clinical Development")) %>% 
  filter(!str_detect(title, "Chef De Partie")) %>% 
  filter(!str_detect(title, "Blood")) %>% 
  filter(!str_detect(title, "Customer Services Agent")) %>% 
  filter(!str_detect(title, "Demi Chef")) %>% 
  filter(!str_detect(title, "Disney Cruise Line")) %>% 
  filter(!str_detect(title, "Energy")) %>% 
  filter(!str_detect(title, "ACMV Technician")) %>% 
  filter(!str_detect(title, "Electrical and Automation Technician")) %>% 
  filter(!str_detect(title, "DIGITAL PRODUCT MANAGER")) %>% 
  filter(!str_detect(title, "ENGINEERING SUPERVISOR")) %>% 
  filter(!str_detect(title, "Environmental Control Officer")) %>% 
  filter(!str_detect(title, "Events Lead")) %>% 
  filter(!str_detect(title, "Patient Safety")) %>%
  filter(!str_detect(title, "Lobby Lounge")) %>% 
  filter(!str_detect(title, "Bioproduction")) %>% 
  filter(!str_detect(title, "Financial Controller")) %>% 
  filter(!str_detect(title, "Finance")) %>% 
  filter(!str_detect(title, "Cabin Services")) %>% 
  filter(!str_detect(title, "Store Packer")) %>% 
  filter(!str_detect(title, "Front Desk Associate")) %>% 
  filter(!str_detect(title, "IT Finance")) %>% 
  filter(!str_detect(title, "Marine")) %>% 
  filter(!str_detect(title, "Social Media")) %>% 
  filter(!str_detect(title, "Graphic Designer")) %>%
  filter(!str_detect(title, "Guest Service")) %>% 
  filter(!str_detect(title, "Philantropy")) %>% 
  filter(!str_detect(title, "Hotel")) %>% 
  filter(!str_detect(title, "HR")) %>%
  filter(!str_detect(title, "IT Systems")) %>% 
  filter(!str_detect(title, "Interpreter")) %>%
  filter(!str_detect(title, "Facilities")) %>% 
  filter(!str_detect(title, "Lead Manufacturing Expert")) %>%
  filter(!str_detect(title, "Cell Line Creation")) %>%
  filter(!str_detect(title, "Site Reliability")) %>% 
  filter(!str_detect(title, "Load Control Assistant")) %>% 
  filter(!str_detect(title, "Logistics Assistant")) %>% 
  filter(!str_detect(title, "Facilities")) %>% 
  filter(!str_detect(title, "Manager, Operations")) %>% 
  filter(!str_detect(title, "Manufacturing Shift Lead")) %>% # takeda pharm
  filter(!str_detect(title, "Med Technologist")) %>% 
  filter(!str_detect(title, "Medical Technologist")) %>% 
  filter(!str_detect(title, "Network Manager")) %>% 
  filter(!str_detect(title, "Network Engineer")) %>% 
  filter(!str_detect(title, "Building Services")) %>% 
  filter(!str_detect(title, "AIC")) %>%
  filter(!str_detect(title, "Pathologist")) %>% 
  filter(!str_detect(title, "Software Engineer")) %>%
  filter(!str_detect(title, "Procurement")) %>% 
  filter(!str_detect(title, "Radiologist")) %>%
  filter(!str_detect(title, "Climate Finance")) %>% 
  filter(!str_detect(title, "Relationship Manager")) %>% 
  filter(!str_detect(title, "Computer Science")) %>% 
  filter(!str_detect(title, "Water Treatment")) %>% 
  filter(!str_detect(title, "RESTAURANT MANAGER")) %>%
  filter(!str_detect(title, "Restaurant")) %>% 
  filter(!str_detect(title, "Climate Finance")) %>% 
  filter(!str_detect(title, "SAP")) %>% 
  filter(!str_detect(title, "Activewear")) %>% 
  filter(!str_detect(title, "Biotechnologist")) %>% 
  filter(!str_detect(title, "Content Creator")) %>%
  filter(!str_detect(title, "Computer Vision")) %>% 
  filter(!str_detect(title, "Import Operations")) %>% 
  filter(!str_detect(title, "TRSM")) %>% 
  filter(!str_detect(title, "Trading Operations")) %>% 
  filter(!str_detect(title, "Baggage")) %>% 
  filter(!str_detect(title, "Trainee")) %>% 
  filter(!str_detect(title, "Human Resources")) %>%
  filter(!str_detect(title, "IIOT")) %>%
  filter(!str_detect(title, "Warehouse")) %>% 
  filter(!str_detect(title, "Project Management")) %>% 
  filter(!str_detect(title, "Electronics Graduate")) %>% 
  filter(!str_detect(title, "Philippines")) %>%
  filter(!str_detect(title, "Detica Product Developer")) %>%
  filter(!str_detect(title, "Exosomes")) %>%
  filter(!str_detect(title, "Data Engineer")) %>% 
  filter(!str_detect(title, "Head of Senior Leadership Development")) %>% 
  filter(!str_detect(title, "Health Data Scientist")) %>% 
  filter(!str_detect(title, "SuccessFactors")) %>% 
  filter(!str_detect(title, "ASSISTANT ENGINEER")) %>% 
  filter(!str_detect(title, "IT Laboratory Support")) %>% 
  filter(!str_detect(title, "IT Technician")) %>% 
  filter(!str_detect(title, "Oncology")) %>% 
  filter(!str_detect(title, "Security Officer")) %>% 
  filter(!str_detect(title, "Machine Learning")) %>% 
  filter(!str_detect(title, "Driver"))

## Clean company text ------
df_cleaned_companies <- clean_company_text(df_cleaned_roles, "a\\*star")
df_cleaned_companies <- clean_company_text(df_cleaned_companies, "abbott")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "Archer Daniels Midland Company", "ADM")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "bd singapore", "bd")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "beam inc\\.", "beam suntory")
str_view(df_cleaned_companies$company, "beam")

df_cleaned_companies <- rename_company_text(df_cleaned_companies, "boxgreen pte ltd", "boxgreen")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "dksh holding ltd", "dksh")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "sats ltd", "sats")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "tum create ltd\\.", "tumcreate")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "dsm-firmenich", "dsm")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "neogen corporation", "neogen")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "iff", "interntional flavors & fragrances")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "ntu \\(nanyang technology university\\- main office-hr\\)", "nanyang technological university")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "interntional flavors & fragrances", "international flavors & fragrances")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "sensient technologies corporation", "sensient technologies")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "eurofins singapore food testing", "eurofins")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "kumar organic products ltd", "kumar organic products")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "tüv süd psb pte. ltd\\.", "tüv süd")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "singhealth group", "singhealth")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "koch engineered solutions", "koch")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "nestl\\é usa", "nestlé")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "prima limited", "prima food pte ltd")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "tum create limited", "tumcreate")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "eurofins singapore clinical diagnostics", "eurofins")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "singhealth group", "singhealth")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "kerry group", "kerry")
df_cleaned_companies <- rename_company_text(df_cleaned_companies, "the coca-cola company", "coca-cola")


df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "accorhotel")

str_view(df_cleaned_companies$company, "addicted")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "am i addicted")

df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "ajay international trading")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "allians")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "atlas copco")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "avetics global")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "beecroft animal specialist & emergency hospital")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "kpmg")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "iix global")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "novotel")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "china telecom")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "slalom")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "infor")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "resorts world sentosa")


# same as foodpanda
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "delivery hero")


df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "goto group") # gojek

df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "mindsg ltd")

df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "crystal jade")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "dss")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "income insurance limited")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "hilton")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "couchbase")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "marriott")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "pwc")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "hongkong land")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "ntt data")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "mandai wildlife reserve")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "terrabit networks")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "rotork")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "security")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "hashmeta")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "umitech")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "planet ta")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "dell technologies")
df_cleaned_companies <- remove_rows_with_company_text(df_cleaned_companies, "ssg hotels")


df_cleaned_companies %>% 
  select(-site) %>% 
  get_dupes(title, company)

# Alfa Laval - Swedish company that provides centrifuges to dairies to separate cream 
# from milk, now in Energy, Marine, Food and Water


# limitations: does not account for cross-industry opportunities

## Clean roles to retain key words only ----

rename_titles_text <- function(df, pattern, replacement){
  {{df}} %>% 
    mutate(title = str_replace_all(title, {{pattern}}, {{replacement}}))
}

df_cleaned_titles <- df_cleaned_companies

df_cleaned_titles <- 
  rename_titles_text(
    df_cleaned_titles,
    "0370 - Compliance Processing Officer \\[ Halal \\| Food Science \\]",
    "Compliance Processing Officer"
  )

df_cleaned_titles <- 
  rename_titles_text(
    df_cleaned_titles,
    "Assistant Manager of Research Development/ RD Assistant \\(5 days\\/ Woodlands\\)",
    "Assistant Manager of Research Development/ RD Assistant"
  )

df_cleaned_titles <- 
  rename_titles_text(
    df_cleaned_titles,
    "Career Opportunities\\:",
    ""
  )

str_view(df_cleaned_titles$title, 
         "Assistant Manager of Research Development")

# keep 1 unique only, but by different jobboard
df_cleaned_companies <- 
  df_cleaned_companies %>% 
  filter(!company == "jobscentral"|title == "Assistant Manager of Research Development\\/ RD Assistant") %>% 
  filter(!company == "jointhire singapore pte ltd"|title == "Assistant Manager of Research Development\\/ RD Assistant") %>% 
  filter(!company == "jobscentral"|title == "Associate Food Technologist \\/ Food Technologist \\(Halal Assurance\\)") %>% 
  filter(!company == "jobscentral"|title == "Associate Professor (Food\\, Chemical and Biotechnology)") %>% 
  filter(!company == "jobscentral"|title == "Plant Manger") %>% 
  filter(!company == "jobscentral"|title == "Quality Control Assistant \\- Food Manufacturing \\(5 Days \\/ Up to \\$2\\,500 \\+ OT \\+ Variable Bonus \\/ Jurong)") %>% 
  filter(!company == "jobscentral"|title == "RESEARCH AND DEVELOPMENT CHEF") %>% 
  filter(!company == "science"|title == "Research Fellow") %>% 
  filter(!company == "a*star"|title == "Principal Investigator / Senior Scientist in Food Oral Processing\\, Sensory")

df_cleaned_titles <- df_cleaned_titles %>% 
  mutate(title = str_trim(title)) %>% 
  mutate(company_title = str_c(company, ";", title)) %>% 
  distinct(company_title, .keep_all = T) %>% 
  select(-company_title) %>% 
  arrange(title)

df_cleaned_titles %>% head()
df_cleaned_titles %>% tail()

df_cleaned_titles %>% 
  datatable()

## Create categories

df_categorized_roles <- 
  df_cleaned_titles %>% 
  mutate(recat_roles = case_when(
    str_detect(title, "Account") ~ "Commercial/Corporate",
    str_detect(title, "Accounts") ~ "Commercial/Corporate",
    str_detect(title, "Sales") ~ "Commercial/Corporate",
    str_detect(title, "Business Development") ~ "Commercial/Corporate",
    str_detect(title, "Business Support") ~ "Commercial/Corporate",
    str_detect(title, "Business Operations") ~ "Commercial/Corporate",
    str_detect(title, "Commercial") ~ "Commercial/Corporate",
    str_detect(title, "Executive Manager") ~ "Commercial/Corporate",
    str_detect(title, "Planning") ~ "Commercial/Corporate",
    str_detect(title, "Marketing") ~ "Commercial/Corporate",
    str_detect(title, "Market Development") ~ "Commercial/Corporate",
    str_detect(title, "Planner") ~ "Commercial/Corporate",
    str_detect(title, "Order") ~ "Commercial/Corporate",
    str_detect(title, "SALES") ~ "Commercial/Corporate",
    str_detect(title, "Product Executive") ~ "Commercial/Corporate",
    str_detect(title, "Product Manager") ~ "Commercial/Corporate",
    str_detect(title, "Retail") ~ "Commercial/Corporate",
    str_detect(title, "Customer Experience") ~ "Commercial/Corporate",
    
    str_detect(title, "Cook") ~ "Chef",
    str_detect(title, "cook") ~ "Chef",
    str_detect(title, "R\\&D Chef") ~ "Chef",
    str_detect(title, "RESEARCH AND DEVELOPMENT CHEF") ~ "Chef",
    str_detect(title, "Head Chef") ~ "Chef",
    str_detect(title, "Chef") ~ "Chef",
    str_detect(title, "KITCHEN DEVELOPMENT CHEF") ~ "Chef",
    
    str_detect(title, "Application") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "New Product Development") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Senior Designer") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Product Developer") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "RDA Scientist") ~ "Application/Food Scientist/Technologist",
    
    str_detect(title, "Director") ~ "Management",
    #str_detect(title, "Supervisor") ~ "Management",
    #str_detect(title, "Manager") ~ "Management",
    str_detect(title, "Foodtech Principal") ~ "Management",
    str_detect(title, "Food and Beverage Manager") ~ "Management",
    str_detect(title, "Food Programme Manager") ~ "Management",
    str_detect(title, "Administrative and Technical Department") ~ "Management",
    
    str_detect(title, "Research") ~ "Research",
    str_detect(title, "R\\&D") ~ "Research",
    str_detect(title, "Fellowship") ~ "Research",
    str_detect(title, "Early Career Awards") ~ "Research",
    str_detect(title, "Researcher") ~ "Research",
    str_detect(title, "Principal Investigator") ~ "Research",
    str_detect(title, "Research Assistant") ~ "Research",
    str_detect(title, "Research Associate") ~ "Research",
    str_detect(title, "Research Engineer") ~ "Research",
    str_detect(title, "Research Fellow") ~ "Research",
    str_detect(title, "Research Officer") ~ "Research",
    str_detect(title, "Research Scientist") ~ "Research",
    str_detect(title, "Research and Development") ~ "Research",
    
    str_detect(title, "Quality") ~ "Quality",
    str_detect(title, "Compliance") ~ "Quality",
    str_detect(title, "Hygienist") ~ "Quality",
    str_detect(title, "Inspector") ~ "Quality",
    str_detect(title, "Microbiologist") ~ "Quality",
    str_detect(title, "QA") ~ "Quality",
    str_detect(title, "QC") ~ "Quality",
    str_detect(title, "Regulatory") ~ "Quality",
    str_detect(title, "Food Safety") ~ "Quality",
    
    str_detect(title, "Professor") ~ "Professor/Lecturer",
    str_detect(title, "Lecturer") ~ "Professor/Lecturer",
    str_detect(title, "Tenure Track") ~ "Professor/Lecturer",
    str_detect(title, "Chemical and Biotechnology") ~ "Professor/Lecturer",
    
    str_detect(title, "Chemist") ~ "Chemist/Lab",
    str_detect(title, "Scientific Officer") ~ "Chemist/Lab",
    str_detect(title, "Flavor analyst") ~ "Chemist/Lab",
    str_detect(title, "Instrument Analyst") ~ "Chemist/Lab",
    str_detect(title, "Chemical Analysis") ~ "Chemist/Lab",
    str_detect(title, "Laboratory") ~ "Chemist/Lab",
    str_detect(title, "Lab Assistant") ~ "Chemist/Lab",
    str_detect(title, "Lab Technician") ~ "Chemist/Lab",
    str_detect(title, "Laboratory Assistant") ~ "Chemist/Lab",
    
    str_detect(title, "Food Science") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Scientist\\/senior Scientist") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Food Scientist") ~ "Application/Food Scientist/Technologist",
    
    str_detect(title, "Food Technologist") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Food Technician") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "FOOD TECHNOLOGIST") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Technology Expert") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Technical Manager") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Technical Lead") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Professional Officer") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Development Technologist") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "FOOD AND DRINK TECHNOLOGIST") ~ "Application/Food Scientist/Technologist",
    str_detect(title, "Drink Technologist") ~ "Application/Food Scientist/Technologist",
    
    str_detect(title, "Sensory Food") ~ "Sensory",
    str_detect(title, "Sensory Specialist") ~ "Sensory",
    str_detect(title, "Taster") ~ "Sensory",
    
    str_detect(title, "Dietitian") ~ "Dietitian/Nutritionist",
    str_detect(title, "Nutrition") ~ "Dietitian/Nutritionist",
    
    str_detect(title, "Flavorist") ~ "Flavorist",
    str_detect(title, "Flavor") ~ "Flavorist",
    str_detect(title, "Flavour") ~ "Flavorist",
    
    str_detect(title, "Factory Manager") ~ "Plant/Production",
    str_detect(title, "Fermentation Pilot Plant Technologist") ~ "Plant/Production",
    str_detect(title, "Manufacturing") ~ "Plant/Production",
    str_detect(title, "Operations") ~ "Plant/Production",
    str_detect(title, "Plant Engineer") ~ "Plant/Production",
    str_detect(title, "Plant Manager") ~ "Plant/Production",
    str_detect(title, "Process Technologist") ~ "Plant/Production",
    str_detect(title, "Production Technologist") ~ "Plant/Production",
    str_detect(title, "Process Developer") ~ "Plant/Production",
    str_detect(title, "Raw Food Trimmer") ~ "Plant/Production",
    str_detect(title, "Food Processing") ~ "Plant/Production",
    
    str_detect(title, "Innovation") ~ "Innovation",
    str_detect(title, "Innovator") ~ "Innovation",
    
    str_detect(title, "Engineer") ~ "Engineer",
    .default = "Others"
  ))
  
  

df_categorized_roles %>% 
  datatable()


write_xlsx(df_cleaned_titles, path_out)


plot_jobs <- 
  df_categorized_roles %>% 
  count(recat_roles) %>% 
  mutate(prop = n/NROW(df_categorized_roles)*100) %>% 
  mutate(prop = round(prop, 2)) %>% 
  mutate(color = case_when(recat_roles == "Quality" ~ "Top 2",
                           recat_roles == "Research" ~ "Top 2",
                           .default = "Others")) %>% 
  filter(recat_roles != "Others") %>% 
  ggplot(aes(x = prop, y = fct_reorder(recat_roles, prop),
             fill = color)) + 
  geom_col(show.legend = F, alpha = 0.8) +
  geom_text(aes(label = prop), hjust = -0.5) +
  scale_fill_jama() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 30)) +
  labs(
    title = "Most of the jobs posted on job boards for food science industry were for Quality and Research",
    subtitle = "Quality jobs: QA/QC/Regulatory/Food Safety; \nResearch: Mostly from Institute of Higher Learning (eg Universities, Polytechnics) and A*STAR",
    x = "%",
    y = "",
    caption = "Data sourced using JobSpy, from Linkedin, Indeed, Glassdoor, Google (2020 Aug - 2025 Apr), n=353 after data cleaning") +
  theme(plot.caption = element_text(hjust = 0))

plot_jobs

path_viz <- here("visualizations", "plot_jobs_scraped.jpg")

ggsave(plot = plot_jobs, path_viz)

        