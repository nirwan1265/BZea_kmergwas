library(dplyr)
# Load sample list and phenotype
sample <- read.csv("data/sample_list.csv")
phenotype <- read.csv("data/phenotype.csv")

phenotype$DTS <- as.Date(phenotype$DTS, format = "%d/%m/%y")
phenotype$DTA <- as.Date(phenotype$DTA, format = "%d/%m/%y")
phenotype$SPAD1_Date <- as.Date(phenotype$SPAD1_Date, format = "%d/%m/%y")
phenotype$SPAD2_Date <- as.Date(phenotype$SPAD2_Date, format = "%d/%m/%y")
phenotype$LI_COR_Date <- as.Date(phenotype$LI_COR_Date, format = "%d/%m/%y")
phenotype <- phenotype %>% select(-c("Leaf_Auringle","Species.1"))
phenotype[c("ST","StPu","StPi","Kinki","Prolif","NBR","SCiO")] <- lapply(phenotype[c("ST","StPu","StPi","Kinki","Prolif","NBR","SCiO")], as.factor)

head(phenotype)
str(phenotype)
count(unique(phenotype$Female_genotype))


averaged_phenotype <- phenotype %>%
  group_by(Female_genotype) %>%
  summarize(
    PH = mean(PH, na.rm = TRUE),
    EH = mean(EH, na.rm = TRUE),
    EN = mean(EN, na.rm = TRUE),
    BW = mean(BW, na.rm = TRUE),
    BL = mean(BL, na.rm = TRUE),
    SL = mean(SL, na.rm = TRUE)
  )

# remove B73
averaged_phenotype <- averaged_phenotype[-1,]


# getting the phenotypes wrt PN*_SID*
names(averaged_phenotype)
names(sample)
colnames(sample)[6] <- "Female_genotype"
names(sample)

# Getting the phenotypes
phenotype_filtered <- inner_join(averaged_phenotype,sample)

# Removing purple check
phenotype_filtered <- phenotype_filtered %>%
  dplyr::filter(Female_genotype != "Purple Check")


