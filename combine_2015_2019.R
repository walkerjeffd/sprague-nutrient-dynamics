library(openxlsx)
library(waldo)
library(tidyverse)
library(lubridate)

# bring in data sets, these have been separated and uploaded by JK

# 2015
SR_2015_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality 2015.xlsx", sheet = "Sprague R Sampling")

SR_2015_blanks <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality 2015.xlsx", sheet = "Blanks")

# 2016
SR_2016_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality 2016.xlsx", sheet = "Sprague R Sampling") # no blank data in 2016, see email and notes for more information

# 2017
SR_2017_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2017.xlsx", sheet = "Sprague R Sampling Data")

SR_2017_blanks <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2017.xlsx", sheet = "Blanks")

# 2018
SR_2018_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2018.xlsx", sheet = "Sprague R Sampling Data")

SR_2018_blanks <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2018.xlsx", sheet = "Blanks")

# 2019
SR_2019_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2019.xlsx", sheet = "Sprague R Sampling Data")

SR_2019_blanks <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2019.xlsx", sheet = "Blanks")


# 2020
SR_2020_df <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2020.xlsx", sheet = "Sprague R Sampling Data")

SR_2020_blanks <- read.xlsx("~/Sprague River/Sprague_2021/Data/Sprague River Water Quality Data 2020.xlsx", sheet = "Blanks")



# data column headers for 2015 and 2016----

colnames(SR_2015_df) <- SR_2015_df[1,]
SR_2015_df <- SR_2015_df[-1,]

colnames(SR_2016_df) <- SR_2016_df[2,]
SR_2016_df <- SR_2016_df[-c(1,2),]


# check data columns present and column headers----
compare(colnames(SR_2015_df),colnames(SR_2016_df)) # different column headers and columns
compare(colnames(SR_2016_df),colnames(SR_2017_df)) # different
compare(colnames(SR_2015_df),colnames(SR_2017_df)) # different


compare(colnames(SR_2017_df),colnames(SR_2018_df)) # same
compare(colnames(SR_2017_df),colnames(SR_2019_df)) # same
compare(colnames(SR_2017_df),colnames(SR_2020_df)) # same


# rename data column headers----

# create unique column headers by adding number labels
colnames(SR_2015_df) <- make.unique(colnames(SR_2015_df))

# rename column headers
SR_2015_df_ <- SR_2015_df %>%
  dplyr::rename(INDEX=INDEX,
         DATE=DATE,
         SITE_DESCRIPTION=`Site ID`,
         SITE=Site,
         JD=JD,
         CJD=CJD,
         MONTH=MO,
         DAY=DAY,
         YEAR=YR,
         TIME=TIME,
         FLOW_cfs= `Q (cfs)`,
         STAGE_ft=`Staff Gage (ft)` ,
         TEMP_degC= `TEMP (˚C)`,
         COND_uScm = `COND (µS/cm)`,
         DO_ppm = `DO (mg/L)`,
         PH_su = `PH`,
         PSAT_pct = `PSAT (%)`,
         TP_ppb = `TP (µg/L)`,
         TP_DUP = `Duplicate` ,
         TP_PASSFAIL = `Pass/Fail`,
         PO4_ppb = `PO4 (µg/L)`,
         PO4_DUP = `Duplicate.1`,
         PO4_PASSFAIL = `Pass/Fail.1`,
         NH4_ppb = `NH4 (µg/L)`,
         NH4_DUP = `Duplicate.2`,
         NH4_PASSFAIL = `Pass/Fail.2`,
         NO2_ppb = `NO2 (µg/L)`,
         NO2_DUP = `Duplicate.3`,
         NO2_PASSFAIL = `Pass/Fail.3`,
         NO23_ppb = `NO3+NO2 (µg/L)`,
         NO23_DUP = `Duplicate.4`,
         NO23_PASSFAIL = `Pass/Fail.4`,
         TN_ppb = `TN (µg/L)`,
         TN_DUP = `Duplicate.5`,
         TN_PASSFAIL = `Pass/Fail.5`,
         CL_ppb = `Cl- (µg/L)`,
         CL_DUP = `Duplicate.6`,
         CL_PASSFAIL = `Pass/Fail.6`,
         SIO2_ppb = `SiO2 (µg/L)`,
         SIO2_DUP = `Duplicate.7`,
         SIO2_PASSFAIL = `Pass/Fail.7`,
         TSS_ppm = `TSS (mg/L)`,
         TSS_DUP = `Duplicate.8`,
         TSS_PASSFAIL = `Pass/Fail.8`,
         TURBIDITY_NTU = `Turbidity (NTU)`,
         TURB_DUP = `Duplicate.9`,
         TURB_PASSFAIL = `Pass/Fail.9`,
         NOTES = `Notes`
         )


# rename column headers
SR_2016_df_ <- SR_2016_df %>%
  dplyr::rename(#INDEX=INDEX,
         DATE=DATE,
         SITE_DESCRIPTION=`Site Name`,
         SITE=`Site #`,
         JD=JD,
         CJD=CJD,
         MONTH=MO,
         DAY=DAY,
         YEAR=YR,
         TIME=TIME,
         FLOW_cfs= `Q (cfs)`,
         STAGE_ft=`Staff Gage (ft)` ,
         TEMP_degC= `TEMP (˚C)`,
         COND_uScm = `COND (µS/cm)`,
         DO_ppm = `DO (mg/L)`,
         PH_su = `PH`,
         PSAT_pct = `PSAT (%)`,
         TP_ppb = `TP (µg/L)`,
         #TP_DUP = `Duplicate` ,
         #TP_PASSFAIL = `Pass/Fail`,
         PO4_ppb = `PO4 (µg/L)`,
         #PO4_DUP = `Duplicate.1`,
         #PO4_PASSFAIL = `Pass/Fail.1`,
         NH4_ppb = `NH4 (µg/L)`,
         #NH4_DUP = `Duplicate.2`,
         #NH4_PASSFAIL = `Pass/Fail.2`,
         NO2_ppb = `NO2 (µg/L)`,
         #NO2_DUP = `Duplicate.3`,
         #NO2_PASSFAIL = `Pass/Fail.3`,
         NO23_ppb = `NO3+NO2 (µg/L)`,
         #NO23_DUP = `Duplicate.4`,
         #NO23_PASSFAIL = `Pass/Fail.4`,
         TN_ppb = `TN (µg/L)`,
         #TN_DUP = `Duplicate.5`,
         #TN_PASSFAIL = `Pass/Fail.5`,
         #CL_ppb = `Cl- (µg/L)`,
         #CL_DUP = `Duplicate.6`,
         #CL_PASSFAIL = `Pass/Fail.6`,
         SIO2_ppb = `SiO2 (µg/L)`,
         #SIO2_DUP = `Duplicate.7`,
         #SIO2_PASSFAIL = `Pass/Fail.7`,
         TSS_ppm = `TSS (mg/L)`,
         #TSS_DUP = `Duplicate.8`,
         #TSS_PASSFAIL = `Pass/Fail.8`,
         TURBIDITY_NTU = `Turbidity (NTU)`,
         #TURB_DUP = `Duplicate.9`,
         #TURB_PASSFAIL = `Pass/Fail.9`,
         NOTES = `Notes`
  )



# create unique column headers by adding number labels
colnames(SR_2017_df) <- make.unique(colnames(SR_2017_df))
SR_2017_df <- select(SR_2017_df, 1:72)
colnames(SR_2018_df) <- make.unique(colnames(SR_2018_df))
colnames(SR_2019_df) <- make.unique(colnames(SR_2019_df))
colnames(SR_2020_df) <- make.unique(colnames(SR_2020_df))


#

SR_2017_df <- SR_2017_df %>%
  mutate(`Q.(cfs)`=as.character(`Q.(cfs)`))%>%
  mutate(`RPD.2`=as.character(`RPD.2`))

SR_2018_df <- SR_2018_df %>%
  mutate(`Q.(cfs)`=as.character(`Q.(cfs)`))%>%
  mutate(`RPD.2`=as.character(`RPD.2`))

SR_2019_df <- SR_2019_df %>%
  mutate(`Q.(cfs)`=as.character(`Q.(cfs)`))%>%
  mutate(`RPD.2`=as.character(`RPD.2`))

SR_2020_df <- SR_2020_df %>%
  mutate(`Q.(cfs)`=as.character(`Q.(cfs)`)) %>%
  mutate(`RPD.2`=as.character(`RPD.2`))


# 2017, 2018, and 2019 are the same headers, so bind then rename
SR_2017_2020_df <- bind_rows(SR_2017_df, SR_2018_df, SR_2019_df,SR_2020_df)


# rename column headers
SR_2017_2020_df <- SR_2017_2020_df %>%
  dplyr::rename(#INDEX=INDEX,
         DATE=DATE,
         SITE_DESCRIPTION=`Site.Name`,
         SITE=`Site.#`,
         JD=JD,
         CJD=CJD,
         MONTH=MO,
         DAY=DAY,
         YEAR=YR,
         TIME=TIME,
         FLOW_cfs= `Q.(cfs)`,
         STAGE_ft=`Staff.Gage.(ft)` ,
         TEMP_degC= `TEMP.(˚C)`,
         COND_uScm = `COND(µS/cm)`,
         DO_ppm = `DO.(mg/L)`,
         PH_su = `PH`,
         PSAT_pct = `PSAT.(%)`,
         TP_ppb = `TP.(µg/L)`,
         TP_DUP = `Duplicate` ,
         TP_MDL = `MDL`,
         TP_PASSFAIL = `Pass/Fail`,
         PO4_ppb = `PO4.(µg/L)`,
         PO4_DUP = `Duplicate.1`,
         PO4_MDL = `MDL.1`,
         PO4_PASSFAIL = `Pass/Fail.1`,
         NH4_ppb = `NH4.(µg/L)`,
         NH4_DUP = `Duplicate.2`,
         NH4_MDL = `MDL.2`,
         NH4_PASSFAIL = `Pass/Fail.2`,
         NO2_ppb = `NO2.(µg/L)`,
         NO2_DUP = `Duplicate.3`,
         NO2_MDL = `MDL.3`,
         NO2_PASSFAIL = `Pass/Fail.3`,
         NO23_ppb = `NO3+NO2.(µg/L)`,
         NO23_DUP = `Duplicate.4`,
         NO23_MDL = `MDL.4`,
         NO23_PASSFAIL = `Pass/Fail.4`,
         TN_ppb = `TN.(µg/L)`,
         TN_DUP = `Duplicate.5`,
         TN_MDL = `MDL.5`,
         TN_PASSFAIL = `Pass/Fail.5`,
       #  CL_ppb = `Cl- (µg/L)`,
        # CL_DUP = `Duplicate.6`,
         #CL_PASSFAIL = `Pass/Fail.6`,
         SIO2_ppb = `SiO2.(µg/L)`,
         SIO2_DUP = `Duplicate.6`,
         SIO2_PASSFAIL = `Pass/Fail.6`,
         TSS_ppm = `TSS.(mg/L)`,
         TSS_DUP = `Duplicate.7`,
         TSS_PASSFAIL = `Pass/Fail.7`,
         TURBIDITY_NTU = `Turbidity.(NTU)`,
         TURB_DUP = `Duplicate.8`,
         TURB_PASSFAIL = `Pass/Fail.8`,
         NOTES = `Notes`
  )


# select appropriate columns for analysis----

  SR_2015_2016 <- bind_rows(
    (SR_2015_df_ %>%
  mutate(DATE = convertToDate(DATE)) %>%

  select(DATE, TIME,  YEAR, MONTH, DAY, SITE_DESCRIPTION, SITE, #LAT, LON,
         FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
         TP_ppb, PO4_ppb, NH4_ppb, NO23_ppb, NO2_ppb, TN_ppb,
         SIO2_ppb, TSS_ppm, TURBIDITY_NTU,
         TP_DUP, PO4_DUP, NH4_DUP, NO23_DUP, NO2_DUP, TN_DUP,
         SIO2_DUP, TSS_DUP,  #TURBIDITY_DUP,
         NOTES)),
(SR_2016_df_ %>%
  mutate(DATE = convertToDate(DATE)) %>%

      select(DATE, TIME,  YEAR, MONTH, DAY, SITE_DESCRIPTION, SITE, #LAT, LON,
         FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
         TP_ppb, PO4_ppb, NH4_ppb, NO23_ppb, NO2_ppb, TN_ppb,
         SIO2_ppb, TSS_ppm, TURBIDITY_NTU,
         NOTES))
) %>%
  mutate_at(vars( FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
                  TP_ppb, PO4_ppb, NH4_ppb, NO23_ppb, NO2_ppb, TN_ppb,
                  SIO2_ppb, TSS_ppm, TURBIDITY_NTU,
                  TP_DUP, PO4_DUP, NH4_DUP, NO23_DUP, NO2_DUP, TN_DUP,
                  SIO2_DUP, TSS_DUP), as.numeric)


SR_2017_2020_df_select <- SR_2017_2020_df %>%
  mutate(DATE = convertToDate(DATE)) %>%
  mutate(TIME=as.character(TIME)) %>%
  mutate(YEAR=as.character(YEAR)) %>%
  mutate(MONTH=as.character(MONTH)) %>%
  mutate(DAY=as.character(DAY)) %>%
  mutate(FLOW_cfs=as.numeric(FLOW_cfs)) %>%
 # mutate(STAGE_ft=as.character(STAGE_ft)) %>%
      select(DATE, TIME,  YEAR, MONTH, DAY, SITE_DESCRIPTION, SITE, #LAT, LON,
         FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
         TP_ppb, PO4_ppb, NH4_ppb, NO23_ppb, NO2_ppb, TN_ppb,
         SIO2_ppb, TSS_ppm, TURBIDITY_NTU,
         TP_MDL, PO4_MDL, NH4_MDL, NO23_MDL, NO2_MDL, TN_MDL,
         #  SIO2_MDL, TSS_MDL,  TURBIDITY_MDL,
         TP_DUP, PO4_DUP, NH4_DUP, NO23_DUP, NO2_DUP, TN_DUP,
           SIO2_DUP, TSS_DUP,#  TURBIDITY_DUP,
           NOTES)


# bind the years into one dataframe----

SR_2014_2020 <- bind_rows(SR_2015_2016,SR_2017_2020_df_select)


## combine with site coordinate information ----
#filename <- file.path('csv', 'kt_sprague_stations_raw.csv')
#cat('Loading file:', filename, '\n')

#stations <- read.csv(filename, stringsAsFactors=FALSE)

# merge stations to dataframe
SR_2014_2020 <- SR_2014_2020 %>%
#left_join(SR_2014_2020, select(stations,SITE,LAT,LON), by = c("SITE")) %>%
  mutate(TP_ppm = as.numeric(as.character(TP_ppb))/1000) %>%
  mutate(NH4_ppm = as.numeric(as.character(NH4_ppb))/1000) %>%
  mutate(NO2_ppm = as.numeric(as.character(NO2_ppb))/1000) %>%
  mutate(NO23_ppm = as.numeric(as.character(NO23_ppb))/1000) %>%
  mutate(TN_ppm = as.numeric(as.character(TN_ppb))/1000) %>%
  mutate(SIO2_ppm = as.numeric(as.character(SIO2_ppb))/1000) %>%
  mutate(PO4_ppm = as.numeric(as.character(PO4_ppb))/1000) %>%
  mutate(TP_DUP = as.numeric(as.character(TP_DUP))/1000) %>%
  mutate(NH4_DUP = as.numeric(as.character(NH4_DUP))/1000) %>%
  mutate(NO2_DUP = as.numeric(as.character(NO2_DUP))/1000) %>%
  mutate(NO23_DUP = as.numeric(as.character(NO23_DUP))/1000) %>%
  mutate(TN_DUP = as.numeric(as.character(TN_DUP))/1000) %>%
  mutate(SIO2_DUP = as.numeric(as.character(SIO2_DUP))/1000) %>%
  mutate(PO4_DUP = as.numeric(as.character(PO4_DUP))/1000) %>%
  mutate(INDEX=rownames(.))


# write to csv
filename <- file.path('data','sprague','kt', 'SR_2014_2020.csv')
cat('Writing csv:', filename, '\n')
write.csv(SR_2014_2020, file=filename, row.names=FALSE)





# blanks ----
SR_blanks <- rbind(SR_2015_blanks,SR_2017_blanks,SR_2018_blanks,SR_2019_blanks)

# make blanks column names unique
colnames(SR_blanks) <- make.unique(colnames(SR_blanks))


# rename blanks ----
SR_blanks <- SR_blanks %>%
 dplyr::rename(#INDEX=INDEX,
    DATE=DATE,
    BLANK.TYPE = Blank.type,
    #SITE_DESCRIPTION=`Site.Name`,
    #SITE=`Site.#`,
    #JD=JD,
    #CJD=CJD,
    #MONTH=MO,
    #DAY=DAY,
    #YEAR=YR,
    #TIME=TIME,
    #FLOW_cfs= `Q.(cfs)`,
    #STAGE_ft=`Staff.Gage.(ft)` ,
    #TEMP_degC= `TEMP.(˚C)`,
    #COND_uScm = `COND(µS/cm)`,
    #DO_ppm = `DO.(mg/L)`,
    #PH_su = `PH`,
    #PSAT_pct = `PSAT.(%)`,
    TP_ppb = `TP.(µg/L)`,
    #TP_DUP = `Duplicate` ,
    TP_PASSFAIL = `Pass/Fail`,
    PO4_ppb = `PO4.(µg/L)`,
    #PO4_DUP = `Duplicate.1`,
    PO4_PASSFAIL = `Pass/Fail.1`,
    NH4_ppb = `NH4.(µg/L)`,
    #NH4_DUP = `Duplicate.2`,
    NH4_PASSFAIL = `Pass/Fail.2`,
    NO2_ppb = `NO2.(µg/L)`,
    #NO2_DUP = `Duplicate.3`,
    NO2_PASSFAIL = `Pass/Fail.3`,
    NO23_ppb = `NO3+NO2.(µg/L)`,
    #NO23_DUP = `Duplicate.4`,
    NO23_PASSFAIL = `Pass/Fail.4`,
    TN_ppb = `TN.(µg/L)`,
    #TN_DUP = `Duplicate.5`,
    TN_PASSFAIL = `Pass/Fail.5`,
    #CL_ppb = `Cl- (µg/L)`,
    #CL_DUP = `Duplicate.6`,
    #CL_PASSFAIL = `Pass/Fail.6`,
    SIO2_ppb = `SiO2.(µg/L)`,
    #SIO2_DUP = `Duplicate.7`,
    SIO2_PASSFAIL = `Pass/Fail.6`,
    #TSS_ppm = `TSS (mg/L)`,
    #TSS_DUP = `Duplicate.8`,
    #TSS_PASSFAIL = `Pass/Fail.8`,
    #TURBIDITY_NTU = `Turbidity (NTU)`,
    #TURB_DUP = `Duplicate.9`,
    #TURB_PASSFAIL = `Pass/Fail.9`,
    NOTES = `Notes`
  ) %>%
  mutate(DATE = convertToDate(DATE)) %>%
  mutate(YEAR = year(DATE)) %>%
  mutate(MONTH = month(DATE)) %>%
  mutate(BLANK.TYPE = as.factor(BLANK.TYPE)) %>%
  mutate(TP_PASSFAIL = as.factor(TP_PASSFAIL)) %>%
  mutate(PO4_PASSFAIL = as.factor(PO4_PASSFAIL)) %>%
  mutate(NH4_PASSFAIL = as.factor(NH4_PASSFAIL)) %>%
  mutate(NO2_PASSFAIL = as.factor(NO2_PASSFAIL)) %>%
  mutate(NO23_PASSFAIL = as.factor(NO23_PASSFAIL)) %>%
  mutate(TN_PASSFAIL = as.factor(TN_PASSFAIL)) %>%
  mutate(SIO2_PASSFAIL = as.factor(SIO2_PASSFAIL))


# combine data and blanks df ----
SR_2015_2019_withBLANKS <- rbind(
  data.frame(c(SR_2015_2019, sapply(setdiff(names(SR_blanks), names(SR_2015_2019)), function(x) NA))),
  data.frame(c(SR_blanks, sapply(setdiff(names(SR_2015_2019), names(SR_blanks)), function(x) NA)))
)

# convert ppb to ppm ----
SR_2015_2019_withBLANKS <- SR_2015_2019_withBLANKS %>%
  mutate(TP_ppm = as.numeric(as.character(TP_ppb))/1000) %>%
  mutate(NH4_ppm = as.numeric(as.character(NH4_ppb))/1000) %>%
  mutate(NO2_ppm = as.numeric(as.character(NO2_ppb))/1000) %>%
  mutate(NO23_ppm = as.numeric(as.character(NO23_ppb))/1000) %>%
  mutate(TN_ppm = as.numeric(as.character(TN_ppb))/1000) %>%
  mutate(SIO2_ppm = as.numeric(as.character(SIO2_ppb))/1000) %>%
  mutate(PO4_ppm = as.numeric(as.character(PO4_ppb))/1000) %>%
  mutate(TP_DUP = as.numeric(as.character(TP_DUP))/1000) %>%
  mutate(NH4_DUP = as.numeric(as.character(NH4_DUP))/1000) %>%
  mutate(NO2_DUP = as.numeric(as.character(NO2_DUP))/1000) %>%
  mutate(NO23_DUP = as.numeric(as.character(NO23_DUP))/1000) %>%
  mutate(TN_DUP = as.numeric(as.character(TN_DUP))/1000) %>%
  mutate(SIO2_DUP = as.numeric(as.character(SIO2_DUP))/1000) %>%
  mutate(PO4_DUP = as.numeric(as.character(PO4_DUP))/1000) %>%
  mutate(INDEX=rownames(.))

# write to csv
filename <- file.path('data','sprague','kt', 'SR_2015_2019_withBLANKS.csv')
cat('Writing csv:', filename, '\n')
write.csv(SR_2015_2019_withBLANKS, file=filename, row.names=FALSE)



filename <- file.path('data', 'sprague', 'kt', 'Sprague River--Water Quality Dataset 2001_2014_revised_20150127.csv')
cat('Loading file:', filename, '\n')

df_prior <- read.csv(filename, stringsAsFactors=FALSE)
df_update <- df_prior %>%
  mutate(VERSION="prior") %>%
    dplyr::rename(  INDEX=INDEX,
                    DATE=DATE,
                    LAT=GPS_N,
                    LON=GPS_W,
                    SITE_DESCRIPTION=Site.ID,
                    SITE=SITE,
                    JD=JD,
                    CJD=CJD,
                    MONTH=MO,
                    DAY=DAY,
                    YEAR=YR,
                    TIME=TIME,
                    FLOW_cfs=Q_CFS,
                    STAGE_ft=STAFF_GAGE_FT,
                    TEMP_degC=TEMP_C,
                    TEMP_CALC=TEMP_CALC,
                    TEMP_ACT=TEMP_ACT,
                    TEMP_DIFF=TEMP_DIFF,
                    COND_mScm=COND_MS_CM,
                    COND_uScm=COND_US_CM,
                    COND_CALC=COND_CALC,
                    COND_ACT=COND_ACT,
                    COND_DIFF=COND_DIFF,
                    DO_ppm=DO_mgl,
                    DO_CALC=DO_CALC,
                    DO_ACT=DO_ACT,
                    DO_DIFF=DO_DIFF,
                    PH_su=PH,
                    PH_CALC =PH_CALC,
                    PH_ACT=PH_ACT,
                    PH_DIFF=PH_DIFF,
                    PSAT_pct=PSAT_perc,
                    PSAT_CALC=PSAT_CALC,
                    PSAT_ACT=PSAT_ACT,
                    PSAT_DIFF=PSAT_DIFF,
                    TP_ppm=TP_MGL,
                    TP_DUP=TP_DUP,
                    TP_SPIKE=TP_SPIKE,
                    TP_DIFF=TP_DIFF,
                    TP_RPD=TP_RPD,
                    TP_PASS_FAIL=TP_PASS_FAIL,
                    PO4_ppm=PO4_MGL,
                    PO4_DUP=PO4_DUP,
                    PO4_SPIKE=PO4_SPIKE,
                    PO4_DIFF=PO4_DIFF,
                    PO4_RPD=PO4_RPD,
                    PO4_PASS_FAIL=PO4_PASS_FAIL,
                    NH4_ppm=NH4_MGL,
                    NH4_DUP=NH4_DUP,
                    NH4_SPIKE=NH4_SPIKE,
                    NH4_DIFF=NH4_DIFF,
                    NH4_RPD=NH4_RPD,
                    NH4_PASS_FAIL=NH4_PASS_FAIL,
                    NO23_ppm=NO3NO2_MGL,
                    NO23_DUP=NO3NO2_DUP,
                    NO23_SPIKE=NO3NO2_SPIKE,
                    NO23_DIFF=NO3NO2_DIFF,
                    NO23_RPD=NO3NO2_RPD,
                    NO23_PASS_FAIL=NO3NO2_PASS_FAIL,
                    NO2_ppm=NO2_MGL,
                    NO2_DUP=NO2_DUP,
                    NO2_SPIKE=NO2_SPIKE,
                    NO2_DIFF=NO2_DIFF,
                    NO2_RPD=NO2_RPD,
                    NO2_PASS_FAIL=NO2_PASS_FAIL,
                    TN_ppm=TN_MGL,
                    TN_DUP=TN_DUP,
                    TN_SPIKE=TN_SPIKE,
                    TN_DIFF=TN_DIFF,
                    TN_RPD=TN_RPD,
                    TN_PASS_FAIL=TN_PASS_FAIL,
                    CL_ppm=CL_MGL,
                    CL_DUP=CL_DUP,
                    CL_SPIKE=CL_SPIKE,
                    CL_DIFF=CL_DIFF,
                    CL_RPD=CL_RPD,
                    CL_PASS_FAIL=CL_PASS_FAIL,
                    SIO2_ppm=SIO2.MGL,
                    SIO2_DUP=SIO2_DUP,
                    SIO2_SPIKE=SIO2_SPIKE,
                    SIO2_DIFF=SIO2_DIFF,
                    SIO2_RPD=SIO2_RPD,
                    SIO2_PASS_FAIL=SIO2_PASS_FAIL,
                    TSS_ppm=TSS_MGL,
                    TSS_DUP=TSS_DUP,
                    TSS_DIFF=TSS_DIFF,
                    TSS_RPD=TSS_RPD,
                    TSS_PASS_FAIL=TSS_PASS_FAIL,
                    TURBIDITY_NTU=TURB_MGL,
                    TURB_DUP=TURB_DUP,
                    TURB_DIFF=TURB_DIFF,
                    TURB_RPD=TURB_RPD,
                    TURB_PASS_FAIL=TURB_PASS_FAIL,
                    CHLA_ppb=CHLA_UGL,
                    CHLA_DUP=CHLA_DUP,
                    CHLA_DIFF=CHLA_DIFF,
                    CHLA_RPD=CHLA_RPD,
                    CHLA_PASS_FAIL=CHLA_PASS_FAIL,
                    PHAE_ppb=PHAE_UGL,
                    PHAE_DUP=PHAE_DUP,
                    PHAE_DIFF=PHAE_DIFF,
                    PHAE_RPD=PHAE_RPD,
                    PHAE_PASS_FAIL=PHAE_PASS_FAIL,
                    NOTES=Notes)




# for reference ----
# headers from load_kt_sprague for matching
headers_load_kt_sprague <-
  c(
INDEX=INDEX,
DATE=DATE,
LAT=GPS_N,
LON=GPS_W,
SITE_DESCRIPTION=Site.ID,
SITE=SITE,
JD=JD,
CJD=CJD,
MONTH=MO,
DAY=DAY,
YEAR=YR,
TIME=TIME,
FLOW_cfs=Q_CFS,
STAGE_ft=STAFF_GAGE_FT,
TEMP_degC=TEMP_C,
TEMP_CALC=TEMP_CALC,
TEMP_ACT=TEMP_ACT,
TEMP_DIFF=TEMP_DIFF,
COND_mScm=COND_MS_CM,
COND_uScm=COND_US_CM,
COND_CALC=COND_CALC,
COND_ACT=COND_ACT,
COND_DIFF=COND_DIFF,
DO_ppm=DO_mgl,
DO_CALC=DO_CALC,
DO_ACT=DO_ACT,
DO_DIFF=DO_DIFF,
PH_su=PH,
PH_CALC =PH_CALC,
PH_ACT=PH_ACT,
PH_DIFF=PH_DIFF,
PSAT_pct=PSAT_perc,
PSAT_CALC=PSAT_CALC,
PSAT_ACT=PSAT_ACT,
PSAT_DIFF=PSAT_DIFF,
TP_ppm=TP_MGL,
TP_DUP=TP_DUP,
TP_SPIKE=TP_SPIKE,
TP_DIFF=TP_DIFF,
TP_RPD=TP_RPD,
TP_PASS_FAIL=TP_PASS_FAIL,
PO4_ppm=PO4_MGL,
PO4_DUP=PO4_DUP,
PO4_SPIKE=PO4_SPIKE,
PO4_DIFF=PO4_DIFF,
PO4_RPD=PO4_RPD,
PO4_PASS_FAIL=PO4_PASS_FAIL,
NH4_ppm=NH4_MGL,
NH4_DUP=NH4_DUP,
NH4_SPIKE=NH4_SPIKE,
NH4_DIFF=NH4_DIFF,
NH4_RPD=NH4_RPD,
NH4_PASS_FAIL=NH4_PASS_FAIL,
NO23_ppm=NO3NO2_MGL,
NO23_DUP=NO3NO2_DUP,
NO23_SPIKE=NO3NO2_SPIKE,
NO23_DIFF=NO3NO2_DIFF,
NO23_RPD=NO3NO2_RPD,
NO23_PASS_FAIL=NO3NO2_PASS_FAIL,
NO2_ppm=NO2_MGL,
NO2_DUP=NO2_DUP,
NO2_SPIKE=NO2_SPIKE,
NO2_DIFF=NO2_DIFF,
NO2_RPD=NO2_RPD,
NO2_PASS_FAIL=NO2_PASS_FAIL,
TN_ppm=TN_MGL,
TN_DUP=TN_DUP,
TN_SPIKE=TN_SPIKE,
TN_DIFF=TN_DIFF,
TN_RPD=TN_RPD,
TN_PASS_FAIL=TN_PASS_FAIL,
CL_ppm=CL_MGL,
CL_DUP=CL_DUP,
CL_SPIKE=CL_SPIKE,
CL_DIFF=CL_DIFF,
CL_RPD=CL_RPD,
CL_PASS_FAIL=CL_PASS_FAIL,
SIO2_ppm=SIO2.MGL,
SIO2_DUP=SIO2_DUP,
SIO2_SPIKE=SIO2_SPIKE,
SIO2_DIFF=SIO2_DIFF,
SIO2_RPD=SIO2_RPD,
SIO2_PASS_FAIL=SIO2_PASS_FAIL,
TSS_ppm=TSS_MGL,
TSS_DUP=TSS_DUP,
TSS_DIFF=TSS_DIFF,
TSS_RPD=TSS_RPD,
TSS_PASS_FAIL=TSS_PASS_FAIL,
TURBIDITY_NTU=TURB_MGL,
TURB_DUP=TURB_DUP,
TURB_DIFF=TURB_DIFF,
TURB_RPD=TURB_RPD,
TURB_PASS_FAIL=TURB_PASS_FAIL,
CHLA_ppb=CHLA_UGL,
CHLA_DUP=CHLA_DUP,
CHLA_DIFF=CHLA_DIFF,
CHLA_RPD=CHLA_RPD,
CHLA_PASS_FAIL=CHLA_PASS_FAIL,
PHAE_ppb=PHAE_UGL,
PHAE_DUP=PHAE_DUP,
PHAE_DIFF=PHAE_DIFF,
PHAE_RPD=PHAE_RPD,
PHAE_PASS_FAIL=PHAE_PASS_FAIL,
NOTES=Notes
)
