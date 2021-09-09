library(tidyverse)
library(janitor)
library(lubridate)
library(prism)
library(sf)
library(exactextractr)
library(fluxr)

# load: prism from 2015 ---------------------------------------------------
# only used to validate new dataset fetched using {prism} package

filename_2015 <- file.path("data", "raw", "prism", "ppt_basins.csv")
prism_2015 <- read_csv(filename_2015, skip = 1)
names(prism_2015) <- toupper(names(prism_2015))
prism_2015 <- dplyr::rename(prism_2015, PRCP=PPT) %>%
  filter(SITE != 'WR1000') %>%
  mutate(MONTHYEAR=as.Date(paste(YEAR, MONTH, 1, sep='-')),
         WYEAR=wyear(MONTHYEAR)) %>%
  select(MONTHYEAR, WYEAR, SITE, PRCP)


# load: prism (latest) ----------------------------------------------------
# fetch using {prism} and compute zonal stats using {exactextractr}

# load subbasins gis
load("gis.Rdata")

# re-project to NAD83 (same as prism rasters)
incbasin_nad83 <- incbasin %>%
  filter(INC_SITE_NAME != "Godowa-SF-NF") %>%
  st_transform(crs = "EPSG:4269")
incbasin_nad83 %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = SITE))

# set download folder for prism (note: this folder is not tracked in repo)
if (!dir.exists("data/raw/prism/rasters")) {
  dir.create("data/raw/prism/rasters", recursive = TRUE)
}
prism_set_dl_dir("data/raw/prism/rasters")

# download prism rasters (monthly, 1981-2020)
get_prism_monthlys(type = "ppt", year = 1981:2020, mon = 1:12, keepZip = FALSE)

# load rasters as stack
prism_files <- pd_to_file(prism_archive_subset("ppt", "monthly", years = 1981:2020, mon = 1:12))
prism_stack <- raster::stack(prism_files)
raster::crs(prism_stack) <- sp::CRS("+init=EPSG:4269")

# compute zonal stats by basin
prism_extract <- as_tibble(exact_extract(prism_stack, incbasin_nad83, "mean", append_cols = c("SITE")))

# convert to long data frame
prism <- prism_extract %>%
  pivot_longer(-c(SITE), names_to = "NAME", values_to = "PRCP") %>%
  mutate(
    MONTHYEAR = ymd(str_c(str_sub(NAME, 29, 34), "01")),
    WYEAR = wyear(MONTHYEAR)
  ) %>%
  dplyr::select(-NAME)

prism %>%
  ggplot(aes(MONTHYEAR, PRCP)) +
  geom_line(aes(color = as.character(SITE)))

# compare latest to 2015
bind_rows(
  prism_2015 %>%
    mutate(dataset = "2015"),
  prism %>%
    mutate(dataset = "latest")
) %>%
  ggplot(aes(MONTHYEAR, PRCP)) +
  geom_line(aes(color = dataset)) +
  facet_wrap(vars(SITE))

# scatterplot comparison
bind_rows(
  prism_2015 %>%
    mutate(dataset = "2015"),
  prism %>%
    mutate(dataset = "latest")
) %>%
  pivot_wider(names_from = "dataset", values_from = "PRCP") %>%
  ggplot(aes(`2015`, latest)) +
  geom_abline() +
  geom_point() +
  facet_wrap(vars(SITE))

# join incbasin
prism <- left_join(prism,
                   filter(incbasin_area, INC_SITE_NAME != "Godowa-SF-NF") %>% #
                     mutate(SITE=as.character(SITE),
                            INC_SITE_NAME=as.character(INC_SITE_NAME)),
                     by='SITE') %>%
  select(MONTHYEAR, WYEAR, INC_SITE_NAME, AREA_KM2, PRCP)

# compute subbasin
prism <- mutate(prism, PRCP_AREA=PRCP*AREA_KM2) %>%
  select(MONTHYEAR, WYEAR, INC_SITE_NAME, PRCP_AREA) %>%
  spread(INC_SITE_NAME, PRCP_AREA) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`,
         NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF-NF`+SF+NF,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine) %>%
  gather(SITE_NAME, PRCP_AREA, -MONTHYEAR, -WYEAR) %>%
  mutate(SITE_NAME=as.character(SITE_NAME))

prism_incbasin <- dplyr::rename(prism, INC_SITE_NAME=SITE_NAME) %>%
  inner_join(select(incbasin_area, INC_SITE_NAME, AREA_KM2) %>%
               mutate(INC_SITE_NAME=as.character(INC_SITE_NAME)),
             by="INC_SITE_NAME") %>%
  mutate(PRCP=PRCP_AREA/AREA_KM2,
         INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME))) %>%
  select(-PRCP_AREA)

prism_subbasin <- prism %>%
  inner_join(select(subbasin_area, SITE_NAME, AREA_KM2) %>%
               mutate(SITE_NAME=as.character(SITE_NAME)),
             by="SITE_NAME") %>%
  mutate(PRCP=PRCP_AREA/AREA_KM2,
         SITE_NAME=ordered(SITE_NAME, levels=levels(subbasin_area$SITE_NAME))) %>%
  select(-PRCP_AREA)

# mean annual precip
prism_subbasin_wyr <- group_by(prism_subbasin, SITE_NAME, AREA_KM2, WYEAR) %>%
  dplyr::summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/10) %>% # mm/yr -> cm/yr
  ungroup %>%
  filter(N_MONTH == 12)

# overall mean annual precip (cm/yr) by subbasin
prism_subbasin_site <- prism_subbasin_wyr %>%
  dplyr::group_by(SITE_NAME, AREA_KM2) %>%
  dplyr::summarise(PRCP=mean(PRCP))
mean_annual_precip <- filter(prism_subbasin_site, SITE_NAME=="Power")$PRCP
cat("\nMean annual precip at Power (cm/yr):", mean_annual_precip, '\n\n')

# overall mean annual precip (cm/yr) by incbasin
group_by(prism_incbasin, INC_SITE_NAME, AREA_KM2, WYEAR) %>%
  dplyr::summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/10) %>% # mm/yr -> cm/yr
  filter(N_MONTH == 12) %>%
  summarise(PRCP=mean(PRCP))

prism_subbasin_wyr %>%
  ggplot(aes(SITE_NAME, PRCP)) +
  geom_boxplot()

# report figures
filename <- "report/prism-annual-precip-update.png"
cat('Saving annual precip plot to:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- ggplot() +
  geom_bar(aes(WYEAR, PRCP), stat="identity",
           data=prism_subbasin_wyr %>%
             filter(SITE_NAME=="Power")) +
  geom_hline(yintercept=mean_annual_precip, color='black', linetype=2) +
  geom_text(aes(x=2013, y=mean_annual_precip+2), label="Mean", hjust=0, vjust=0, size=4) +
  labs(x="Water Year", y="Annual Precipitation (cm/yr)") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_x_continuous(breaks=seq(1980, 2015, 5))
print(p)
dev.off()

filename <- "report/prism-monthly-precip-update.png"
cat('Saving monthly precip boxplots to:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- prism_subbasin %>%
  filter( SITE_NAME=="Power") %>%
  dplyr::group_by(WYEAR) %>%
  dplyr::mutate(N=n()) %>%
  filter(N==12) %>%
  mutate(MONTH=month(MONTHYEAR),
         MONTH=ordered(as.character(MONTH), levels=as.character(c(10:12, 1:9))),
         PRCP=PRCP/10) %>%
  ggplot(aes(MONTH, PRCP,group=MONTH)) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_boxplot(fill="grey80") +
  labs(x="Month", y="Monthly Precipitation (cm/mon)")
print(p)
dev.off()


# save ----
filename <- 'prism.Rdata'
cat('\nSaving PRISM dataset to:', filename, '\n')
save(prism_subbasin, prism_incbasin, file=filename)

cat('\n\n')

