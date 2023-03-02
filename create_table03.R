######################
### create table 3 ###
######################

### unwrapped phase and coherence stats
### for all three insar pairs
### jack tarricone
### february 24th, 2023

# set working dir
setwd("/Users/jacktarricone/ch1_jemez/")

### feb 12-19

## bring in vallee grand wkt
vg <-vect("./vector_data/valle_grande_aoi.geojson")
vg

######  calculate the total number of pixels from the coherence image from each date
# feb 12-19
feb_12_19_hh_path <-list.files("./feb12-19", pattern = "*HH_01.cor*", full.names = TRUE)
hh_cor <-rast(feb_12_19_hh_path)
values(hh_cor)[values(hh_cor) == 0] <-NA
p1_total_pixels <-as.numeric(global(hh_cor, fun="notNA"))

# feb 19-26
feb_19_26_hh_path <-list.files("./feb19-26v1", pattern = "*HH_01.cor.grd.tif*", full.names = TRUE)
feb_19_26_hh_cor <-rast(feb_19_26_hh_path)
values(hh_cor)[values(feb_19_26_hh_cor) == 0] <-NA
p2_total_pixels <-as.numeric(global(feb_19_26_hh_cor, fun="notNA"))

# feb 12-26
feb_12_26_hh_path <-list.files("./feb12-26/HH", pattern = "*HH_01.cor", full.names = TRUE)
feb_12_26_hh_cor <-rast(feb_12_26_hh_path)
values(hh_cor)[values(feb_12_26_hh_cor) == 0] <-NA
p3_total_pixels <-as.numeric(global(feb_12_26_hh_cor, fun="notNA"))


####
## coherence stats
### 

# hh mean full scene
cor_hh_list <-list.files("./feb12-19", pattern = "*HH_01.cor*", full.names = TRUE)
cor_hh <-rast(cor_hh_list[1])
values(cor_hh)[values(cor_hh) == 0] <-NA
cor_hh_full_mean <-round(as.numeric(global(cor_hh, mean, na.rm = TRUE)), digits = 2)

# vv mean full scene
cor_vv_list <-list.files("./feb12-19", pattern = "*VV_01.cor*", full.names = TRUE)
cor_vv <-rast(cor_vv_list[1])
values(cor_vv)[values(cor_vv) == 0] <-NA
cor_vv_full_mean <-round(as.numeric(global(cor_vv, mean, na.rm = TRUE)), digits = 2)


# hh mean study area
cor_hh_vg <-crop(cor_hh, vg)
cor_hh_vg_mean <-round(as.numeric(global(cor_hh_vg, mean, na.rm = TRUE)), digits = 2)

# vv mean study area
cor_vv_vg <-crop(cor_vv, vg)
cor_vv_vg_mean <-round(as.numeric(global(cor_vv_vg, mean, na.rm = TRUE)), digits = 2)

####
## unw stats
###

# hh full scene pixels lost
unw_hh_list <-list.files('./feb12-19', pattern = "*HH_01.unw*", full.names = TRUE)
unw_hh <-rast(unw_hh_list[1])
values(unw_hh)[values(unw_hh) == 0] <-NA
plot(unw_hh)
unw_hh_pixels <-as.numeric(global(hh, fun="notNA"))
hh_perc_lost <-round(100-(unw_hh_pixels/p1_total_pixels)*100, digits = 1)

# vv full scene pixels lost
unw_vv_list <-list.files('./feb12-19', pattern = "*VV_01.unw*", full.names = TRUE)
unw_vv <-rast(unw_vv_list[1])
values(unw_vv)[values(unw_vv) == 0] <-NA
plot(unw_vv)
unw_vv_pixels <-as.numeric(global(unw_vv, fun="notNA"))
vv_perc_lost <-round(100-(unw_vv_pixels/p1_total_pixels)*100, digits = 1)

# study area pixels
hh_cor_study_area <-crop(hh_cor, vg)
hh_cor_study_area
study_area_pixels <-as.numeric(global(hh_cor_study_area, fun="notNA"))


# hh
unw_hh_vg <-crop(unw_hh, vg)
plot(unw_hh_vg)
unw_hh_vg_pixels <-as.numeric(global(unw_hh_vg, fun="notNA"))
unw_hh_vg_perc_lost <-round(100-(unw_hh_vg_pixels/study_area_pixels)*100, digits = 1)
unw_hh_vg_perc_lost_vg <-100-(unw_hh_vg_pixels/study_area_pixels)*100

# vv
unw_vv_vg <-crop(unw_vv, vg)
plot(unw_vv_vg)
unw_vv_vg_pixels <-as.numeric(global(unw_vv_vg, fun="notNA"))
unw_vv_vg_perc_lost <-round(100-(unw_vv_vg_pixels/study_area_pixels)*100, digits = 1)
unw_vv_vg_perc_lost_vg <-100-(unw_vv_vg_pixels/study_area_pixels)*100


