######################
### create table 3 ###
######################

### unwrapped phase and coherence stats
### for all three insar pairs
### jack tarricone
### february 24th, 2023

# set working dir
setwd("/Users/jacktarricone/ch1_jemez/feb12-19")

### feb 12-19

# hh
hh_list <-list.files(pattern = "*HH_01.cor*")
hh <-rast(hh_list[1])
values(hh)[values(hh) == 0] <-NA
hh_full_mean <-round(as.numeric(global(hh, mean, na.rm = TRUE)), digits = 2)

# vv
vv_list <-list.files(pattern = "*VV_01.cor*")
vv <-rast(vv_list[1])
values(vv)[values(vv) == 0] <-NA
vv_full_mean <-round(as.numeric(global(vv, mean, na.rm = TRUE)), digits = 2)

## bring in vallee grand wkt
vg <-vect("/Users/jacktarricone/ch1_jemez/vector_data/valle_grande_aoi.geojson")
vg

# hh
hh_vg_v1 <-mask(hh, vg)
hh_vg <-crop(hh_vg_v1, vg)
plot(hh_vg)
hh_vg_mean <-global(hh_vg, mean, na.rm = TRUE)

# vv
vv_vg_v1 <-mask(vv, vg)
vv_vg <-crop(vv_vg_v1, vg)
plot(vv_vg)
vv_vg_mean <-global(vv_vg, mean, na.rm = TRUE)



r1 <-cbind(hh_full_mean, hv_full_mean, vh_full_mean, vv_full_mean)
names(r1) <-c("hh","hv","vh","vv")
r2 <-cbind(hh_vg_mean, hv_vg_mean, vh_vg_mean, vv_vg_mean)
names(r2) <-c("hh","hv","vh","vv")

df <-rbind(r1,r2)
print(df)
