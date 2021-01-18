library(rgdal)
library(ncdf4)
library(raster)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)

# Carregando o shapefile da Amazônia Legal
shp_amz_legal = readOGR("shp/brazilian_legal_amazon_border/brazilian_legal_amazon.shp")

# Carregando arquivos do CMIP
pr_hist = brick("cmip_hadgem/hist/pr_HadGEM2-ES_hist_1959-2005.nc")
tas_hist = brick("cmip_hadgem/hist/tas_HadGEM2-ES_hist_1959-2005.nc")
pr_rcp85 = brick("cmip_hadgem/rcp85/pr_HadGEM2-ES_rcp85_2030-2080.nc")
tas_rcp85 = brick("cmip_hadgem/rcp85/tas_HadGEM2-ES_rcp85_2030-2080.nc")

# Colocando shapefile na mesma projeção, elipse e datum dos dados do CMIP
shp_amz_legal = spTransform(shp_amz_legal, crs(pr_hist))

# Ajustando Longitudes dos dados do CMIP (de 0:360 para -180:180)
pr_hist = rotate(pr_hist)
tas_hist = rotate(tas_hist)
pr_rcp85 = rotate(pr_rcp85)
tas_rcp85 = rotate(tas_rcp85)

# Recortando e mascarando dados para a área da Amazônia Legal
pr_hist_amz = mask(crop(pr_hist, extent(shp_amz_legal)), shp_amz_legal)
tas_hist_amz = mask(crop(tas_hist, extent(shp_amz_legal)), shp_amz_legal)
pr_rcp85_amz = mask(crop(pr_rcp85, extent(shp_amz_legal)), shp_amz_legal)
tas_rcp85_amz = mask(crop(tas_rcp85, extent(shp_amz_legal)), shp_amz_legal)

# Selecionando apenas os anos de interesse
hist_date = getZ(pr_hist_amz)
grep("1976-01-15", hist_date) # 194
grep("2005-11-11", hist_date) # 552 (último)
pr_hist_amz_70_05 = subset(pr_hist_amz, 194:552)
pr_hist_amz_70_05@z$time = pr_hist_amz@z$time[194:552]
tas_hist_amz_70_05 = subset(tas_hist_amz, 194:552)
tas_hist_amz_70_05@z$time = tas_hist_amz@z$time[194:552]

rcp85_date = getZ(pr_rcp85_amz)
grep("2041-01-15", rcp85_date) # 122
grep("2070-12-16", rcp85_date) # 481
pr_rcp85_amz_41_70 = subset(pr_rcp85_amz, 122:481)
pr_rcp85_amz_41_70@z$time = pr_rcp85_amz@z$time[122:481] # colocando datas de volta
tas_rcp85_amz_41_70 = subset(tas_rcp85_amz, 122:481)
tas_rcp85_amz_41_70@z$time = tas_hist_amz@z$time[122:481] # colocando datas de volta

# Convertendo unidades para °C (Temperatura) e mm/dia (Precipitação)
pr_hist_amz_70_05 = pr_hist_amz_70_05*86400
tas_hist_amz_70_05 = tas_hist_amz_70_05 - 273.15
pr_rcp85_amz_41_70 = pr_rcp85_amz_41_70*86400
tas_rcp85_amz_41_70 = tas_rcp85_amz_41_70 - 273.15

# Salvando dados processados como rasters
writeRaster(pr_hist_amz_70_05, filename = "pr_hist_amz_70_05", format = "raster")
writeRaster(tas_hist_amz_70_05, filename = "tas_hist_amz_70_05", format = "raster")
writeRaster(pr_rcp85_amz_41_70, filename = "pr_rcp85_amz_41_70", format = "raster")
writeRaster(tas_rcp85_amz_41_70, filename = "tas_rcp85_amz_41_70", format = "raster")


# Calculando a média climatológica dos meses
indices = format(as.Date(names(pr_hist_amz_70_05), format = "X%Y.%m.%d"), format = "%m")
indices = as.numeric(indices)
pr_hist_amz_clim_70_05 = stackApply(pr_hist_amz_70_05, indices, fun = mean)
names(pr_hist_amz_clim_70_05) <- month.abb
tas_hist_amz_clim_70_05 = stackApply(tas_hist_amz_70_05, indices, fun = mean)
names(tas_hist_amz_clim_70_05) <- month.abb

indices85 = format(as.Date(names(pr_rcp85_amz_41_70), format = "X%Y.%m.%d"), format = "%m")
indices85 = as.numeric(indices85)
pr_rcp85_amz_clim_41_70 = stackApply(pr_rcp85_amz_41_70, indices85, fun = mean)
names(pr_rcp85_amz_clim_41_70) <- month.abb
tas_rcp85_amz_clim_41_70 = stackApply(tas_rcp85_amz_41_70, indices85, fun = mean)
names(tas_rcp85_amz_clim_41_70) <- month.abb

# Plotando e salvando distribuição espacial das variáveis
png("pr_hist_amz_clim_70-05.png")
blues <- colorRampPalette(brewer.pal(9, "Blues"))
print(levelplot(pr_hist_amz_clim_70_05, col.regions = blues, main = "Precipitação (mm/dia)") + layer(sp.polygons(shp_amz_legal)))
dev.off()
png("tas_hist_amz_clim_70-05.png")
jet.colors =  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
print(levelplot(tas_hist_amz_clim_70_05, col.regions = jet.colors, main = "Temperatura do Ar (°C)") + layer(sp.polygons(shp_amz_legal)))
dev.off()

png("pr_rcp85_amz_clim_41-70.png")
print(levelplot(pr_rcp85_amz_clim_41_70, col.regions = blues, main = "Precipitação (mm/dia)") + layer(sp.polygons(shp_amz_legal)))
dev.off()
png("tas_rcp85_amz_clim_41-70.png")
print(levelplot(tas_rcp85_amz_clim_41_70, col.regions = jet.colors, main = "Temperatura do Ar (°C)") + layer(sp.polygons(shp_amz_legal)))
dev.off()

# Calculando média de todo o domínio
pr_hist_amz_tot_mean_70_05 = cellStats(pr_hist_amz_clim_70_05, 'mean')
tas_hist_amz_tot_mean_70_05 = cellStats(tas_hist_amz_clim_70_05, 'mean')
pr_rcp85_amz_tot_mean_41_70 = cellStats(pr_rcp85_amz_clim_41_70, 'mean')
tas_rcp85_amz_tot_mean_41_70 = cellStats(tas_rcp85_amz_clim_41_70, 'mean')

# Plotando e salvando valores médios para cada mês do ano climatológico
png("barplot_pr_hist_amz_70-05.png")
barplot(pr_hist_amz_tot_mean_70_05, col = "cornflowerblue", beside=TRUE, main = "Média Climatológica Mensal de Precipitação (1970-2005)", xlab = "Meses", ylab = "Precipitação (mm/dia)", ylim=range(pretty(c(0, pr_hist_amz_tot_mean_70_05))))
dev.off()
png("barplot_tas_hist_amz_70-05.png")
barplot(tas_hist_amz_tot_mean_70_05, col = "coral4", beside=TRUE, xlab = "Meses", main = "Média Climatológica Mensal de Temperatura do Ar (1970-2005)", ylab = "Temperatura (°C)", ylim=range(pretty(c(0, tas_hist_amz_tot_mean_70_05))))
dev.off()

png("barplot_pr_rcp85_amz_41-70.png")
barplot(pr_rcp85_amz_tot_mean_41_70, col = "cornflowerblue", beside=TRUE, xlab = "Meses", main = "Média Climatológica Mensal de Precipitação (2041-2070)", ylab = "Precipitação (mm/dia)", ylim=range(pretty(c(0, pr_rcp85_amz_tot_mean_41_70))))
dev.off()
png("barplot_tas_rcp85_amz_41-70.png")
barplot(tas_rcp85_amz_tot_mean_41_70, col = "coral4", beside=TRUE, xlab = "Meses", main = "Média Climatológica Mensal de Temperatura do Ar (2041-2070)", ylab = "Temperatura (°C)", ylim=range(pretty(c(0, tas_rcp85_amz_tot_mean_41_70))))
dev.off()
