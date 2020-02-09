#datas

myQuakes = quakes

lats = quakes$lat             #lats = latitudinile
longs = quakes$long           #longs = lungitudinile
depths = quakes$depth         #depths = adancimile
mags = quakes$mag             #mags = magnitudinile
stationss = quakes$stations   #stationss = statiunile


# QUARTILES

latsQuantile = quantile(lats)
longsQuantile = quantile(longs)
depths_quantile = quantile(depths)
mags_quantile = quantile(mags)
stations_quantile = quantile(stationss)


# MEAN (media efectiva)

lats_mean = mean(lats)
longs_mean = mean(longs)
depths_mean = mean(depths)
mags_mean = mean(mags)
stations_mean = mean(stationss)

# VARIANCES

lats_var = var(lats)
longs_var = var(longs)
depths_var = var(depths)
mags_var = var(mags)
stations_var = var(stationss)

#BOXPLOTS (daca le rulez pe toate, o sa imi apara doar ultima)

lats_boxplot = boxplot(lats)
longs_boxplot = boxplot(longs)
depths_boxplot = boxplot(depths)
mags_boxplot = boxplot(mags)
stations_vboxplot = boxplot(stationss)

#Ce observam in urma statisticilor dataset-ului "quakes" (interpretari) :

# Latitudinea maxima - 10.72
#
# Au fost foarte putine cutremure care s-au intamplat la o adancime intre
# 250-500 km comparat cu restul adancimilor de pana in 250 si respectiv peste 500
# 
# Aproximativ 1/3 din toate cutremurele au avut o longitudine de 180-182
#
# Cea mai mare magnitudine a fost de 6.4 iar majoritatea au avut peste 5

