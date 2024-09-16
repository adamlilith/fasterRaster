if (grassStarted()) {

madRivers <- fastData("madRivers")
rivers <- fast(madRivers)

.vCats(rivers)
.vHasDatabase(rivers)
.vAsDataTable(rivers)
.vRecat(rivers) # dangerous--re-categorizes geometries!
.vValidCats(rivers)

.vDetachDatabase(rivers) # dangerous--detaches database!
.vAttachDatabase(rivers)

# Ensure we don't use this for other examples!
.rm(rivers) # delete in GRASS
rm(rivers) # delete in R

}
