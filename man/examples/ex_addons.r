if (grassStarted()) {

# Does the addons folder exist?
ao <- addons(fail = "warning")
if (ao) print("Addons is folder is probably correctly specified.")

# Does this particular module exist?
addon <- "v.centerpoint"
exten <- addons(addon, fail = FALSE)

if (exten) print("Extension `v.centerpoints` is installed.")

}
