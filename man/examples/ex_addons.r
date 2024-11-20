if (grassStarted()) {

ao <- addons(onFail = "warning")
if (ao) print("Addons is folder is correctly specified.")

addon <- "v.centerpoints"
exten <- addons(addon, onFail = "warning")

if (exten) print("Extension `v.centerpoints` is installed.")

}
