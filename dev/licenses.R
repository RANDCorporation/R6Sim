
dependencies <- renv::dependencies(path = "./R")

packages <- unique(dependencies$Package)


# Use the function below to discover licenses:

ListLicenses = function(packages) {

  for (package_name in sort(packages)) {

    first_order_dependencies <- packrat:::getPackageDependencies(packages, lib.loc = .libPaths()[1], fields = c("Depends", "Imports"))

    nth_order_dependencies = packrat:::recursivePackageDependencies(package_name, lib.loc = .libPaths()[1], ignores = c(), fields = c("Depends", "Imports"))

    nth_order_dependencies <- nth_order_dependencies[!nth_order_dependencies %in% first_order_dependencies]

    # Originally used, but only works for CRAN packages:

    #dependencies = tools::package_dependencies(package_name, which = c("Depends", "Imports"), recursive = TRUE)

    license_info = packageDescription(package_name, fields="License")

    print(glue::glue("{package_name}: {license_info}"))

    print("1st Order Dependencies:")

    for (dependency in unlist(first_order_dependencies)) {

      dependency_license_info = packageDescription(dependency, fields="License")

      print(glue::glue("\t{dependency}: {dependency_license_info}"))

    }

    print("2-Nth Order Dependencies:")

    for (dependency in unlist(nth_order_dependencies)) {

      dependency_license_info = packageDescription(dependency, fields="License")

      print(glue::glue("\t{dependency}: {dependency_license_info}"))

    }

  }

}



sink("licenses.txt")

ListLicenses(packages)

sink()



