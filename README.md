# svamp
An R package that generates a geographic summary report for farm data in Sweden

This package is a work in progress and is currently being modified to allow others to use it.
We have removed all data, therefore nothing is working.

# Implementation of features in URAX

Because this project lacks maintenance development resources, we would
like to try to implement some of the features in URAX, an internal
project at SVA for managing a animal disease suspicions.

## Key features and priorities for re-implementation

The interface to the tool takes three arguments:

1. A comma separated list of PPNs
2. A comma separated list of distances in km
3. A time in days

In the feature list below, the PPN(s) in the argument will be referred
to as the 'investigated PPN(s)'. These arguments are used to subset
and match some data to produce a report. The features list here is in
no particular order and needs to be prioritised:

- [] Plot the PPN(s) of interest on a map
- [] If the PPN(s) of interest are missing spatial identifier, give a warning
- [] Tabulate owner/associated person of the PPN(s) of interest
- [] Graph the number of animals by species at the PPN(s) of interest
- [] Tabulate the contact information for the veterinary district
  associated with the PPN(s) of interest
- [] Tabulate and graph the number animals by species in
  the restriction zones defined by the distances in argument 2.
- [] List the PPN(s) in the restriction zones
- [] Plot the PPN(s) of interest, restriction zones and PPN(s) in the
  restriction zones on a map.
- [] Tabulate and graph the number herds by species in
  the restriction zones.
- [] Contact tracing for the PPN(s) of interest for the time given in
  argument 3
- [] tabulate herds with missing coordinates who's postal code intersect
  the restriction zones
- [] Plot map of postalcode that intersect restriction zones and
  contain PPN(s)
- [] tabulate SVASSS data for herds in the restriction zones
- [] tabulate SJV clinical data for herds in the restriction zones


## Additional features not yet implemented

- []
