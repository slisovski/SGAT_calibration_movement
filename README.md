# Geolocation by Light: movement analysis and calibration methods in SGAT

Geolocation by light refers to a tracking method for animals and notably birds using archival tags that record light intensities over time. From light, twilight events or periods (sunrise, sunset) can be identified. Using a reference zenith angle defining the suns position during the defined sunrise/sunset time, locations can be calculated. Given that light is subject to multiple sources of shading (e.g., clouds, vegetation), the method results in low accuracy compared to e.g., gps locations. Multiple methods have been developed to derive robust location estimates and quantify the location specific error (see Lisovski et al. 2012, 2020). 

**Calibration** is a crucial part of geolocation by light. Getting the correct zenith angle for the defined twilight events events is usually done using recordings with known location (e.g., after deployment on the breeding site of the individual). However, often the conditions (weather, habitat) on the breeding sites do not match the conditions during the migration and wintering period. This can lead to a bias in location estimates and even lower accuracy than on would expect given the twilight error distribution.

**_For a specific study on the migrations of Pied Flycatchers (Fraser et al. in prep)_**, we developed a method that finds optimal zenith angles and twiligth error distributions from  stationary periods at unknown locations, based on the principles of the so called [Hill-Ekstrom calibration](https://geolocationmanual.vogelwarte.ch/GeoLight.html#hill-ekstrom-calibration) (Lisovski et al. 2020).

This method requires a **movement analysis**, a prior definition of periods when the bird was stationary. Here, we used a method previously described (e.g., Sander et al. 2021, Meier et al. 2022) and published in a github repository [invMovement](https://github.com/slisovski/invMovement). In a nutshell, the method investigates changes in sunrise and sunset and identifies changes that are larger than the expected error due to shading (based on estimates during periods at known location). Such changes are labeled as movement periods, while no change in sunrise/sunset over consecutive days, or small changes that are within the error distribution are labelled as stationary period.




## References

Lisovski, S., Hewson, C.M., Klaassen, R.H.G., Korner-Nievergelt, F., Kristensen, M.W. and Hahn, S. (2012), Geolocation by light: accuracy and precision affected by environmental factors. Methods in Ecology and Evolution, 3: 603-612. https://doi.org/10.1111/j.2041-210X.2012.00185.x.

Lisovski, S., Bauer S., Briedis, M., Davidson S. C., Dhanjal-Adams K. L., Hallworth, M. T., Karagicheva, J., et al. 2020. “Light-Level Geolocator Analyses: A User’s Guide.” Journal of Animal Ecology 89 (1): 221–36. https://doi.org/10.1111/1365-2656.13036.

Meier, C.M., Rime, Y., Lisovski, S., Buchmann, M. and Liechti, F. (2022), Locally adapted migration strategies? Comparing routes and timing of northern wheatears from alpine and lowland European populations. J Avian Biol, 2022: e02932. https://doi.org/10.1111/jav.02932

Sander MM, Chamberlain D, Mermillon C, Alba R, Jähnig S, Rosselli D, Meier CM and Lisovski S (2021) Early Breeding Conditions Followed by Reduced Breeding Success Despite Timely Arrival in an Alpine Migratory Songbird. Front. Ecol. Evol. 9:676506. doi: https://doi.org/10.3389/fevo.2021.676506.
