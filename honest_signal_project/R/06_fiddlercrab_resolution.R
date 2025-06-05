
library(pavo)
fiddlercrab <- getimg("data/raw/photo_res")

# Model the appearance of our butterflies given the reduced
# visual acuity of another animal viewer as per the
# AcuityView 2.0 algorithm (Caves & Johnsen 2018). Here
# our butterfly is 60 cm away, the image width is 10 cm,
# and the spatial resolution of the viewer is 0.2-degrees.
fiddler_acuity <- procimg(fiddlercrab,
                              obj_dist = 12,
                              obj_width = 2,
                              eye_res = 1
)

# See the results

plot(fiddler_acuity)
