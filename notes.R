# AUS-project
# AUS development project

# where I left off --------------------------------------------------------
# 

# current problems requiring resolution -----------------------------------


# The read qualtrics function will accurately identify the Date Time structure of the primary qulatrics dataset, but not the pilot data. 
# Manually setting date time; strptime(dat$StartDate,format = '%m/%d/%y %H:%M'); is necessary for the pilot data to be imported accurately, but it turns all of the date values for the primary dataset to NA. Trying to force the pilot dataset import to match the primary dataset specifications; spec <- spec(df); col_types = spec; yields misspecification error.

# wrong.code object is no longer being appropriately generated