# AUS-project
# AUS development project

# where I left off --------------------------------------------------------
# Need to remove duplicate Worker IDs from the dataset
# Subset to only the valid items and non-redundant worker IDs
# moved participant number assignment to clean section to avoid duplicate numbers

# current problems requiring resolution -----------------------------------


# The read qualtrics function will accurately identify the Date Time structure of the primary qulatrics dataset, but not the pilot data. 
# Manually setting date time; strptime(dat$StartDate,format = '%m/%d/%y %H:%M'); is necessary for the pilot data to be imported accurately, but it turns all of the date values for the primary dataset to NA. Trying to force the pilot dataset import to match the primary dataset specifications; spec <- spec(df); col_types = spec; yields misspecification error.

# wrong.code object is no longer being appropriately generated

## 1/28/20 
# worked on cleaning data
# created function to import qualtrics .csv file
# created fucntion to import Mturk .csv file

## 1/29/20
# Generate syntax for identifying participants that should be rejected
# 

## 1/30/20
# coding for Mturk reject
# 

#2/4/20
# identifying participants that should be rejected