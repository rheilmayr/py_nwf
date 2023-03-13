#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Paraguay NWF analysis
# Date: 3-13-23
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(R.utils)

# Define the path to your local code directory
code_dir <- "D:/dev/paraguay/py_nwf/"

# Define the path to your local google drive Treeconomics\\Data directory 
data_dir <- "L:/Shared drives/emlab/projects/current-projects/cel-paraguay-nwf/Data/"

  
createLink(paste0(code_dir, 'remote'), data_dir, overwrite = FALSE)

