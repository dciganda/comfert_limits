# comfert
The Limits to Fertility Recuperation. Code &amp; Data

The local/ folder contains the R files and data needed to run the comfert algorithm in a local machine. 

Open main.R and choose the country ("FR","IR" or "ES"), the time window, the size of the initial cohorts and the number of repetitions to run in parallel (depening on the resources available in your machine).

main.R calls the files with the data and parameters for the selected country and the model (comfert.R). The parameters can be modified by accessing the files in data/'country'/in.

After all the runs are finished main.R creates a new directory, saves the results and plots simulated vs observed indicators.

The cluster/ folder contains the files needed to run the model for a large sample of parameter combinations in a computing cluster.   
