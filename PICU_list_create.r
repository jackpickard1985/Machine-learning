# generate a list of all the patients who have had a PICU stay
# accesses 11846/ward_stays.csv


#load data
ward_stay_data <- read.delim("11846/ward_stays.csv", header=TRUE, sep=",", quote="", stringsAsFactors = FALSE)

PICU_patients <- list()

#search through data for presence of PICU in the ward_code column
for (i in 1:nrow(ward_stay_data)) {
    if (ward_stay_data[i, "ward_code"] == "PICU") {
        PICU_patients <- append(PICU_patients, ward_stay_data[i, "project_id"])
    }
}
