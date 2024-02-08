library(rStrava)
library(dplyr)
library(ggplot2)
library(languageserver)

app_name <- "R_running" # chosen by user
app_client_id  <- "120469" # an integer, assigned by Strava
# an alphanumeric secret, assigned by Strava
app_secret <- "6899acfd77201cc9adfb3cbd989e728cc6169e8e"


# create the authentication to strava
stoken = httr::config(token = strava_oauth(app_name, app_client_id,app_secret,app_scope="activity:read_all")) 

# Get info
myinfo <- get_athlete(stoken, id = "53001950")
head(myinfo)

act = get_activity(id = "10624367606",stoken = stoken)
#Meters
distances = lapply(act$splits_metric,FUN = function(x) {x$distance}) %>% unlist()
#BPM
hr = lapply(act$splits_metric,FUN = function(x) {x$average_heartrate}) %>% unlist()
#meters per second
avg_speed = lapply(act$splits_metric,FUN = function(x) {x$average_speed}) %>% unlist()

splits_df = data.frame(distances,hr,avg_speed) %>% mutate(hr_speed_ratio = hr/avg_speed) %>% mutate(split_number = row_number())

#Simple plot of hr_speed_ratio
ggplot(data = splits_df, aes(x = split_number,y = hr_speed_ratio)) + geom_point()

## Get all activities from Strava
act_list = get_activity_list(stoken = stoken)

## Get activity IDs
act_list_IDs = lapply(act_list,FUN = function(x) {x$id}) %>% unlist()


## Function to get all the splits and associated HR from an activity
gather_distance = function(activity) {
    #browser()
    act_dat = get_activity(id = activity,stoken = stoken)
    splits_metric = act_dat$splits_metric
    distances_metric = lapply(splits_metric,function(x) {x$distance}) %>% unlist()
    elapsed_times = lapply(splits_metric,function(x) {x$elapsed_time}) %>% unlist()
    elevation_gain = lapply(splits_metric,function(x) {x$elevation_difference}) %>% unlist()
    split_HR = lapply(splits_metric,function(x) {x$average_heartrate}) %>% unlist()


    return(data.frame(distances_metric,elapsed_times,elevation_gain,split_HR))
}

gather_distance(act_list_IDs[1])

print("check check")



