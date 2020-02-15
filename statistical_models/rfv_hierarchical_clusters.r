# Segmenting users  -------------------------------------------------------

# example using a large dataset of users of a language learning application

# Generally two options for segmentation:
# 1) Rule based - how would business stakeholders group customers?
# 2) Statistical - what groups would a model identify within a given dataset?


# Adapting RFV segmentation for this use case -----------------------------

#Frequency = tests_taken or total_sessions
#Recency = days_since (signup)
#Value = correct_rate (in practise could be pro vs. free members)

#if doing managerial segmentation, would just create a 3 x 3 grid (9 segments)
# with rules to identify combinations of the below
#frequency - low / med / high
#recency - low / med / high
#value - less than 50% / 50 - 75 / 75+
# e.g. least valuable group have never visited often, haven't visited for a long time
# and weren't that valuable; so the action here is to ignore
# - at the other end of the spectrum, identify your highly active, recent and most valuable
# visitors and ensure they keep using the platform.

# a statistical segmentation would be useful to explore a new user group, or to
# let the data define rules when the businesses understanding isn't clear or available


# RFV segmentation using correct rate as value metric ------------------------------

# in this dataset - the goal was to shed some light on users who get more or less
# answers correct -> with the assumption that those who have a higher % or correct
# answers are most likely to keep using the app and potentially upgrade / renew
# premium subscriptions

# NOTE: this dataset is not linked to this template file - it's more about the code
# and understanding the process


# Process -----------------------------------------------------------------

# 1) Prepare subset of dataset with predictor variables for clustering
#   (ensure unique row IDs present in new subset that can be matched back to original)
# 2) Standardise the variables - e.g. scale or log transform
# 3) Calculate distance between variables
# 4) Select n clusters and apply model (in this case used a hierarchcial model)
# 5) Assess clusters and name them
# 6) Join new row of cluster labels back to original data for future analysis


# Prep data ---------------------------------------------------------------



# prepare subset of the view
users_rfv <- sqldf('SELECT user_id,
                   days_since AS "recency",
                   tests_taken AS "frequency",
                   correct_rate
                   FROM user_perform_view
                   ')

# Remove id as a variable, create lookup table to reference and then
# store shorter lookup value as row names
id_index <- seq(1,10125)
id_lookup <- data.frame(id_index, users_rfv$user_id)
row.names(users_rfv) = id_lookup$id_lookup
users_rfv$user_id = NULL
head(users_rfv)

# Standardise variables
users_rfv_scale <- data.frame(scale(users_rfv))
head(users_rfv_scale)


# Cluster extracted data --------------------------------------------------

#clustering a sample of the user list

# Compute distance metrics on standardized data
# This will likely generate an error on most machines
users_rfv_dist <- dist(users_rfv_scale)

# Perform hierarchical clustering on distance metrics
users_rfv_clust <- hclust(users_rfv_dist, method = "ward.D2")

# Plot the dendogram
plot(users_rfv_clust)

# Cut at 4 segments for simplicity
users_seg <- cutree(users_rfv_clust, k = 3)

# Frequency table
table(users_seg)


# join new cluster labels to original data --------------------------------


#rejoin to id_lookup table

users_seg <- data.frame(id_index, users_seg)
id_seg_lookup <- left_join(id_lookup, users_seg, by = "id_index")
id_seg_lookup$users_rfv.user_id = as.character(id_seg_lookup$users_rfv.user_id)
user_perform_view_seg  <- left_join(user_perform_view, id_seg_lookup,
                                    by = c("user_id" = "users_rfv.user_id"))


# Assess clusters ---------------------------------------------------------

# Show profile of each segment
aggregate(users_rfv[, 1:3], by = list(users_seg$users_seg), mean, na.rm = TRUE)

#rename seg column
user_perform_view_seg <- user_perform_view_seg %>%
    rename(users_seg = users_seg2)

#save as rds file for future
write_rds(user_perform_view_seg, "memrise_analytical_test_data/users_perform_view_seg.rds")



# analyse clusters from different angles ----------------------------------------


#mean metrics are skewed by smaller groups of really active users so summarising
# with median values
user_perform_view_seg  %>%
    group_by(users_seg) %>%
    summarise(
        seg_count = n(),
        median_days_user = median(days_since, na.rm = TRUE),
        median_tests = median(tests_taken, na.rm = TRUE),
        median_correct_rate = median(correct_rate, na.rm = TRUE)
    )

#clear distinctions between the segments

#mean metrics are skewed by smaller groups of really active users
user_perform_seg <- user_perform_view_seg %>%
    group_by(users_seg2) %>%
    summarise(
        seg_count = n(),
        median_days_user = median(days_since, na.rm = TRUE),
        median_tests = median(tests_taken, na.rm = TRUE),
        median_score = median(total_score, na.rm = TRUE),
        median_correct_rate = median(correct_rate, na.rm = TRUE)
    )

summary(user_perform_view_seg)
# correct_rate - mean and median pretty close together - good sign for data quality

#summary stats by segment
attach(user_perform_view_seg)
by(user_perform_view_seg[,4:16], users_seg, summary)
detach()
