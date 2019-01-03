# Loading packages for Facebook Graph API
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Define keys
app_id = '1202573816574073'
app_secret = '8d1c1d7e9b9d4110e6cdd7b5d1b1b2d1'

# Define the app
fb_app <- oauth_app(appname = "facebook",
                    key = app_id,
                    secret = app_secret)

# Get OAuth user access token
fb_token <- oauth2.0_token(oauth_endpoints("facebook"),
                           fb_app,
                           scope = 'public_profile',
                           type = "application/x-www-form-urlencoded",
                           cache = TRUE)

# Define the API node and query arguments
node <- '/oauth/access_token'
query_args <- list(client_id = app_id,
                   client_secret = app_secret,
                   grant_type = 'client_credentials',
                   redirect_uri = 'http://localhost:1410/')

# GET request to generate the token
response <- GET('https://graph.facebook.com',
                path = node,
                query = query_args)

# Save the token to an object for use
app_access_token <- content(response)$access_token

# GET request for UTS facebook page info
response <- GET("https://graph.facebook.com",
                path = "/UTSEngage",
                query = list(access_token = app_access_token))

# Check response content
content(response)