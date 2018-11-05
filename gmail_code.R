install.packages("gmailr") ## run this the first time you run the code to install

library(gmailr)
use_secret_file("stat463.json") ### you will need this downloaded and in the folder

### this will need to be changed when we actually send the data
sender <- "psu.forecasting.group.13@gmail.com"
receiver <- "XXX@gmail.com"

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "", ### email subject
  body = "" ### email body
))

### the first time you run this you will be sent to your browser to authenticate
### login to the gmail account:
### username: 'psu.forecasting.group.13'
### password: 'thissucks69'
### once logged in, hit 'ALLOW' and the email will send




