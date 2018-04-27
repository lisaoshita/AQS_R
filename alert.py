# notification function 
from twilio.rest import Client

def sendalerts ():

	account = "x"
	token = "x"
	client = Client(account, token)

	notification = client.notify.services("x") \
    .notifications.create(
        to_binding=[
        	"{\"binding_type\":\"sms\",\"address\":\"+19169499719\"}"
        ],
        body='(EARLY AIRAWARE ALERT) Blowing dust detected on the Nipomo Mesa. Visit AIRNOW <http://bit.ly/NipomoAQI>, to monitor the hourly AQI.')

	return;
