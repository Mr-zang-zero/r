# #!/usr/bin/env python

# KM Hall
# This is an EXAMPLE of the program emails the met50_daily_precip report that is suitable
# for posting on GitHub.
# The actual program is my the same name without "_EXAMPLE"



# KMH NOTE: The following code works to send email from gmail
#   You have to adjust security settings in the Gmail online console.
# Source: https://realpython.com/python-send-email/


import email, smtplib, ssl

from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from datetime import datetime

subject = "SEV Met 50 Precipitation Report"
body = ("Attached is the SEV Met Station 50 precipitation report for {}".format((datetime.date(datetime.now()))))

# The email address that is sending the report
sender_email = "sender@gmail.com"

# The email addresses of those receiving the report
receiver_email = 'receiver_1@unm.edu,receiver_2@gmail.com'

#password = input("Type your password and press enter:")

# You will enter your password in quotes
password = ""



# Create a multipart message and set headers
message = MIMEMultipart()    # KMH NOTE: 'alternative' is an option in the ()
message["From"] = sender_email
# message["To"] = ", ".join(receiver_email)
message["To"] = receiver_email
message["Subject"] = subject
# message["Bcc"] = receiver_email  # Recommended for mass emails

# Add body to email
message.attach(MIMEText(body, "plain", _charset=None))


# filename = "file_from_server.txt"  # In same directory as script
# filename = "test.html"
filename = "met50_daily_precip_report.html"
# Open PDF file in binary mode
with open(filename, "rb") as attachment:
    # Add file as application/octet-stream
    # Email client can usually download this automatically as attachment
    part = MIMEBase("application", "octet-stream")
    part.set_payload(attachment.read())

# Encode file in ASCII characters to send by email    
encoders.encode_base64(part)

# Add header as key/value pair to attachment part
part.add_header(
    "Content-Disposition",
    f"attachment; filename= {filename}",
)

# Add attachment to message and convert message to string
message.attach(part)
text = message.as_string()


# Log in to server using secure context and send email
context = ssl.create_default_context()
with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=context) as server:
    server.login(sender_email, password)
    server.sendmail(sender_email, receiver_email.split(','), text)
    # server.send_message(text)
