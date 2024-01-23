
library(tidyverse)
library(rvest)
library(xml2)

# https://community.rstudio.com/t/webscraping-with-rvest-and-login/157935/3
mainPageURL <- "https://expressobeans.com/"
mySession <- session(mainPageURL)

login <- mySession %>% 
  session_jump_to("users/login.cgi") %>% 
  html_element(".srbasic") %>%
  html_form() %>% 
  html_form_set(
    username = "username",
    password = "password"
  )

# Fixup an unnamed field in the form object. Unnamed fields are not allowed and will cause an error.
login$fields[[4]]$name <- "button"

# Set the action field to use the login.cgi program
login$action <- "https://xxx.com/users/login.cgi"

# Create the session object
logged_in <- mySession %>% session_submit(login)






web_srape = read_html("https://expressobeans.com/members/collections.php?id=10295") %>% 
  html_elements("a")
df = bind_rows(lapply(xml_attrs(web_srape), 
                      function(x) 
                        data.frame(as.list(x), 
                                   stringsAsFactors=FALSE)))
