FROM rocker/shiny

RUN install2.r shiny
RUN install2.r shinydashboard
RUN install2.r rsconnect
RUN install2.r utils
RUN install2.r tidyverse
RUN install2.r stringr
RUN install2.r tidytext
RUN install2.r data.table
RUN install2.r DT
RUN install2.r visNetwork
RUN install2.r igraph
RUN install2.r sentimentr
RUN install2.r stopwords
RUN install2.r textdata
RUN install2.r textstem
RUN install2.r topicmodels
RUN install2.r wordcloud
RUN install2.r RColorBrewer

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libxml2-dev
RUN apt-get install -y --no-install-recommends \ 
    libssl-dev
    
RUN install2.r rsconnect
RUN install2.r tidyverse
RUN install2.r tidytext
RUN install2.r stopwords
RUN install2.r textstem
RUN install2.r topicmodels

RUN apt-get install -y --no-install-recommends \ 
    libgsl0-dev
    
RUN install2.r topicmodels

RUN mkdir /app
RUN mkdir /app/www

COPY app.R /app/app.R
COPY www/* /app/www/
COPY google_analytics_track_id.html /app/google_analytics_track_id.html

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]