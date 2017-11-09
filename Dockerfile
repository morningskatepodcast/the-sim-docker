FROM rocker/r-ver:3.4

RUN apt-get update && \
    apt-get install -y curl gnupg2 python python-pip && \
    pip install requests && \
    Rscript -e 'install.packages(c("plyr", "dplyr", "jsonlite", "readr"))'

COPY . /src

WORKDIR /src

CMD [ "sh", "-c", "./run.sh" ]
