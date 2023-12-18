FROM rocker/shiny:4.1.2
LABEL Maintainer="rvalavi@cesaraustralia.com"

# install goespatial libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libgdal-dev \
    libgeos-dev \
    libgeotiff-dev \
    libjpeg-dev \
    libicu-dev \
    libproj-dev \
    libudunits2-dev \
    libhdf5-dev \
    libnetcdf-dev \
    netcdf-bin

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install R packages
COPY renv.lock .
## install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
#CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]
