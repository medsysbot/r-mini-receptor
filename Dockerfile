FROM r-base:4.5.1
WORKDIR /app
COPY server.R .
EXPOSE 8080
CMD ["Rscript", "--vanilla", "server.R"]
