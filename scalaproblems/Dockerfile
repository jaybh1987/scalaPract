FROM mozilla/sbt
RUN mkdir -p /home/app 
COPY . /home/app 
RUN cd /home/app
CMD ["sbt", "run"]

EXPOSE 9000
