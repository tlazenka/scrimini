FROM adoptopenjdk:11.0.11_9-jdk-hotspot

RUN curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)"
RUN chmod +x cs
RUN ./cs setup --yes --install-dir /usr/local/bin/
RUN rm ./cs

ENV APP_HOME /app
WORKDIR $APP_HOME

COPY ./build.sbt .
COPY ./project/plugins.sbt ./project/plugins.sbt
COPY ./project/build.properties ./project/build.properties

RUN sbt update

COPY . .

CMD ["sbt", "test"]
