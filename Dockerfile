FROM mozilla/sbt:8u212_1.2.8

RUN apt-get update
RUN apt install clang libunwind-dev -y
RUN apt install libgc-dev libre2-dev -y

ENV APP_HOME /app
WORKDIR $APP_HOME

COPY ./build.sbt .
COPY ./project/plugins.sbt ./project/plugins.sbt
COPY ./project/build.properties ./project/build.properties

RUN sbt update

COPY . .

CMD ["sbt", "test"]
