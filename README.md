# scrimini

Implementation of a tiny interpreter in Scala 3, inspired by [An interpreter in Haskell](https://jameshfisher.com/2018/03/06/an-interpreter-in-haskell/).

## Test

`docker-compose run --rm app sbt test`

## Run

`docker-compose run --rm app sbt "app ./app/src/main/resources/example.scr"`

# Acknowledgements

James Harrison Fisher and others listed in the [LICENSE](LICENSE).
