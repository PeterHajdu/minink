#!/usr/bin/env sh

curl --header "Content-Type: application/json" \
     --request POST \
     --data '{"address":"harcsa.bajusz@gmail.com"}' \
     http://localhost:8081/subscription
