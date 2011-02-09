#!/bin/bash

erlc -o ebin src/*.erl

# start erlang with ebin in path
erl -pa ebin

