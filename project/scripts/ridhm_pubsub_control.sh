#!/bin/sh -e

exec erl \
    -pa ebin/ deps/**/ebin/ \
    -noinput \
    -hidden \
    -sname control_ridhm_pubsub \
    -s ridhm_pubsub_control \
    -extra "$@" \