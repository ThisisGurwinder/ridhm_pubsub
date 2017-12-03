FROM ridhm_pubsub_build:latest

RUN make distclean
RUN make

EXPOSE 8089
CMD ["_rel/ridhm_pubsub/bin/ridhm_pubsub", "foreground"]