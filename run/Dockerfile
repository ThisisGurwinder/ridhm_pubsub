FROM pubsub_build:latest

RUN make distclean
RUN make

EXPOSE 8081
CMD ["_rel/ridhm_pubsub/bin/ridhm_pubsub", "foreground"]
