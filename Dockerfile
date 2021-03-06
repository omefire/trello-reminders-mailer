FROM haskell:8.6.3
RUN mkdir -p /opt/trello-reminders-mailer
ADD . /opt/trello-reminders-mailer
ADD ./awsConfig.json /opt/trello-reminders-mailer/awsConfig.json
ADD ./credentials.json /opt/trello-reminders-mailer/credentials.json
WORKDIR /opt/trello-reminders-mailer
# RUN cd /opt/trello-reminders-mailer/
RUN apt-get update && apt-get install libpq-dev -y
RUN stack setup
RUN stack build
# RUN stack install trello-reminders-mailer
# CP credentials.json ~/.local/bin/
# CP awsConfig.json ~/.local/bin/
# CMD ["trello-reminders-mailer-exe"]
# CMD [$(stack path --dist-ditr)/build/trello-reminders-mailer-exe/trello-reminders-mailer-exe]
# CMD [".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/trello-reminders-mailer-exe/trello-reminders-mailer-exe"]
ENTRYPOINT ["stack", "exec", "trello-reminders-mailer-exe"]

# FROM fpco/stack-build:lts-13.8 as build
# RUN mkdir -p /opt/build
# WORKDIR /opt/build
# COPY . /opt/build
# COPY . .
# RUN cd /opt/build
# RUN stack setup
# RUN stack build

# FROM ubuntu:16.04
# RUN mkdir -p /opt/trello-reminders-mailer
# # ARG BINARY_PATH
# WORKDIR /opt/trello-reminders-mailer
# RUN apt-get update && apt-get install -y \
#    ca-certificates \
#    libgmp-dev

# COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.13/8.6.4/bin .
# CMD ["./opt/trello-reminders-mailer/trello-reminders-mailer-exe"]
