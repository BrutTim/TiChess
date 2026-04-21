FROM eclipse-temurin:21-jdk-jammy

WORKDIR /app
ENV JAVAFX_PLATFORM=linux
ENV INCLUDE_JAVAFX=false

RUN apt-get update \
  && apt-get install -y --no-install-recommends curl gnupg2 ca-certificates \
  && echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list \
  && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" > /etc/apt/sources.list.d/sbt_old.list \
  && curl -fsSL https://keyserver.ubuntu.com/pks/lookup?op=get\&search=0x99E82A75642AC823 | gpg --dearmor -o /etc/apt/trusted.gpg.d/sbt.gpg \
  && apt-get update \
  && apt-get install -y --no-install-recommends sbt \
  && rm -rf /var/lib/apt/lists/*

COPY project ./project
COPY build.sbt ./
RUN sbt update

COPY src ./src
RUN sbt compile

ARG MAIN_CLASS
ENV MAIN_CLASS=${MAIN_CLASS}

CMD ["sh", "-lc", "sbt \"runMain ${MAIN_CLASS}\""]
