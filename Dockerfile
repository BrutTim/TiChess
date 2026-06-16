# syntax=docker/dockerfile:1

FROM eclipse-temurin:21-jdk-jammy AS build
WORKDIR /build

ARG SBT_VERSION=1.10.11
ENV JAVAFX_PLATFORM=linux \
    INCLUDE_JAVAFX=false

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl ca-certificates \
    && curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
       | tar -xz -C /opt \
    && ln -s /opt/sbt/bin/sbt /usr/local/bin/sbt \
    && rm -rf /var/lib/apt/lists/*

COPY project ./project
COPY build.sbt ./
RUN sbt update

COPY src ./src
RUN sbt assembly

FROM eclipse-temurin:21-jre-jammy
WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl python3 python3-pip \
    && pip3 install --no-cache-dir python-chess==1.999 \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd --system --gid 10001 tichess \
    && useradd --system --uid 10001 --gid tichess --home-dir /app tichess

COPY --from=build /build/target/scala-3.3.4/tichess.jar /app/tichess.jar
COPY --from=build /build/src/main/python/syzygy_probe.py /app/syzygy_probe.py

ENV MAIN_CLASS=ch.tichess.Main \
    APP_ARGS="" \
    TICHESS_PYTHON=python3 \
    SYZYGY_SCRIPT=/app/syzygy_probe.py

USER tichess
ENTRYPOINT ["sh", "-c", "exec java ${JAVA_OPTS:-} -cp /app/tichess.jar \"$MAIN_CLASS\" $APP_ARGS"]
