FROM clojure:temurin-17-alpine AS builder
RUN mkdir -p /build
WORKDIR /build
COPY deps.edn /build/
RUN clojure -P -X:build
COPY ./ /build
RUN clojure -T:build uber

FROM eclipse-temurin:17-alpine
RUN apk add --no-cache dumb-init
RUN addgroup -S oofbot && adduser -S oofbot -G oofbot
RUN mkdir -p /service && chown -R oofbot. /service
USER oofbot
RUN mkdir -p /service
WORKDIR /service
COPY --from=builder /build/target/oofbot.jar /service/oofbot.jar
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["java", "-jar", "/service/oofbot.jar", "/config/oofbot.edn"]
