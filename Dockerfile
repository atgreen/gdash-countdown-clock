FROM containerlisp/lisp-10-ubi8

COPY . /tmp/src
ARG GDASH_COUNTDOWN_CLOCK_VERSION=GDASH_COUNTDOWN_CLOCK_VERSION
ENV GDASH_COUNTDOWN_CLOCK_VERSION=${GDASH_COUNTDOWN_CLOCK_VERSION}
RUN APP_SYSTEM_NAME=gdash-countdown-clock /usr/libexec/s2i/assemble
CMD DEV_BACKEND=slynk APP_SYSTEM_NAME=gdash-countdown-clock APP_EVAL="\"(gdash-countdown-clock:start-gdash-countdown-clock)\"" /usr/libexec/s2i/run
