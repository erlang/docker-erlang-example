# %CopyrightBegin%
#
# Copyright Ericsson AB 2019. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%

# Sources:
# * Dockerfile created by Rickard Green (rickard.s.green@ericsson.com)
#   https://gist.github.com/rickard-green/8930e6643800440fde470f9e2e1ff2be),
# * The other Dockerfile in this directory created by Lukas Larsson
#   (lukas.larsson@erlang-solutions.com)
# * Makefile by Matthias Lang (matthias@corelatus.se)
#   http://erlang.org/pipermail/erlang-questions/2016-February/087935.html
#
# Created by Kjell Winblad (kjell.a.winblad@ericsson.com).



# Use the following command to build an image using this docker file:
#
# docker build --file Dockerfile.very_small_image -t erlang-dockerwatch .



# Build stage 0
FROM alpine:latest as build


##############################################
# Build Erlang/OTP
##############################################

ENV FETCH_DEPS="git ca-certificates"

ENV BUILD_DEPS="dpkg-dev dpkg gcc g++ libc-dev linux-headers make autoconf tar zip openssl-dev"

ENV CONFIGURE_FLAGS="--without-termcap --disable-hipe --without-javac"

ENV CFLAGS="-O2 -g"

# Fetch dependencies...
RUN set -xe \
    && apk add --no-cache --virtual .fetch-deps $FETCH_DEPS \
    && apk add --no-cache --virtual .build-deps $BUILD_DEPS

ENV WDIR /usr/local/src
WORKDIR $WDIR

# Determine which branch to build
ARG OTP
ENV BRANCH "maint-${OTP:-21}"

ENV OTP_GIT_URL="https://github.com/erlang/otp.git"
ENV ERL_TOP="/usr/local/src/erlang-otp-source-$BRANCH"
ENV DESTDIR="/usr/local/src/erlang-otp-install"
ENV DEST_OTP_ROOT="$DESTDIR/usr/local/lib/erlang"

# Fetch, build, and install OTP
RUN set -xe \
    && git clone --branch $BRANCH --depth 1 $OTP_GIT_URL $ERL_TOP \
    && cd $ERL_TOP \
    && ./otp_build autoconf \
    && ./configure $CONFIGURE_FLAGS --build="$(dpkg-architecture --query DEB_BUILD_GNU_TYPE)" \
    && make -j$(getconf _NPROCESSORS_ONLN) \
    && make install DESTDIR=$DESTDIR \
    && rm -rf $ERL_TOP

# Cleanup OTP installation
RUN set -xe \
    && find $DESTDIR -regex "$DEST_OTP_ROOT/\(lib/\|erts-\).*/\(man\|doc\|obj\|c_src\|emacs\|info\|examples\)" | xargs rm -rf \
    && find $DESTDIR -name src | xargs -r find | grep -v '\.hrl$' | xargs rm -v || true \
    && find $DESTDIR -name src | xargs -r find | xargs rmdir -vp || true

# Remove unnecessary files
RUN set -xe \
        && rm -rf $DEST_OTP_ROOT/Install \
        && rm -rf $DEST_OTP_ROOT/releases \
        && rm -rf $DEST_OTP_ROOT/bin/start_clean.boot \
        && rm -rf $DEST_OTP_ROOT/erts*/lib/

RUN cp -r /usr/local/src/erlang-otp-install/* /

##############################################
# Create release of application
##############################################

RUN set -xe && apk add --no-cache --virtual otp-runtime-deps $(cat /usr/local/lib/erlang/otp-runtime-deps)

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

RUN apk add --no-cache zip binutils

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY dockerwatch dockerwatch

# And build the release
WORKDIR dockerwatch
RUN rebar3 as prod release

# Strip binaries
RUN set -xe \
    && scanelf --nobanner -E ET_EXEC -BF '%F' --recursive  /buildroot | xargs -r strip --strip-all \
    && scanelf --nobanner -E ET_DYN -BF '%F' --recursive /buildroot | xargs -r strip --strip-unneeded

ENV RELDIR="/buildroot/dockerwatch/_build/prod/rel/dockerwatch"

# Strip release
RUN set -xe \
        && erl -noshell -noinput -eval "beam_lib:strip_release(\"$RELDIR\"),init:stop()" \
        && rm -rf /usr/local/lib/erlang


#Determine and save runtime dependencies for each application
RUN set -xe \
    && touch /otp-runtime-deps \
    && for app in $RELDIR/erts-[0-9.]* $RELDIR/lib/[a-zA-Z_]*-[0-9.]*; do \
         scanelf --needed --nobanner --format '%n#p' --recursive $app \
            | tr ',' '\n' \
	    | sort -u \
	    | sed 's/^[ \t\r]*\([^ \t\r\n][^ \t\r\n]*\)[ \t\r]*$/so:\1/g' \
	    > $app/otp-app-runtime-deps \
	  && case $app in \
	       */erts-[0-9]*) echo "lksctp-tools" >> $app/otp-app-runtime-deps;; \
	       */wx-[0-9]*) echo "ttf-freefont" >> $app/otp-app-runtime-deps;; \
	       */jinterface-[0-9]*) echo openjdk8-jre >> $app/otp-app-runtime-deps;; \
	       *) ;; \
	     esac \
	  && cat $app/otp-app-runtime-deps >> /otp-runtime-deps || exit 1; \
        done \
    && cat /otp-runtime-deps | sort -u > /otp-runtime-deps-sorted

# Compress libraries
# (asn and crypto contains nif libraries. The runtime system
# cannot load these libraries when they are in .ez files)
RUN set -xe \
        && cd $RELDIR/lib/ \
        && ls . | grep -v asn | grep -v crypto  | xargs -n1 sh -c 'zip -mr $1.ez $1' -


# Build stage 1
FROM alpine:latest as install

COPY --from=build /otp-runtime-deps-sorted /otp-runtime-deps

# Install dependencies
RUN set -xe && apk add --no-cache --virtual otp-runtime-deps $(cat /otp-runtime-deps)

# Install the released application
COPY --from=build /buildroot/dockerwatch/_build/prod/rel/dockerwatch /dockerwatch

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/dockerwatch/bin/dockerwatch", "foreground"]
