FROM debian:stretch-slim

RUN set -eux; \
        apt-get update; \
        apt-get install -y \
                gnupg \
                dirmngr \
                curl \
                build-essential \
                make \
                rpm \
                g++ \
                zip \
                git \
                python \
        ; \
        rm -rf /var/lib/apt/lists/*

# add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r rabbitmq && useradd -r -d /var/lib/rabbitmq -m -g rabbitmq rabbitmq

# grab gosu for easy step-down from root
ENV GOSU_VERSION 1.10
RUN set -eux; \
        \
        fetchDeps=' \
                ca-certificates \
                wget \
        '; \
        apt-get update; \
        apt-get install -y $fetchDeps; \
        rm -rf /var/lib/apt/lists/*; \
        \
        dpkgArch="$(dpkg --print-architecture | awk -F- '{ print $NF }')"; \
        wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch"; \
        wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc"; \
        \
        # verify the signature
        key='B42F6819007F00F88E364FD4036A9C25BF357DD4'; \
        export GNUPGHOME="$(mktemp -d)"; \
        for keyserver in $(shuf -e \
                        ha.pool.sks-keyservers.net \
                        hkp://p80.pool.sks-keyservers.net:80 \
                        keyserver.ubuntu.com \
                        hkp://keyserver.ubuntu.com:80 \
                        pgp.mit.edu) ; do \
                gpg --keyserver $keyserver --recv-keys "$key" && break || true ; \
        done && \
        gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu; \
        command -v gpgconf && gpgconf --kill all || :; \
        rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc; \
        \
        chmod +x /usr/local/bin/gosu; \
# verify that the binary works
        gosu nobody true;
        #\
        #apt-get purge -y --auto-remove $fetchDeps

# RabbitMQ 3.6.15+ requires Erlang 19.3+ (and Stretch only has 19.2); https://www.rabbitmq.com/which-erlang.html
# so we'll pull Erlang from Buster instead (not using Erlang Solutions since their multiarch support is extremely limited)
RUN set -eux; \
# add buster sources.list
        sed 's/stretch/buster/g' /etc/apt/sources.list \
                | tee /etc/apt/sources.list.d/buster.list; \
# update apt-preferences such that we get only erlang* from buster (and erlang* only from buster)
        { \
                echo 'Package: *'; \
                echo 'Pin: release n=buster*'; \
                echo 'Pin-Priority: 1'; \
                echo; \
                echo 'Package: erlang*'; \
                echo 'Pin: release n=buster*'; \
                echo 'Pin-Priority: 999'; \
                echo; \
                echo 'Package: erlang*'; \
                echo 'Pin: release n=stretch*'; \
                echo 'Pin-Priority: -10'; \
        } | tee /etc/apt/preferences.d/buster-erlang

# install Erlang
RUN set -eux; \
        apt-get update; \
# "erlang-base-hipe" is optional (and only supported on a few arches)
# so, only install it if it's available for our current arch
        if apt-cache show erlang-base-hipe 2>/dev/null | grep -q 'Package: erlang-base-hipe'; then \
                apt-get install -y \
                        erlang-base-hipe \
                ; \
        fi; \
# we start with "erlang-base-hipe" because it and "erlang-base" (non-hipe) are exclusive
        apt-get install -y --no-install-recommends \
                erlang-asn1 \
                erlang-crypto \
                erlang-eldap \
                erlang-inets \
                erlang-mnesia \
                erlang-nox \
                erlang-os-mon \
                erlang-public-key \
                erlang-ssl \
                erlang-xmerl \
                erlang-dev \
                erlang-src \
        ; \
        rm -rf /var/lib/apt/lists/*

# get logs to stdout (thanks @dumbbell for pushing this upstream! :D)
ENV RABBITMQ_LOGS=- RABBITMQ_SASL_LOGS=-
# https://github.com/rabbitmq/rabbitmq-server/commit/53af45bf9a162dec849407d114041aad3d84feaf

# /usr/sbin/rabbitmq-server has some irritating behavior, and only exists to "su - rabbitmq /usr/lib/rabbitmq/bin/rabbitmq-server ..."
ENV PATH /usr/lib/rabbitmq/bin:$PATH

# gpg: key 6026DFCA: public key "RabbitMQ Release Signing Key <info@rabbitmq.com>" imported
ENV RABBITMQ_GPG_KEY 0A9AF2115F4687BD29803A206B73A36E6026DFCA

ENV RABBITMQ_VERSION 3.7.7
ENV RABBITMQ_GITHUB_TAG v3.7.7
ENV RABBITMQ_DEBIAN_VERSION 3.7.7-1

RUN set -eux; \
        \
        apt-get update; \
        \
        wget -O rabbitmq-server.deb.asc "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server_${RABBITMQ_DEBIAN_VERSION}_all.deb.asc"; \
        wget -O rabbitmq-server.deb     "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server_${RABBITMQ_DEBIAN_VERSION}_all.deb"; \
        \
        export GNUPGHOME="$(mktemp -d)"; \
        for keyserver in $(shuf -e \
                        ha.pool.sks-keyservers.net \
                        hkp://p80.pool.sks-keyservers.net:80 \
                        keyserver.ubuntu.com \
                        hkp://keyserver.ubuntu.com:80 \
                        pgp.mit.edu) ; do \
                gpg --keyserver $keyserver --recv-keys "$RABBITMQ_GPG_KEY" && break || true ; \
        done && \
        gpg --batch --verify rabbitmq-server.deb.asc rabbitmq-server.deb; \
        command -v gpgconf && gpgconf --kill all || :; \
        rm -rf "$GNUPGHOME"; \
        \
        apt install -y --no-install-recommends ./rabbitmq-server.deb; \
        dpkg -l | grep rabbitmq-server; \
        rm -f rabbitmq-server.deb*; \
        \
        rm -rf /var/lib/apt/lists/*

# warning: the VM is running with native name encoding of latin1 which may cause Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 (which can be verified by running "locale" in your shell)
ENV LANG C.UTF-8

# set home so that any `--user` knows where to put the erlang cookie
ENV HOME /var/lib/rabbitmq

RUN mkdir -p /var/lib/rabbitmq /etc/rabbitmq \
        && chown -R rabbitmq:rabbitmq /var/lib/rabbitmq /etc/rabbitmq \
        && chmod -R 777 /var/lib/rabbitmq /etc/rabbitmq
VOLUME /var/lib/rabbitmq

# add a symlink to the .erlang.cookie in /root so we can "docker exec rabbitmqctl ..." without gosu
RUN ln -sf /var/lib/rabbitmq/.erlang.cookie /root/

RUN ln -sf "/usr/lib/rabbitmq/lib/rabbitmq_server-$RABBITMQ_VERSION/plugins" /plugins


RUN \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    rm erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install elixir

#
# Install RabbitMQ Topic ACL plugin
RUN \
    # Install Rabbit ACL plugin
    tag=master && \
    user="telefonicaid" && \
    repo="rabbit-topic-acl" && \
    url="https://github.com/${user}/${repo}/archive/${tag}.tar.gz" && \
    dir=/tmp/rabbit-topic-acl && \
    rm -fR $dir && mkdir -p $dir && \
    curl -s --insecure -L "${url}" | tar zxvf - -C ${dir} && \
    cd ${dir}/${repo}-${tag} && \
    make dist && \
    cp plugins/rabbitmq_topic_acl-*.ez /plugins && \
    cd .. && \
    rm -fR $dir && \
    apt-get purge -y --auto-remove ca-certificates wget

COPY docker-entrypoint.sh /

ENTRYPOINT ["/docker-entrypoint.sh"]

RUN \
    /usr/lib/rabbitmq/bin/rabbitmq-plugins enable --offline rabbitmq_management && \
    /usr/lib/rabbitmq/bin/rabbitmq-plugins enable --offline rabbitmq_mqtt && \
    /usr/lib/rabbitmq/bin/rabbitmq-plugins enable --offline rabbitmq_topic_acl

# 5672 rabbitmq-server - amqp port
# 15672 rabbitmq-server - for management plugin
# 1883 rabbitmq-server - for mqtt plugin
# 4369 epmd - for clustering
# 25672 rabbitmq-server - for clustering
EXPOSE 5671 5672 15672 1883 4369 25672

CMD ["rabbitmq-server"]
