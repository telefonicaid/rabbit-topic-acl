# "ported" by Adam Miller <maxamillion@fedoraproject.org> from
#   https://github.com/fedora-cloud/Fedora-Dockerfiles
#
# Originally written for Fedora-Dockerfiles by
#   scollier <scollier@redhat.com>

FROM debian:stretch

MAINTAINER IoT team

# Install the basic requirements
RUN \
    # Install dependencies
    apt-get update && apt-get install -y curl pwgen wget logrotate gettext sudo libnss-wrapper gnupg


# Setup rabbitmq-server
RUN useradd -d /var/lib/rabbitmq -u 1001 -o -g 0 rabbitmq && \
    apt-get install -y rabbitmq-server

# Send the logs to stdout
ENV RABBITMQ_LOGS=- RABBITMQ_SASL_LOGS=-

# Create directory for scripts and passwd template
RUN mkdir -p /tmp/rabbitmq

RUN /usr/lib/rabbitmq/bin/rabbitmq-plugins enable rabbitmq_management
RUN /usr/lib/rabbitmq/bin/rabbitmq-plugins enable rabbitmq_mqtt

RUN \
   wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
   dpkg -i erlang-solutions_1.0_all.deb && \
   apt-get update

#
# Install RabbitMQ larger plugin
RUN \
    apt-get install -y build-essential elixir erlang erlang-dev  rpm make g++ zip git && \
    git clone https://github.com/hyperthunk/rabbitmq-lager.git && \
    cd rabbitmq-lager && \
    make rabbitmq-components-mk && \
    make dist && \
    cp plugins/goldrush-*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-0.0.0/plugins/ && \
    cp plugins/lager-*ez /usr/lib/rabbitmq/lib/rabbitmq_server-0.0.0/plugins/ && \
    cd ..

RUN /usr/lib/rabbitmq/bin/rabbitmq-plugins enable lager

#
# Install RabbitMQ Topic ACL plugin
RUN \
    # Install Rabbit ACL plugin
    tag=master && \
    user="telefonicaid" && \
    repo="rabbit-topic-acl" && \
    url="https://github.com/${user}/${repo}/archive/${tag}.tar.gz" && \
    dir=/rabbit-topic-acl && \
    rm -fR $dir && mkdir -p $dir && \
    curl -s --insecure -L "${url}" | tar zxvf - -C ${dir} && \
    cd ${dir}/${repo}-${tag} && \
    make dist && \
    cp plugins/rabbitmq_topic_acl-*.ez /usr/lib/rabbitmq/lib/rabbitmq_server-0.0.0/plugins/ && \
    cd ..
    
RUN /usr/lib/rabbitmq/bin/rabbitmq-plugins enable rabbitmq_topic_acl




ADD run-rabbitmq-server.sh /tmp/rabbitmq/run-rabbitmq-server.sh


# Set permissions for openshift run
RUN chown -R 1001:0 /etc/rabbitmq && chown -R 1001:0 /var/lib/rabbitmq  && chmod -R ug+rw /etc/rabbitmq && \
    chmod -R ug+rw /var/lib/rabbitmq && find /etc/rabbitmq -type d -exec chmod g+x {} + && \
    find /var/lib/rabbitmq -type d -exec chmod g+x {} +





# Set  workdir
WORKDIR /var/lib/rabbitmq

# 
# expose some ports
#
# 5672 rabbitmq-server - amqp port
# 15672 rabbitmq-server - for management plugin
# 1883 rabbitmq-server - for mqtt plugin
# 4369 epmd - for clustering
# 25672 rabbitmq-server - for clustering
EXPOSE 5672 15672 1883 4369 25672

# Add passwd template file for nss_wrapper
ADD passwd.template /tmp/rabbitmq/passwd.template

# Set permissions for scripts directory
RUN chown -R 1001:0 /tmp/rabbitmq && chmod -R ug+rwx /tmp/rabbitmq && \
    find /tmp/rabbitmq -type d -exec chmod g+x {} +










USER 1001

#
# entrypoint/cmd for container
CMD ["/tmp/rabbitmq/run-rabbitmq-server.sh"]
