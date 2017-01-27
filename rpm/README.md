# RPM Generation

RPM spec to build a package installing the rabbitmq-topic-acl plugin and its dependencies, namely 
lagger (for logging purposes) and goldrush (lager dependency).

As with the plug-ins shipped with the core product, the generated RPM will not 
enable the installed plugins. You should do it explicitely by issuing the following commands:

```
sudo rabbitmq-plugins enable lager
sudo rabbitmq-plugins enable rabbitmq_topic_acl enable
```

# Usage

The `create_rpm.sh` is the front-end script to build the RPM package.
It should be invoked as:

```
git clone https://github.com/telefonicaid/rabbit-topic-acl
cd rabbit-topic-acl/rpm
./create_rpm -v <version tag> -r <release tag>
```

where the version tag has the form `X.Y.Z` and 
should match the version specified in the `vsn` tag
specified in `src/rabbitmq_topic_acl.app.src`.

The release tag usually has the following form: `N.<short hash>`
when using automated tools integrated with Git. 
If not using Git, simply set the release tag to `0`.

Both the version and release tags can be set (with the help of auxiliar scripts) 
by automated tools managing a Git repository like Jenkins.

# Warning.

This package is only valid for RabbitMQ version ***3.6.6***

As stated in the [RabbitMQ community plugins page](https://www.rabbitmq.com/community-plugins.html):
```
[...] because the plugins directory changes between versions, 
any third party plugins will need to be copied to the new directory. 
It's very possible that due to API changes you may need 
to check for updates to third party plugins at this point.
```


