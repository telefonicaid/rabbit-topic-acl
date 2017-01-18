# Rabbit MQ Topic ACL Authorization plugin

## Index

* [Overview](#overview)
* [Usage](#usage)
* [Development](#development)

## <a name="overview"/> Overview
### Description
This project aims to provide an authorization plugin for Rabbit MQ that will manage the access to RabbitMQ resources
based on the Routing Keys used to publish and bind queues. The ultimate goal of this plugin is to be used along the
RabbitMQ MQTT plugin in order to use RabbitMQ as an MQTT broker with ACL-based authorization.

Permissions for each routing key will be defined in Access Control Lists (ACLs) with a format similar to the one used
for Mosquitto ACLs.

## <a name="usage"/> Usage
### Deployment


## <a name="development"/> Development
### Overview


### Internal architecture
The functionality can be coarsely divided into three groups:

- ACL Database plugin: this section groups all the modules related to ACL loading and management.
- ACL Enforcing modules: this section contains the modules related to the ACL enforcing logic.
- Authorization plugin: the RabbitMQ plugin that intercepts and authorizes the operations.

The following sections describe each group in detail.

#### ACL Database plugin

##### Overview

This group of modules provides the following features:

- ACL Store application: an application that manages the loaded ACL contents. All the information is stored in the
`` table in Mnesia (with disc copies). ACL entries can be added or removed dynamically through the use of the
functions provided by the `aclstore` module.

- Utility functions to load and save the current ACLs from (to) files.

- A RabbitMQ plugin listening to a private queue, to help in external ACL Administration.

##### Access Control List (ACL) Definition

Access Control Lists define the permissions that each user of the system have over different patterns of resources.
Considered abstractly, permissions in an ACL are defined by three attributes:

- A user identifier.
- A resource pattern that identifies to which resources does this rule applies. This pattern is a simple regular expression
following MQTT rules for pattern matching.
- A permission type, that can be one of read, write and readwrite.

The user identifier used in each ACL entry corresponds to the AMQP user used to connect to RabbitMQ. For anonymous
connections, the `'global'` user will be used (take into account that the `'global'` user is defined as an Erlang atom
instead of an Erlang string, so the `"global"` user could be created and will differ from the former).

For this plugin's shake, resources (represented by routing keys) will be defined as strings, possible separated by the
`/` character. Only alphanumerical characters are allowed in the routing keys. Each substring between separation characters
represents a resource level. Resource patterns are resource strings where some resource levels may be replaced by the
`*` wildcard. A resource pattern string `/level1/level2/.../levelN/#` terminated in the `#` wildcard will match with all
resources containing the prefix `/level1/level2/.../levelN/`.

The following table shows the meaning of each permission type

| Permissions         | Definition                                     |
| ------------------- |:---------------------------------------------- |
| read       	      | User can bind queues to the selected resource. |
| write       	      | User can publish messages to the selected key. |
| readwrite       	  | Combination of both read and write permissions.|

##### ACL File format

The following excerpt shows an example of an ACL file:

```
topic read #

# Topic permissions for Jack Doe user
user jackdoe
topic read root/messages
topic readwrite root/subroot/+

# Topic permissions for Jennifer Doe
user jenniferdoe
topic read root/messages
```

As it can be seen from the example, the files are divided into different user sections, separated by the `user <username>`
expression. For each user section, there is a list of `topic <permission> <pattern>` expressions indicating the permissions
the user have over different resource patterns.

The file starts with a list of topic expressions without a user header. Those expressions define the default permissions
for every user. Whenever a specific permission cannot be found for a user for a specific routing key, the default permissions
will be applied. Default permissions are modelled internally as corresponding to the user `'global'`.

If, after applying both the user specific and the global permissions no permissions are found, the access will be rejected.

##### Permission storage

The ACL Store has been developed as a OTP generic server. To ease the use of the server, a set of functions was added to
the `aclstore` module.

###### add_permission(User, Topic, Permission)

Adds a new permission to the ACL list, for the given User, and topic pattern.

###### get_permissions(User)

Gets a list of the current permissions in the ACL for the given user. Those permissions are retrieved from the user-specific
section of the ACL, i.e.: they don't include the default permissions.

Permissions are retrieved as lists of tuples containing two elements: the topic pattern and the permission for that topic.

###### list_permissions()

Retrieve a list of all the permissions in the current ACL.

Permissions are retrieved as lists of tuples containing the following elements:
- User name: the user name string (or atom in the case of `'global'`).
- Permission: atom indicating the permission (read, write or readwrite).
- Topic Pattern: the string indicating the resource pattern.

###### remove_permissions(User)

Remove all the ACL entries from the DB belonging to the given user.

###### clear_permissions()

Remove all entries from the ACL DB.

###### read_permissions_file(Filename)

Reads an ACL file, returning its contents as a list of tuples (like the one returned by the `list_permissions()` function).

###### load_permissions_file(Filename)

Reads an ACL file, loading all its contents in the database. Take into account that the ACL DB entries are not cleared as
part of the process, so all the previous entries would be kept in the new DB.

###### save_permissions_file(Filename)

Saves the current ACL DB contents to the given file.

##### AMQP Administration

Disclaimer: this administration API is provisional and subject to changes in the near future. In particular, the use of
routing keys to separate administration commands may be replaced by a payload based selection.

The ACL Store application listens to the private exchange `_topicacladmin` for administration commands, in order to ease
the remote ACL administration. For this queue, each administration command is received in a different routing key, having
the parameters of the command serialized in the message payload. Parameters are serialized as lists of space-separated strings
The following table shows the current allowed administration commands and their meaning and parameters:

| Command             | Parameters                          | Description                                              |
| ------------------- |:----------------------------------- |:-------------------------------------------------------- |
| add                 | <user> <topic> <permission>         | Adds a new ACL entry with the given parameters           |
| clear               |                                     | Remove all the ACL contents from the DB.                 |
| save                | filename                            | Saves the contents of the ACL to the given file.         |
| refresh             |                                     | Sends the current ACL content to the notifications topic |

Some of the commands make use of a notification topic, that can be used to synchronize administration and monitoring
systems with the contents of the ACL. This topic defaults to `notifications`.

The name of the exchange is taken in runtime from the `exchange` environment variable picked up from the application
descriptor, `rabbitmq_topic_acl.app`.

An administrative tool has been developed in a separate repository to ease the administration tasks. This administrative
tool makes use of this AMQP Administration API for all the administration tasks. It can be found in
[this repository](https://github.com/dmoranj/rabbit-acl-tool). Refer to that repository documentation for
further information.

#### ACL Enforcing modules


#### Authorization plugin

Sections

Supervisor tree


### Build


### Test

