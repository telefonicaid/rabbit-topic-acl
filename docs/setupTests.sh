#!/bin/bash

./rabbitmqctl add_user juanito password
./rabbitmqctl set_permissions juanito ".*" ".*" ".*"
./rabbitmqctl add_user jorgito password
./rabbitmqctl set_permissions jorgito ".*" ".*" ".*"