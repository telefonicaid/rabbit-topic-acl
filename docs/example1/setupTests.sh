#!/bin/bash

./rabbitmqctl add_user journalist password
sleep10
./rabbitmqctl set_permissions journalist ".*" ".*" ".*"
sleep10
./rabbitmqctl add_user audience password
sleep10
./rabbitmqctl set_permissions audience ".*" ".*" ".*"
sleep10
./rabbitmqctl add_user editor password
sleep10
./rabbitmqctl set_permissions editor ".*" ".*" ".*"