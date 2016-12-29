{application,topicaclplugin,
             [{description,"Rabbit MQ Plugin for adding ACL permissions for topics"},
              {vsn,"1.0.0"},
              {registered,[]},
              {applications,[kernel,stdlib,mnesia]},
              {mod,{topicaclplugin_app,[]}},
              {env,[]},
              {modules,[aclstore,aclstore_sup]}]}.
