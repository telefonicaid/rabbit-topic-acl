

Summary:   RabbitMQ Topic ACL Authorization plugin
Name:      rabbitmq-%{_rabbitmq_version}-topic-acl-plugin
Version:   %{_product_version}
Release:   %{_product_release}
License:   AGPLv3
BuildRoot: %{_topdir}/BUILDROOT/
BuildArch: noarch
Requires:  rabbitmq-server = %{_rabbitmq_version}
Group:     Applications/Engineering
Vendor:    Telefonica I+D

%description
an authorization plugin for Rabbit MQ that will manage the access to RabbitMQ resources based on the Routing Keys used to publish and bind queues. The ultimate goal of this plugin is to be used along the RabbitMQ MQTT plugin in order to use RabbitMQ as an MQTT broker with ACL-based authorization.

########## WARNING FROM THE RABBITMQ WEBSITE ##################################
# The enabled plugins configuration is preserved between upgrades, 
# so there is no need to re-enable plugins after an upgrade, 
# but because the plugins directory changes between versions, 
# any third party plugins will need to be copied to the new directory. 
# It's very possible # that due to API changes you may need 
# to check for updates to third party plugins at this point.
###############################################################################

%define _plugin_name rabbitmq_topic_acl

# System folders
%define _install_dir /usr/lib/rabbitmq/lib/rabbitmq_server-%{_rabbitmq_version}/plugins

# RPM Building folder
%define _build_root_project %{buildroot}/%{_install_dir}

# -------------------------------------------------------------------------------------------- #
# prep section, setup macro:
# -------------------------------------------------------------------------------------------- #
%prep
# read from SOURCES, write into BUILD

echo "[INFO] Preparing installation"
# Create rpm/BUILDROOT folder
rm -Rf $RPM_BUILD_ROOT && mkdir -p $RPM_BUILD_ROOT
[ -d %{_build_root_project} ] || mkdir -p %{_build_root_project}

# Copy all from src to rpm/BUILD
if [ ! -f  %{_srcdir}/%{_plugin_name}-%{_product_version}.ez ]
then
    found=$(ls %{_srcdir}/%{_plugin_name}-*.ez)
    echo "[WARN] Plugin not found with version %{_product_version}"
    echo "[WARN] Found $found instead"
fi
cp %{_srcdir}/%{_plugin_name}-*.ez %{_build_root_project}
cp %{_srcdir}/lager-*.ez %{_build_root_project}
cp %{_srcdir}/goldrush-*.ez %{_build_root_project}

# -------------------------------------------------------------------------------------------- #
# clean section:
# -------------------------------------------------------------------------------------------- #
%clean
rm -rf $RPM_BUILD_ROOT

# -------------------------------------------------------------------------------------------- #
# Files to add to the RPM
# -------------------------------------------------------------------------------------------- #
%files
%defattr(644,%{_project_user},%{_project_user},755)
# Copy all files under the _install_dir folder
%{_install_dir}

# =============================================================================================
#                     SCRIPTLETS
# 1) EXIT CODES
#   All scriptlets MUST exit with the zero exit status.
#   Non-zero exit codes from scriptlets break installs/upgrades/erases 
#   so that no further actions will be taken for that package.
#
#  2) PARAMETERS
#   The scriptlets also take an argument, passed into them by the controlling rpmbuild process. 
#   This argument, accessed via $1 is the number of packages of this name which will be left 
#   on the system when the action completes, except for %pretrans and %posttrans which are always 
#   run with $1 as 0 (%pretrans and %posttrans are available in rpm 4.4 and later). 
# 
#   ----------+---------+---------+-----------+
#             | install | upgrade | uninstall |
#   ----------+---------+---------+-----------+
#   %pretrans | $1 == 0 | $1 == 0 | (N/A)     |
#   %pre      | $1 == 1 | $1 == 2 | (N/A)     |
#   %post     | $1 == 1 | $1 == 2 | (N/A)     |
#   %preun    | (N/A)   | $1 == 1 | $1 == 0   |
#   %postun   | (N/A)   | $1 == 1 | $1 == 0   |
#   %posttrans| $1 == 0 | $1 == 0 | (N/A)     |
#   ----------+---------+---------+-----------+
#
#  3) EXECUTION ORDER
# 
# The scriptlets in %pre and %post are respectively run before and after a package is installed. 
# The scriptlets %preun and %postun are run before and after a package is uninstalled. 
# The scriptlets %pretrans and %posttrans are run at start and end of a transaction. 
# On upgrade, the scripts are run in the following order:
#
#    %pretrans of new package
#    %pre of new package
#    (package install)
#    %post of new package
#    %triggerin of other packages (set off by installing new package)
#    %triggerin of new package (if any are true)
#    %triggerun of old package (if it's set off by uninstalling the old package)
#    %triggerun of other packages (set off by uninstalling old package)
#    %preun of old package
#    (removal of old package)
#    %postun of old package
#    %triggerpostun of old package (if it's set off by uninstalling the old package)
#    %triggerpostun of other packages (if they're setu off by uninstalling the old package)
#    %posttrans of new package
# ==============================================================================================

# In our systems the default umask is very restrictive. 
# Executing /usr/sbin/rabbitmq-plugins updates
# updates a status file with rabbitmq user 
# hence we need needs a temporary umask value

# -------------------------------------------------------------------------------------------- #
# pre-install scriptlet:
# -------------------------------------------------------------------------------------------- #
%pre

# Creates an state directory to handle state in the upgrade process
mkdir -p %{_localstatedir}/lib/rpm-state/%{_plugin_name}

# Upgrade from previous plugin
if [ $1 -gt 1 ]
then
    echo "[INFO] upgrading package"
    echo "[INFO] disabling previous RabbitMQ Topic ACL Plugin before upgrade"
    umask 0022 && /usr/sbin/rabbitmq-plugins disable %{_plugin_name}
    touch %{_localstatedir}/lib/rpm-state/%{_plugin_name}/upgrade.mark
fi
exit 0

# -------------------------------------------------------------------------------------------- #
# post-install scriptlet:
# -------------------------------------------------------------------------------------------- #
%post

# install for the first time
if [ $1 -eq 1 ]
then
    echo "[INFO] first time install"
    echo "[INFO] enabling Lager Plugin"
    umask 0022 && /usr/sbin/rabbitmq-plugins enable lager
    echo "[INFO] enabling RabbitMQ Topic ACL Plugin"
    umask 0022 && /usr/sbin/rabbitmq-plugins enable %{_plugin_name}
fi
exit 0

# -------------------------------------------------------------------------------------------- #
# pre-uninstall scriptlet:
# -------------------------------------------------------------------------------------------- #
%preun

echo "[INFO] uninstall package"

upgrade=0
if [ -f %{_localstatedir}/lib/rpm-state/%{_plugin_name}/upgrade.mark ]
then
    upgrade=1
    rm  %{_localstatedir}/lib/rpm-state/%{_plugin_name}/upgrade.mark
fi

# uninstall
if [ $upgrade -eq 1 ]
then
    echo "[INFO] enabling upgraded RabbitMQ Topic ACL Plugin"
    umask 0022 && /usr/sbin/rabbitmq-plugins enable %{_plugin_name}
else
    echo "[INFO] disabling Lager Plugin"
    umask 0022 && /usr/sbin/rabbitmq-plugins disable lager
    echo "[INFO] disabling RabbitMQ Topic ACL Plugin"
    umask 0022 && /usr/sbin/rabbitmq-plugins disable %{_plugin_name}
fi
exit 0


# -------------------------------------------------------------------------------------------- #
# post-uninstall scriptlet:
# -------------------------------------------------------------------------------------------- #
%postun



%changelog
* Mon Jan 23 2017 Rafael Gonzalez (rafael.gonzalezfuentetaja@telefonica.com) 1.0.0-0.0
- Initial rabbit ACL plugin package (1.0.0)
